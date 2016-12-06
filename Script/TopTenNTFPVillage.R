#-------------------------------
# Top 10 villages for different NTFPs
# -
#-------------------------------
# Author:   VR
# Date:     25-10-2016
# Modified: 
# Source:   D:\8_TABI\02_NTFPdb\00_OriginalData\1_NTFP_DB_Analysis_AH
#-------------------------------
# 

library(ggplot2)
library(waffle)
library(RColorBrewer)

setwd("D:/8_TABI/02_NTFPdb/00_OriginalData/4_NTFP_DB_CDE")
outwd <- "D:/8_TABI/02_NTFPdb/NTFP data analysis/NTFPAnalysis-figure/"

code <- read.csv("D:/8_TABI/02_NTFPdb/00_OriginalData/TABI_PDK_codes.csv", 
                 header=TRUE, stringsAsFactors = FALSE)
ntfpdb <- read.csv("NTFP_2016.csv", header=TRUE, stringsAsFactors = FALSE)
ntfpdb_cl <- ntfpdb[,c(8,5,13:22,24,26,27,29:56, 60:90)]
nt <- ntfpdb_cl

# Merge codes and nt data to get provinces, districts and kumbans for the data----
nt <- merge(code, nt, by.x="Vil_Code")

# Get the province id as a unique number only----

prov_id <- sort(unique(nt$PCODE), decreasing=FALSE)


impNTFP <- list()

coul <- brewer.pal(10, "Paired")


#******************************************************
# Visualizing with treemaps----
# Source: https://rpubs.com/brandonkopp/creating-a-treemap-in-r

library(xtable)
library(dplyr)
library(treemap)


# Sort all the data into the 10 or 20 most important NTFPs
prov_id <- sort(unique(nt$PCODE), decreasing=FALSE)
impNTFP <- list()

for(k in seq_along(prov_id)){
  nt_p <- subset(nt, PCODE==prov_id[k], na.rm=TRUE)
  vil_id <- sort(unique(nt_p$Vil_Code), decreasing=FALSE)
  
  for(i in seq_along(vil_id)){
    
    t <- subset(nt_p, Vil_Code==vil_id[i], na.rm=TRUE)
    
    # Show the 10 most important NTFPs per village
    t <- t[with(t, order(-Qty_All)),]
    t <- t[,c(1:5,7,8,16)]
    t <- t[1:10,]
    t <- t[complete.cases(t),]
    
    impNTFP <- rbind(impNTFP, t)
  }
}



# Data handling----
# group unique NTFP names per district and sum up the quantity
# source: https://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html

by_district <- group_by(impNTFP, Dname)
fps <- summarise(by_district, 
                 count=n(),
                 ntfp =n_distinct(Scientific_Name_Cleaned),
                 ntfps = mean(Qty_All, na.rm=TRUE))


by_NTFP <- group_by(impNTFP, DCODE, Dname, Scientific_Name_Cleaned, PCODE, Pname, Vil_Code, Vil_Name)

# Sum up unique NTFPs per district
(per_ntfp <- summarise(by_NTFP, ntfp=sum(Qty_All)))
(per_ntfp <- arrange(per_ntfp, PCODE, Pname, DCODE, Dname, desc(ntfp), Vil_Code))
# rename(per_ntfp, Scientific_Name_Cleaned = SciName)

# Sort by district and take only the 10 (or 20) most important NTFPs----
di_id <- sort(unique(per_ntfp$Dname), decreasing=FALSE)

dist_ntfp <- list()

for (i in seq_along(di_id)){
  b <- subset(per_ntfp, Dname==di_id[i], na.rm=TRUE)
  b <- b[1:10,]
  b <- b[complete.cases(b),]
  b <- as.data.frame(b)
  
  dist_ntfp <- rbind(dist_ntfp,b)
}

#************************
# Sort by district and choose 10 relevant NTFPs with dplyr----
district10 <- per_ntfp %>%
  filter(Dname %in% di_id) %>%
  group_by(Dname) %>%
  do(head(., 10))



#*******************************************************************
# Sum up to district level
distr <- data.frame()
for (i in seq_along(di_id)){
  c <- subset(per_ntfp, Dname==di_id[i], na.rm=TRUE)
  sumC <- sum(c$ntfp)
  sumC <- cbind(as.numeric(unique(c$DCODE)), as.numeric(unique(c$PCODE)), unique(c$Pname), sumC)
  
  distr <- rbind(distr, sumC)
  
}
# row.names(distr) <- NULL
distr <- cbind(di_id, distr)
colnames(distr) <- c("DName", "DCode", "PCode", "PName", "Qty")
distr$Qty <- as.numeric(levels(distr$Qty))[distr$Qty]

#***************************************************************************************************
#***************************************************************************************************
#****************************      PLOTTING                             ****************************
#***************************************************************************************************
#***************************************************************************************************

# Plot NTFP data per village and percentage of distribution of the 10 most use NTFPs

# Source the script for square pies
source("D:/8_TABI/02_NTFPdb/01_Scripts/Functions/squarePie.R") 


# Plot every village in every district----
for(k in seq_along(prov_id)){
  par(mfrow=c(4,4), mar=c(2,3,4,1), oma=c(0,0,2,0))
  nt_p <- subset(nt, PCODE==prov_id[k], na.rm=TRUE)
  vil_id <- sort(unique(nt_p$Vil_Code), decreasing=FALSE)
  
  for(i in seq_along(vil_id)){
    
    t <- subset(nt_p, Vil_Code==vil_id[i], na.rm=TRUE)
    
    # Show the 10 most important NTFPs per village
    t <- t[with(t, order(-Qty_All)),]
    t <- t[,c(1:5,7,8,16)]
    t <- t[1:10,]
    t <- t[complete.cases(t),]
    qty <- t$Qty_All
    
    if(length(qty)<1){
      next
    } else {
      # name village and province
      vn <- unique(t$Vil_Name)
      pn <- unique(t$Pname)
      
      squarePie(qty, col=coul)
      
    }
  }
  title(paste("Distribution of the 10 most collected NTFPs \nin ", pn, " Province [1 square = 1% of total]", 
              sep=""), outer=TRUE, cex=1.5)
}





#**************************************************************************************************
#**************************************************************************************************
#**************************************************************************************************

# biodiv <- nt %>% select(Vil_Code, Vil_Name, PCODE, Pname, Dname, Scientific_Name_Cleaned, Qty_All) %>%
#   group_by(Pname, Vil_Code, Scientific_Name_Cleaned) 
# print(xtable(biodiv[1:50, ]), type=)

# Plot Treemaps

# Static treemap of village data----
library(Cairo)
# impNTFP <- read.csv("")

CairoPS(file=paste(outwd, "NTFP_TreeMaps_Village.ps", sep=""), 
         width = 12, height=6,
         family = "Helvetica")
treemap(impNTFP, 
        index=c("Pname", "Vil_Name", "Scientific_Name_Cleaned"),   # A list of categorical variables
        vSize="Qty_All",   # This is the quantitative variable
        type="index",
        palette="Pastel1",
        title="Distribution of NTFPs per villages",
        fontsize.title=14
)

dev.off()


# Treemap of all NTFP district data----
# CairoPS(file="D:/8_TABI/02_NTFPdb/NTFP data analysis/NTFPAnalysis-figure/NTFP_TreeMaps_Districst.ps", 
#          width = 12, height=6,
#          family = "Helvetica")
# per_ntfp$label <- paste(per_ntfp$Scientific_Name_Cleaned, ", ", per_ntfp$ntfp, " kg", sep="")
postscript(paste(outwd, "NTFP_Treemap_Districts2.eps", sep=""), 
           fonts=c("sans"), width=16, height=12)
treemap(per_ntfp, force.print.labels = TRUE,
        index=c("Pname", "Dname", "Scientific_Name_Cleaned"),
        vSize="ntfp",
        type="index",
        palette="Pastel1",
        title="Distribution of NTFP per District",
        fontsize.title=14)
dev.off()


# Treemap of the 10 most important NTFPs per district ----
dist_ntfp$nmbr <- prettyNum(dist_ntfp$ntfp, big.mark="'", scientific=FALSE)
dist_ntfp$label <- paste(dist_ntfp$Scientific_Name_Cleaned, ", ", dist_ntfp$nmbr, " kg", sep="")

postscript(paste(outwd, "NTFP_treemap_District_10only.eps", sep=""), 
           fonts=c("sans"), width=16, height=12)
treemap(dist_ntfp, force.print.labels=TRUE,
        index=c("Pname", "Dname", "label"),
        vSize="ntfp",
        type="index",
        palette="Pastel1",
        title="Distribution of the 10 most important NTFPs per district",
        fontsize.title=18,
        fontsize.labels=8)
dev.off()


#******************************************************************
# interactive treemap----
library(d3treeR)


d3tree2(
  treemap(
    impNTFP
    ,index=c("Pname", "Vil_Name")
    ,vSize="Qty_All"
    ,vColor="Pname"
    
  )
)



#***************************************************************************************************
#***************************************************************************************************
#**********************************    MAPPING NTFPs   *********************************************
#***************************************************************************************************
#***************************************************************************************************
# Source: https://github.com/sjewo/cartogram



InMapfld <- "D:/8_TABI/02_NTFPdb/02_Map data/01_Laos/"
Di <- "District/"
Pr <- "Province/"
Co <- "Country/"


# Add up all quantities of NTFP to single districts


# Work with the shapefile
distrShp <- readShapePoly(paste(InMapfld, Di, "l_dist_bnd_2015.shp", sep=""))

proj4string(distrShp) <- CRS("+init=epsg:32648")

distrShp <- spTransform(distrShp, CRS("+init=epsg:32648"))
distrShp@data <- distrShp@data[,c(1,2,4,5,7,8)]
# Merge shp file and data file
district <- merge(distrShp, distr, by.x="DCode", by.y="DCode")
district@data$Qty[is.na(district@data$Qty)] <- 0

plot(district, col=district@data$Qty)
library(tmap)
qtm(district, "Qty")

# Create cartogram for districts----
dis <- cartogram(district, "Qty", itermax=15, prepare="adjust", threshold=0.05)

tm_shape(dis) + tm_fill("Qty", style="jenks") +
  tm_borders() + tm_layout(frame=FALSE)

