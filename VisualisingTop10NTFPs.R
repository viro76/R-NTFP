#*************************************************
# TABI NTFP database analysis
# 
# Author:   VR
# Date:     21-11-1016
#*************************************************

# load relevant libraries----

library(ggplot2)
library(waffle)
library(RColorBrewer)
library(xtable)
library(dplyr)
library(treemap)

# set the working directory and the directory for outputs-----
setwd("D:/8_TABI/02_NTFPdb/00_OriginalData/4_NTFP_DB_CDE")
outwd <- "D:/8_TABI/02_NTFPdb/NTFP data analysis/NTFPAnalysis-figure/"

# read the provincial, district and kumban codes----
code <- read.csv("D:/8_TABI/02_NTFPdb/00_OriginalData/TABI_PDK_codes.csv", 
                 header=TRUE, stringsAsFactors = FALSE)
# load the data from a csv file----
ntfpdb <- read.csv("NTFP_2016.csv", header=TRUE, stringsAsFactors = FALSE)
ntfpdb_cl <- ntfpdb[,c(8,5,13:22,24,26,27,29:56, 60:90)]
nt <- ntfpdb_cl

# Merge codes and nt data to get provinces, districts and kumbans for the data----
nt <- merge(code, nt, by.x="Vil_Code")

# Get the province id as a unique number only----
prov_id <- sort(unique(nt$PCODE), decreasing=FALSE)
di_id <- sort(unique(nt$DCODE), decreasing=FALSE)
vi_id <- sort(unique(nt$Vil_Code), decreasing=FALSE)
vi_code <- sort(unique(nt$Vil_Code, decreasing=FALSE))

coul <- brewer.pal(10, "Paired")

#************************************************************************************
# Data handling----
# group unique NTFP names per village or district and sum up the quantity
# source: https://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html
# source: http://www.listendata.com/2016/08/dplyr-tutorial.html

# Extract 10 most selected NTFPs per village----
Village10 <- nt %>%
  filter(Vil_Code %in% vi_code) %>%
  group_by(Vil_Code) %>%
  do(head(., 10))


by_NTFP <- group_by(Village10, DCODE, Dname, Scientific_Name_Cleaned,
                    PCODE, Pname, Vil_Code, Vil_Name)
# Sum up unique NTFPs per district
(per_ntfp <- summarise(by_NTFP, ntfp=sum(Qty_All)))
(per_ntfp <- arrange(per_ntfp, PCODE, Pname, DCODE, Dname, desc(ntfp), Vil_Code))
# rename(per_ntfp, Scientific_Name_Cleaned = SciName)


# by district----
by_district <- group_by(nt, DCODE, Dname, Pname, PCODE)
# group by district----
fps <- summarise(by_district, 
                 TotalInVil=n(),
                 DistinctInVil =n_distinct(Scientific_Name_Cleaned),
                 MeanQty = mean(Qty_All, na.rm=TRUE),
                 TotalQty = sum(Qty_All, na.rm=TRUE))

# group by NTFP
by_distr_ntfp <- group_by(nt, Scientific_Name_Cleaned, DCODE, Dname, Pname, PCODE)

by_dist_spec <- summarise(by_distr_ntfp,
                          TotalInDistr = n(),
                          MeanQty = mean(Qty_All, na.rm=TRUE),
                          TotalQty = sum(Qty_All, na.rm=TRUE))

# Sort by district and choose 10 (or 20) relevant species per district
ntfp10 <- by_dist_spec %>%
  filter(DCODE %in% di_id) %>%
  group_by(Dname) %>%
  do(head(., 10))



# by Province---
by_province <- group_by(nt, PCODE, Pname)
by_prov <- summarise(by_province,
          TotalInProv=n(),
          DistinctInProv = n_distinct(Scientific_Name_Cleaned),
          MeanQty = mean(Qty_All, na.rm=TRUE),
          TotalQty=sum(Qty_All, na.rm=TRUE))

#************************


# Sort by district and choose 10 (or 20) relevant NTFPs per district with dplyr----
district10 <- per_ntfp %>%
  filter(DCODE %in% di_id) %>%
  group_by(Dname) %>%
  do(head(., 10))






#*******************************************************************************
# Plotting
#******

# VILLAGES----
Village10$nmbr <- prettyNum(Village10$Qty_All, big.mark="'", scientific=FALSE)
Village10$label <- paste(Village10$Scientific_Name_Cleaned, ", ",
                         Village10$nmbr, " kg", sep="")
postscript(paste(outwd, "NTFP_treemap_Village_10only.eps", sep=""), 
           fonts=c("sans"), width=16, height=12)
treemap(Village10, force.print.labels=TRUE,
        index=c("Pname", "Dname", "Vil_Name", "label"),
        vSize="Qty_All",
        type="index",
        palette="Pastel1",
        title="Distribution of the 10 most important NTFPs per village",
        fontsize.title=18,
        fontsize.labels=8)
dev.off()

# DISTRICTS----
#**************************
# Treemap of the 10 most important NTFPs per district ----
district10$nmbr <- prettyNum(district10$ntfp, big.mark="'", scientific=FALSE)
district10$label <- paste(district10$Scientific_Name_Cleaned, ", ", 
                          district10$nmbr, " kg", sep="")

postscript(paste(outwd, "NTFP_treemap_District_10only.eps", sep=""), 
           fonts=c("sans"), width=16, height=12)
treemap(district10, force.print.labels=TRUE,
        index=c("Pname", "Dname", "label"),
        vSize="ntfp",
        type="index",
        palette="Pastel1",
        title="Distribution of the 10 most important NTFPs per district",
        fontsize.title=18,
        fontsize.labels=8)
dev.off()
#**************************
# PROVINCES----
treemap(by_prov, force.print.labels=TRUE,
        index=c("Pname"),
        vSize="TotalQty",
        type="index",
        palette="Pastel1",
        title="Distribution on NTFPs per province",
        fontsize.title=18,
        fontsize.labels=8)


#****************************************************************
# Plotting SquarePies

source("D:/8_TABI/02_NTFPdb/01_Scripts/Functions/squarePie.R") 




for(k in seq_along(prov_id)){
  par(mfrow=c(4,4), mar=c(2,3,4,1), oma=c(0,0,2,0))
  t <- subset(ntfp10, PCODE==prov_id[k])
  di_id <- sort(unique(t$DCODE), decreasing=FALSE)
  
  for (i in seq_along(di_id)){
    s <- subset(ntfp10, DCODE==di_id[i])
    s <- s[with(s, order(-TotalQty)),]
    qty <- s$TotalQty
    
    dn <- unique(s$Dname)
    pn <- unique(s$Pname)
    
    squarePie(qty, col=coul, main=dn)
    
  }
  
  title(paste("Distribution of the 10 most collected NTFPs \nin ", pn, " Province [1 square = 1% of total]", 
              sep=""), outer=TRUE, cex=1.5)
}





























