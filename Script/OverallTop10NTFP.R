#******************************************
# visualizing overall NTFPs importance
# Date: 05-01-2017
#******************************************


library(RColorBrewer)
library(dplyr)
library(ggplot2)
# library(waffle)
# library(xtable)
# library(treemap)

#*************************************************************#
#*************************************************************#
#                 First prepare the data                      #
#*************************************************************#
#*************************************************************#

# set the working directory and the directory for outputs-----
setwd("D:/8_TABI/02_NTFPdb/NTFP data analysis/Data/")
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


# Extract 10 most selected NTFPs per village----
Village10 <- nt %>%
  filter(Vil_Code %in% vi_code) %>%
  group_by(Vil_Code) %>%
  do(head(., 10))


by_NTFP <- group_by(Village10, DCODE, Dname, Scientific_Name_Cleaned,
                    PCODE, Pname, Vil_Code, Vil_Name)

# Sum up globally most important NTFPs-----
overall_data <- Village10 %>%
  group_by(Scientific_Name_Cleaned) %>%
  summarise(QtyAll=sum(Qty_All)) %>%
  arrange(desc(QtyAll))

overall_data <- overall_data[c(1:15), ]
overall_data$fraction=overall_data$QtyAll/sum(overall_data$QtyAll)
overall_data$ymax <- cumsum(overall_data$fraction)
overall_data$ymin <- c(0, head(overall_data$ymax, n=-1))
overall_data$perc <- round(overall_data$fraction*100, 2)



#*************************************************************#
#*************************************************************#
#                 Plot the data with different plots          #
#*************************************************************#
#*************************************************************#

# Plot doughnut graph overall-----
library(ggplot2)

p <- ggplot(overall_data, aes(fill=Scientific_Name_Cleaned, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0,4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank())+
  # annotate("text", x=0, y=0, label="Distribution of 10 most collected NTFPs") +
  labs(title="Distribution of NTFPs")
p


#********************************************************************
# Doughnut with plotly
library(plotly)
library(RColorBrewer)
col <- brewer.pal(dim(overall_data)[1], "Spectral")
col <- colorRampPalette(c(rev(col)))(15)

overall_data %>%
  # group_by(Scientific_Name_Cleaned) %>%
  plot_ly(labels=~Scientific_Name_Cleaned, values=~QtyAll, showlegend=FALSE,
          marker=list(colors=col), 
          # mode="markers", 
          hoverinfo="text",
          textposition="none",
          text=~paste("Species: ", Scientific_Name_Cleaned,
                      "</br> Amount: ", QtyAll, "kg",
                      "</br> Percentage: ", round(fraction*100, 2), "%")) %>%
  add_pie(hole=0.5) %>%
  layout(title="Distribution of the 15 most relevant NTFPs overall", showlegend=FALSE,
         xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
         yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))
 


# Simple pie chart with Plotly----
plot_ly(overall_data, labels=~Scientific_Name_Cleaned, values=~QtyAll, type="pie", 
        marker=list(colors=col), 
        textposition="none",
        textinfo="Scientific_Name_Cleaned",
        hoverinfo="text", 
        text=~paste("Species: ", Scientific_Name_Cleaned,
                    "</br> Amount: ", QtyAll, "kg",
                    "</br> Percentage: ", round(fraction*100, 2), "%")) %>%
layout(title="Distribution of NTFP",
       # subtitle="10 most collected NTFPs in all provinces",
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

# Barplot with plotly----
plot_ly(overall_data, x=~Scientific_Name_Cleaned, y=~perc, type="bar", 
        color=col,
        textposition="none") %>%
  layout(title="Distribution of the 15 most important NTFPs collected",
         xaxis=list(title=""),
         yaxis=list(title="Percent [%]"))

  

# Barplot---------
x <- overall_data
names(x) <- c("SciName", "QtyAll", "fraction", "ymax", "ymin", "perc")
par(mar=c(5.1, 10, 4,2))
barplot(x$perc, horiz=TRUE, col=col, names.arg=x$SciName, las=2,
        xlab="Percentage of total of NTFP [%]", ylab="", 
        main="Distribution of top 10 NTFPs overall", 
        sub="Percentage of ")
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Barplot with ggplot----
x <- transform(x, SciName=reorder(SciName, -perc))
ggplot() +
  geom_bar(aes(x=SciName, y=perc), data=x, stat="identity", fill=col) +
  geom_text(data=x, aes(x=SciName, y=perc, label=paste0(round(perc,1), "%")), size=3, hjust=1) +
  coord_flip()+
  labs(y="", x="Scientific name of species")+
  ggtitle("Distribution of top 15 NTFPs over the entire dataset")








