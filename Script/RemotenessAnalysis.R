#***************************************
# NTFP db analysis
#*********************
# date: 26-10-2016
# author: VR
# folder: D:\8_TABI\02_NTFPdb
#
#********************


# Load necessary libraries----
library(Cairo)
library(circlize)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(lattice)
library(ez)

# Add multiplot script from source----
source(file = "C:/Users/Vincent Roth/Documents/R/Scripts/17_Functions/multiplot.R")
source(file = "C:/Users/Vincent Roth/Documents/R/Scripts/17_Functions/panel_cor.R")
source(file = "C:/Users/Vincent Roth/Documents/R/Scripts/17_Functions/ggcorplot.R")

# set the working directory
# getwd()
# setwd("D:/8_TABI/02_NTFPdb/NTFP data analysis")

directory <- "D:/8_TABI/02_NTFPdb/00_OriginalData/"
ntfp <- read.csv(paste(directory, "NTFPdb.csv", sep=""), header=TRUE, na.strings = "NA")
nt <- ntfp[, c(1:10, 13:15, 17:20)]





# Remoteness vs NTFP_count----
lmRNT <- lm(Remoteness~NTFP_count, data=nt)

m <- ggplot(data=nt, aes(Remoteness, NTFP_count))+
  labs(x="Remoteness from district capital [min]", y="Count of sing. species",
       title="Remoteness vs. count of collected NTFP species")+
  theme(plot.title = element_text(
    colour="black", 
    size=20, 
    hjust=.5,
    vjust=1.5
  ))+
  geom_point(aes(colour=Pname, size=Price_all_mio))+
  #scale_colour_brewer(palette="Set1")+
  geom_smooth()  # +
# facet_wrap(~Pname, ncol=2)
# Print to file----
# ggsave("remoteness_vs_count.eps", device="eps",
#        path=paste(getwd(), "/NTFPAnalysis-figure/Remoteness/", sep=""),
#        dpi=300)
m

#****************************************************************************
# Remoteness vs av travel time----
n <- ggplot(data=nt, aes(Remoteness, Avg_travel_time))+
  labs(x="Remoteness from district capital [min]", y="Average travel time for NTFP collection", 
       title="Remoteness vs. average travel time")+
  theme(plot.title=element_text(
    colour="black",
    size=20,
    hjust=.5,
    vjust=1.5
  ))+
  # annotate("rect", xmin=750, xmax=950, ymin=25, ymax=125, alpha=.2, fill="red")+
  # geom_text(check_overlap=TRUE, hjust=0, nudge_x=0.05, size=3)+
  geom_point(aes(colour=Pname, size=Price_all_mio))+
  geom_smooth(span=.8)  #  method="lm", se=FALSE

n +scale_size(range=c(0,10))


# Loop through the df and plot every pair
namlp <- names(nt[,c(8:17)])

df <- list()
for(i in seq_along(namlp)){
  x <- namlp[1]
  y <- namlp[i]
  
  n <- ggplot(data=nt, aes_string(x, y))+
    labs(title=paste(x, " vs. ", y, sep=""), 
         x=paste(x, " from district capital [min]",sep=""),
         y=y)+
    theme(plot.title=element_text(
      colour="black",
      size=20,
      hjust=.5
    ))+
    geom_point(aes(colour=Pname, size=Price_all_mio))+
    geom_smooth(span=.8)+ 
    annotate("text", x=750, y=max(nt[,y]), label="Span = 0.8")+
    scale_size(range=c(0,10))
  
  df[[i+1]] <-  n
  
  # calculate the linear regression
  lmx <- lm(nt[, x]~nt[,y])
  print("****************************************************************")
  print("****************************************************************")
  print(paste("Linear Regression summary for ", x, " vs. ", y, sep=""))
  print(summary(lmx))
  
  par(mfrow=c(2,2))
  plot(lmx, main=paste(x, " vs. ", y, sep=""))
  
}

par(mfrow=c(1,1))
multiplot(df, cols=1)

#***************************************************


