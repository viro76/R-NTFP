#***************************************
# Analysis of the NTFP database
# Data preparation file
#***************************************
# Created: 2016-10-13
# Modified: --
# Author: VR
# Part of: TABI NTFP data analysis
#**************************************

# Load necessary libraries
library(Cairo)
library(circlize)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(lattice)
library(ez)
library(GGally)

# Add multiplot script from source
source(file = "C:/Users/Vincent Roth/Documents/R/Scripts/17_Functions/multiplot.R")
source(file = "C:/Users/Vincent Roth/Documents/R/Scripts/17_Functions/panel_cor.R")
# source(file = "C:/Users/Vincent Roth/Documents/R/Scripts/17_Functions/ggcorplot.R")

# set the working directory
# getwd()
# setwd("D:/8_TABI/02_NTFPdb/NTFP data analysis")

directory <- "D:/8_TABI/02_NTFPdb/00_OriginalData/"
ntfp <- read.csv(paste(directory, "NTFPdb.csv", sep=""), header=TRUE, na.strings = "NA")
nt <- ntfp[, c(1:10, 13:15, 17:20)]

#************************
pairs(nt[,c(8:17)], pch=20)


n <- ggplot(data=nt, aes(Remoteness, Avg_travel_time, label=Vil_Name))+
  scale_x_continuous("Remoteness")+
  scale_y_continuous("Average time traveled [min]")+
  ggtitle("Remoteness and Average time travelled")+
  geom_point(aes(colour=NTFP_count, size=Price_all_mio))+
  scale_fill_distiller(palette="Greens")+
  annotate("text", x=700, y=200, label="Span = 0.8")+
  # annotate("rect", xmin=750, xmax=950, ymin=25, ymax=125, alpha=.2, fill="red")+
  # geom_text(check_overlap=TRUE, hjust=0, nudge_x=0.05, size=3)+
  geom_smooth(span=.8)  #  method="lm", se=FALSE

n

#****************
# Remoteness and all other variables----

# Rm and NTFP_count----
lmRNT <- lm(Remoteness~NTFP_count, data=nt)
cor(nt[,c(8:17)])

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
  geom_smooth()   # +
  # facet_wrap(~PCODE, ncol=3)
  # Print to file----
  # ggsave("remoteness_vs_count.eps", device="eps",
  #        path=paste(getwd(), "/NTFPAnalysis-figure/Remoteness/", sep=""),
  #        dpi=300)
m

n <- ggplot(data=nt, aes(Remoteness, Avg_travel_time, label=Vil_Name))+
  scale_x_continuous("Remoteness")+
  scale_y_continuous("Average time traveled [min]")+
  ggtitle("Remoteness and Average time travelled")+
  geom_point(aes(colour=NTFP_count, size=Price_all_mio))+
  scale_fill_distiller(palette="Greens")+
  annotate("text", x=700, y=200, label="Span = 0.8")+
  # annotate("rect", xmin=750, xmax=950, ymin=25, ymax=125, alpha=.2, fill="red")+
  # geom_text(check_overlap=TRUE, hjust=0, nudge_x=0.05, size=3)+
  geom_smooth(span=.8)  #  method="lm", se=FALSE

n




#*********************
# Pairwise comparison
coln <- names(nt)[8:17]

# myplot <- function(df, x_string, y_string){
#   ggplot(data=nt, aes_string(x=x_string, y=y_string))+
#     geom_point(aes(colour=Pname))+
#     #scale_colour_brewer(palette="Set1")+
#     geom_smooth()  # +
# }

df <- list()
for(i in seq_along(coln)-1){
  m <-  ggplot(data=nt, aes_string(coln[1], coln[i+1]))+
    geom_point(aes(colour=Pname, size=Price_all_mio))+
    ggtitle(paste("Remoteness and ", coln[i+1], sep=""))+
    geom_smooth()
    
  # print(m)
  # assign(paste("n.",i,sep=""), m)
  # name <- paste("n.", i,sep="")
  df[[i+1]] <- m
}


multiplot(plotlist=df, cols=2)

#*****************************************
# Use ggpairs function for pairwise comparison----

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping=mapping) +
    geom_point() +
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g <- ggpairs(data=nt[,c(8:17)], 
             columns=1:10, 
             lower=list(continuous=my_fn))
g



