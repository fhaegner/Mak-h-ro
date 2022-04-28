#call rm() function to remove all objects
rm(list = ls())


#set working directory
setwd("C:/Users/...")

#get packages
library(mFilter)
library(tidyverse)
library(data.table)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)

#get data
run <- read_csv("C:/Users/.../run-0.csv")

keep <- c("10", "19", "27") #randomly chosen
mtm <- run[run$id %in% keep, ]


################################################################################
#Generate plot
################################################################################
g <-
  ggplot(mtm, aes(time, investmentPeriod, group = id)) +
  geom_line(aes(linetype=factor(id)),size = 0.7) +
  #scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,120), expand = c(0, 0)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  labs(y = "Investment in nominal units",
       x = "Time in periods",
       linetype = "Firm id") +
  theme_bw() +
  theme(legend.position = c(0.1, 0.86),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust = 2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.75), keywidth = 3)) +
  xlim(200,300)

#save graph in working directory
  cairo_pdf("SF9_lumpy_investment.pdf", width=8, height=6)
  #jpeg(filename = "SF9_lumpy_investment.jpeg", width = 888, height = 688, quality = 100)
  print(g)
  dev.off()