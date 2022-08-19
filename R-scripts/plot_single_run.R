#call rm() function to remove all objects
rm(list = ls())

#set working directory
setwd("C:/Users/...")


library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble*, stringr, forcats, rlang*, lubridate*, pillar and more
library("colorspace")
library("gridExtra")
library("ggloop")
library("rlang")
library("ellipsis")
library("acepack")
library("rlist")


deleteRowsByColumn <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


#get data
run <- read_csv("C:/Users/.../run-0.csv")

list <- colnames(run)
listM <- list()
for (i in 1:length(list)){
  if (ragtop::is.blank(run_0[1,i])){
    listM <- append(listM, list(colnames(run_0)[i]))
  }
}

listF = colnames(run)
listF = listF[!listF %in% listM]
remove = c("id","time","idM")
listF = listF [! listF %in% remove]

run_0F <- deleteRowsByColumn(run,"id")
run_0F <- run_0F[ , !(names(run_0F) %in% listM)]

run_0M <- deleteRowsByColumn(run,"idM")
run_0M <- run_0M[ , !(names(run_0M) %in% listF)]
run_0M <- subset(run_0M, select = -c(id))


fn <- paste("output_",format(Sys.time(), "%d-%m-%Y %H-%M-%S.pdf"),sep="")
pdf(fn)

for (i in seq_along(listF)) { 
  plot <-
  ggplot(run_0F, aes_string("time", listF[i])) +
    geom_line(aes(group=id,color=factor(id))) +
    scale_colour_discrete("Firm") +
    geom_smooth() +
    stat_summary(fun = mean, aes(time), color = "black", geom = "line", size = 0.8) + #na.rm = TRUE +
    guides(color = FALSE) +
    ggtitle(paste(listF[i])) +
    theme_bw() +
    theme(legend.position = "none")
print(plot)
}

for (k in listM) { 
  plot <-
    ggplot(run_0M, aes_string("time", k)) +
    geom_line() +
    ggtitle(paste(k)) +
    theme_bw() +
    theme(legend.position = "none")
  print(plot)
}

dev.off()

