#call rm() function to remove all objects
rm(list = ls())

#set working directory
setwd("C:/Users/...")


library("readr")
library("ggplot2")
library("colorspace")
library("gridExtra")
library("lubridate")
library("ggloop")
library("rlang")
library("tidyr")
library("ellipsis")
library("pillar")
library("acepack")
library("dplyr")
library("rlist")


deleteRowsByColumn <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


#get data
run_0 <- read_csv("C:/Users/.../run-0.csv")


list <- colnames(run_0)
listM <- list()
for (i in 1:length(list)){
  if (ragtop::is.blank(run_0[1,i])){
    listM <- append(listM, list(colnames(run_0)[i]))
  }
}

listF = colnames(run_0)
listF = listF[!listF %in% listM]
remove = c("id","time","idM")
listF = listF [! listF %in% remove]

run_0F <- deleteRowsByColumn(run_0,"id")
run_0F <- run_0F[ , !(names(run_0F) %in% listM)]

run_0M <- deleteRowsByColumn(run_0,"idM")
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

