#call rm() function to remove all objects
rm(list = ls())

#set working directory
setwd("C:/Users/...")



library("gridExtra")
library("rlang")
library("tibble")



#create folder list
folders <- list.files(path="C:/Users/.../results",
                      pattern="config-*")

val <- data.frame(value = numeric(0))
dfList <- list()

library(magrittr) # for %>%
library(tidyverse)
library(readr) # for read_CSV
library(ggplot2)
library(ggpubr)

###Load all files
for(i in folders){
  path <- file.path("C:/Users/.../results",i)
  files <- list.files(path, pattern = "run-*", full.names = TRUE)
  all_csv <- sapply(files, read_csv, simplify = FALSE) %>%
    bind_rows(.id="id")
  all_csv_mean <- aggregate(all_csv[,], list(all_csv$time), mean)[-1]
  all_csv_mean <- subset(all_csv_mean, select = -c(id))
  
  path_config <- file.path(path,"config.csv")
  config <- read_csv(path_config)
  val_i <- config %>% dplyr::filter(var %in% "INSERT PARAMETER NAME")
  val_i <- val_i[2]
  val_iA <- config %>% dplyr::filter(var %in% "INSERT 2nd PARAMETER NAME")
  val_iA <- val_iA[2]
  #val_iB <- config %>% dplyr::filter(var %in% "INSERT 3rd PARAMETER NAME")
  #val_iB <- val_iB[2]
  var <- paste("INSERT PARAMETER NAME")
  varA <- paste("INSERT 2nd PARAMETER NAME")
  #varB <- paste("INSERT 3rd PARAMETER NAME")
  
  
  plist = sapply(names(all_csv_mean)[-grep("time", names(all_csv_mean))], function(col) {
    
   
    ggplot(all_csv_mean, aes_string("time", col)) +
      geom_line() +
      #adapt parameter
      ggtitle("",subtitle = paste0(var," = ", val_i, " | ", varA," = ", val_iA)) + #, " | ", varB," = ", val_iB)) +
      geom_smooth() +
      theme_bw()
    
  }, simplify=FALSE)
  
  here <- assign(i,plist)
  dfList <- append(dfList, list(here))
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

list_time = colnames(all_csv_mean)[-1]
pList <- list()
capitalLetters <- firstup(letters[1:length(list_time)])


fn <- paste("output_",format(Sys.time(), "%d-%m-%Y %H-%M-%S.pdf"),sep="")
pdf(fn)

for (k in list_time){
  plot_list <- list()
  folders <- list.files(path="C:/Users/.../results", pattern="config-*")
  for (i in dfList){
    p = (i[[k]])
    here <- assign(folders,p)
    plot_list <- append(plot_list, list(here)) #c(plot_list,here)
    folders <- folders[-1]
  }
  
  multiPage <- ggarrange(plotlist = plot_list,nrow=3,ncol=2)
  there <- assign(capitalLetters,multiPage)
  pList <- append(pList, list(there))
  capitalLetters <- capitalLetters[-1]
  
}
pList

dev.off()




