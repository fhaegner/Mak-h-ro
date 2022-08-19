#call rm() function to remove all objects
rm(list = ls())

#set working directory
setwd("C:/Users/...")

#set path to results
path="C:/Users/.../results"


#get packages
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble*, stringr, forcats, rlang*, lubridate*, pillar and more
library(ggpubr) #provides some easy-to-use functions for creating and customizing 'ggplot2'- based publication ready plots.

#create folder list
folders <- list.files(path, pattern="config-*")

val <- data.frame(value = numeric(0))
dfList <- list()


###Load all files
for(i in folders){
  folderPath <- file.path(path,i)
  files <- list.files(folderPath, pattern = "run-*", full.names = TRUE)
  all_csv <- read_csv(files) %>% bind_rows(.id="id")
  all_csv_mean <- subset(aggregate(all_csv[,], list(all_csv$time), mean)[-1], select = -c(id))
  
  #all_csv_mean[-c(1:25),]
  #assign(paste(i,sep = ""),all_csv_mean)
  
  config <- read_csv(paste0(folderPath,"/config.csv"))
  val_i <- config %>% dplyr::filter(var %in% "INSERT PARAMETER NAME") %>% select(-var)
  #val_iA <- config %>% dplyr::filter(var %in% "INSERT 2nd PARAMETER NAME") %>% select(-var)
  #val_iB <- config %>% dplyr::filter(var %in% "INSERT 3rd PARAMETER NAME")%>% select(-var)
  var <- paste("INSERT PARAMETER NAME")
  #varA <- paste("INSERT 2nd PARAMETER NAME")
  #varB <- paste("INSERT 3rd PARAMETER NAME")
  
  
  plist = sapply(names(all_csv_mean)[-grep("time", names(all_csv_mean))], function(col) {
    
    ggplot(all_csv_mean, aes_string("time", col)) +
      geom_line() +
      #adapt parameter
      ggtitle("",subtitle = paste0(var," = ", val_i)) + #, " | ", varA," = ", val_iA)) + #, " | ", varB," = ", val_iB)) +
      geom_smooth() +
      theme_bw()
    
  }, simplify=FALSE)
  
  here <- assign(i,plist)
  dfList <- append(dfList, list(here)) #c(plot_list,here)
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
  folders <- list.files(path, pattern="config-*")
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

