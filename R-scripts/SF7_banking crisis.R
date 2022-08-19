#call rm() function to remove all objects
rm(list = ls())
gc()

#set working directory that directly links to the folder with all replications
#setwd("C:/Users/.../results")
setwd("C:/Users/Florian/IdeaProjects/model-v1/results/runs")

#get packages
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, rlang, lubridate, pillar
library(data.table) 

files <- list.files(pattern = "run-*")
run_list <- list()
k <- 1

#Load all files
for(i in seq_along(files)){
  run <- read_csv(file = files[i])
  run <- run[!(run$time<200),]
  
  ##############################################################################
  #bank bankruptcies and length of banking crisis
  #calculate mean
  #mean of column bankRuptcyCheck per group time
  mtm <- aggregate(run[, "bankRuptcyCheck"], list(run$time), mean)
  mtm <- mtm %>% rename(time = Group.1)
  
  #set 1 if bankRuptcyCheck > 0 else 0
  mtm <- transform(mtm, recession = ifelse(bankRuptcyCheck > 0, +1, 0))
  
  #sum for each group with connected values of 1
  mtm <- mtm %>%
    add_column(length = 0)
  head(mtm)
  
  
  a <- 2 #due to NA in first row
  j <- 0
  
  # for-loop over rows
  for(a in 2:nrow(mtm)) { 
    if (mtm[a,"recession"] == 1){
      j = j + 1
    }
    if (mtm[a,"recession"] == 0){
      mtm[a-1,"length"] <- j
      j = 0
    }
  }
  ##############################################################################
  
  run_list[[k]] <- mtm
  k = k + 1
}

#merge
mtm <- rbindlist(run_list)

#rectify time sequence
mtm <- mtm %>% mutate(time = 1:n())



#delete all periods with no banking crisis
mtm <- mtm[apply(mtm, 1, function(row) all(row !=0 )), ]


mtm$log_length <- log(mtm$length)
plot(density(mtm$log_length))
hist(mtm$length, breaks=5, freq = FALSE)
curve(dnorm(x, mean=mean(mtm$length), sd=sd(mtm$length)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

freq_duration <- as.data.frame(table(mtm$length))

library(e1071)
skewness(mtm$length)
kurtosis(mtm$length) #it might occur a different value than 4.888 which can be explained with the packages used in SF8 

mean(mtm$length)
min(mtm$length)
max(mtm$length)
sd(mtm$length)
median(mtm$length)


calcmode <- function(a) {  
  vector <- unique(a)  
  vector[which.max(tabulate(match(a, vector)))]  
}  

calcmode(mtm$length)
