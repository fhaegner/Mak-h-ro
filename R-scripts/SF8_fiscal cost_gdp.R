#call rm() function to remove all objects
rm(list = ls())
gc()

#set working directory that directly links to the folder with all replications
setwd("C:/Users/.../results")

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
  df <- as.data.frame(run$time)
  df <- df %>% rename(time = "run$time")
  
  col_names_run = c("Stock", "investmentConsume", "consumption", "prices", "id", "govBailoutGDP")
  col_names_df = c("stock", "invNom", "conNom", "price", "id", "govBailoutGDP")

  df[col_names_df] <- run[col_names_run]

  #generate nominal GDP
  firmSize <- max(df$id) + 1 #id starts at 0
  df$stock_nominal <- df$stock * df$price
  setDT(df)[, stock_change_nominal := stock_nominal - shift(stock_nominal, n=firmSize)]
  df[is.na(df)] <- 0
  df$gdp_nom <- df$invNom + df$conNom + df$stock_change_nominal 
  
  
  #calculate mean
  #mean of column production per group time
  mtm <- aggregate(df[, "govBailoutGDP"], list(df$time), mean) #mean because bailout is measured as the sum of each bank within a period
  
  mtm$gdp_nom <- aggregate(df[, "gdp_nom"], list(df$time), sum)$gdp_nom
  
  #rename time
  mtm <- mtm %>% rename(time = Group.1)
  
  #delete first two rows, because of gdp_nom
  mtm <- mtm[-c(1:2),]
  
  mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
  mtm[is.na(mtm)] <- 0
  
  
  #banking crisis in a row
  #when assumed, that banking crisis are combined when occurring in the next period
  #set 1 if bankRuptcyCheck > 0 else 0
  mtm <- transform(mtm, recession = ifelse(govBailoutGDP > 0, +1, 0))
  
  #sum for each group with connected values of 1
  mtm <- mtm %>%
    add_column(length = 0)
  head(mtm)
  
  a <- 2 #due to NA in first row
  m <- 0
  
  # for-loop over rows
  for(a in 2:nrow(mtm)) {
    if (mtm[a,"recession"] == 1){
      m = m + mtm[a,"gdp_nom"]
    }
    if (mtm[a,"recession"] == 0){
      mtm[a-1,"gdp_sum"] <- m
      m = 0
    }
  }
  
  a <- 2 #due to NA in first row
  m <- 0
  
  # for-loop over rows
  for(a in 2:nrow(mtm)) {
    if (mtm[a,"recession"] == 1){
      m = m + mtm[a,"govBailoutGDP"]
    }
    if (mtm[a,"recession"] == 0){
      mtm[a-1,"length"] <- m
      m = 0
    }
  }
  
  mtm$gdp_sum <- round(mtm$gdp_sum, digits = 0)
  mtm$length <- round(mtm$length, digits = 0)
  
  #cumulative bailout sum
  mtm$fiscalCost <- mtm$length / mtm$gdp_sum
  
  mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
  mtm[is.na(mtm)] <- 0
  ##############################################################################
  

  run_list[[k]] <- mtm
  k = k + 1
}

#merge
mtm <- rbindlist(run_list)

#rectify time sequence
mtm <- mtm %>% mutate(time = 1:n())


mtm_1 <- mtm[,c("time","fiscalCost")]
mtm_1 <- mtm_1[apply(mtm_1, 1, function(row) all(row !=0 )), ]


hist(mtm_1$fiscalCost,freq = FALSE)
curve(dnorm(x, mean=mean(mtm_1$fiscalCost), sd=sd(mtm_1$fiscalCost)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#check kurtosis calculation in Excel
#install.packages("writexl")
#library("writexl")
#write_xlsx(mtm_1,"C:/Users/.../file name.xlsx")

#mode estimation
calcmode <- function(a) {  
  vector <- unique(a)  
  vector[which.max(tabulate(match(a, vector)))]  
}  

#install.packages("moments")
#install.packages("e1071")
#bailout by duration and yearly GDP
#library(e1071)
e1071::skewness(mtm_1$fiscalCost) 
e1071::kurtosis(mtm_1$fiscalCost)
e1071::kurtosis(mtm_1$fiscalCost,,1)
moments::kurtosis(mtm_1$fiscalCost)

mean(mtm_1$fiscalCost)
sd(mtm_1$fiscalCost)
min(mtm_1$fiscalCost)
max(mtm_1$fiscalCost)
median(mtm_1$fiscalCost)
calcmode(mtm_1$fiscalCost)



library(moments)
kurtosis(mtm_1$fiscalCost)


#install.packages("ie2misc")
library(ie2misc)

madstat(mtm_1$fiscalCost)
madstat(mtm_1$fiscalCost) / sd(mtm_1$fiscalCost)

shapiro.test(mtm_1$fiscalCost)
#install.packages("tseries")
library(tseries)
jarque.bera.test(mtm_1$fiscalCost)