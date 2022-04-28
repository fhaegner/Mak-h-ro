#call rm() function to remove all objects
rm(list = ls())
gc()

#set working directory that directly links to the folder with all replications
setwd("C:/Users/.../config-0")


library(mFilter)
library(tidyverse)
library(data.table)
library(magrittr)
library(dplyr)

gc(reset = TRUE)

files <- list.files(pattern = "run-*")
run_list <- list()
k <- 1

#Load all files
for(i in seq_along(files)){
  run <- read_csv(file = files[i], lazy=FALSE)
  run <- run[!(run$time<200),]
  
  ##############################################################################
  df <- as.data.frame(run$time)
  df <- df %>% rename(time = "run$time")
  
  df$stock <- run$Stock
  df$price <- run$prices
  df$id <- run$id
  
  #pro-cyclical
  df$inv <- run$investmentConsumeUnit
  df$invNom <- run$investmentConsume
  df$inflation <- run$inflationAverage
  df$con <- run$consumptionUnit
  df$conNom <- run$consumption
  
  df$employees <- run$employees
  
  df$investmentQuarterly <- run$investmentQuarterly
  df$supplyQuarterly <- run$supplyQuarterly
  
  
  #get firm size
  firmSize <- max(df$id) + 1 #id starts at 0
  
  #change in inventories per period
  setDT(df)[, stock_change := stock - shift(stock, n=firmSize)]
  df[is.na(df)] <- 0
  
  #nominal
  df$stock_nominal <- df$stock * df$price
  setDT(df)[, stock_change_nominal := stock_nominal - shift(stock_nominal, n=firmSize)]
  df[is.na(df)] <- 0
  
  #quarterly change in inventories
  setDT(df)[, stock_change_quart := stock - shift(stock, n=(firmSize*2))]
  df[is.na(df)] <- 0
  
  
  #generate real GDP
  df$gdp_real <- df$inv + df$con + df$stock_change
  df$gdp_real_alt <- df$inv + df$con #check alternative gdp measurement
  
  #generate nominal GDP
  df$gdp_nom <- df$invNom + df$conNom + df$stock_change_nominal 
  
  
  #aggregate mean per period
  #mean of column production per group time
  mtm <- aggregate(df[, "gdp_real"], list(df$time), mean) #real gdp due to Napoletano et al. (2006)
  gdp_alt <- aggregate(df[, "gdp_real_alt"], list(df$time), mean)
  gdp_nom <- aggregate(df[, "gdp_nom"], list(df$time), mean)
  
  #pro-cyclical
  real_investment <- aggregate(df[, "inv"], list(df$time), mean)
  nom_investment <- aggregate(df[, "invNom"], list(df$time), mean)
  stock_change <- aggregate(df[, "stock_change"], list(df$time), mean)
  inflation <- aggregate(df[, "inflation"], list(df$time), mean)
  con <- aggregate(df[, "con"], list(df$time), mean)
  conNom <- aggregate(df[, "conNom"], list(df$time), mean)
  price <- aggregate(df[, "price"], list(df$time), mean)
  
  #counter-cyclical
  employees <- aggregate(df[, "employees"], list(df$time), mean)
  unemployees <- (10 - employees) * firmSize #10 = average firm labor capacity
  
  
  #aggregate mean quarterly
  inv_quart <- aggregate(df[, "investmentQuarterly"], list(run$time), mean)
  gdp_quart <- aggregate(df[, "supplyQuarterly"], list(run$time), mean)
  
  
  #rename time
  mtm <- mtm %>% rename(time = Group.1)
  
  #merge
  mtm$gdp_alt <- gdp_alt$gdp_real_alt
  mtm$gdp_nom <- gdp_nom$gdp_nom
  mtm$real_investment <- real_investment$inv
  mtm$invNom <- nom_investment$invNom
  mtm$stock_change <- stock_change$stock_change
  mtm$inflation <- inflation$inflation
  mtm$con <- con$con
  mtm$conNom <- conNom$conNom
  
  mtm$price <- price$price
  mtm$unemployees <- unemployees$employees
  mtm$employees_sum <- employees$employees
  
  mtm$inv_quart <- inv_quart$investmentQuarterly
  mtm$gdp_quart <- gdp_quart$supplyQuarterly
  
  ##############################################################################
  
  run_list[[k]] <- mtm
  k = k + 1
}

#merge separate runs
mtm <- rbindlist(run_list)

#rectify time sequence
mtm <- mtm %>% mutate(time = 1:n())

######################
#quarterly measurement
mtm_quart <- data.frame(mtm$time)
mtm_quart <- mtm_quart %>% rename(time = mtm.time)
mtm_quart$inv_quart <- mtm$inv_quart
mtm_quart <- setDT(mtm_quart)[,.(inv_quart=mean(inv_quart)), time -0:2]

mtm_gdp <- data.frame(mtm$time)
mtm_gdp <- mtm_gdp %>% rename(time = mtm.time)
mtm_gdp$gdp_quart <- mtm$gdp_quart
mtm_gdp <- setDT(mtm_gdp)[,.(gdp_quart=mean(gdp_quart)), time -0:2]

mtm_quart$gdp_quart <- mtm_gdp$gdp_quart


mtm_quart$time <- NULL
mtm_quart$time <- seq.int(nrow(mtm_quart))


######################
#log per period
mtm$log_gdp <- log(mtm$gdp_real)
mtm$log_gdp_alt <- log(mtm$gdp_alt)
mtm$log_gdp_nom <- log(mtm$gdp_nom)

mtm$log_consumption <- log(mtm$con)
mtm$log_conNom <- log(mtm$conNom)
mtm$log_investment_real <- log(mtm$real_investment)
mtm$log_invNom <- log(mtm$invNom)
#no log for stock_change due to negative values
#no log for inflation due to negative values

mtm$log_price <- log(mtm$price)
mtm$log_unemployees <- log(mtm$unemployees)

mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
mtm[is.na(mtm)] <- 0


#log quarterly
mtm_quart$log_inv_quart <- log(mtm_quart$inv_quart)
mtm_quart$log_gdp_quart <- log(mtm_quart$gdp_quart)

mtm_quart <- do.call(data.frame,lapply(mtm_quart, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
mtm_quart[is.na(mtm_quart)] <- 0


#bandpass filter per period
#run Baxter-King filter
bk_gdp <- bkfilter(mtm$log_gdp, pl = 6, pu = 32, nfix = 12)
bk_gdp_alt <- bkfilter(mtm$log_gdp_alt, pl = 6, pu = 32, nfix = 12)
bk_gdp_nom <- bkfilter(mtm$log_gdp_nom, pl = 6, pu = 32, nfix = 12)

bk_consumption <- bkfilter(mtm$log_consumption, pl = 6, pu = 32, nfix = 12)
bk_conNom <- bkfilter(mtm$log_conNom, pl = 6, pu = 32, nfix = 12)
bk_investment_real <- bkfilter(mtm$log_investment_real, pl = 6, pu = 32, nfix = 12)
bk_invNom <- bkfilter(mtm$log_invNom, pl = 6, pu = 32, nfix = 12)

gc(reset = TRUE)

bk_price <- bkfilter(mtm$log_price, pl = 6, pu = 32, nfix = 12)
bk_unemployees <- bkfilter(mtm$log_unemployees, pl = 6, pu = 32, nfix = 12) 

bk_stock_change <- bkfilter(mtm$stock_change, pl = 6, pu = 32, nfix = 12) #bp without logarithm
bk_inflation <- bkfilter(mtm$inflation, pl = 6, pu = 32, nfix = 12) #bp without logarithm


#bandpass filter quarterly
bk_inv_quart <- bkfilter(mtm_quart$log_inv_quart, pl = 6, pu = 32, nfix = 12)
bk_gdp_quart <- bkfilter(mtm_quart$log_gdp_quart, pl = 6, pu = 32, nfix = 12)


#add cyclical component to the data frame
#real GDP per period
mtm <- mtm %>%
  mutate(bk_gdp = bk_gdp$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_gdp_alt = bk_gdp_alt$cycle[, 1])
#nominal GDP per period
mtm <- mtm %>%
  mutate(bk_gdp_nom = bk_gdp_nom$cycle[, 1])

#consumption
mtm <- mtm %>%
  mutate(bk_conNom = bk_conNom$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_consumption = bk_consumption$cycle[, 1])
#investment real
mtm <- mtm %>%
  mutate(bk_investment_real = bk_investment_real$cycle[, 1])
#investment nominal
mtm <- mtm %>%
  mutate(bk_invNom = bk_invNom$cycle[, 1])
#stock change
mtm <- mtm %>%
  mutate(bk_stock_change = bk_stock_change$cycle[, 1])
#inflation
mtm <- mtm %>%
  mutate(bk_inflation = bk_inflation$cycle[, 1])
#price
mtm <- mtm %>%
  mutate(bk_price = bk_price$cycle[, 1])
#unemployees
mtm <- mtm %>%
  mutate(bk_unemployees = bk_unemployees$cycle[, 1])

mtm[is.na(mtm)] <- 0


#add cyclical component to the quarterly data frame
#investment real quarterly
mtm_quart <- mtm_quart %>%
  mutate(bk_inv_quart = bk_inv_quart$cycle[, 1])
#gdp real quarterly
mtm_quart <- mtm_quart %>%
  mutate(bk_gdp_quart = bk_gdp_quart$cycle[, 1])

mtm_quart[is.na(mtm_quart)] <- 0


#for relative standard deviation
library(e1071)
sd(mtm$bk_gdp)
sd(mtm$bk_conNom) / sd(mtm$bk_gdp)
sd(mtm$bk_consumption) / sd(mtm$bk_gdp)
sd(mtm$bk_investment_real) / sd(mtm$bk_gdp)
sd(mtm$bk_unemployees) / sd(mtm$bk_gdp)


################################################################################
#Correlogram
################################################################################

#get cross correlation function tables
#gdp and gdp
g <- ccf(mtm$bk_gdp, mtm$bk_gdp, lag.max = 4, type="correlation")
d <- do.call(rbind.data.frame, g)
#get correlation coefficient
f <- d[1,]
#get lag coefficient and set as header
colnames(f) <- (d[4,])
#round to 3 digits
f[nrow(f),] <- round(as.numeric(f[nrow(f),]), 3)
print(f)

#gdp and investment
g1 <- ccf(mtm$bk_investment_real, mtm$bk_gdp, lag.max = 4, type="correlation")
d1 <- do.call(rbind.data.frame, g1)
f1 <- d1[1,]
colnames(f1) <- (d1[4,])
f1[nrow(f1),] <- round(as.numeric(f1[nrow(f1),]), 3)
print(f1)

#gdp and consumption
g1 <- ccf(mtm$bk_consumption, mtm$bk_gdp, lag.max = 4, type="correlation")
d1 <- do.call(rbind.data.frame, g1)
f1 <- d1[1,]
colnames(f1) <- (d1[4,])
f1[nrow(f1),] <- round(as.numeric(f1[nrow(f1),]), 4)
print(f1)

#gdp and stock_change
g2 <- ccf(mtm$bk_stock_change, mtm$bk_gdp, lag.max = 4, type="correlation") #stock change can be negative and not logarithmized
d2 <- do.call(rbind.data.frame, g2)
#get correlation coefficient
f2 <- d2[1,]
#get lag coefficient and set as header
colnames(f2) <- (d2[4,])
#round to 3 digits
f2[nrow(f2),] <- round(as.numeric(f2[nrow(f2),]), 3)
print(f2)

#gdp and inflation
#inflation is measured as the average over 12 periods -> ccf with production per period
g3 <- ccf(mtm$bk_inflation, mtm$bk_gdp, lag.max = 4, type="correlation") #inflation can be negative and not logarithmized
d3 <- do.call(rbind.data.frame, g3)
f3 <- d3[1,]
colnames(f3) <- (d3[4,])
f3[nrow(f3),] <- round(as.numeric(f3[nrow(f3),]), 3)
print(f3)

#gdp and prices
g4 <- ccf(mtm$bk_price, mtm$bk_gdp, lag.max = 4, type="correlation")
d4 <- do.call(rbind.data.frame, g4)
f4 <- d4[1,]
colnames(f4) <- (d4[4,])
f4[nrow(f4),] <- round(as.numeric(f4[nrow(f4),]), 3)
print(f4)

#gdp and unemployment
g5 <- ccf(mtm$bk_unemployees, mtm$bk_gdp, lag.max = 4, type="correlation")
d5 <- do.call(rbind.data.frame, g5)
f5 <- d5[1,]
colnames(f5) <- (d5[4,])
f5[nrow(f5),] <- round(as.numeric(f5[nrow(f5),]), 3)
print(f5)


##########
#quarterly
##########

# #gdp and gdp on quarterly basis
# g6 <- ccf(mtm_quart$bk_gdp_quart, mtm_quart$bk_gdp_quart, lag.max = 4, type="correlation")
# d6 <- do.call(rbind.data.frame, g6)
# #get correlation coefficient
# f6 <- d6[1,]
# #get lag coefficient and set as header
# colnames(f6) <- (d6[4,])
# #round to 3 digits
# f6[nrow(f6),] <- round(as.numeric(f6[nrow(f6),]), 3)
# print(f6)
# 
# #gdp and investment on quarterly basis
# g7 <- ccf(mtm_quart$bk_inv_quart, mtm_quart$bk_gdp_quart, lag.max = 4, type="correlation")
# d7 <- do.call(rbind.data.frame, g7)
# f7 <- d7[1,]
# colnames(f7) <- (d7[4,])
# f7[nrow(f7),] <- round(as.numeric(f7[nrow(f7),]), 3)
# print(f7)


#gdp and gdp without change of inventories
g6 <- ccf(mtm$bk_gdp_alt, mtm$bk_gdp_alt, lag.max = 4, type="correlation")
d6 <- do.call(rbind.data.frame, g6)
#get correlation coefficient
f6 <- d6[1,]
#get lag coefficient and set as header
colnames(f6) <- (d6[4,])
#round to 3 digits
f6[nrow(f6),] <- round(as.numeric(f6[nrow(f6),]), 3)
print(f6)

#gdp and investment without change of inventories
g7 <- ccf(mtm$bk_investment_real, mtm$bk_gdp_alt, lag.max = 4, type="correlation")
d7 <- do.call(rbind.data.frame, g7)
f7 <- d7[1,]
colnames(f7) <- (d7[4,])
f7[nrow(f7),] <- round(as.numeric(f7[nrow(f7),]), 3)
print(f7)


#summarize in data table for latex
datatable <- rbind(f, f1, f2, f3, f4, f5, f6, f7)

library(xtable)
#print table in latex style
print(
  xtable(
    datatable
  ),
  tabular.environment="longtable",
  latex.environments=c("center"), 
  floating=FALSE, 
  include.rownames=FALSE
)