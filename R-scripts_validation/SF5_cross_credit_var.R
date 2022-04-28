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
  df$id <- run$id
  
  #pro-cyclical
  df$inv <- run$investmentConsumeUnit
  df$con <- run$consumptionUnit
  
  df$firmDebt <- run$loanLiability
  df$default <- run$creditDefault
  df$interest <- run$interestLevel
  
  
  #indentify firm size
  firmSize <- max(df$id) + 1 #id starts at 0
  
  #change in inventories per period
  setDT(df)[, stock_change := stock - shift(stock, n=firmSize)]
  df[is.na(df)] <- 0
  
  
  #generate real GDP
  df$gdp_real <- df$inv + df$con + df$stock_change
  
  #aggregate mean per period
  #mean of column production per group time
  mtm <- aggregate(df[, "gdp_real"], list(df$time), mean) #real gdp due to Napoletano et al. (2006)
  
  #pro-cyclical
  firmDebt <- aggregate(df[, "firmDebt"], list(df$time), mean) #loanLiability
  
  #counter-cyclical
  default <- aggregate(df[, "default"], list(df$time), mean) #credit defaults -> newLoan
  interest <- aggregate(df[, "interest"], list(df$time), mean)
  
  #rename time
  mtm <- mtm %>% rename(time = Group.1)
  
  #merge
  mtm$firmDebt <- firmDebt$firmDebt
  
  mtm$default <- default$default
  mtm$interest <- interest$interest
  
  mtm$bankProfits <- mtm$firmDebt * mtm$interest
  
  ##############################################################################
  
  run_list[[k]] <- mtm
  k = k + 1
}

#merge separate runs
mtm <- rbindlist(run_list)

#rectify time sequence
mtm <- mtm %>% mutate(time = 1:n())


#log per period
mtm$log_gdp <- log(mtm$gdp_real)

mtm$log_bankProfits <- log(mtm$bankProfits)
mtm$log_firmDebt <- log(mtm$firmDebt)

mtm$log_default <- log(mtm$default)
mtm$log_interest <- log(mtm$interest)

mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
mtm[is.na(mtm)] <- 0




#bandpass filter per period
#run Baxter-King filter
bk_gdp <- bkfilter(mtm$log_gdp, pl = 6, pu = 32, nfix = 12) 

bk_bankProfits <- bkfilter(mtm$log_bankProfits, pl = 6, pu = 32, nfix = 12)
bk_firmDebt <- bkfilter(mtm$log_firmDebt, pl = 6, pu = 32, nfix = 12)
bk_default <- bkfilter(mtm$log_default, pl = 6, pu = 32, nfix = 12)
bk_interest <- bkfilter(mtm$log_interest, pl = 6, pu = 32, nfix = 12)


#add cyclical component to the data frame
#real GDP per period
mtm <- mtm %>%
  mutate(bk_gdp = bk_gdp$cycle[, 1])

#Bank profits
mtm <- mtm %>%
  mutate(bk_bankProfits = bk_bankProfits$cycle[, 1])
#Loan liabilities
mtm <- mtm %>%
  mutate(bk_firmDebt = bk_firmDebt$cycle[, 1])
#Credit default
mtm <- mtm %>%
  mutate(bk_default = bk_default$cycle[, 1])
#Interest level
mtm <- mtm %>%
  mutate(bk_interest = bk_interest$cycle[, 1])

mtm[is.na(mtm)] <- 0



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

#real GDP and bank profits
g1 <- ccf(mtm$bk_bankProfits, mtm$bk_gdp, lag.max = 4, type="correlation")
d1 <- do.call(rbind.data.frame, g1)
#get correlation coefficient
f1 <- d1[1,]
#get lag coefficient and set as header
colnames(f1) <- (d1[4,])
#round to 3 digits
f1[nrow(f1),] <- round(as.numeric(f1[nrow(f1),]), 3)
print(f1)


#real GDP and firm debt
g2 <- ccf(mtm$bk_firmDebt, mtm$bk_gdp, lag.max = 4, type="correlation")
d2 <- do.call(rbind.data.frame, g2)
#get correlation coefficient
f2 <- d2[1,]
#get lag coefficient and set as header
colnames(f2) <- (d2[4,])
#round to 3 digits
f2[nrow(f2),] <- round(as.numeric(f2[nrow(f2),]), 3)
print(f2)


#real GDP and default
g3 <- ccf(mtm$bk_default, mtm$bk_gdp, lag.max = 4, type="correlation")
d3 <- do.call(rbind.data.frame, g3)
#get correlation coefficient
f3 <- d3[1,]
#get lag coefficient and set as header
colnames(f3) <- (d3[4,])
#round to 3 digits
f3[nrow(f3),] <- round(as.numeric(f3[nrow(f3),]), 3)
print(f3)


#summarize in data table for latex
datatable <- rbind(f, f1, f2, f3)
rownames(datatable) <- c("f", "f1", "f2", "f3")

library(xtable)
#print table in latex style
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("Series & \\multicolumn{9}{l}{Output} \\\\\n",
                      " & t-4 & t-3 & t-2 & t-1 & t & t+1 & t+2 & t+3 & t+4 \\\\\n")

print(
  xtable(
    datatable, align="lccccccccc"
  ),
  tabular.environment="tabular",
  latex.environments=c("center"), 
  floating=FALSE, 
  add.to.row = addtorow, include.colnames = FALSE, include.rownames = TRUE
)