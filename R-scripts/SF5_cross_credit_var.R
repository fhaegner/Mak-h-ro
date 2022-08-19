#call rm() function to remove all objects
rm(list = ls())
gc()

#set working directory that directly links to the folder with all replications
setwd("C:/Users/.../results")

#get packages
library(mFilter) #for Baxter-King filter
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, rlang, lubridate, pillar
library(data.table)



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
  

  col_names_run = c("Stock", "id", "investmentConsumeUnit", "consumptionUnit", "loanLiability", "creditDefault", "interestLevel")
  col_names_df = c("stock", "id", "inv", "con", "firmDebt", "default", "interest")

  df[col_names_df] <- run[col_names_run]
  
  
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
  mtm$firmDebt <- aggregate(df[, "firmDebt"], list(df$time), mean)$firmDebt #loanLiability
  
  #counter-cyclical
  mtm$default <- aggregate(df[, "default"], list(df$time), mean)$default #credit defaults -> newLoan
  mtm$interest <- aggregate(df[, "interest"], list(df$time), mean)$interest
  
  #rename time
  mtm <- mtm %>% rename(time = Group.1)

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

#Names for the columns for the results of the Baxter-King filter
new_col_bk = c("bk_gdp", "bk_bankProfits","bk_firmDebt","bk_default","bk_interest")
log = c("log_gdp", "log_bankProfits", "log_firmDebt", "log_default", "log_interest")

#Copying the log values to the columns on which the Baxter-King filter will be applied
mtm[new_col_bk] <- mtm[log]

#run Baxter-King filter on the specified columns
for (i in new_col_bk){
  
  temp <- mtm[i]
  temp %>% rename(bk = colnames(temp))
  mtm[i] <- bkfilter(temp$bk, pl = 6, pu = 32, nfix = 12)$cycle[, 1] #add only the cyclical component to the main data frame
  
}

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
