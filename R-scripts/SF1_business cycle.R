#call rm() function to remove all objects
rm(list = ls())

#set working directory
#setwd("C:/Users/...")


#get packages
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, rlang, lubridate, pillar
library(mFilter) #for Baxter-King filter
library(data.table)

#get data
#SF1 uses run-0
run <- read_csv("C:/Users/.../run-0.csv")

df <- as.data.frame(run$time)
df <- df %>% rename(time = "run$time")
df$stock <- run$Stock #stock of inventories
df$inv <- run$investmentConsumeUnit
df$con <- run$consumptionUnit

#get firm size
firmSize <- max(run$id) + 1 #id starts at 0

#stock change
setDT(df)[, stock_change := stock - shift(stock, n=firmSize)]
df[is.na(df)] <- 0

#generate GDP
df$gdp_real <- df$inv + df$con + df$stock_change

#Names for the resulting columns of mean and sd
mean_sd = c("gdp_real", "sdGDP", "real_investment", "sd_real_investment", "consumption", "sd_consumption")

#mean of column production per group time
mtm <-  setDT(df)[, setNames(sapply(.SD, function(x) list(mean = mean(x), sd=sd(x))), 
                          mean_sd),by = time, .SDcols = c("gdp_real", "inv", "con")]

setDF(mtm)

#Names of columns used in the calculation of marginError and lower/upper bound
marginError = c("marginErrorGDP", "marginErrorInvest", "marginErrorCon")
sd = c("sdGDP", "sd_real_investment", "sd_consumption")
mean = c("gdp_real", "real_investment", "consumption")
upper = c("upperBoundGDP", "upperBoundInvest", "upperBoundCon")
lower = c("lowerBoundGDP", "lowerBoundInvest", "lowerBoundCon")

#calculate marginError for GDP, Invest and Con
mtm[marginError] <-  qnorm(.95)*(mtm[sd]/sqrt(firmSize)) #10% confidence interval

#get lower/upper bound for GDP, Invest and Con
mtm[lower] <- mtm[mean] - mtm[marginError]
mtm[upper] <- mtm[mean] + mtm[marginError]

#delete first row to get rid of initial time 0
mtm <- mtm[-1,]

column_names <- colnames(mtm)
column_names <- column_names[!column_names %in% c("time", marginError, sd)]
new_col_log <- paste("log",sep="_", column_names)
new_col_log[1] <- "log_gdp_monthly"

#generate log
mtm[new_col_log] <- log(mtm[column_names])

mtm[is.na(mtm)] <- 0

#delete inf if necessary
mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))

#bandpass filter
#Names for the columns for the results of the Baxter-King filter
new_col_bk = paste("bk",sep="_", column_names)
new_col_bk[1] <- "bk_gdp_monthly"


#Copying the log values to the columns on which the Baxter-King filter will be applied
mtm[new_col_bk] <- mtm[new_col_log]


#run Baxter-King filter on the specified columns
for (i in new_col_bk){
  
  temp <- mtm[i]
  temp %>% rename(bk = colnames(temp))
  mtm[i] <- bkfilter(temp$bk, pl = 6, pu = 32, nfix = 12)$cycle[, 1] #add only the cyclical component to the main data frame
  
}

mtm[is.na(mtm)] <- 0

mtm <- mtm[!(mtm$time<200),]



################################################################################
#Generate plots
################################################################################
#GDP and investment
g_real <-
  ggplot(mtm, aes(x = time)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(aes(y = bk_gdp_monthly, linetype = "GDP")) +
  geom_line(aes(y = bk_real_investment, linetype = "Investment")) +
  scale_y_continuous(breaks = scales::breaks_extended(n=10)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = bk_lowerBoundGDP, ymax = bk_upperBoundGDP), alpha = 0.9) +
  geom_ribbon(aes(ymin = bk_lowerBoundInvest, ymax = bk_upperBoundInvest), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.92),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.background = element_rect(fill = "white"), 
        legend.title = element_blank(),
        legend.key.height = unit(0.2,"cm"),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 3)) +
  labs(x = "Time in periods", y = "Deviation from trend in %") +
  xlim(200,600)


#GDP and consumption
c_real <-
  ggplot(mtm, aes(x = time)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(aes(y = bk_gdp_monthly, linetype = "GDP")) +
  geom_line(aes(y = bk_consumption, linetype = "Consumption")) +
  scale_y_continuous(breaks = scales::breaks_extended(n=10)) +
  scale_linetype_manual(values = c("dashed", "solid"), labels = c("Consumption", "GDP")) +
  guides(values = guide_legend(reverse = TRUE)) +
  geom_ribbon(aes(ymin = bk_lowerBoundGDP, ymax = bk_upperBoundGDP), alpha = 0.9) +
  geom_ribbon(aes(ymin = bk_lowerBoundCon, ymax = bk_upperBoundCon), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.92),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.background = element_rect(fill = "white"), 
        legend.title = element_blank(),
        legend.key.height = unit(0.2,"cm"),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 3)) +
  labs(x = "Time in periods", y = "Deviation from trend in %") +
  ylim(-0.15,0.25) +
  xlim(200,600)



#save graph in working directory
#investment and GDP
cairo_pdf("SF1_bk_cycle_inv.pdf", width=8, height=6)
print(g_real)
dev.off()

#consumption and GDP
cairo_pdf("SF1_bk_cycle_con.pdf", width=8, height=6)
print(c_real)
dev.off()

