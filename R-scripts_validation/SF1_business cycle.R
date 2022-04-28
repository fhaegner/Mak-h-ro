#call rm() function to remove all objects
rm(list = ls())

#set working directory
  setwd("C:/Users/...")

#get packages
library(ggplot2)
library(mFilter)
library(tidyverse)
library(data.table)
library(magrittr) #needs to be run every time you start R and want to use %>%
library(dplyr)    #alternatively, this also loads %>%

#get data
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


#mean of column production per group time
mtm <- aggregate(df[, "gdp_real"], list(df$time), mean)
sdGDP <- aggregate(df[, "gdp_real"], list(df$time), sd)

real_investment <- aggregate(df[, "inv"], list(df$time), mean)
sd_real_investment <- aggregate(df[, "inv"], list(df$time), sd)

consumption <- aggregate(df[, "con"], list(df$time), mean)
sd_consumption <- aggregate(df[, "con"], list(df$time), sd)


#rename time
mtm <- mtm %>% rename(time = Group.1)


#merge GDP
mtm$sdGDP <- sdGDP$gdp_real

#get lower/upper bound for real GDP
mtm$marginErrorGDP <- qnorm(.95)*(mtm$sdGDP/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundGDP <- mtm$gdp_real - mtm$marginErrorGDP
mtm$upperBoundGDP <- mtm$gdp_real + mtm$marginErrorGDP

#merge investment
mtm$real_investment <- real_investment$inv
mtm$sd_real_investment <- sd_real_investment$inv

#get lower/upper bound for investment
mtm$marginErrorInvest <- qnorm(.95)*(mtm$sd_real_investment/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundInvest <- mtm$real_investment - mtm$marginErrorInvest
mtm$upperBoundInvest <- mtm$real_investment + mtm$marginErrorInvest

#merge consumption
mtm$consumption <- consumption$con
mtm$sd_consumption <- sd_consumption$con

#get lower/upper bound for consumption
mtm$marginErrorCon <- qnorm(.95)*(mtm$sd_consumption/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundCon <- mtm$consumption - mtm$marginErrorCon
mtm$upperBoundCon <- mtm$consumption + mtm$marginErrorCon

#delete first row to get rid of initial time 0
mtm <- mtm[-1,]


#generate log
#GDP
mtm$log_gdp_monthly <- log(mtm$gdp_real)
mtm$log_lowerBoundGDP <- log(mtm$lowerBoundGDP)
mtm$log_upperBoundGDP <- log(mtm$upperBoundGDP)

#investment
mtm$log_real_investment <- log(mtm$real_investment)
mtm$log_lowerBoundInvest <- log(mtm$lowerBoundInvest)
mtm$log_upperBoundInvest <- log(mtm$upperBoundInvest)

#consumption
mtm$log_consumption <- log(mtm$consumption)
mtm$log_lowerBoundCon <- log(mtm$lowerBoundCon)
mtm$log_upperBoundCon <- log(mtm$upperBoundCon)

mtm[is.na(mtm)] <- 0

#delete inf if necessary
mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))


#bandpass filter
#run Baxter-King filter
#GDP
bk_gdp_monthly <- bkfilter(mtm$log_gdp_monthly, pl = 6, pu = 32, nfix = 12)
bk_lowerGDP <- bkfilter(mtm$log_lowerBoundGDP, pl = 6, pu = 32, nfix = 12)
bk_upperGDP <- bkfilter(mtm$log_upperBoundGDP, pl = 6, pu = 32, nfix = 12)

#investment
bk_real_investment <- bkfilter(mtm$log_real_investment, pl = 6, pu = 32, nfix = 12)
bk_lowerInvest <- bkfilter(mtm$log_lowerBoundInvest, pl = 6, pu = 32, nfix = 12)
bk_upperInvest <- bkfilter(mtm$log_upperBoundInvest, pl = 6, pu = 32, nfix = 12)

#consumption
bk_consumption <- bkfilter(mtm$log_consumption, pl = 6, pu = 32, nfix = 12)
bk_lowerCon <- bkfilter(mtm$log_lowerBoundCon, pl = 6, pu = 32, nfix = 12)
bk_upperCon <- bkfilter(mtm$log_upperBoundCon, pl = 6, pu = 32, nfix = 12)

#add cyclical component to the main data frame
#GDP
mtm <- mtm %>%
  mutate(bk_gdp_monthly = bk_gdp_monthly$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_lowerGDP = bk_lowerGDP$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_upperGDP = bk_upperGDP$cycle[, 1])

#investment
mtm <- mtm %>%
  mutate(bk_real_investment = bk_real_investment$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_lowerInvest = bk_lowerInvest$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_upperInvest = bk_upperInvest$cycle[, 1])

#consumption
mtm <- mtm %>%
  mutate(bk_consumption = bk_consumption$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_lowerCon = bk_lowerCon$cycle[, 1])
mtm <- mtm %>%
  mutate(bk_upperCon = bk_upperCon$cycle[, 1])


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
  geom_ribbon(aes(ymin = bk_lowerGDP, ymax = bk_upperGDP), alpha = 0.9) +
  geom_ribbon(aes(ymin = bk_lowerInvest, ymax = bk_upperInvest), alpha = 0.3) +
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
  geom_ribbon(aes(ymin = bk_lowerGDP, ymax = bk_upperGDP), alpha = 0.9) +
  geom_ribbon(aes(ymin = bk_lowerCon, ymax = bk_upperCon), alpha = 0.3) +
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

