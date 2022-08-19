#call rm() function to remove all objects
rm(list = ls())

#set working directory
setwd("C:/Users/...")

#get packages
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, rlang, lubridate, pillar
library(data.table)

#get data
#SF4 uses run-0
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


#calculate mean
#mean of column production per group time
mtm <- aggregate(df[, "gdp_real"], list(df$time), mean) #real gdp due to Napoletano et al. (2006)
mtm$sdGDP <- aggregate(df[, "gdp_real"], list(df$time), sd)$gdp_real

mtm$investment <- aggregate(run[, "investmentPool"], list(df$time), mean)$investmentPool
mtm$sdInvestment <- aggregate(run[, "investmentPool"], list(df$time), sd)$investmentPool
mtm$price <- aggregate(run[, "prices"], list(run$time), mean)$prices

#rename time
mtm <- mtm %>% rename(time = Group.1)

#real aggregate investment
mtm$investment <- mtm$investment / mtm$price


mtm$marginErrorGDP <- qnorm(.95)*(mtm$sdGDP/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundGDP <- mtm$gdp_real - mtm$marginErrorGDP
mtm$upperBoundGDP <- mtm$gdp_real + mtm$marginErrorGDP

mtm$marginErrorInvest <- qnorm(.95)*(mtm$sdInvestment/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundInvest <- mtm$investment - mtm$marginErrorInvest
mtm$upperBoundInvest <- mtm$investment + mtm$marginErrorInvest


#no separate scale limit adjustment in ggplot
mtm <- mtm[!(mtm$investment>55),]
mtm <- mtm[!(mtm$investment<10),]
mtm <- mtm[!(mtm$gdp_real<10),]
mtm <- mtm[!(mtm$gdp_real>30),]


################################################################################
#Generate plot
################################################################################
g_real <-
  ggplot(mtm, aes(x = time)) +
  geom_line(aes(y = gdp_real, linetype = "GDP (LHS)")) +  
  geom_line(aes(y = investment/2, linetype = "Investment (RHS)")) + # 3.5
  scale_y_continuous(expand = c(0,0), breaks = c(10,15,20,25), sec.axis = sec_axis(~.*2, name = "Investment in production units")) + 
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("GDP (LHS)" = "#000000", "Investment (RHS)" = "#000000")) +
  geom_ribbon(aes(ymin = lowerBoundGDP, ymax = upperBoundGDP), alpha = 0.2) +
  labs(y = "GDP in production units",
       x = "Time in periods",
       linetype = "") +
  guides(linetype = guide_legend(override.aes = list(size = 0.69) )) +
  theme_bw() +
  theme(legend.position = c(0.83, 0.92),
        legend.title = element_blank(),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=2.5),
        axis.title.y.right = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 3)) +
  xlim(200,600)


#save graph in working directory
  cairo_pdf("SF4_invest_pro-cycle.pdf", width=8, height=6)
  print(g_real)
  dev.off()
