#call rm() function to remove all objects
rm(list = ls())


#set working directory
setwd("C:/Users/...")


#get packages
library(mFilter)
library(tidyverse)
library(data.table)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%


#get data
run <- read_csv("C:/Users/.../interbank experiment/config-1/run-0.csv")


df <- as.data.frame(run$time)
df <- df %>% rename(time = "run$time")
df$rate <- run$interbankMargin
df$excess <- run$reservesExcess
df$volume <- run$interbankVolume 

#get bank size
bankSize <- max(run$id)


#mean of column production per group time
mtm <- aggregate(df[, "rate"], list(df$time), mean)
rateSD <- aggregate(df[, "rate"], list(df$time), sd) #check standard deviation of interbank rate
excess <- aggregate(df[, "excess"], list(df$time), mean)
excessSD <- aggregate(df[, "excess"], list(df$time), sd)
volume <- aggregate(df[, "volume"], list(df$time), mean)

#rename time
mtm <- mtm %>% rename(time = Group.1)
mtm <- mtm %>% rename(rate = x)
excess <- excess %>% rename(excess = x)
excessSD <- excessSD %>% rename(excessSD = x)
rateSD <- rateSD %>% rename(rateSD = x)
volume <- volume %>% rename(volume = x)


#merge GDP
mtm$excess <- excess$excess
mtm$excessSD <- excessSD$excessSD
mtm$rateSD <- rateSD$rateSD
mtm$volume <- volume$volume

mtm$marginErrorRate <- qnorm(.95)*(mtm$rateSD/sqrt(bankSize)) #10% confidence interval
mtm$lowerBoundRate <- mtm$rate - mtm$marginErrorRate
mtm$upperBoundRate <- mtm$rate + mtm$marginErrorRate


mtm$marginErrorExcess <- qnorm(.95)*(mtm$excessSD/sqrt(bankSize)) #10% confidence interval
mtm$lowerBoundExcess <- mtm$excess - mtm$marginErrorExcess
mtm$upperBoundExcess <- mtm$excess + mtm$marginErrorExcess

#delete first row to get rid of initial time 0
mtm <- mtm[-1,]


mtm_normal <- mtm[mtm$time < 51, ]
mtm_excess <- mtm[mtm$time < 101, ]
mtm_excess <- mtm_excess[mtm_excess$time > 50, ]

library(e1071)
mean(mtm_normal$rateSD)
mean(mtm_excess$rateSD)

mtm_normal$experiment <- mtm_excess$rateSD
#install.packages("ggpubr") 
t.test(mtm_normal$rateSD, mtm_normal$experiment, alternative = "two.sided", var.equal = FALSE)

mtm_normal$experimentVol <- mtm_excess$volume
mean(mtm_normal$volume)
mean(mtm_excess$volume)
t.test(mtm_normal$volume, mtm_normal$experimentVol, alternative = "two.sided", var.equal = FALSE)


################################################################################
#Generate plot
################################################################################
g_real <-
  ggplot(mtm, aes(x = time)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(aes(y = rate*100, linetype = "Interbank rate (LHS)")) +
  geom_line(aes(y = excess/100, linetype = "Excess reserves (RHS)")) +
  scale_x_continuous(breaks=seq(0, 150, 25)) +
  scale_y_continuous(expand = c(0,0.05),sec.axis = sec_axis(~.*100, name = "Excess reserves in nominal units")) + 
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lowerBoundRate*100, ymax = upperBoundRate*100), alpha = 0.35) +
  geom_ribbon(aes(ymin = lowerBoundExcess/100, ymax = upperBoundExcess/100), alpha = 0.2) +
  theme_bw() +
  theme(legend.position = c(0.20, 0.92),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.background = element_rect(fill = "white"), 
        legend.title = element_blank(),
        legend.key.height = unit(0.2,"cm"),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=2),
        axis.title.y.right = element_text(vjust=2)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 3)) +
  labs(x = "Time in periods", y = "Interbank rate in %")



#save graph in working directory
  cairo_pdf("SF13_interbank.pdf", width=8, height=6)
  print(g_real)
  dev.off()
