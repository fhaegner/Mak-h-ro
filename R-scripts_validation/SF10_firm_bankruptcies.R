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
library(ggplot2)

#get data
run <- read_csv("C:/Users/.../run-0.csv")


df <- as.data.frame(run$time)
df <- df %>% rename(time = "run$time")
df$stock <- run$Stock #stock of inventories
df$inv <- run$investmentConsumeUnit
df$con <- run$consumptionUnit

df$countBankruptcy <- run$countBankruptcy

df$id <- run$id

#get firm size
firmSize <- max(df$id) + 1 #id starts at 0

#stock change
setDT(df)[, stock_change := stock - shift(stock, n=firmSize)]
df[is.na(df)] <- 0

#generate real GDP
df$gdp_real <- df$inv + df$con + df$stock_change




#calculate mean
#mean of column production per group time
mtm <- aggregate(df[, "gdp_real"], list(df$time), mean) #real gdp due to Napoletano et al. (2006)
sdGDP <- aggregate(df[, "gdp_real"], list(df$time), sd)

firmBankruptcies <- aggregate(run[, "countBankruptcy"], list(df$time), sum)

#rename time
mtm <- mtm %>% rename(time = Group.1)

#merge
mtm$sdGDP <- sdGDP$gdp_real

mtm$firmBankruptcies <- firmBankruptcies$countBankruptcy

setDT(mtm)[, change := firmBankruptcies - shift(firmBankruptcies)]
mtm[is.na(mtm)] <- 0

mtm <- transform(mtm, change = ifelse(change < 0, firmBankruptcies, change))


#delete first two rows, because of gdp_nom and consumption calculation
mtm <- mtm[-c(1:2),]

mtm$marginErrorGDP <- qnorm(.95)*(mtm$sdGDP/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundGDP <- mtm$gdp_real - mtm$marginErrorGDP
mtm$upperBoundGDP <- mtm$gdp_real + mtm$marginErrorGDP


#generate log
mtm$log_gdp <- log(mtm$gdp_real)

mtm$log_change <- log(mtm$change)

mtm <- do.call(data.frame,lapply(mtm, function(log_prod) replace(log_prod, is.infinite(log_prod),0)))
mtm[is.na(mtm)] <- 0


#bandpass filter
#run Baxter-King filter
bk_gdp <- bkfilter(mtm$log_gdp, pl = 6, pu = 32, nfix = 12) 

bk_change <- bkfilter(mtm$log_change, pl = 6, pu = 32, nfix = 12)

#add cyclical component to the main data frame
mtm <- mtm %>%
  mutate(bk_gdp = bk_gdp$cycle[, 1])

mtm <- mtm %>%
  mutate(bk_change = bk_change$cycle[, 1])

mtm[is.na(mtm)] <- 0


################################################################################
#Generate plot
################################################################################
g_real <- 
  ggplot(mtm, aes(x = time)) +
  geom_line(aes(y = gdp_real, linetype = "GDP (LHS)", colour = "#000000"), size = 0.8) + 
  geom_bar(aes(y=(change+12)),stat="identity", colour="000000", width = 0.3) +
  scale_y_continuous(breaks = c(14,16,18,20,22), expand = c(0,0), sec.axis = sec_axis(~(.-12), name = "Firm bankruptcies (quantity)", breaks = c(0,1,2,3,4,5))) +
  scale_linetype_manual(values = c("GDP (LHS)" = "dashed", "Firm bankruptcies (RHS)" = "solid")) +
  scale_color_manual(values = c("#000000","#000000")) +
  geom_ribbon(aes(ymin = lowerBoundGDP, ymax = upperBoundGDP), alpha = 0.2) +
  labs(y = "GDP in production units",
       x = "Time in periods",
       linetype = "") + 
  guides(linetype = guide_legend(override.aes = list(size = 0.69) )) +
  guides(colour = FALSE) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.93),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=2.5),
        axis.title.y.right = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.75), keywidth = 3)) +
  xlim(200,300) +
  coord_cartesian(ylim=c(12,24))



#save graph in working directory
  cairo_pdf("SF10_firm_bankruptcies_counter-cyc_real.pdf", width=8, height=6)
  print(g_real)
  dev.off()


# #Optional
# ################################################################################
# #Correlogram
# ################################################################################
# #get cross correlation function tables
# #gdp and gdp
# g <- ccf(mtm$bk_gdp, mtm$bk_gdp, lag.max = 4, type="correlation")
# d <- do.call(rbind.data.frame, g)
# #get correlation coefficient
# f <- d[1,]
# #get lag coefficient and set as header
# colnames(f) <- (d[4,])
# #round to 4 digits
# f[nrow(f),] <- round(as.numeric(f[nrow(f),]), 4)
# print(f)
# 
# #gdp and firmBankruptcies
# g <- ccf(mtm$bk_change, mtm$bk_gdp, lag.max = 8, type="correlation")
# d <- do.call(rbind.data.frame, g)
# #get correlation coefficient
# f <- d[1,]
# #get lag coefficient and set as header
# colnames(f) <- (d[4,])
# #round to 4 digits
# f[nrow(f),] <- round(as.numeric(f[nrow(f),]), 4)
# print(f)
