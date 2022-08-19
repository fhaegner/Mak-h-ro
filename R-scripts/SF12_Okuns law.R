#call rm() function to remove all objects
rm(list = ls())
gc()

#set working directory that directly links to the folder with all replications
setwd("C:/Users/.../config-0")

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
  
  df <- as.data.frame(run$time)
  df <- df %>% rename(time = "run$time")
  df$stock <- run$Stock #stock of inventories
  df$inv <- run$investmentConsumeUnit
  df$con <- run$consumptionUnit
  df$employees <- run$employees
  df$id <- run$id
  
  #get firm and household size
  firmSize <- max(df$id) + 1 #id starts at 0
  hhSize <- firmSize * 10 #Mak(h)ro_0 assumes household size ten times firmSize
  
  
  #stock change
  setDT(df)[, stock_change := stock - shift(stock, n=firmSize)]
  df[is.na(df)] <- 0
  
  #generate real GDP
  df$gdp_real <- df$inv + df$con + df$stock_change
  
    
  #calculate mean
  #mean of column production per group time
  mtm <- aggregate(df[, "gdp_real"], list(df$time), sum)

  #calculate unemployment 
  employeesSum <- aggregate(df[, "employees"], list(df$time), sum)
  unemployeesSum <- (hhSize - employeesSum) / hhSize

  #rename time
  mtm <- mtm %>% rename(time = Group.1)

  #merge
  mtm$unemployeesSum <- unemployeesSum$employees


  run_list[[k]] <- mtm
  k = k + 1
}

#merge
mtm <- rbindlist(run_list)

#rectify time sequence
mtm <- mtm %>% mutate(time = 1:n())



#generate log
mtm$log_gdp <- log(mtm$gdp_real)

mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
mtm[is.na(mtm)] <- 0

#calculate lag difference
setDT(mtm)[, gdp_change := gdp_real/shift(gdp_real) - 1]
setDT(mtm)[, unemployees_change := unemployeesSum - shift(unemployeesSum)]

mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
mtm[is.na(mtm)] <- 0


#Alternative calculation
#unemployment gap
mtm$uGap <- mtm$unemployeesSum - 0.05

#production gap
#potential production
potProd <- (hhSize * 2 * 0.95) #labor size * productivity parameter * 0.95
mtm$prodGap <- (mtm$gdp_real - potProd)/mtm$gdp_real * 100

mtm <- do.call(data.frame,lapply(mtm, function(delete_inf) replace(delete_inf, is.infinite(delete_inf),0)))
mtm[is.na(mtm)] <- 0


#get rid of outlier
mtm <- mtm[mtm$unemployees_change > -0.1, ]
mtm <- mtm[mtm$unemployees_change < 0.09, ]
mtm <- mtm[mtm$gdp_change > -0.25, ]

#delete duplicates
mtm <- mtm[!duplicated(mtm$unemployees_change), ]
#mtm <- mtm[!duplicated(mtm$prod_change), ]

model <- lm(gdp_change ~ unemployees_change, data=mtm)
summary(model)


################################################################################
#Generate plot
################################################################################
g <- 
  ggplot(mtm, aes(x = unemployees_change, y = gdp_change)) +
  geom_point(shape = 16, size = 2, aes(colour = "Simulated data")) +
  geom_smooth(aes(linetype = " fitted line"), color=1, method = "lm", level = 0.90, se = TRUE) +
  scale_color_manual(values = c("#808080")) +
  labs(y = "GDP in percentage change",
       x = "Unemployment Rate in percentage change",
       linetype = "",
       colour = "") + 
  #guides(linetype = guide_legend(override.aes = list(size = 0.69) )) +
  #guides(colour = FALSE) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.92),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.background = element_rect(fill = "white"), 
        legend.title = element_blank(),
        legend.key.height = unit(0.2,"cm"),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 1), keywidth = 3))


#save graph in working directory
  cairo_pdf("SF12_okuns_law.pdf", width=8, height=6)
  print(g)
  dev.off()


# model <- lm(prodGap ~ uGap, data=mtm)
# summary(model)

# #Alternative measurement with output gap
# ggplot(mtm, aes(x = uGap, y = prodGap)) +
#   geom_point(shape = 16, size = 3, aes(colour = "Simulated data")) +
#   geom_smooth(aes(linetype = "fitted line"),color=1,method = "lm", se = FALSE) +
#   scale_color_manual(values = c("#808080")) +
#   labs(y = "GDP gap",
#        x = "uGap",
#        linetype = "",
#        colour = "") + 
#   #guides(linetype = guide_legend(override.aes = list(size = 0.69) )) +
#   #guides(colour = FALSE) +
#   theme_bw() +
#   theme(legend.position = c(0.8, 0.92),
#         legend.spacing.y = unit(-0.1, 'cm'),
#         legend.background = element_rect(fill = "white"), 
#         legend.title = element_blank(),
#         legend.key.height = unit(0.2,"cm"),
#         text = element_text(family = "Arial", size = 14),
#         axis.text = element_text(size = 12)) +
#   guides(linetype = guide_legend(override.aes = list(size = 1), keywidth = 3))




