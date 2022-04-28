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
  run <- read_csv(file = files[i])
  run <- run[!(run$time<200),]
  
#get firm size
firmSize <- max(run$id) + 1 #id starts at 0  
hhSize <- firmSize * 10 #Mak(h)ro_0 assumes household size ten times firmSize
  
#calculate mean
#mean of column production per group time
mtm <- aggregate(run[, "inflationAverage"], list(run$time), mean)
employment <- aggregate(run[, "employees"], list(run$time), mean)
employmentSum <- aggregate(run[, "employees"], list(run$time), sum)
unemployment <- (10 - employment) * firmSize
unemployeesSum <- (hhSize - employmentSum) / hhSize

#rename time
mtm <- mtm %>% rename(time = Group.1)

#merge
mtm$unemployment <- unemployment$employees
mtm$employment <- employment$employees
mtm$unemploymentSum <- unemployeesSum$employees

#unemployment percentage
mtm$unemployment <- mtm$unemployment / hhSize
mtm$natural <- mtm$unemployment - 0.05 #0.05 = due to natural unemployment

run_list[[k]] <- mtm
k = k + 1
}

#merge
mtm <- rbindlist(run_list)

#rectify time sequence
mtm <- mtm %>% mutate(time = 1:n())


mtm <- mtm[mtm$natural < 0.2, ]
mtm <- mtm[mtm$inflationAverage > -0.1, ]

######################
#quarterly measurement
mtm_unemploy <- data.frame(mtm$time)
mtm_unemploy <- mtm_unemploy %>% rename(time = mtm.time)
mtm_unemploy$unemployees <- mtm$natural
#mtm_unemploy <- mtm_unemploy[-1,]
mtm_unemploy <- setDT(mtm_unemploy)[,.(unemployees=mean(unemployees)), time -0:2]

mtm_inflation <- data.frame(mtm$time)
mtm_inflation <- mtm_inflation %>% rename(time = mtm.time)
mtm_inflation$inflation <- mtm$inflationAverage
#mtm_inflation <- mtm_inflation[-1,]
mtm_inflation <- setDT(mtm_inflation)[,.(inflation=mean(inflation)), time -0:2]

mtm_unemploy$inflation <- mtm_inflation$inflation
mtm_unemploy$time <- NULL
mtm_unemploy$time <- seq.int(nrow(mtm_unemploy))
#mtm_unemploy$inflation <- round(mtm_unemploy$inflation,5)
#mtm_unemploy$unemployees <- round(mtm_unemploy$unemployees,5)

#Delete duplicates
mtm_unemploy <- mtm_unemploy[!duplicated(mtm_unemploy$unemployees), ]

model <- lm(inflation ~ unemployees, data=mtm_unemploy)
summary(model)
######################
model <- lm(inflation ~ unemployees, data=mtm_unemploy)
summary(model)

#can be included in graph
lm_eqn <- function(mtm){
  m <- lm(inflation ~ unemployees, mtm_unemploy);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
print(lm_eqn(mtm))


################################################################################
#Generate plot
################################################################################
g <-
  ggplot(mtm_unemploy, aes(x = unemployees, y = inflation)) +
  geom_point(shape = 16, size = 2, aes(colour = "Simulated data")) +
  geom_smooth(aes(linetype = "fitted line"),color=1,method = "lm", se = TRUE) +
  scale_color_manual(values = c("#808080")) +
  scale_x_continuous(breaks = c(-0.05,0,0.05,0.1,0.15,0.2)) +
  labs(y = "Inflation in %",
       x = "Unemployment Rate (minus natural unemployment rate) in %",
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
  cairo_pdf("SF11_philipps_curve.pdf", width=8, height=6)
  print(g)
  dev.off()

