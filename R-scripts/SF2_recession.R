#call rm() function to remove all objects
rm(list = ls())


#set working directory
#setwd("C:/Users/...")

#get packages
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, rlang, lubridate, pillar
library(data.table)


#get data
#SF2 uses run-0
#run <- read_csv("C:/Users/.../run-0.csv")


df <- as.data.frame(run$time)
df <- df %>% rename(time = "run$time")
df$stock <- run$Stock #stock of inventories
df$inv <- run$investmentConsumeUnit
df$con <- run$consumptionUnit
df$id <- run$id


#delete warm-up phase of the model = first 200 periods
df <- df[!(df$time < 200),]

#get firm size
firmSize <- max(df$id) + 1 #id starts at 0

#stock change
setDT(df)[, stock_change := stock - shift(stock, n=firmSize)]
df[is.na(df)] <- 0

#generate GDP
df$gdp_real <- df$inv + df$con + df$stock_change

#calculate mean
#mean of column production per group time
mtm_real <- aggregate(df[, "gdp_real"], list(df$time), mean)
mtm_real <- mtm_real %>% rename(time = Group.1)


#calculate difference in production
#1st -> delete time 0
mtm_real <- mtm_real[-1,]

setDT(mtm_real)[, pct_change := gdp_real/shift(gdp_real) - 1]

#set 1 if production < 0, else 0
mtm_real <- transform(mtm_real, recession = ifelse(pct_change < 0, +1, 0))
mtm_real[is.na(mtm_real)] <- 0



#sum for each group with connected values of 1
mtm_real <- mtm_real %>%
  add_column(length = 0)
head(mtm_real)


#generate empty values
i <- 2 #start with second period due to NA in first row
k <- 0

#for-loop over rows within mtm_real
for(i in 2:nrow(mtm_real)) { 
  if (mtm_real[i,"recession"] == 1){
    k = k + 1
  }
  if (mtm_real[i,"recession"] == 0){
    mtm_real[i-1,"length"] <- k
    k = 0
  }
}


#delete all periods with positive growth
mtm_real <- mtm_real[apply(mtm_real, 1, function(row) all(row !=0 )), ]

#get frequency data
freq_real <- as.data.frame(table(mtm_real$length))


#################################
#delete long end - CHECK MANUALLY
#################################
#e.g., freq_real <- freq_real[-c(10:12),]
#e.g., freq_real <- freq_real[-c(7),]

freq_real <- transform(freq_real, relative = prop.table(Freq))

#rename variable for better organization
freq_real <- freq_real %>% rename(x = "Var1")
freq_real <- freq_real %>% rename(y = "Freq")

#set related variables as numeric
freq_real$x <- as.numeric(freq_real$x)
freq_real$relative <- as.numeric(freq_real$relative)

hist(mtm_real$length)


################################################################################
#Generate plot
################################################################################
g_real <-
  ggplot(freq_real, aes(x=x,y=relative)) + 
  geom_point(shape=16, size = 2, aes(color = "       Simulated data")) + 
  geom_smooth(method="lm", formula= (y ~ exp(x)), se=FALSE, color=1,aes(linetype = "Exponential fit")) +
  labs(x = "Duration of recessions in periods",
       y = "log(density)",
       linetype = "",
       colour = "") +
  scale_color_manual(values = "#000000") +
  scale_x_log10(
    breaks = freq_real$x) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(y) 10^y),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "trbl",
                      short = unit(1,"mm"),
                      mid = unit(3,"mm"),
                      long = unit(4,"mm")) +
  #scale_fill_manual(labels = c("1977", "")) +
  scale_shape_manual(values = c("Simulated data" = 16)) +
  scale_linetype_manual(values = c("Exponential fit" = 1)) +
  #guides(linetype = guide_legend(override.aes = list(shape = ""))) +
  #guides(linetype = guide_legend(override.aes = list(size = 0.69) )) +
  theme_bw() +
  theme(legend.position = c(0.83, 0.90),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.spacing.y = unit(-0.3, 'cm'),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Arial", size = 15),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 3))


#save graph in working directory
  cairo_pdf("SF2_recession_real.pdf", width=8, height=6)
  print(g_real)
  dev.off()
