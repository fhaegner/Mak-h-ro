#call rm() function to remove all objects
rm(list = ls())


#set working directory
setwd("C:/Users/...")

#get packages
library(tidyverse) #contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, rlang, lubridate, pillar

#get data
#SF6 uses run-0
run <- read_csv("C:/Users/.../run-0.csv")

#get firm size
firmSize <- max(run$id) + 1 #id starts at 0

#calculate mean
#mean of column production per group time
mtm <- aggregate(run[, "loanLiability"], list(run$time), sum) #needs to be aggregated
mtm$default <- aggregate(run[, "creditDefault"], list(run$time), mean)$creditDefault #already aggregated
mtm$sdDebt <- aggregate(run[, "loanLiability"], list(run$time), sd)$loanLiability

#rename time
mtm <- mtm %>% rename(time = Group.1)

mtm$sdDebt <- mtm$sdDebt * firmSize

mtm$marginErrorDebt <- qnorm(.95)*(mtm$sdDebt/sqrt(firmSize)) #10% confidence interval
mtm$lowerBoundDebt <- mtm$loanLiability - mtm$marginErrorDebt
mtm$upperBoundDebt <- mtm$loanLiability + mtm$marginErrorDebt


mtm <- mtm[!(mtm$loanLiability>6000),]
mtm <- mtm[!(mtm$default>1400),]


################################################################################
#Generate plot - GDP and default
################################################################################
g <-
  ggplot(mtm, aes(x = time)) +
  geom_line(aes(y = loanLiability/1000, linetype = "Firm debt (LHS)", colour = "#000000")) +
  geom_line(aes(y = (default*4)/1000, linetype = "Loan losses (RHS)", colour = "000000")) +
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", scientific = FALSE), 
                     sec.axis = sec_axis(~(./4), name = "Loan losses in nominal units (thds.)", 
                                         labels=function(x) format(x, big.mark = ",", scientific = FALSE))) +
  scale_linetype_manual(values = c("Firm debt (LHS)" = "dashed", "Loan losses (RHS)" = "solid")) +
  scale_color_manual(values = c("#000000","#000000")) +
  geom_ribbon(aes(ymin = lowerBoundDebt/1000, ymax = upperBoundDebt/1000), alpha = 0.2) +
  labs(y = "Firm debt in nominal units (thds.)",
       x = "Time in periods",
       linetype = "") + 
  guides(linetype = guide_legend(override.aes = list(size = 0.69) )) +
  guides(colour = FALSE) +
  theme_bw() +
  theme(legend.position = c(0.82, 0.92),
        legend.title = element_blank(),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(vjust=1.5),
        axis.title.y.right = element_text(vjust=2.5)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 3)) +
  xlim(200,600)

#save graph in working directory
cairo_pdf("SF6_debt_default.pdf", width=8, height=6)
print(g)
dev.off()
