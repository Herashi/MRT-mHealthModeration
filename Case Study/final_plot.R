## This file generates direct effect estimates 
## If RDS files for the models already exist
## Go to the file "present_results.R" to avoid re-running 
## as code takes a while to generate the MI estimates.

## Code to combine data over several impuations and properly pools the results, 
## Since the randomization probability is constant, we can do this with gee

setwd("~/MRT/Case Study/lag effect")
library(ggplot2)




#############################
# plot mood
#############################

direct_mood_agg_results <- readRDS("direct_mood_agg_results.RDS")
direct_mood_agg_results2 <- readRDS("direct_mood_agg_results2.RDS")
sum_MOOD_wcls = rep(0,2)
sum_MOOD_cwcls = rep(0,2)
varsum_MOOD_wcls = matrix(0, nrow = 2, ncol = 2)
varsum_MOOD_cwcls = matrix(0, nrow = 2, ncol = 2)


for (i in 1:20){
  ### WCLS
  coeff = direct_mood_agg_results[[i]][["coefficients"]]
  coeff = coeff[c("week_category_new","week_category_new:MOODprev")]
  sum_MOOD_wcls = sum_MOOD_wcls +coeff
  
  
  vcov_m = vcov(direct_mood_agg_results[[i]])
  
  varsum_MOOD_wcls[1,1] = varsum_MOOD_wcls[1,1]+ vcov_m["week_category_new","week_category_new"]
  varsum_MOOD_wcls[1,2] = varsum_MOOD_wcls[1,2]+ vcov_m["week_category_new","week_category_new:MOODprev"]
  varsum_MOOD_wcls[2,1] = varsum_MOOD_wcls[1,2]
  varsum_MOOD_wcls[2,2] = varsum_MOOD_wcls[2,2]+ vcov_m["week_category_new:MOODprev","week_category_new:MOODprev"]
  
  ### C-WCLS
  coeff = direct_mood_agg_results2[[i]][["coefficients"]]
  coeff = coeff[c("week_category_new","week_category_new:MOODprev")]
  print(coeff)
  sum_MOOD_cwcls = sum_MOOD_cwcls +coeff
  
  vcov_m = vcov(direct_mood_agg_results2[[i]])
  
  varsum_MOOD_cwcls[1,1] = varsum_MOOD_cwcls[1,1]+ vcov_m["week_category_new","week_category_new"]
  varsum_MOOD_cwcls[1,2] = varsum_MOOD_cwcls[1,2]+ vcov_m["week_category_new","week_category_new:MOODprev"]
  varsum_MOOD_cwcls[2,1] = varsum_MOOD_cwcls[1,2]
  varsum_MOOD_cwcls[2,2] = varsum_MOOD_cwcls[2,2]+ vcov_m["week_category_new:MOODprev","week_category_new:MOODprev"]
  
}

MOOD_wcls = sum_MOOD_wcls/20
MOOD_cwcls = sum_MOOD_cwcls/20
var_MOOD_wcls = varsum_MOOD_wcls/20
var_MOOD_cwcls = varsum_MOOD_cwcls/20

data = as.data.frame(matrix(NA, nrow = 200, ncol = 5))
colnames(data) = c("x","y","lower","upper","Method")
data$x = rep(seq(1,10,length.out = 100),2)
data$Method = rep(c("C-WCLS","WCLS"),each = 100)

data$y[1:100] = MOOD_cwcls[1]+ MOOD_cwcls[2] *data$x[1:100]
data$y[101:200] = MOOD_wcls[1]+MOOD_wcls[2]*data$x[101:200]



########## part 1
var_cor_cwcls = var_MOOD_cwcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_cor_cwcls%*%c(1,x))

se_cor = sapply(data$x[1:100],FUN=calculate_se)


# length(unique(temp$Specialty)) = 24
t_quantile <- qt(0.975, 24 - 13)

data$lower[1:100] = data$y[1:100] - t_quantile*se_cor
data$upper[1:100] = data$y[1:100] + t_quantile*se_cor


####### part2

var_cor_wcls = var_MOOD_wcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_cor_wcls%*%c(1,x))

se_cor = sapply(data$x[101:200],FUN=calculate_se)


# length(unique(temp$UserID)) = 1562
t_quantile <- qt(0.975, 1562 - 13)

data$lower[101:200] = data$y[101:200] - t_quantile*se_cor
data$upper[101:200] = data$y[101:200] + t_quantile*se_cor



p<-ggplot(data=data, aes(x=x, y=y, colour=Method)) + 
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
  xlab("Average Mood Score in Week t")+
  ylab("Mood Score")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_hline(yintercept =0, linetype=2)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


###############################
# plot steps
################################


direct_step_results <- readRDS("direct_step_results.RDS")
direct_step_results2 <- readRDS("direct_step_results2.RDS")
sum_STEP_COUNT_wcls = rep(0,2)
sum_STEP_COUNT_cwcls = rep(0,2)
varsum_STEP_COUNT_wcls = matrix(0, nrow = 2, ncol = 2)
varsum_STEP_COUNT_cwcls = matrix(0, nrow = 2, ncol = 2)


for (i in 1:20){
  ### WCLS
  coeff = direct_step_results[[i]][["coefficients"]]
  coeff = coeff[c("week_category_new","week_category_new:STEP_COUNTprev")]
  sum_STEP_COUNT_wcls = sum_STEP_COUNT_wcls +coeff
  
  
  vcov_m = vcov(direct_step_results[[i]])
  
  varsum_STEP_COUNT_wcls[1,1] = varsum_STEP_COUNT_wcls[1,1]+ vcov_m["week_category_new","week_category_new"]
  varsum_STEP_COUNT_wcls[1,2] = varsum_STEP_COUNT_wcls[1,2]+ vcov_m["week_category_new","week_category_new:STEP_COUNTprev"]
  varsum_STEP_COUNT_wcls[2,1] = varsum_STEP_COUNT_wcls[1,2]
  varsum_STEP_COUNT_wcls[2,2] = varsum_STEP_COUNT_wcls[2,2]+ vcov_m["week_category_new:STEP_COUNTprev","week_category_new:STEP_COUNTprev"]
  
  ### C-WCLS
  coeff = direct_step_results2[[i]][["coefficients"]]
  coeff = coeff[c("week_category_new","week_category_new:STEP_COUNTprev")]
  print(coeff)
  sum_STEP_COUNT_cwcls = sum_STEP_COUNT_cwcls +coeff
  
  vcov_m = vcov(direct_step_results2[[i]])
  
  varsum_STEP_COUNT_cwcls[1,1] = varsum_STEP_COUNT_cwcls[1,1]+ vcov_m["week_category_new","week_category_new"]
  varsum_STEP_COUNT_cwcls[1,2] = varsum_STEP_COUNT_cwcls[1,2]+ vcov_m["week_category_new","week_category_new:STEP_COUNTprev"]
  varsum_STEP_COUNT_cwcls[2,1] = varsum_STEP_COUNT_cwcls[1,2]
  varsum_STEP_COUNT_cwcls[2,2] = varsum_STEP_COUNT_cwcls[2,2]+ vcov_m["week_category_new:STEP_COUNTprev","week_category_new:STEP_COUNTprev"]
  
}

STEP_COUNT_wcls = sum_STEP_COUNT_wcls/20
STEP_COUNT_cwcls = sum_STEP_COUNT_cwcls/20
var_STEP_COUNT_wcls = varsum_STEP_COUNT_wcls/20
var_STEP_COUNT_cwcls = varsum_STEP_COUNT_cwcls/20

data = as.data.frame(matrix(NA, nrow = 200, ncol = 5))
colnames(data) = c("x","y","lower","upper","Method")
data$x = rep(seq(2,30,length.out = 100),2)
data$Method = rep(c("C-WCLS","WCLS"),each = 100)

data$y[1:100] = STEP_COUNT_cwcls[1]+ STEP_COUNT_cwcls[2] *data$x[1:100]
data$y[101:200] = STEP_COUNT_wcls[1]+STEP_COUNT_wcls[2]*data$x[101:200]



########## part 1
var_cor_cwcls = var_STEP_COUNT_cwcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_cor_cwcls%*%c(1,x))

se_cor = sapply(data$x[1:100],FUN=calculate_se)


# length(unique(temp$Specialty)) = 24
t_quantile <- qt(0.975, 24 - 13)

data$lower[1:100] = data$y[1:100] - t_quantile*se_cor
data$upper[1:100] = data$y[1:100] + t_quantile*se_cor


####### part2

var_cor_wcls = var_STEP_COUNT_wcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_cor_wcls%*%c(1,x))

se_cor = sapply(data$x[101:200],FUN=calculate_se)


# length(unique(temp$UserID)) = 1562
t_quantile <- qt(0.975, 1562 - 13)

data$lower[101:200] = data$y[101:200] - t_quantile*se_cor
data$upper[101:200] = data$y[101:200] + t_quantile*se_cor



p2<-ggplot(data=data, aes(x=x, y=y, colour=Method)) + 
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
  xlab("Average Step Count in Week t")+
  ylab("Step Count")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_hline(yintercept =0, linetype=2)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


###############################
# plot sleep
################################

direct_sleep_results <- readRDS("direct_sleep_results.RDS")
direct_sleep_results2 <- readRDS("direct_sleep_results2.RDS")
sum_SLEEP_COUNT_wcls = rep(0,2)
sum_SLEEP_COUNT_cwcls = rep(0,2)
varsum_SLEEP_COUNT_wcls = matrix(0, nrow = 2, ncol = 2)
varsum_SLEEP_COUNT_cwcls = matrix(0, nrow = 2, ncol = 2)


for (i in 1:20){
  ### WCLS
  coeff = direct_sleep_results[[i]][["coefficients"]]
  coeff = coeff[c("week_category_new","week_category_new:SLEEP_COUNTprev")]
  sum_SLEEP_COUNT_wcls = sum_SLEEP_COUNT_wcls +coeff
  
  
  vcov_m = vcov(direct_sleep_results[[i]])
  
  varsum_SLEEP_COUNT_wcls[1,1] = varsum_SLEEP_COUNT_wcls[1,1]+ vcov_m["week_category_new","week_category_new"]
  varsum_SLEEP_COUNT_wcls[1,2] = varsum_SLEEP_COUNT_wcls[1,2]+ vcov_m["week_category_new","week_category_new:SLEEP_COUNTprev"]
  varsum_SLEEP_COUNT_wcls[2,1] = varsum_SLEEP_COUNT_wcls[1,2]
  varsum_SLEEP_COUNT_wcls[2,2] = varsum_SLEEP_COUNT_wcls[2,2]+ vcov_m["week_category_new:SLEEP_COUNTprev","week_category_new:SLEEP_COUNTprev"]
  
  ### C-WCLS
  coeff = direct_sleep_results2[[i]][["coefficients"]]
  coeff = coeff[c("week_category_new","week_category_new:SLEEP_COUNTprev")]
  print(coeff)
  sum_SLEEP_COUNT_cwcls = sum_SLEEP_COUNT_cwcls +coeff
  
  vcov_m = vcov(direct_sleep_results2[[i]])
  
  varsum_SLEEP_COUNT_cwcls[1,1] = varsum_SLEEP_COUNT_cwcls[1,1]+ vcov_m["week_category_new","week_category_new"]
  varsum_SLEEP_COUNT_cwcls[1,2] = varsum_SLEEP_COUNT_cwcls[1,2]+ vcov_m["week_category_new","week_category_new:SLEEP_COUNTprev"]
  varsum_SLEEP_COUNT_cwcls[2,1] = varsum_SLEEP_COUNT_cwcls[1,2]
  varsum_SLEEP_COUNT_cwcls[2,2] = varsum_SLEEP_COUNT_cwcls[2,2]+ vcov_m["week_category_new:SLEEP_COUNTprev","week_category_new:SLEEP_COUNTprev"]
  
}

SLEEP_COUNT_wcls = sum_SLEEP_COUNT_wcls/20
SLEEP_COUNT_cwcls = sum_SLEEP_COUNT_cwcls/20
var_SLEEP_COUNT_wcls = varsum_SLEEP_COUNT_wcls/20
var_SLEEP_COUNT_cwcls = varsum_SLEEP_COUNT_cwcls/20

data = as.data.frame(matrix(NA, nrow = 200, ncol = 5))
colnames(data) = c("x","y","lower","upper","Method")
data$x = rep(seq(9,30,length.out = 100),2)
data$Method = rep(c("C-WCLS","WCLS"),each = 100)

data$y[1:100] = SLEEP_COUNT_cwcls[1]+ SLEEP_COUNT_cwcls[2] *data$x[1:100]
data$y[101:200] = SLEEP_COUNT_wcls[1]+SLEEP_COUNT_wcls[2]*data$x[101:200]



########## part 1
var_cor_cwcls = var_SLEEP_COUNT_cwcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_cor_cwcls%*%c(1,x))

se_cor = sapply(data$x[1:100],FUN=calculate_se)


# length(unique(temp$Specialty)) = 24
t_quantile <- qt(0.975, 24 - 13)

data$lower[1:100] = data$y[1:100] - t_quantile*se_cor
data$upper[1:100] = data$y[1:100] + t_quantile*se_cor


####### part2

var_cor_wcls = var_SLEEP_COUNT_wcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_cor_wcls%*%c(1,x))

se_cor = sapply(data$x[101:200],FUN=calculate_se)


# length(unique(temp$UserID)) = 1562
t_quantile <- qt(0.975, 1562 - 13)

data$lower[101:200] = data$y[101:200] - t_quantile*se_cor
data$upper[101:200] = data$y[101:200] + t_quantile*se_cor


p3<-ggplot(data=data, aes(x=x, y=y, colour=Method)) + 
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
  xlab("Average Sleep Count in Week t")+
  ylab("Sleep Count")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_hline(yintercept =0, linetype=2)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))



ggpubr::ggarrange(p,p2,p3,legend = "top",common.legend = T,nrow = 1)
