<<<<<<< HEAD
## This file generates direct effect estimates 
## If RDS files for the models already exist
## Go to the file "present_results.R" to avoid re-running 
## as code takes a while to generate the MI estimates.

## Code to combine data over several impuations and properly pools the results, 
## Since the randomization probability is constant, we can do this with gee

setwd("~/MRT/Case Study")
library(zoo)
source("xzoo.R")

load("~/MRT/Case Study/imputation_list_daily_separated_20.RData")
library(readr)
survey_dat = read_csv("IHSdata_2018.csv")

mood_results = mood_results2 =  mood_results3 = list()
mood_agg_results = mood_agg_results2 = mood_agg_results3 = list()
step_results = step_results2 = step_results3 = list()
step_agg_results = step_agg_results2 = step_agg_results3 = list()
sleep_results = sleep_results2 = sleep_results3 = list()
sleep_agg_results = sleep_agg_results2 = sleep_agg_results3 = list()

library(geepack)
library(mitml)

num_impute = impute_list$num_impute
for(impute_iter in 1:num_impute){
  cur_list = impute_list[[impute_iter]]
  
  all_baseline = cur_list$all_baseline
  full_data = cur_list$full_data
  colnames(full_data)[1] = "UserID"
  full_data_complete = cur_list$full_data_complete
  
  baseline_step = apply(all_baseline[,c(10,13,16)], MARGIN = 1, FUN = mean)
  baseline_sleep = apply(all_baseline[,c(11,14,17)], MARGIN = 1, FUN = mean)
  baseline_mood = apply(all_baseline[,c(12,15,18)], MARGIN = 1, FUN = mean)
  baseline_average = cbind(all_baseline$UserID, baseline_step, baseline_sleep, baseline_mood)
  baseline_average = data.frame(baseline_average)
  names(baseline_average)[1] = 'UserID'
  baseline_average$study_week = 1
  names(baseline_average)[2:4] = c('STEP_COUNTprev', 'SLEEP_COUNTprev', 'MOODprev')
  baseline_average$week_categoryprev = NA
  
  aggregate_weekly = aggregate(full_data_complete[, 5:7], by = full_data_complete[,c(1,3,4)], FUN = mean)
  aggregate_weekly2 = aggregate_weekly
  names(aggregate_weekly2)[3:6] = paste(names(aggregate_weekly2)[3:6], "prev", sep = '')
  aggregate_weekly2$study_week = aggregate_weekly2$study_week + 1
  aggregate_weekly2 = rbind(aggregate_weekly2, baseline_average)
  aggregate_weekly_new = merge(aggregate_weekly, aggregate_weekly2, by = c('UserID', 'study_week'), all.x = TRUE)
  
  aggregate_weekly_new1 = merge(aggregate_weekly_new, all_baseline[,1:9], by = 'UserID', all.x = TRUE)
  
  analysis_dat = aggregate_weekly_new1[, -7]
  analysis_dat = analysis_dat[analysis_dat$week_category != 'unsure', ]
  analysis_dat$week_category_new = as.numeric(analysis_dat$week_category != 'None')
  analysis_dat$week_category = relevel(analysis_dat$week_category, ref = 'None')
  analysis_dat_gee = analysis_dat[order(analysis_dat$UserID, analysis_dat$study_week), ]
  analysis_dat_gee$week_category= droplevels(analysis_dat_gee$week_category)
  
  temp = merge(analysis_dat_gee, survey_dat[,c(1,4,5)], by = 'UserID', all.x = TRUE)
  temp$INSTITUTION_STANDARD = as.factor(temp$INSTITUTION_STANDARD)
  temp$Specialty = as.factor(temp$Specialty)
  temp = temp[!is.na(temp$Specialty),]
  test = aggregate(UserID ~ Specialty, data = temp, FUN = function(x){length(unique(x))})
  
  # weights
  howmany = function(x) {test$UserID[test$Specialty == x]}
  w = 1/unlist(lapply(temp$Specialty, howmany))
  # 
  # Boruvka's weights
  p_tilde = mean(analysis_dat_gee$week_category_new)
  analysis_dat_gee$weights = ifelse(analysis_dat_gee$week_category_new==1,p_tilde/(3/4),(1-p_tilde)/(1/4))
  
  p_tilde = mean(temp$week_category_new)
  temp$weights = ifelse(temp$week_category_new==1,p_tilde/(3/4),(1-p_tilde)/(1/4))
  temp$weights = temp$weights * w


  ## CONSTRUCT GROUP-LEVEL MOODprev
  aggMOODprev = aggregate(MOODprev~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = mean)
  countMOODprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = sum)
  names(aggMOODprev) = c("study_week", "Specialty", "MOODprev")
  names(countMOODprev) = c("study_week", "Specialty", "MOODprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggMOODprev$MOODprev[aggMOODprev$study_week == x$study_week & aggMOODprev$Specialty == x$Specialty]
    count = countMOODprev$MOODprev[countMOODprev$study_week == x$study_week & countMOODprev$Specialty == x$Specialty]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggMOODprev = test
  
  # Construct Y_{t+2}
  temp$MOODnext = delay(temp$UserID, temp$study_week, temp$MOOD,-1)
  analysis_dat_gee$MOODnext = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$MOOD,-1)
  
  ## ANOVA ONLY FOR 1st IMPUTATION!
  # if(impute_iter == 1){
  #   two.way <- aov(MOOD ~ as.factor(study_week) + INSTITUTION_STANDARD:week_category + Specialty:week_category, data = temp)
  #   print(summary(two.way))
  # }
  
  gee_result_mood = geeglm(MOOD ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                             Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev, data = analysis_dat_gee, weights = weights,id = UserID, scale.fix = T)
  gee_result_mood2 = geeglm(MOOD ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                             Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result_mood3 = geeglm(MOOD ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                              Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev + week_category:aggMOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  mood_results[[impute_iter]] = gee_result_mood
  mood_results2[[impute_iter]] = gee_result_mood2
  mood_results3[[impute_iter]] = gee_result_mood3
  
  gee_result_mood_agg = geeglm(MOOD ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 + 
                                 Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev, data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result_mood_agg2 = geeglm(MOOD ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 + 
                                 Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result_mood_agg3 = geeglm(MOOD ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 + 
                                  Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev + week_category_new:aggMOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  mood_agg_results[[impute_iter]] = gee_result_mood_agg
  mood_agg_results2[[impute_iter]] = gee_result_mood_agg2
  mood_agg_results3[[impute_iter]] = gee_result_mood_agg3
  
  ## CONSTRUCT GROUP-LEVEL STEP_COUNTprev
  aggSTEP_COUNTprev = aggregate(STEP_COUNTprev~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = mean)
  countSTEP_COUNTprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = sum)
  names(aggSTEP_COUNTprev) = c("study_week", "Specialty", "STEP_COUNTprev")
  names(countSTEP_COUNTprev) = c("study_week", "Specialty", "STEP_COUNTprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggSTEP_COUNTprev$STEP_COUNTprev[aggSTEP_COUNTprev$study_week == x$study_week & aggSTEP_COUNTprev$Specialty == x$Specialty]
    count = countSTEP_COUNTprev$STEP_COUNTprev[countSTEP_COUNTprev$study_week == x$study_week & countSTEP_COUNTprev$Specialty == x$Specialty]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggSTEP_COUNTprev = test
  
  # Construct Y_{t+2}
  temp$STEP_COUNTnext = delay(temp$UserID, temp$study_week, temp$STEP_COUNT,-1)
  analysis_dat_gee$STEP_COUNTnext = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$STEP_COUNT,-1)

  gee_result = geeglm(STEP_COUNT  ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:STEP_COUNTprev, data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result2 = geeglm(STEP_COUNT  ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:STEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result3 = geeglm(STEP_COUNT  ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                         Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:STEP_COUNTprev + week_category_new:aggSTEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  step_results[[impute_iter]] = gee_result
  step_results2[[impute_iter]] = gee_result2
  step_results3[[impute_iter]] = gee_result3

  ## CONSTRUCT GROUP-LEVEL SLEEP_COUNTprev
  aggSLEEP_COUNTprev = aggregate(SLEEP_COUNTprev~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = mean)
  countSLEEP_COUNTprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = sum)
  names(aggSLEEP_COUNTprev) = c("study_week", "Specialty", "SLEEP_COUNTprev")
  names(countSLEEP_COUNTprev) = c("study_week", "Specialty", "SLEEP_COUNTprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggSLEEP_COUNTprev$SLEEP_COUNTprev[aggSLEEP_COUNTprev$study_week == x$study_week & aggSLEEP_COUNTprev$Specialty == x$Specialty]
    count = countSLEEP_COUNTprev$SLEEP_COUNTprev[countSTEP_COUNTprev$study_week == x$study_week & countSLEEP_COUNTprev$Specialty == x$Specialty]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggSLEEP_COUNTprev = test
  
  # Construct Y_{t+2}
  temp$SLEEP_COUNTnext = delay(temp$UserID, temp$study_week, temp$SLEEP_COUNT,-1)
  analysis_dat_gee$SLEEP_COUNTnext = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$SLEEP_COUNT,-1)

  gee_result = geeglm(SLEEP_COUNT  ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:SLEEP_COUNTprev, data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result2 = geeglm(SLEEP_COUNT  ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:SLEEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result3 = geeglm(SLEEP_COUNT  ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                         Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:SLEEP_COUNTprev + week_category_new:aggSLEEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  sleep_results[[impute_iter]] = gee_result
  sleep_results2[[impute_iter]] = gee_result2
  sleep_results3[[impute_iter]] = gee_result3

  print(impute_iter)
}
testEstimates(mood_results)
testEstimates(mood_results2)
testEstimates(mood_results3)

saveRDS(mood_results, file = "direct_mood_results.RDS")
saveRDS(mood_results2, file = "direct_mood_results2.RDS")
saveRDS(mood_results3, file = "direct_mood_results3.RDS")

testEstimates(mood_agg_results)
testEstimates(mood_agg_results2)
testEstimates(mood_agg_results3)

saveRDS(mood_agg_results, file = "direct_mood_agg_results.RDS")
saveRDS(mood_agg_results2, file = "direct_mood_agg_results2.RDS")
saveRDS(mood_agg_results3, file = "direct_mood_agg_results3.RDS")

testEstimates(step_results)
testEstimates(step_results2)
testEstimates(step_results3)

saveRDS(step_results, file = "direct_step_results.RDS")
saveRDS(step_results2, file = "direct_step_results2.RDS")
saveRDS(step_results3, file = "direct_step_results3.RDS")

testEstimates(sleep_results)
testEstimates(sleep_results2)
testEstimates(sleep_results3)

saveRDS(sleep_results, file = "direct_sleep_results.RDS")
saveRDS(sleep_results2, file = "direct_sleep_results2.RDS")
saveRDS(sleep_results3, file = "direct_sleep_results3.RDS")

specialties = unique(temp$Specialty)
result = vector(length = length(specialties))
for (i in 1:length(specialties)) {
  specialty = specialties[i]
  result[i] = length(unique(temp$UserID[temp$Specialty == specialty]))
}
result
=======
## This file generates direct effect estimates 
## If RDS files for the models already exist
## Go to the file "present_results.R" to avoid re-running 
## as code takes a while to generate the MI estimates.

## Code to combine data over several impuations and properly pools the results, 
## Since the randomization probability is constant, we can do this with gee

setwd("~/MRT/Case Study/lag effect")
library(zoo)
source("xzoo.R")

load("imputation_list_daily_separated_20.RData")
library(readr)
survey_dat = read_csv("IHSdata_2018.csv")

mood_results = mood_results2 =  mood_results3 = list()
mood_agg_results = mood_agg_results2 = mood_agg_results3 = list()
step_results = step_results2 = step_results3 = list()
step_agg_results = step_agg_results2 = step_agg_results3 = list()
sleep_results = sleep_results2 = sleep_results3 = list()
sleep_agg_results = sleep_agg_results2 = sleep_agg_results3 = list()

library(geepack)
library(mitml)

num_impute = impute_list$num_impute
for(impute_iter in 1:num_impute){
  cur_list = impute_list[[impute_iter]]
  
  all_baseline = cur_list$all_baseline
  full_data = cur_list$full_data
  colnames(full_data)[1] = "UserID"
  full_data_complete = cur_list$full_data_complete
  
  baseline_step = apply(all_baseline[,c(10,13,16)], MARGIN = 1, FUN = mean)
  baseline_sleep = apply(all_baseline[,c(11,14,17)], MARGIN = 1, FUN = mean)
  baseline_mood = apply(all_baseline[,c(12,15,18)], MARGIN = 1, FUN = mean)
  baseline_average = cbind(all_baseline$UserID, baseline_step, baseline_sleep, baseline_mood)
  baseline_average = data.frame(baseline_average)
  names(baseline_average)[1] = 'UserID'
  baseline_average$study_week = 1
  names(baseline_average)[2:4] = c('STEP_COUNTprev', 'SLEEP_COUNTprev', 'MOODprev')
  baseline_average$week_categoryprev = NA
  
  aggregate_weekly = aggregate(full_data_complete[, 5:7], by = full_data_complete[,c(1,3,4)], FUN = mean)
  aggregate_weekly2 = aggregate_weekly
  names(aggregate_weekly2)[3:6] = paste(names(aggregate_weekly2)[3:6], "prev", sep = '')
  aggregate_weekly2$study_week = aggregate_weekly2$study_week + 1
  aggregate_weekly2 = rbind(aggregate_weekly2, baseline_average)
  aggregate_weekly_new = merge(aggregate_weekly, aggregate_weekly2, by = c('UserID', 'study_week'), all.x = TRUE)
  
  aggregate_weekly_new1 = merge(aggregate_weekly_new, all_baseline[,1:9], by = 'UserID', all.x = TRUE)
  
  analysis_dat = aggregate_weekly_new1[, -7]
  analysis_dat = analysis_dat[analysis_dat$week_category != 'unsure', ]
  analysis_dat$week_category_new = as.numeric(analysis_dat$week_category != 'None')
  analysis_dat$week_category = relevel(analysis_dat$week_category, ref = 'None')
  analysis_dat_gee = analysis_dat[order(analysis_dat$UserID, analysis_dat$study_week), ]
  analysis_dat_gee$week_category= droplevels(analysis_dat_gee$week_category)
  
  temp = merge(analysis_dat_gee, survey_dat[,c(1,4,5)], by = 'UserID', all.x = TRUE)
  temp$INSTITUTION_STANDARD = as.factor(temp$INSTITUTION_STANDARD)
  temp$Specialty = as.factor(temp$Specialty)
  temp = temp[!is.na(temp$Specialty),]
  test = aggregate(UserID ~ Specialty, data = temp, FUN = function(x){length(unique(x))})
  
  # weights
  howmany = function(x) {test$UserID[test$Specialty == x]}
  w = 1/unlist(lapply(temp$Specialty, howmany))

  # Boruvka's weights
  p_tilde = mean(analysis_dat_gee$week_category_new)
  analysis_dat_gee$weights = ifelse(analysis_dat_gee$week_category_new==1,p_tilde/(3/4),
                                    (1-p_tilde)/(1/4))
  
  p_tilde = mean(temp$week_category_new)
  temp$weights = ifelse(temp$week_category_new==1,p_tilde/(3/4),(1-p_tilde)/(1/4))
  temp$weights = temp$weights * w
  

  ## CONSTRUCT GROUP-LEVEL MOODprev
  aggMOODprev = aggregate(MOODprev~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = mean)
  countMOODprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = sum)
  names(aggMOODprev) = c("study_week", "Specialty", "MOODprev")
  names(countMOODprev) = c("study_week", "Specialty", "MOODprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggMOODprev$MOODprev[aggMOODprev$study_week == x$study_week & aggMOODprev$Specialty == x$Specialty]
    count = countMOODprev$MOODprev[countMOODprev$study_week == x$study_week & countMOODprev$Specialty == x$Specialty]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggMOODprev = test
  
  # all 1 weights
  # temp$week_category_new_next = delay(temp$UserID, temp$study_week, temp$week_category_new, -1)
  # analysis_dat_gee$week_category_new_next = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$week_category_new, -1)
  # 
  # analysis_dat_gee$weights = ifelse(analysis_dat_gee$week_category_new_next==1,analysis_dat_gee$weights * (1/(3/4)),0)
  # temp$weights = ifelse(temp$week_category_new_next==1,temp$weights * (1/(3/4)),0)
  
  # Construct Y_{t+2}
  temp$MOODnext = delay(temp$UserID, temp$study_week, temp$MOOD, -1)
  analysis_dat_gee$MOODnext = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$MOOD,-1)
  
  gee_result_mood = geeglm(MOODnext ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                             Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev, data = analysis_dat_gee, weights = weights,id = UserID, scale.fix = T)
  gee_result_mood2 = geeglm(MOODnext ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                             Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result_mood3 = geeglm(MOODnext ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                              Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev + week_category:aggMOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  mood_results[[impute_iter]] = gee_result_mood
  mood_results2[[impute_iter]] = gee_result_mood2
  mood_results3[[impute_iter]] = gee_result_mood3
  
  gee_result_mood_agg = geeglm(MOODnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 + 
                                 Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev, data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result_mood_agg2 = geeglm(MOODnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 + 
                                 Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result_mood_agg3 = geeglm(MOODnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 + 
                                  Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev + week_category_new:aggMOODprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  mood_agg_results[[impute_iter]] = gee_result_mood_agg
  mood_agg_results2[[impute_iter]] = gee_result_mood_agg2
  mood_agg_results3[[impute_iter]] = gee_result_mood_agg3
  
  ## CONSTRUCT GROUP-LEVEL STEP_COUNTprev
  aggSTEP_COUNTprev = aggregate(STEP_COUNTprev~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = mean)
  countSTEP_COUNTprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = sum)
  names(aggSTEP_COUNTprev) = c("study_week", "Specialty", "STEP_COUNTprev")
  names(countSTEP_COUNTprev) = c("study_week", "Specialty", "STEP_COUNTprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggSTEP_COUNTprev$STEP_COUNTprev[aggSTEP_COUNTprev$study_week == x$study_week & aggSTEP_COUNTprev$Specialty == x$Specialty]
    count = countSTEP_COUNTprev$STEP_COUNTprev[countSTEP_COUNTprev$study_week == x$study_week & countSTEP_COUNTprev$Specialty == x$Specialty]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggSTEP_COUNTprev = test
  
  # Construct Y_{t+2}
  temp$STEP_COUNTnext = delay(temp$UserID, temp$study_week, temp$STEP_COUNT,-1)
  analysis_dat_gee$STEP_COUNTnext = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$STEP_COUNT,-1)

  gee_result = geeglm(STEP_COUNTnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:STEP_COUNTprev, data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result2 = geeglm(STEP_COUNTnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:STEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result3 = geeglm(STEP_COUNTnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                         Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:STEP_COUNTprev + week_category_new:aggSTEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  step_results[[impute_iter]] = gee_result
  step_results2[[impute_iter]] = gee_result2
  step_results3[[impute_iter]] = gee_result3

  ## CONSTRUCT GROUP-LEVEL SLEEP_COUNTprev
  aggSLEEP_COUNTprev = aggregate(SLEEP_COUNTprev~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = mean)
  countSLEEP_COUNTprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty), data = temp, FUN = sum)
  names(aggSLEEP_COUNTprev) = c("study_week", "Specialty", "SLEEP_COUNTprev")
  names(countSLEEP_COUNTprev) = c("study_week", "Specialty", "SLEEP_COUNTprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggSLEEP_COUNTprev$SLEEP_COUNTprev[aggSLEEP_COUNTprev$study_week == x$study_week & aggSLEEP_COUNTprev$Specialty == x$Specialty]
    count = countSLEEP_COUNTprev$SLEEP_COUNTprev[countSTEP_COUNTprev$study_week == x$study_week & countSLEEP_COUNTprev$Specialty == x$Specialty]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggSLEEP_COUNTprev = test
  
  # Construct Y_{t+2}
  temp$SLEEP_COUNTnext = delay(temp$UserID, temp$study_week, temp$SLEEP_COUNT,-1)
  analysis_dat_gee$SLEEP_COUNTnext = delay(analysis_dat_gee$UserID, analysis_dat_gee$study_week, analysis_dat_gee$SLEEP_COUNT,-1)

  gee_result = geeglm(SLEEP_COUNTnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:SLEEP_COUNTprev, data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result2 = geeglm(SLEEP_COUNTnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:SLEEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  gee_result3 = geeglm(SLEEP_COUNTnext ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                         Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category_new:SLEEP_COUNTprev + week_category_new:aggSLEEP_COUNTprev, data = temp, id = Specialty, scale.fix = T, weights = weights)
  sleep_results[[impute_iter]] = gee_result
  sleep_results2[[impute_iter]] = gee_result2
  sleep_results3[[impute_iter]] = gee_result3

  print(impute_iter)
}
testEstimates(mood_results)
testEstimates(mood_results2)
testEstimates(mood_results3)

saveRDS(mood_results, file = "direct_mood_results.RDS")
saveRDS(mood_results2, file = "direct_mood_results2.RDS")
saveRDS(mood_results3, file = "direct_mood_results3.RDS")

testEstimates(mood_agg_results)
testEstimates(mood_agg_results2)
testEstimates(mood_agg_results3)

saveRDS(mood_agg_results, file = "direct_mood_agg_results.RDS")
saveRDS(mood_agg_results2, file = "direct_mood_agg_results2.RDS")
saveRDS(mood_agg_results3, file = "direct_mood_agg_results3.RDS")

testEstimates(step_results)
testEstimates(step_results2)
testEstimates(step_results3)

saveRDS(step_results, file = "direct_step_results.RDS")
saveRDS(step_results2, file = "direct_step_results2.RDS")
saveRDS(step_results3, file = "direct_step_results3.RDS")

testEstimates(sleep_results)
testEstimates(sleep_results2)
testEstimates(sleep_results3)

saveRDS(sleep_results, file = "direct_sleep_results.RDS")
saveRDS(sleep_results2, file = "direct_sleep_results2.RDS")
saveRDS(sleep_results3, file = "direct_sleep_results3.RDS")

specialties = unique(temp$Specialty)
result = vector(length = length(specialties))
for (i in 1:length(specialties)) {
  specialty = specialties[i]
  result[i] = length(unique(temp$UserID[temp$Specialty == specialty]))
}
result
>>>>>>> 7540d70758f2c0c4d3d46bfc9436dd8c63c76216
