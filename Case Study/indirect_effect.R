## This file generates indirect effect estimates 
## If RDS files for the models already exist
## Go to the file "present_results.R" to avoid re-running 
## as code takes a while to generate the MI estimates.

## Code to combine data over several impuations and properly pools the results, 
## Since the randomization probability is constant, we can do this with gee

load(file = '../2018 data/imputed data_full/imputation_list_daily_separated_20.RData')

survey_dat = read.csv('../2018 data/survey data/IHSdata_2018.csv')

mood_agg_results = list()
step_agg_results = list()
sleep_agg_results = list()

library(geepack)
library(mitml)

num_impute = impute_list$num_impute
for(impute_iter in 1:num_impute){
  print(paste("On iteration", impute_iter))
  cur_list = impute_list[[impute_iter]]
  
  all_baseline = cur_list$all_baseline
  full_data = cur_list$full_data
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
  test = aggregate(UserID ~ Specialty + INSTITUTION_STANDARD, data = temp, FUN = function(x){length(unique(x))})
  # howmany = function(x) {
  #   g = test$UserID[test$Specialty == x[20] & test$INSTITUTION_STANDARD == x[19]]
  #   return(g*(g-1))
  # }
  # weights = 1/unlist(apply(X = temp, MARGIN = 1, FUN = howmany))
  # temp = temp[!(weights == Inf),]
  # weights = weights[!(weights==Inf)]
  
  ## CONSTRUCT GROUP-LEVEL MOODprev
  aggMOODprev = aggregate(MOODprev~as.factor(study_week)+as.factor(Specialty) + as.factor(INSTITUTION_STANDARD), data = temp, FUN = mean)
  countMOODprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty)+ as.factor(INSTITUTION_STANDARD), data = temp, FUN = sum)
  names(aggMOODprev) = c("study_week", "Specialty", 
                         "INSTITUTION_STANDARD", "MOODprev")
  names(countMOODprev) = c("study_week", "Specialty", 
                           "INSTITUTION_STANDARD", "MOODprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggMOODprev$MOODprev[aggMOODprev$study_week == x$study_week & aggMOODprev$Specialty == x$Specialty & 
                                 aggMOODprev$INSTITUTION_STANDARD == x$INSTITUTION_STANDARD]
    count = countMOODprev$MOODprev[countMOODprev$study_week == x$study_week & countMOODprev$Specialty == x$Specialty & 
                                     aggMOODprev$INSTITUTION_STANDARD == x$INSTITUTION_STANDARD]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggMOODprev = test
  
  ## CONSTRUCT GROUP-LEVEL STEP_COUNTprev
  aggSTEP_COUNTprev = aggregate(STEP_COUNTprev~as.factor(study_week)+as.factor(Specialty) + 
                                  as.factor(INSTITUTION_STANDARD), data = temp, FUN = mean)
  countSTEP_COUNTprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty) + 
                                    as.factor(INSTITUTION_STANDARD), data = temp, FUN = sum)
  names(aggSTEP_COUNTprev) = c("study_week", "Specialty", 
                               "INSTITUTION_STANDARD", "STEP_COUNTprev")
  names(countSTEP_COUNTprev) = c("study_week", "Specialty", 
                                 "INSTITUTION_STANDARD", "STEP_COUNTprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggSTEP_COUNTprev$STEP_COUNTprev[aggSTEP_COUNTprev$study_week == x$study_week & 
                                             aggSTEP_COUNTprev$Specialty == x$Specialty & 
                                             aggSTEP_COUNTprev$INSTITUTION_STANDARD == x$INSTITUTION_STANDARD]
    count = countSTEP_COUNTprev$STEP_COUNTprev[countSTEP_COUNTprev$study_week == x$study_week & 
                                                 countSTEP_COUNTprev$Specialty == x$Specialty & 
                                                 countSTEP_COUNTprev$INSTITUTION_STANDARD == x$INSTITUTION_STANDARD]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggSTEP_COUNTprev = test
  
  ## CONSTRUCT GROUP-LEVEL SLEEP_COUNTprev
  aggSLEEP_COUNTprev = aggregate(SLEEP_COUNTprev~as.factor(study_week)+as.factor(Specialty) + 
                                   as.factor(INSTITUTION_STANDARD), data = temp, FUN = mean)
  countSLEEP_COUNTprev = aggregate(rep(1,nrow(temp))~as.factor(study_week)+as.factor(Specialty) + 
                                     as.factor(INSTITUTION_STANDARD), data = temp, FUN = sum)
  names(aggSLEEP_COUNTprev) = c("study_week", "Specialty", 
                                "INSTITUTION_STANDARD","SLEEP_COUNTprev")
  names(countSLEEP_COUNTprev) = c("study_week", "Specialty", 
                                  "INSTITUTION_STANDARD","SLEEP_COUNTprev")
  match = function(iter) {
    x = temp[iter,]
    agg = aggSLEEP_COUNTprev$SLEEP_COUNTprev[aggSLEEP_COUNTprev$study_week == x$study_week & 
                                               aggSLEEP_COUNTprev$Specialty == x$Specialty & 
                                               aggSLEEP_COUNTprev$INSTITUTION_STANDARD == x$INSTITUTION_STANDARD]
    count = countSLEEP_COUNTprev$SLEEP_COUNTprev[countSTEP_COUNTprev$study_week == x$study_week & 
                                                   countSLEEP_COUNTprev$Specialty == x$Specialty & 
                                                   countSLEEP_COUNTprev$INSTITUTION_STANDARD == x$INSTITUTION_STANDARD]
    if (count == 1) {
      return(0)
    } else {
      return(count/(count-1) * agg - x$MOODprev/(count-1))
    }
  }
  test = unlist(lapply(1:nrow(temp), match))
  temp$aggSLEEP_COUNTprev = test
  
  ## MAKE INDIRECT DATASET
  combos = aggregate(UserID ~ Specialty + INSTITUTION_STANDARD, data = temp, FUN = function(x){length(unique(x))})
  ## REMOVE BLANK INSTITUTIONS
  combos = combos[-which(combos$INSTITUTION_STANDARD == ""), ]
  setofweeks = unique(temp$study_week)
  indirectfulldata = data.frame()
  for (i in 1:nrow(combos)) {
    print(paste("On specialty:", combos[i,1]))
    specialty = combos[i,1]
    print(paste("On Institution:", combos[i,2]))
    institution = combos[i,2]
    if(combos[i,3] > 1) {print("Bigger than 1")}
    for (j in 1:length(setofweeks)) {
      week= setofweeks[j]
      test = subset(temp, Specialty == specialty & study_week == week & INSTITUTION_STANDARD == institution)
      groupsize = nrow(test)
      setofusers = unique(test$UserID)
      # newweight = exp(log(subsetsize) - log(groupsize) - log(choose(groupsize,subsetsize)))
      if(groupsize > 1) {
        for (k in 1:length(setofusers)) {
          user = test$UserID[k]
          otherusers = test$UserID[-k]
          otherusersaction = test$week_category_new[is.element(test$UserID, otherusers)]
          otherusersmood = test$MOODprev[is.element(test$UserID, otherusers)]
          newdata = cbind(test[rep(1,groupsize-1),], otherusers, otherusersaction, otherusersmood)
          newdata$newweights = rep(1/(groupsize*(groupsize-1)),groupsize-1)
          indirectfulldata = rbind(indirectfulldata, (newdata))
        }
      }
    }
  }  
  indirectfulldata$centeredaction = (1-indirectfulldata$week_category_new)*(indirectfulldata$otherusersaction-0.5)
  indirectfulldata$centeredaction2 = indirectfulldata$week_category_new*(indirectfulldata$otherusersaction-0.5)
  
  indirectfulldata$centeredaction_week = indirectfulldata$week_category_new*(indirectfulldata$otherusersaction-0.5)*indirectfulldata$study_week
  indirectfulldata$centeredaction2_week = indirectfulldata$week_category_new*(indirectfulldata$otherusersaction-0.5)*indirectfulldata$study_week
  
  whichcombo <- function(x) {
    which(combos$Specialty == x[20] & combos$INSTITUTION_STANDARD == x[19])
  }
  indirectfulldata$specinst_id = apply(indirectfulldata, MARGIN = 1, FUN = whichcombo)
  
  saveRDS(indirectfulldata, file = paste("indirectdata_impute_", impute_iter, ".RDS", sep = ""))
}


