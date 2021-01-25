mood_model_list = list()
step_model_list = list()
sleep_model_list = list()
max_iter = 20

for(impute_iter in 1:max_iter) {

mood_model = readRDS(file = paste("mood_model_impute_", impute_iter, ".RDS", sep = ""))

mood_model_list[[impute_iter]] = cbind(mood_model$coef, sqrt(diag(mood_model$Sigma)))

step_model = readRDS(file = paste("step_model_impute_", impute_iter, ".RDS", sep = ""))

step_model_list[[impute_iter]] = cbind(step_model$coef, sqrt(diag(step_model$Sigma)))

sleep_model = readRDS(file = paste("sleep_model_impute_", impute_iter, ".RDS", sep = ""))

sleep_model_list[[impute_iter]] = cbind(sleep_model$coef, sqrt(diag(sleep_model$Sigma)))

}

rubinsrule <- function(model_list) {
  max_iter = length(model_list)
  within_model_var = rep(0,4)
  mean_imp = rep(0,4)
  for(impute_iter in 1:max_iter) {
    mean_imp = mean_imp + model_list[[impute_iter]][,1]/max_iter
    within_model_var = within_model_var + model_list[[impute_iter]][,2]/max_iter
  }
  between_model_var = rep(0,4)
  for(impute_iter in 1:max_iter) {
    between_model_var = between_model_var + (model_list[[impute_iter]][,1] - mean_imp)^2/(max_iter-1)
  }
  final_var = within_model_var + (1+1/max_iter) * between_model_var
  return(cbind(mean_imp, final_var))
}

mood_rubin = rubinsrule(mood_model_list)
step_rubin = rubinsrule(step_model_list)
sleep_rubin = rubinsrule(sleep_model_list)

mood_tval = mood_rubin[,1]/ mood_rubin[,2]
step_tval = step_rubin[,1]/ step_rubin[,2]
sleep_tval = sleep_rubin[,1]/ sleep_rubin[,2]

mood_pval = 1-pnorm(mood_tval)
step_pval = 1-pnorm(step_tval)
sleep_pval = 1-pnorm(sleep_tval)

round(
rbind(
  cbind(mood_rubin[3:4,], mood_tval[3:4], mood_pval[3:4]),
  
  cbind(step_rubin[3:4,], step_tval[3:4], step_pval[3:4]),
  
  cbind(sleep_rubin[3:4,], sleep_tval[3:4], sleep_pval[3:4])
),
3)
