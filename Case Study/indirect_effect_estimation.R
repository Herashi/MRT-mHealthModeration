## DEFINE FUNCTIONS
compute_Sigma <- function(model, model.matrix, weights, groupids) {
  ## COMPUTES THE ROBUST STD ERROR 
  ## USING MANCL and DEROUEN SMALL SAMPLE CORRECTION
  q = matrix(0, nrow = ncol(model.matrix), ncol = ncol(model.matrix))
  for(group in unique(groupids)) {
    ingroup = which(groupids==group)
    d_matrix = temp.mm[ingroup,]
    weight_matrix = diag(weights[ingroup])
    q = q + t(d_matrix) %*% weight_matrix %*% d_matrix
  }
  
  lambda = matrix(0,nrow = ncol(model.matrix), ncol = ncol(model.matrix))
  for(group in unique(groupids)) {
    print(paste("On group",group))
    ingroup = which(indirectfulldata.subset$specinst_id==group)
    d_matrix = model.matrix[ingroup,]
    if (nrow(d_matrix) > 10000) {
      print(paste("On group",group))
      print(paste("Number of rows is", nrow(d_matrix)))
    }
    weight_matrix = diag(weights[ingroup])
    h = d_matrix%*%solve(q)%*%t(d_matrix)%*% weight_matrix
    res_group = model$residuals[ingroup]
    proj_res = solve(diag(rep(1,nrow(h)))-h, res_group)
    lambda_comp = t(d_matrix)%*%weight_matrix %*% proj_res
    lambda = lambda + lambda_comp%*%t(lambda_comp)
  }
  
  Sigma  = solve(q, lambda)%*% solve(q)
  
  return(Sigma)
}


setwd("D:/github/Temp folder for Zhenke/code/")

for(impute_iter in 9:20) {
  
  indirectfulldata = readRDS(file = paste("indirectdata_impute_", impute_iter, ".RDS", sep = ""))
  
  if(!require("sgd")){install.packages("sgd")}
  library("sgd")
  ## Remove the one large group for ease of computation
  badids = c(304,547)
  indirectfulldata.subset = subset(indirectfulldata, !is.element(specinst_id,badids))
  indirectfulldata.subset$centeredaction_mood = indirectfulldata.subset$centeredaction * indirectfulldata.subset$otherusersmood
  indirectfulldata.subset$centeredaction_mood2 = indirectfulldata.subset$centeredaction2 * indirectfulldata.subset$otherusersmood
  
  ## MOOD MODEL
  sgd.theta <- sgd(MOOD ~ MOODprev + 
                     centeredaction + centeredaction2, 
                   data = indirectfulldata.subset, model="lm",
                   model.control = list(weights = indirectfulldata$newweights))
  
  temp.mm = model.matrix(MOOD ~ MOODprev + 
                           centeredaction + centeredaction2,
                         data = indirectfulldata.subset)
  Sigma = compute_Sigma(sgd.theta, temp.mm, indirectfulldata.subset$newweights, indirectfulldata.subset$specinst_id)
  
  mood_model = list("coef" = sgd.theta$coefficients, "Sigma" = Sigma)
  
  saveRDS(mood_model,  paste("mood_model_impute_", impute_iter, ".RDS", sep = ""))
  
  ## STEP MODEL
  sgd.theta_step <- sgd(STEP_COUNT ~ STEP_COUNTprev + 
                          centeredaction + centeredaction2,
                        data=indirectfulldata.subset, model="lm",
                        model.control = list(weights = indirectfulldata$newweights))
  
  temp.mm = model.matrix(STEP_COUNT ~ STEP_COUNTprev + 
                           centeredaction + centeredaction2,
                         data = indirectfulldata.subset)
  Sigma_step = compute_Sigma(sgd.theta_step, temp.mm, indirectfulldata.subset$newweights, indirectfulldata.subset$specinst_id)
  
  step_model = list("coef" = sgd.theta_step$coefficients, "Sigma" = Sigma_step)
  
  saveRDS(step_model,  paste("step_model_impute_", impute_iter, ".RDS", sep = ""))
  
  ## SLEEP MODEL
  sgd.theta_sleep <- sgd(SLEEP_COUNT ~ SLEEP_COUNTprev + 
                           centeredaction + centeredaction2, 
                         data=indirectfulldata.subset, model="lm",
                         model.control = list(weights = indirectfulldata$newweights))
  
  temp.mm = model.matrix(SLEEP_COUNT ~ SLEEP_COUNTprev + 
                           centeredaction + centeredaction2,
                         data = indirectfulldata.subset)
  Sigma_sleep = compute_Sigma(sgd.theta_sleep, temp.mm, indirectfulldata.subset$newweights, indirectfulldata.subset$specinst_id)
  
  sleep_model = list("coef" = sgd.theta_sleep$coefficients, "Sigma" = Sigma_sleep)
  
  cbind(sleep_model$coef, sqrt(diag(sleep_model$Sigma)))
  
  
  saveRDS(sleep_model,  paste("sleep_model_impute_", impute_iter, ".RDS", sep = ""))
}
