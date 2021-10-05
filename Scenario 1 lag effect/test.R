setwd("~/MRT/Scenario 1 lag effect")


library("foreach")
library("doParallel")
library("parallel")
source("init.R")
source("group.R")



## set number of Monte Carlo replicates

M <- 1000

## set number of threads to use for parallel processing and the random seed
## (nb: these two values ensure that the results are replicable)
cores <- 4
seed <- 0

cl <- makeCluster(getOption("cl.cores", cores))
clusterEvalQ(cl, source("init.R"))
registerDoParallel(cl)



sim.omit <- function() {
  out <- NULL
  ## low, medium and high degrees of moderation by state
  for (n in 250) {
    group = group_all[[as.character(n)]]
    for (tmax in 30) {
      clusterSetRNGStream(cl, seed)
      out <-
        rbind(out,
              cbind(level = 0.2,
                    sim_wc(n, tmax, M, 
                           ## regress response on state and proximal treatment,
                           ## ignoring the underlying interaction between the two
                           y.formula = list(w = y ~ lag1state + I(lag1a-pn)),
                           contrast_vec = c(0,0,1),
                           y.names = c(w = "Weighted and centered"),
                           ## term labels for proximal treatment
                           y.label = list(w = "I(lag1a - pn)"),
                           ## specify weights and working correlation structure
                           y.args = list(w = list(wn = "pn", wd = "prob")),
                           ## specify weight numerator model
                           a.formula = list(pn = a ~ 1),
                           a.names = c(pn = "Intercept-only"),
                           ## use default generative model, but with the specified
                           ## level of moderation by the time-varying state
                           group_ls = group,
                           lag = 1)))
    }
  }
 out
}


omit <- sim.omit()
save(omit,file = "test.RData")

stopCluster(cl)