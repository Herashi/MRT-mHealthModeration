# setwd("/home/herashi/MRT")

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


sim.omit <- function(k) {
  out <- NULL
  ## low, medium and high degrees of moderation by state
  for (b in c(0.2, 0.5, 0.8)) {
    for (n in 30) {
      group = group_all[[as.character(n)]]
      group[["slope sigma2"]] = rep(k,6)
      for (tmax in c(30,50)) {
        clusterSetRNGStream(cl, seed)
        out <-
          rbind(out,
                cbind(level = paste("$\\beta_{11}^* = ", b, "$", sep = ""),
                      sim(n, tmax, M, 
                          ## regress response on state and proximal treatment,
                          ## ignoring the underlying interaction between the two
                          y.formula = list(w = y ~ state + I(a - pn)),
                          y.names = c(w = "Weighted and centered"),
                          ## term labels for proximal treatment
                          y.label = list(w = "I(a - pn)"),
                          ## specify weights and working correlation structure
                          y.args = list(w = list(wn = "pn", wd = "prob")),
                          ## specify weight numerator model
                          a.formula = list(pn = a ~ 1),
                          a.names = c(pn = "Intercept-only"),
                          ## use default generative model, but with the specified
                          ## level of moderation by the time-varying state
                          group_ls = group,
                          beta0 = c(-0.2, 0, 0, b, 0))))
      }
    }
  }
  out
}


for (k in 0:10/10){
  omit <- sim.omit(k)
  save(omit,file = paste0("sim-",k,".RData"))
}


stopCluster(cl)
