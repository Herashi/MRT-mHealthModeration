setwd("/home/herashi/MRT/indirect_effect/100_25")

library(foreach)
library(doParallel)
library(parallel)
source("init.R")
source("group.R")


## set number of Monte Carlo replicates
# M <- 1


## set number of threads to use for parallel processing and the random seed
## (nb: these two values ensure that the results are replicable)
# cores <- 4
# seed <- 0
# 
# cl <- makeCluster(getOption("cl.cores", cores))
# clusterEvalQ(cl, source("init.R"))
# registerDoParallel(cl)
#tmax_linux <- 50
tmax_linux <- as.numeric(Sys.getenv("tmax"))

sim.omit <- function() {

  ## low, medium and high degrees of moderation by state
  for (b in 0.2) {
    for (n in 2500) {
      group = group_all[[as.character(n)]]
      for (tmax in tmax_linux)  {
        # clusterSetRNGStream(cl, seed)
        out <-  sim_wc(n, tmax,
                       ## regress response on state and proximal treatment,
                       ## ignoring the underlying interaction between the two
                       y.formula = list(w = y ~ state +indirect  +indir),
                       contrast_vec = c(0,0,1,0),
                       y.names = c(w = "Weighted and centered"),
                       ## term labels for proximal treatment
                       y.label = list(w = "indirect"),
                       ## specify weights and working correlation structure
                       y.args = list(w = list(wn = "pn", wd = "prob")),
                       ## specify weight numerator model
                       a.formula = list(pn = cl ~ 1),
                       a.names = c(pn = "Intercept-only"),
                       ## use default generative model, but with the specified
                       ## level of moderation by the time-varying state
                       group_ls = group,
                       beta0 = c(-0.2, 0, 0, b, 0))
      }
    }
  }
  out
}




omit <- sim.omit()
arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
save(omit, file=paste0("fity_", arrayid, "_tmax_", tmax_linux, ".RData"))

# stopCluster(cl)
