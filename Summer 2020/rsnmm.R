library(purrr)
library("zoo")
library(foreach)
source("xzoo.R")
## load functions needed for variance estimation
source("xgeepack.R")
library(Matrix)
library(MASS)
library(geepack)



#define expit(a)
expit = function(a){
  return(exp(a) / (1 + exp(a)))
}

group_str = function(group){
  group[["group size"]] = unname(table(group[["group_id"]]))
  group[["#groups"]] = length(unique(group[["group_id"]]))
  group[["Cov"]] = diag(group[["sigma2"]])
  
  err = c()
  for (i in 1:group[["#groups"]]){
    e = rnorm(1,mean = 0,sd = sqrt(group[["sigma2"]][i]))
    err = c(err,e)
  }
  group[["err"]] = err
  group[["group err"]] = rep(group[["err"]],group[["group size"]])
  
  return(group)
} 



rsnmm = function(n, T,
                 ty, tmod, tavail, tstate,
                 beta, eta, mu, theta,
                 coefavail, coefstate, coeferr,
                 avail, base, state, a, prob,
                 y, err, statec, ac, availc, group_err){
  # a list indicating grouping structure
  
  for (i in 0:(n-1)) {
    for (j in 2:T) {
      # probability of availabilty 
      r = expit(coefavail[1]
                + coefavail[2] * tavail[j]
                + coefavail[3] * a[i*T + j-1]
                + coefavail[4] * y[i*T + j-1])
      # availability - uncentered and centered 
      avail[i*T + j] = as.numeric(rbernoulli(1,r))
      availc[i*T + j] = avail[i*T + j] - r
      # probability that binary state is +1 
      q = expit(coefstate[1]
                + coefstate[2] * tstate[j]
                + coefstate[3] * base[i*T + j-1 ]
                + coefstate[4] * state[i*T + j-1]
                + coefstate[5] * a[i*T + j-1])
      # binary state on {-1, 1} - uncentered and centered 
      state[i*T + j] = ifelse(as.numeric(rbernoulli(1,q)) < 1 ,-1 ,1)
      statec[i*T + j] = state[i*T + j] - (q - (1 - q))
      # treatment probability 
      prob[i*T + j] = avail[i*T + j] * expit(eta[1]
                                             + eta[2] * base[i*T + j]
                                             + eta[3] * state[i*T + j]
                                             + eta[4] * a[i*T + j - 1]
                                             + eta[5] * y[i*T + j - 1])
      # treatment indicator - uncentered and centered 
      a[i*T + j] = as.numeric(rbernoulli(1, prob[i*T + j]))
      ac[i*T + j] = a[i*T + j] - prob[i*T + j]
      # conditional mean response 
      ym = mu[1]+ 
        mu[2] * ty[j]+  # pre-evaluated time function 
        mu[3] * base[i*T + j]+
        ac[i*T + j] * (beta[1]+
                         beta[2] * tmod[j] + # pre-evaluated time function
                         beta[3] * base[i*T + j]+
                         beta[4] * state[i*T + j]+
                         beta[5] * a[i*T + j - 1])+
        ac[i*T + j - 1] * (beta[6]+
                             beta[7] * tmod[j - 1]+
                             beta[8] * base[i*T + j - 1]+
                             beta[9] * state[i*T + j - 1])+
        theta[1] * availc[i*T + j]+
        theta[2] * statec[i*T + j]+
        theta[3] * availc[i*T + j - 1]+
        theta[4] * statec[i*T + j - 1]
      # error 
      err[i*T + j] = err[i*T + j]+ coeferr * err[i*T + j - 1] 
      # response 
      y[i*T + j] = ym + err[i*T + j]+ group_err[i+1]
    }
  }
  
  d = data.frame(ty = ty, tmod = tmod, tavail = tavail, tstate = tstate,
                 base = base, state = state, a = a, y = y, err = err,
                 avail = avail, p = prob, a.center = ac,
                 state.center = statec, avail.center = availc)
  return(d)
}



rsnmm.control <- function(origin = 1, sd = 1,
                          coralpha = sqrt(0.5),
                          corstr = c("ar1", "exchangeable"),
                          beta0 = c(-0.2, 0, 0, 0.2, 0), beta1 = rep(0, 4),
                          eta = c(0, 0, 0.8, -0.8, 0), mu = rep(0, 3),
                          theta0 = c(0, 0.8), theta1 = c(0, 0),
                          coef.avail = c(100, rep(0, 3)), coef.state = rep(0, 5),
                          tfun = NULL, lag = 3 + any(beta1 != 0)) {
  corstr <- match.arg(corstr)
  if (is.null(tfun))
    tfun <- rep(list(function(tcur, tmax) rep(0, length(tcur))), 4)
  list(origin = 1, lag = lag,
       ## error SD, correlation
       sd = sd, coralpha = coralpha, corstr = corstr,
       ## proximal effect coefficients
       beta0 = setNames(beta0, c("one", "tmod", "base", "state", "lag1a")),
       ## delayed effect coefficients
       beta1 = setNames(beta1, c("one", "lag1tmod", "base", "lag1state")),
       ## treatment probability model coefficients
       eta = setNames(eta, c("one", "base", "state", "lag1a", "lag1y")),
       ## exogenous or time-invariant main effects
       mu = setNames(mu, c("one", "ty", "base")),
       ## time-varying main effects, centered and proximal
       theta0 = setNames(theta0, c("avail", "state")),
       ## time-varying main effects, centered and delayed
       theta1 = setNames(theta1, c("lag1avail", "lag1state")),
       ## availability model coefficients
       coef.avail = setNames(coef.avail, c("one", "tavail", "lag1a", "lag1y")),
       ## binary state model coefficients
       coef.state = setNames(coef.state,
                             c("one", "tstate", "base", "lag1state", "lag1a")),
       ## functions of time in the main effect, proximal effect,
       ## availability model, and binary state model
       tfun = setNames(tfun, c("ty", "tmod", "tavail", "tstate")))
}



rsnmm.R <- function(n, tmax, group_ls, control, ...) {
  control <- if (missing(control)) rsnmm.control(...)
  else do.call("rsnmm.control", control)
  tmax <- tmax + (tmax %% 2) + 1
  time <- rep(0:(tmax - 1), n)
  tfun <- do.call("data.frame", lapply(control$tfun, function(f) f(time, tmax)))
  coef.err <- 0
  control$cormatrix <- matrix(control$coralpha, tmax, tmax)
  diag(control$cormatrix) <- 1
  if (control$corstr == "exchangeable") {
    err <- sqrt(control$coralpha) * rep(rnorm(n, sd = control$sd), each = tmax)
    err <- err + rnorm(n * tmax, sd = sqrt(with(control, sd^2 * (1 - coralpha))))
  }else {
    ## provisional error
    err <- ifelse(time == 0, rnorm(n, sd = control$sd),
                  rnorm(n * (tmax - 1),
                        sd = sqrt(with(control, sd^2 * (1 - coralpha^2)))))
    err[time == 0] <- rnorm(n, sd = control$sd)
    coef.err <- control$coralpha
    control$cormatrix <- matrix(with(control,
                                     coralpha^(abs(row(cormatrix) -
                                                     col(cormatrix)))), tmax, tmax)
  }
  group = group_str(group_ls)
  group_err = group[["group err"]]
  
  d <- rsnmm(
    n = as.integer(n) ,
    T = as.integer(tmax),
    ty = as.double(tfun$ty),
    tmod = as.double(tfun$tmod),
    tavail = as.double(tfun$tavail),
    tstate = as.double(tfun$tstate),
    beta = with(control, as.double(c(beta0, beta1))),
    eta = as.double(control$eta),
    mu = as.double(control$mu),
    theta = with(control, as.double(c(theta0, theta1))),
    coefavail = as.double(control$coef.avail),
    coefstate = as.double(control$coef.state),
    coeferr = as.double(coef.err),
    avail = as.integer(rep(0, n * tmax)),
    base = as.double(rep(rnorm(n), each = tmax)),
    state = as.integer(rep(0, n * tmax)),
    a = as.integer(rep(0, n * tmax)),
    prob = as.double(rep(0, n * tmax)),
    y = as.double(rep(0, n * tmax)),
    err = as.double(err),
    statec = as.double(rep(0, n*tmax)),
    ac = as.double(rep(0, n*tmax)),
    availc = as.double(rep(0, n*tmax)),
    group_err =as.double(group_err))
  
  d <- data.frame(id = rep(1:n, each = tmax), time = time,
                  ty = d$ty, tmod = d$tmod, tavail = d$tavail, tstate = d$tstate,
                  base = d$base, state = d$state, a = d$a, y = d$y, err = d$err,
                  group_err = rep(group_err, each = tmax), avail = d$avail, 
                  prob = d$p, a.center = d$a.center, state.center = d$state.center, 
                  avail.center = d$avail.center, one = 1)
  
  ## nb: for a given row, y is the proximal response
  d$lag1y <- with(d, delay(id, time, y))
  d$lag2y <- with(d, delay(id, time, y, 2))
  d$lag1err <- with(d, delay(id, time, err))
  d$lag1avail <- with(d, delay(id, time, avail))
  d$lag1avail.center <- with(d, delay(id, time, avail.center))
  d$lag2avail <- with(d, delay(id, time, avail, 2))
  d$lag2avail.center <- with(d, delay(id, time, avail.center, 2))
  d$lag1a <- with(d, delay(id, time, a))
  d$lag2a <- with(d, delay(id, time, a, 2))
  d$lag1prob <- with(d, delay(id, time, prob))
  d$lag2prob <- with(d, delay(id, time, prob, 2))
  d$lag1a.center <- with(d, delay(id, time, a.center))
  d$lag2a.center <- with(d, delay(id, time, a.center, 2))
  d$lag1tmod <- with(d, delay(id, time, tmod))
  d$lag2tmod <- with(d, delay(id, time, tmod, 2))
  d$lag1state <- with(d, delay(id, time, state))
  d$lag1state.center <- with(d, delay(id, time, state.center))
  rownames(d) <- NULL
  attributes(d) <- c(attributes(d), control)
  
  # cat(n,tmax,mean(unique(d$group_err)),var(unique(d$group_err)),sep = " ","\n")
  
  return(d)
}


sim <- function(n = 30, tmax = 30, M = 1000,
                ## response regression models
                y.formula = list(w = y ~ I(a - pn) * (base + state),
                                 u = y ~ a * (base + state)),
                ## names for each regression model
                y.names = c(w = "Weighted and centered",
                            u = "GEE AR(1)"),
                ## labels for regression terms of the treatment effect
                y.label = list(w = "I(a - pn)"),
                ## names of the treatment probability models or variables used
                ## for the weight numerator ('wn') or denominator ('wd') and
                ## arguments for the estimation routine
                y.args = list(w = list(wn = "pn", wd = "pd"),
                              u = list(corstr = "ar1")),
                ## treatment probability models named in 'y.args'
                a.formula = list(pn = a ~ lag1a,
                                 pd = a ~ lag1a + state),
                ## names for each treatment probability model
                a.names = c(pn = "Last treatment",
                            pd = "Last treatment and current state"),
                ## proximal (0) or delayed (1) treatment effect?
                lag = 0,
                ## print generative and analysis model details
                verbose = TRUE,
                ## group structure
                group_ls,
                ## control parameters for 'rsnmm.R'
                control, ...) {
  control <- if (missing(control)) rsnmm.control(...)
  else control <- do.call("rsnmm.control", control)
  ## times to use in the model fit
  runin.fita <- control$lag
  runin.fity <- control$lag + lag
  ## retrieve causal control parameter values
  ## nb: if the regression models 'y.formula' average over an underlying
  ##     moderator these will not represent the true causal effect unless this
  ##     moderator has conditional mean zero
  y.coef <- mapply(which.terms, x = y.formula, label = y.label,
                   stripnames = TRUE, SIMPLIFY = FALSE)
  truth <- control[[paste0("beta", lag)]]
  truth <- truth[Reduce("intersect", lapply(y.coef, names))]
  y.coef <- lapply(y.coef, function(x) x[names(truth)])
  ## corresponding treatment probability models
  ## nb: we avoid delayed evaluation in 'y.args' (e.g. passing a 'weights'
  ##     argument directly) to avoid scoping issues in 'foreach'
  if (!is.null(a.formula)) {
    y.prob <- lapply(y.args, function(x) do.call("c", x[c("wn", "wd")]))
    y.prob <- lapply(y.prob, function(x) x[x %in% names(a.formula)])
  }
  else y.prob <- lapply(y.formula, function(x) list())
  ## print generative and analysis model properties
  if (verbose) {
    cat("\nGenerative model attributes\n\n")
    print(control)
    cat("Analysis models\n\n")
    mapply(function(f, nm) write.table(cbind("  ", nm, ": y ~ ",
                                             as.character(f)[3]), sep = "",
                                       row.names = FALSE, col.names = FALSE,
                                       quote = FALSE, eol = "\n\n"),
           f = y.formula, nm = y.names)
    cat("Treatment probability models\n\n")
    mapply(function(f, nm) write.table(cbind("  ", nm, ": a ~ ",
                                             as.character(f)[3]), sep = "",
                                       row.names = FALSE, col.names = FALSE,
                                       quote = FALSE, eol = "\n\n"),
           f = a.formula, nm = a.names)
  }
  ## general model fitter
  ## nb: d is the data frame for the replicate
  fitter <- function(formula, args, prob, coef, label, response = "y",
                     addvar = NULL) {
    if (response == "a") {
      args$family <- binomial()
      runin <- runin.fita
    }
    else runin <- runin.fity
    r <- which(d$time >= runin)
    l <- list(x = model.matrix(formula, data = d[r, ]), y = d[r, response])
    if (is.null(args$wn) & is.null(args$wd)) l$w <- rep(1, nrow(d))
    else {
      #calculate the weights
      l$w <- ifelse(d[, "a"] == 1, d[, args$wn] / d[, args$wd],
                    (1 - d[, args$wn]) / (1 - d[, args$wd]))
      args[c("wn", "wd")] <- NULL
    }
    # no availability has 0 weight
    l$w <- l$w * d$avail
    # lag != 0
    if (lag) l$w <- delay(d$id, d$time, l$w, lag)
    l$w <- l$w[r]
    if (!is.null(args$corstr)) {
      fun <- "geese.glm"
      l$id <- d$id[r]
    }
    else if (!is.null(args$family)) fun <- "glm.fit"
    else fun <- "lm.wfit"
    fit <- do.call(fun, c(l, args))
    if (!inherits(fit, "geeglm")) {
      fit <- glm2gee(fit, d$id[r])
      fit$terms <- terms(formula)
      fit$geese$X <- l$x
      fit$y <- l$y
    }
    if (!is.null(addvar)) {
      newvar <- paste0(c("", "lag1"), addvar)
      d[, newvar] <<- NA
      d[r, newvar[1]] <<- fit$fitted.values
      d[, newvar[2]] <<- delay(d$id, d$time, d[, newvar[1]])
    }
    else {
      ## usual variance sandwich estimator
      fit$vcov <- vcov.geeglm(fit)
      est <- estimate(fit)[coef, 1:4, drop = FALSE]
      ## correction for any estimates in weights
      l <- if (length(prob)) setNames(fita[prob], gsub("^w", "p", names(prob))) #Vector replacement
      else NULL
      fit$vcov <- NULL
      fit$vcov <- do.call("vcov.geeglm", c(list(x = fit, label = label), l))
      estc <- estimate(fit)[coef, 1:4, drop = FALSE]
      fit <- data.frame(moderator = names(coef), truth = truth,
                        est = est[, "Estimate"], se = est[, "SE"],
                        lcl = est[, "95% LCL"], ucl = est[, "95% UCL"],
                        sec = estc[, "SE"], lclc = estc[, "95% LCL"],
                        uclc = estc[, "95% UCL"], row.names = NULL)
    }
    fit
  }
  fita <- list()
  ## for each replicate...
  out <- foreach(m = 1:M, .combine = "rbind") %dopar% {
    ## ... generate data
    d <- rsnmm.R(n, tmax,group_ls, control = control)
    d$pn <- d$pd <- d$prob
    ## ... fit treatment probability models
    if (!is.null(a.formula))
      fita <- mapply(fitter, formula = a.formula, addvar = names(a.formula),
                     MoreArgs = list(args = list(), prob = list(),
                                     coef = list(), label = list(),
                                     response = "a"), SIMPLIFY = FALSE)
    ## ... fit response models
    fity <- mapply(fitter, formula = y.formula, args = y.args, prob = y.prob,
                   coef = y.coef, label = y.label, SIMPLIFY = FALSE)
    fity <- mapply(function(nm, d) data.frame(iter = m, method = nm, d,
                                              row.names = NULL),
                   nm = y.names[names(fity)], d = fity, SIMPLIFY = FALSE)
    out <- do.call("rbind", setNames(fity, NULL))
  }
  out <- data.frame(n, tmax, out)
  ## 95% CI coverage probability using uncorrected SEs
  out$cp <- with(out, lcl <= truth & truth <= ucl)
  ## coverage probability using SEs corrected for estimates in weights
  out$cpc <- with(out, lclc <= truth & truth <= uclc)
  ## root MSE
  out$rmse <- with(out, (est - truth)^2)
  ## mean and SD estimate, number of replicates
  out <- cbind(aggregate(cbind(est, se, sec, cp, cpc, rmse) ~
                           method + moderator + truth + n + tmax,
                         data = out, FUN = mean),
               sd = aggregate(est ~ method + moderator + truth + n + tmax,
                              data = out, FUN = sd)$est,
               iter = aggregate(iter ~ method + moderator + truth + n + tmax,
                                data = out,
                                FUN = function(x) length(unique(x)))$iter)
  out$rmse <- sqrt(out$rmse)
  out
}
