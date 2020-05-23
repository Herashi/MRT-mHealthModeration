library(purrr)
library("zoo")
source("xzoo.R")
## load functions needed for variance estimation
source("xgeepack.R")


#define expit(a)
expit = function(a){
  return(exp(a) / (1 + exp(a)))
}


rsnmm = function(n, T,
                 ty, tmod, tavail, tstate,
                 beta, eta, mu, theta,
                 coefavail, coefstate, coeferr,
                 avail, base, state, a, prob,
                 y, err, statec, ac, availc){
  # int i, j, k, n = *size, T = *tmax;
  # double r, q, ym;
  # for (i = 0; i < n; i++) {
  #  for (j = 1; j < T; j++) {
  
  
  
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
      # error ????
      err[i*T + j] = err[i*T + j]+ coeferr * err[i*T + j - 1]
      # response 
      y[i*T + j] = ym + err[i*T + j]
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



rsnmm.R <- function(n, tmax, control, ...) {
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
    availc = as.double(rep(0, n*tmax)))
  
  d <- data.frame(id = rep(1:n, each = tmax), time = time,
                  ty = d$ty, tmod = d$tmod, tavail = d$tavail, tstate = d$tstate,
                  base = d$base, state = d$state, a = d$a, y = d$y, err = d$err,
                  avail = d$avail, prob = d$p, a.center = d$a.center,
                  state.center = d$state.center, avail.center = d$avail.center,
                  one = 1)
  
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
  d
}
