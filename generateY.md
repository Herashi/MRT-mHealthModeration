
# generateY


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(data.table)
require(kableExtra)
library(MASS)
set.seed(0808)
```


## Functions

### expit function

```{r}
expit = function(a){
  return(exp(a) / (1 + exp(a)))
}
```


### Action and State generating function:

+ Action generating function, $A_t \in \{ 0,1 \}$

$$
p_t(1|H_t) = \text{expit} (\eta_1A_{t-1}+\eta_2S_t)
$$

```{r}
Action = function(eta_1,eta_2,A_tm1,S_t){
  p_t = expit(eta_1*A_tm1 + eta_2*S_t)
  if (p_t == 0.5){
    A_t = as.numeric(rbernoulli(1))
  }else {
    A_t = ifelse(p_t>0.5,1,0)
  }
  return(c(A_t,p_t))
}
```

+ State generating function, $S_t \in \{ -1,1 \}$


$$
Pr(S_t=1|A_{t-1},H_{t-1}) = \text{expit} (\xi A_{t-1})
$$


```{r}
State = function(xi,A_tm1){
  p_t = expit(xi*A_tm1)
  if (p_t == 0.5){
    S_t = ifelse(as.numeric(rbernoulli(1))==1, 1, -1)
  }else {
    S_t = ifelse(p_t>0.5,1,-1)
  }
  
  return(c(S_t,p_t))
}
```


### Generating sequences of actions and states. 

The sequence for certain person should look like : $\{ A_0, S_1, A_1, Y_2, S_2,A_2,Y_3, \dots,S_T,A_T, Y_{T+1} \}$, and by assumption, $A_0 = 0$ for everyone.


The outcome was given by:

$$
Y_{t+1} = \theta_1(S_t - E[S_t|A_{t-1},H_{t-1}])+ \theta_2(A_{t-1}-p_{t-1}(1|H_{t-1})) + (A_t - p_t(1|H_t))(\beta^*_{10}+\beta^*_{11}S_t) + \epsilon_{t+1}
$$


```{r}
Outcome = function(j,S_t,A_t,xi,A_tm1,eta_1,eta_2,A_tm2,S_tm1,theta_1,theta_2,beta_10,beta_11){
  if (j ==1){
    y_tp1 = theta_1*(S_t - (2 *State(xi,A_tm1)[2]- 1))+
    (A_t-Action(eta_1,eta_2,A_tm1,S_t)[2])*(beta_10+beta_11*S_t)
  } else{
    y_tp1 = theta_1*(S_t - (2 *State(xi,A_tm1)[2]- 1))+ 
    theta_2*(A_tm1 - Action(eta_1,eta_2,A_tm2,S_tm1)[2]) +
    (A_t-Action(eta_1,eta_2,A_tm1,S_t)[2])*(beta_10+beta_11*S_t)
  }
  return(y_tp1)
}

ar1_cor <- function(n, rho) {
exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
    (1:n - 1))
rho^exponent
}
```


```{r}
sim_ASY = function(n,T,xi,eta_1,eta_2,theta_1,theta_2,beta_10,beta_11){
  # generate mean
  df = matrix(NA,nrow = n*(T+1), ncol = 5)
  colnames(df) = c("id","t","S_t","A_t","Y_(t+1)")
  df[,"id"] = rep(1:n, each = T+1)
  df[,"t"] = rep(0:T, times = n)
  
  
  for (i in 0:(n-1)){
    df[i*(T+1)+1,c("S_t","A_t","Y_(t+1)")] = c(0,0,0)
    for (j in 2:(T+1)){
        df[i*(T+1)+j,"S_t"] = State(xi,df[i*(T+1)+j-1,"A_t"])[1]
        df[i*(T+1)+j,"A_t"] = Action(eta_1,eta_2,df[i*(T+1)+j-1,"A_t"],df[i*(T+1)+j,"S_t"])[1]
        df[i*(T+1)+j,"Y_(t+1)"] = Outcome(j-1,df[i*(T+1)+j,"S_t"],
                                          df[i*(T+1)+j,"A_t"],xi,df[i*(T+1)+j-1,"A_t"],
                                          eta_1,eta_2,df[i*(T+1)+j-2,"A_t"],df[i*(T+1)+j-1,"S_t"],
                                          theta_1,theta_2,beta_10,beta_11)
    }
  }
  df = as.data.frame(df)
  df = subset(df, t != 0)
  
  # generate noises
  cor = ar1_cor(T,0.5)
  epsilon = matrix(mvrnorm(n, rep(0,T), cor), ncol = 1, byrow = TRUE)
  df[,"Y_(t+1)"] = df[,"Y_(t+1)"] +epsilon
  
  rownames(df) = 1:(n*T)
  return(df)
  
}
```



## Implementation

Set parameters as follows:

$$
\xi = 0,\eta_1 = -0.8,\eta_2 = 0.8, \theta_1 = 0.8, \theta_2 = 0, \beta^*_{10} = -0.2, \beta^*_{11} \in \{0.2,0.5,0.8\}
$$


```{r}
df = sim_ASY(n = 30,T = 30,xi = 0,eta_1 = -0.8,eta_2 = 0.8,theta_1 = 0.8,theta_2=0,beta_10=-0.2,beta_11 =0.2)

df[1:10,] %>% 
  kable(caption = "Action State Outcome table (first 10 rows)", align = "c") %>%
  kable_styling("striped")
```
