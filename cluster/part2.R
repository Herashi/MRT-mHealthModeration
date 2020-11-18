

#Below this should be another job (part2) after the array jobs finish running
#set empty matrix
# SET IT HERE

setwd("/home/herashi/MRT")   
MASTERLIST =list.files(pattern="^fity")

out <- NULL

for (i in 1:length(MASTERLIST)){
  load(file= MASTERLIST[i])
  out <-rbind(out, cbind(level = paste("$\\beta_{11}^* = ", 0.2, "$", sep = ""), omit))
}

print(length(MASTERLIST))

## 95% CI coverage probability using uncorrected SEs
out$cp <- with(out, lcl <= -0.1 & -0.1 <= ucl)
## coverage probability using SEs corrected for estimates in weights
out$cpc <- with(out, lclc <= -0.1 & -0.1 <= uclc)
## root MSE
out$rmse <- with(out, (estc - (-0.1))^2)


## mean and SD estimate, number of replicates
out <- cbind(aggregate(cbind(est,estc, se, sec, cp, cpc, rmse,lclc, uclc) ~
                         method + moderator +  n + tmax,
                       data = out, FUN = mean),
             sd = aggregate(estc ~ method + moderator + n + tmax,
                            data = out, FUN= sd)$estc)
#iter = aggregate(iter ~ method + moderator  + n + tmax,
#                data = out,
#               FUN = function(x) length(unique(x)))$iter)
out$rmse <- sqrt(out$rmse)

save(out,file = "test.RData")

