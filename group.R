# 'group' should be a list containing n, group_id, corstr and rho
# n = 30 # amount of participants
# group_id = rep(1:3,each = 10), # a vector with length n
# # a character vector specifying correlation structures of every group
# corstr = c("ar1","exchangeable","independent"), 
# rho = c(0.1,0.1,0) # corresponding parameters, independent <- 0

## specify group structure
group_all = list()
group_all[["30"]] = list()
group_all[["60"]] = list()

# 30 (pairwise)
group_all[["30"]][["n"]] = 30
group_all[["30"]][["group_id"]] = rep(1:15,each = 2)
group_all[["30"]][["cor structure"]] = rep("ar1",15)
group_all[["30"]][["rho"]] =  rep(0.5,15)

# 60 (a combination of different group sizes)
group_all[["60"]][["n"]] = 60
group_all[["60"]][["group_id"]] = rep(1:3, c(10,20,30))
group_all[["60"]][["cor structure"]] = c("independent","exchangeable","ar1")
group_all[["60"]][["rho"]] =  c(0,0.5,0.5)




  



