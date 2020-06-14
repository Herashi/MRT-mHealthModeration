## specify group structure
group_all = list()
group_all[["30"]] = list()
group_all[["100"]] = list()

# 30 (pairwise)
group_all[["30"]][["n"]] = 30
group_all[["30"]][["group_id"]] = rep(1:6,each = 5)
group_all[["30"]][["rho"]] = 1:6


group_all[["100"]][["n"]] = 100
group_all[["100"]][["group_id"]] = rep(1:4,c(10,20,30,40))
group_all[["100"]][["rho"]] = rep(5,4)






