## specify group structure
group_all = list()
group_all[["30"]] = list()
group_all[["100"]] = list()

# 30 (euqal group size, different variances)
group_all[["30"]][["n"]] = 30
group_all[["30"]][["group_id"]] = rep(1:6,each = 5)
group_all[["30"]][["sigma2"]] = (1:6)/10



# 100 (equal variance, different group sizes)
group_all[["100"]][["n"]] = 100
group_all[["100"]][["group_id"]] = rep(1:4,c(10,20,30,40))
group_all[["100"]][["sigma2"]] = rep(0.5,4)





