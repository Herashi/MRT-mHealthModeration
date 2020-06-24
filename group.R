## specify group structure
group_all = list()
group_all[["30"]] = list()


# 30 (euqal group size, different variances)
group_all[["30"]][["n"]] = 30
group_all[["30"]][["group_id"]] = rep(1:6,each = 5)
group_all[["30"]][["baseline sigma2"]] = 1:6/10
#group_all[["30"]][["slope sigma2"]] = rep(0.1,6)







