## specify group structure
group_all = list()
group_all[["250"]] = list()


# 625 (euqal group size, different variances)
group_all[["250"]][["n"]] = 250
group_all[["250"]][["group_id"]] = rep(1:25,each=10)
group_all[["250"]][["baseline sigma2"]] = rep(0.5,25)
group_all[["250"]][["slope sigma2"]] = rep(0,25)
group_all[["250"]][["bg2 sigma2"]] = rep(0,25)


 


