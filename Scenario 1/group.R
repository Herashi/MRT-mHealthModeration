## specify group structure
group_all = list()
group_all[["2500"]] = list()


# 625 (euqal group size, different variances)
group_all[["2500"]][["n"]] = 2500
group_all[["2500"]][["group_id"]] = rep(1:100,each=25)
group_all[["2500"]][["baseline sigma2"]] = rep(0.5,100)
group_all[["2500"]][["slope sigma2"]] = rep(0,100)
group_all[["2500"]][["bg2 sigma2"]] = rep(0,100)


 


