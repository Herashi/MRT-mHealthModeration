## specify group structure
group_all = list()
group_all[["625"]] = list()


# 625 (euqal group size, different variances)
group_all[["625"]][["n"]] = 625
group_all[["625"]][["group_id"]] = rep(1:25,each=25)
group_all[["625"]][["baseline sigma2"]] = rep(0.5,25)
group_all[["625"]][["slope sigma2"]] = rep(0.1,25)
group_all[["625"]][["bg2 sigma2"]] = rep(0,25)


 


