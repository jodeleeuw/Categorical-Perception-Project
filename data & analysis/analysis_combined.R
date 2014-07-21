#### SECTION: required packages ####

require(plyr)
require(sciplot)
require(ez)
require(ggplot2)
require(ggthemes)

filterdata_original <- read.csv(file="filterdata_original.csv")
filterdata_original$experiment <- "original"
filterdata_flipped <- read.csv(file="filterdata_flipped.csv")
filterdata_flipped$experiment <- "flipped"
filterdata_all <- rbind(filterdata_original, filterdata_flipped)

### SD data

sd_classic_cp_shape0 <- ddply(filterdata_all[filterdata_all$trial_type=="same-different" & filterdata_all$xdist==0 & filterdata_all$distance >0,],
                            .(mturk_id, train_type, stim_type, experiment),
                            function(subset)with(subset, c(mean_score = mean(correct))))

sd_classic_cp_shape0$varying_dimension <- "TAIL"

sd_classic_cp_shape0$feature_type <- sapply(sd_classic_cp_shape0$experiment, function(experiment) { 
  if(experiment == 'original') {
    return('irrelevant')
  } else {
    return('relevant')
  }
})

sd_classic_cp_tail0 <- ddply(filterdata_all[filterdata_all$trial_type=="same-different" & filterdata_all$ydist==0 & filterdata_all$distance > 0,],
                              .(mturk_id, train_type, stim_type, experiment),
                              function(subset)with(subset, c(mean_score = mean(correct))))

sd_classic_cp_tail0$varying_dimension <- "SHAPE"

sd_classic_cp_tail0$feature_type <- sapply(sd_classic_cp_tail0$experiment, function(experiment) { 
  if(experiment == 'original') {
    return('relevant')
  } else {
    return('irrelevant')
  }
})

sd_classic_cp_all <- rbind(sd_classic_cp_shape0, sd_classic_cp_tail0)

ezANOVA(data=sd_classic_cp_all,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(feature_type, varying_dimension),
        between = .(stim_type, train_type))

sim_classic_cp_shape0 <- ddply(filterdata_all[filterdata_all$trial_type=="similarity" & filterdata_all$xdist==0,],
                              .(mturk_id, train_type, stim_type, experiment),
                              function(subset)with(subset, c(mean_score = mean(sim_score))))

sim_classic_cp_shape0$varying_dimension <- "TAIL"

sim_classic_cp_shape0$feature_type <- sapply(sim_classic_cp_shape0$experiment, function(experiment) { 
  if(experiment == 'original') {
    return('irrelevant')
  } else {
    return('relevant')
  }
})

sim_classic_cp_tail0 <- ddply(filterdata_all[filterdata_all$trial_type=="similarity" & filterdata_all$ydist==0,],
                             .(mturk_id, train_type, stim_type, experiment),
                             function(subset)with(subset, c(mean_score = mean(sim_score))))

sim_classic_cp_tail0$varying_dimension <- "SHAPE"

sim_classic_cp_tail0$feature_type <- sapply(sim_classic_cp_tail0$experiment, function(experiment) { 
  if(experiment == 'original') {
    return('relevant')
  } else {
    return('irrelevant')
  }
})

sim_classic_cp_all <- rbind(sim_classic_cp_shape0, sim_classic_cp_tail0)

ezANOVA(data=sim_classic_cp_all,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(feature_type, varying_dimension),
        between = .(stim_type, train_type))

xab_classic_cp_shape0 <- ddply(filterdata_all[filterdata_all$trial_type=="xab" & filterdata_all$xdist==0,],
                              .(mturk_id, train_type, stim_type, experiment),
                              function(subset)with(subset, c(mean_score = mean(correct))))

xab_classic_cp_shape0$varying_dimension <- "TAIL"

xab_classic_cp_shape0$feature_type <- sapply(xab_classic_cp_shape0$experiment, function(experiment) { 
  if(experiment == 'original') {
    return('irrelevant')
  } else {
    return('relevant')
  }
})

xab_classic_cp_tail0 <- ddply(filterdata_all[filterdata_all$trial_type=="xab" & filterdata_all$ydist==0,],
                             .(mturk_id, train_type, stim_type, experiment),
                             function(subset)with(subset, c(mean_score = mean(correct))))

xab_classic_cp_tail0$varying_dimension <- "SHAPE"

xab_classic_cp_tail0$feature_type <- sapply(xab_classic_cp_tail0$experiment, function(experiment) { 
  if(experiment == 'original') {
    return('relevant')
  } else {
    return('irrelevant')
  }
})

xab_classic_cp_all <- rbind(xab_classic_cp_shape0, xab_classic_cp_tail0)

ezANOVA(data=xab_classic_cp_all,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(feature_type, varying_dimension),
        between = .(stim_type, train_type))

bargraph.CI(stim_type, mean_score, train_type, 
            data = sd_classic_cp_all[sd_classic_cp_all$varying_dimension=="SHAPE" & 
                                       sd_classic_cp_all$relDimension=="SHAPE",], 
             main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = sd_classic_cp_all[sd_classic_cp_all$varying_dimension=="TAIL" & 
                                       sd_classic_cp_all$relDimension=="SHAPE",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = sd_classic_cp_all[sd_classic_cp_all$varying_dimension=="TAIL" & 
                                       sd_classic_cp_all$relDimension=="TAIL",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = sd_classic_cp_all[sd_classic_cp_all$varying_dimension=="SHAPE" & 
                                       sd_classic_cp_all$relDimension=="TAIL",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = sim_classic_cp_all[sim_classic_cp_all$varying_dimension=="SHAPE" & 
                                       sim_classic_cp_all$relDimension=="SHAPE",], 
            main="", ylab="", xlab="", legend = T)

layout(matrix(1:4, nrow=1))
bargraph.CI(train_type, mean_score, 
            data = sim_classic_cp_all[sim_classic_cp_all$relDimension=="SHAPE" & sd_classic_cp_all$varying_dimension=="SHAPE",],  
            main="SHAPE RELEVANT SHAPE CONSTANT", ylab="", xlab="")
bargraph.CI(train_type, mean_score, 
            data = sim_classic_cp_all[sim_classic_cp_all$relDimension=="TAIL" & sd_classic_cp_all$varying_dimension=="TAIL",],  
            main="TAIL RELEVANT TAIL CONSTANT", ylab="", xlab="")
bargraph.CI(train_type, mean_score, 
            data = sim_classic_cp_all[sim_classic_cp_all$relDimension=="SHAPE" & sd_classic_cp_all$varying_dimension=="TAIL",],  
            main="SHAPE RELEVANT TAIL CONSTANT", ylab="", xlab="")
bargraph.CI(train_type, mean_score, 
            data = sim_classic_cp_all[sim_classic_cp_all$relDimension=="TAIL" & sd_classic_cp_all$varying_dimension=="SHAPE",],  
            main="TAIL RELEVANT SHAPE CONSTANT", ylab="", xlab="")

bargraph.CI(stim_type, mean_score, train_type, 
            data = sim_classic_cp_all[sim_classic_cp_all$varying_dimension=="TAIL" & 
                                       sim_classic_cp_all$relDimension=="SHAPE",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = sim_classic_cp_all[sim_classic_cp_all$varying_dimension=="TAIL" & 
                                       sim_classic_cp_all$relDimension=="TAIL",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = sim_classic_cp_all[sim_classic_cp_all$varying_dimension=="SHAPE" & 
                                       sim_classic_cp_all$relDimension=="TAIL",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = xab_classic_cp_all[xab_classic_cp_all$varying_dimension=="SHAPE" & 
                                        xab_classic_cp_all$relDimension=="SHAPE",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = xab_classic_cp_all[xab_classic_cp_all$varying_dimension=="TAIL" & 
                                        xab_classic_cp_all$relDimension=="SHAPE",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = xab_classic_cp_all[xab_classic_cp_all$varying_dimension=="TAIL" & 
                                        xab_classic_cp_all$relDimension=="TAIL",], 
            main="", ylab="", xlab="", legend = T)

bargraph.CI(stim_type, mean_score, train_type, 
            data = xab_classic_cp_all[xab_classic_cp_all$varying_dimension=="SHAPE" & 
                                        xab_classic_cp_all$relDimension=="TAIL",], 
            main="", ylab="", xlab="", legend = T)
