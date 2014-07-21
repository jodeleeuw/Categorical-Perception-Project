#### SECTION: required packages ####

require(plyr)
require(sciplot)
require(ez)
require(ggplot2)
require(ggthemes)

filterdata_original <- read.csv(file="raw-data/filterdata_original.csv")
filterdata_original$relDimension <- "SHAPE"
filterdata_flipped <- read.csv(file="raw-data/filterdata_flipped.csv")
filterdata_flipped$relDimension <- "TAIL"
filterdata_all <- rbind(filterdata_original, filterdata_flipped)

### SD data

sd_classic_cp_shape0 <- ddply(filterdata_all[filterdata_all$trial_type=="same-different" & filterdata_all$xdist==0 & filterdata_all$distance >0,],
                            .(mturk_id, train_type, stim_type, relDimension),
                            function(subset)with(subset, c(mean_score = mean(correct))))

sd_classic_cp_shape0$constant_dimension <- "SHAPE"

sd_classic_cp_tail0 <- ddply(filterdata_all[filterdata_all$trial_type=="same-different" & filterdata_all$ydist==0 & filterdata_all$distance > 0,],
                              .(mturk_id, train_type, stim_type, relDimension),
                              function(subset)with(subset, c(mean_score = mean(correct))))

sd_classic_cp_tail0$constant_dimension <- "TAIL"

sd_classic_cp_all <- rbind(sd_classic_cp_shape0, sd_classic_cp_tail0)

ezANOVA(data=sd_classic_cp_all,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(constant_dimension),
        between = .(stim_type, train_type, relDimension))

sim_classic_cp_shape0 <- ddply(filterdata_all[filterdata_all$trial_type=="similarity" & filterdata_all$xdist==0,],
                              .(mturk_id, train_type, stim_type, relDimension),
                              function(subset)with(subset, c(mean_score = mean(sim_score))))

sim_classic_cp_shape0$constant_dimension <- "SHAPE"

sim_classic_cp_tail0 <- ddply(filterdata_all[filterdata_all$trial_type=="similarity" & filterdata_all$ydist==0,],
                             .(mturk_id, train_type, stim_type, relDimension),
                             function(subset)with(subset, c(mean_score = mean(sim_score))))

sim_classic_cp_tail0$constant_dimension <- "TAIL"

sim_classic_cp_all <- rbind(sim_classic_cp_shape0, sim_classic_cp_tail0)

ezANOVA(data=sim_classic_cp_all,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(constant_dimension),
        between = .(stim_type, train_type, relDimension))

xab_classic_cp_shape0 <- ddply(filterdata_all[filterdata_all$trial_type=="xab" & filterdata_all$xdist==0,],
                              .(mturk_id, train_type, stim_type, relDimension),
                              function(subset)with(subset, c(mean_score = mean(correct))))

xab_classic_cp_shape0$constant_dimension <- "SHAPE"

xab_classic_cp_tail0 <- ddply(filterdata_all[filterdata_all$trial_type=="xab" & filterdata_all$ydist==0,],
                             .(mturk_id, train_type, stim_type, relDimension),
                             function(subset)with(subset, c(mean_score = mean(correct))))

xab_classic_cp_tail0$constant_dimension <- "TAIL"

xab_classic_cp_all <- rbind(xab_classic_cp_shape0, xab_classic_cp_tail0)

ezANOVA(data=xab_classic_cp_all,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(constant_dimension),
        between = .(stim_type, train_type, relDimension))
