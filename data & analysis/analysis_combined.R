#### required packages ####

require(plyr)
require(sciplot)
require(ez)
require(ggplot2)
require(ggthemes)

#### load data ####

alldata <- read.csv2('data & analysis/raw-data/filtered-data-no-mturk-id.csv')

#### adding columns for relevant_distance and irrelevant_distance
alldata$relevant_distance <- mapply(function(x,y,relevant){
  if(relevant == 'shape-relevant'){ return(x) }
  if(relevant == 'tail-relevant'){ return(y) }
}, alldata$xdist, alldata$ydist, alldata$dimension)

alldata$irrelevant_distance <- mapply(function(x,y,relevant){
  if(relevant == 'shape-relevant'){ return(y) }
  if(relevant == 'tail-relevant'){ return(x) }
}, alldata$xdist, alldata$ydist, alldata$dimension)

#### training data ####

training_data <- subset(alldata, trial_type == "adaptive_t")
subject_training_info <- ddply(training_data, .(mturk_id, stim_type), function(s){
  return(c(trial_count=nrow(subset(s, trial_type=="adaptive_t"))))
})

# does it take more training trials to learn LD stimuli versus HD stimuli?
t.test(trial_count ~ stim_type, data=subject_training_info)

#### testing data ####

similarity_test_data <- subset(alldata, trial_type == "similarity")
sd_test_data <- subset(alldata, trial_type == "same-different" & distance > 0) # exclude same trials
xab_test_data <- subset(alldata, trial_type == "xab")

#### summarized data: within v between ####
# this section is looking at within versus between analysis ignoring factors
# like dimension of variation and distance.

w_v_bw_sim <- ddply(similarity_test_data, .(mturk_id, category_type, train_type, stim_type), function(s){
  c(mean_sim_score = mean(s$sim_score))
})
layout(c(1,2))
bargraph.CI(category_type, mean_sim_score, train_type, data=subset(w_v_bw_sim, stim_type=="LD"), legend=T)
bargraph.CI(category_type, mean_sim_score, train_type, data=subset(w_v_bw_sim, stim_type=="HD"), legend=T)
ezANOVA(data=w_v_bw_sim, dv=mean_sim_score, wid=mturk_id, between=.(train_type, stim_type), within=(category_type))

w_v_bw_xab <- ddply(xab_test_data, .(mturk_id, category_type, train_type, stim_type), function(s){
  c(mean_score = mean(s$correct))
})
layout(c(1,2))
bargraph.CI(category_type, mean_score, train_type, data=subset(w_v_bw_xab, stim_type=="LD"), legend=T)
bargraph.CI(category_type, mean_score, train_type, data=subset(w_v_bw_xab, stim_type=="HD"), legend=T)
ezANOVA(data=w_v_bw_xab, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type), within=(category_type))

w_v_bw_sd <- ddply(sd_test_data, .(mturk_id, category_type, train_type, stim_type), function(s){
  c(mean_score = mean(s$correct))
})
layout(c(1,2))
bargraph.CI(category_type, mean_score, train_type, data=subset(w_v_bw_sd, stim_type=="LD"), legend=T)
bargraph.CI(category_type, mean_score, train_type, data=subset(w_v_bw_sd, stim_type=="HD"), legend=T)
ezANOVA(data=w_v_bw_sd, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type), within=(category_type))

#### boundary effects ####
# looking for boundary-specific expansion and compression
# pairs that vary on relevant dimension only
# pairs that vary by only 1 or 2 units on relevant dimension

boundary_sim <- ddply(subset(similarity_test_data, irrelevant_distance==0 & relevant_distance > 0 & relevant_distance < 3),
                      .(mturk_id, category_type, train_type, stim_type), function(s){
                        return(c(mean_score=mean(s$sim_score)))
                      })
ezANOVA(data=boundary_sim, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type), within=(category_type))

boundary_xab <- ddply(subset(xab_test_data, irrelevant_distance==0 & relevant_distance > 0 & relevant_distance < 3),
                      .(mturk_id, category_type, train_type, stim_type), function(s){
                        return(c(mean_score=mean(s$correct)))
                      })
ezANOVA(data=boundary_xab, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type), within=(category_type))

boundary_sd <- ddply(subset(sd_test_data, irrelevant_distance==0 & relevant_distance > 0 & relevant_distance < 3),
                      .(mturk_id, category_type, train_type, stim_type), function(s){
                        return(c(mean_score=mean(s$correct)))
                      })
ezANOVA(data=boundary_sd, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type), within=(category_type))

#### dimension effects ####
# looking for acquired distinctiveness and acquired equivalence effects
# pairs that vary only on relevant or irrelevant dimension

# similarity
rel_dimension_sim <- ddply(subset(similarity_test_data, irrelevant_distance==0),
                       .(mturk_id, train_type, stim_type), function(s){
                         return(c(mean_score=mean(s$sim_score)))
                       })
ezANOVA(data=rel_dimension_sim, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type))
bargraph.CI(stim_type, mean_score, train_type, data=rel_dimension_sim)

irrel_dimension_sim <- ddply(subset(similarity_test_data, relevant_distance==0),
                           .(mturk_id, train_type, stim_type), function(s){
                             return(c(mean_score=mean(s$sim_score)))
                           })
ezANOVA(data=irrel_dimension_sim, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type))
bargraph.CI(stim_type, mean_score, train_type, data=irrel_dimension_sim)

# xab
rel_dimension_xab <- ddply(subset(xab_test_data, irrelevant_distance==0),
                           .(mturk_id, train_type, stim_type), function(s){
                             return(c(mean_score=mean(s$correct)))
                           })
ezANOVA(data=rel_dimension_xab, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type))
bargraph.CI(stim_type, mean_score, train_type, data=rel_dimension_xab)

irrel_dimension_xab <- ddply(subset(xab_test_data, relevant_distance==0),
                             .(mturk_id, train_type, stim_type), function(s){
                               return(c(mean_score=mean(s$correct)))
                             })
ezANOVA(data=irrel_dimension_xab, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type))
bargraph.CI(stim_type, mean_score, train_type, data=irrel_dimension_xab)

# sd
rel_dimension_sd <- ddply(subset(sd_test_data, irrelevant_distance==0),
                           .(mturk_id, train_type, stim_type), function(s){
                             return(c(mean_score=mean(s$correct)))
                           })
ezANOVA(data=rel_dimension_sd, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type))
bargraph.CI(stim_type, mean_score, train_type, data=rel_dimension_sd)

irrel_dimension_sd <- ddply(subset(sd_test_data, relevant_distance==0),
                             .(mturk_id, train_type, stim_type), function(s){
                               return(c(mean_score=mean(s$correct)))
                             })
ezANOVA(data=irrel_dimension_sd, dv=mean_score, wid=mturk_id, between=.(train_type, stim_type))
bargraph.CI(stim_type, mean_score, train_type, data=irrel_dimension_sd)

