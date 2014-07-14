# must run analysis script first to generate data frames...

# xab_influence_irrelevant2
# sim_influence_irrelevant2
# sd_influence_irrelevant2

# xab_influence_relevant2
# sim_influence_relevant2
# sd_influence_relevant2
simsave_r = sim_influence_relevant2
simsave_i = sim_influence_irrelevant2

sim_influence_irrelevant2$V1 = -sim_influence_irrelevant2$V1/100
sim_influence_relevant2$V1 = -sim_influence_relevant2$V1/100

xab_influence_irrelevant2$task = "XAB"
sim_influence_irrelevant2$task = "SIM"
sd_influence_irrelevant2$task = "SD"

xab_influence_relevant2$task = "XAB"
sim_influence_relevant2$task = "SIM"
sd_influence_relevant2$task = "SD"

xab_influence_irrelevant2$dimension = "Irrelevant"
sim_influence_irrelevant2$dimension = "Irrelevant"
sd_influence_irrelevant2$dimension = "Irrelevant"

xab_influence_relevant2$dimension = "Relevant"
sim_influence_relevant2$dimension = "Relevant"
sd_influence_relevant2$dimension = "Relevant"

influencedata <- rbind.fill(xab_influence_irrelevant2,sim_influence_irrelevant2,
                       sd_influence_irrelevant2,xab_influence_relevant2,
                       sim_influence_relevant2,sd_influence_relevant2)

influencedata_flipped <- influencedata
influencedata_original <- influencedata

influencedata_original$study = 'original'
influencedata_flipped$study = 'flipped'

influencedata_all <- rbind(influencedata_original, influencedata_flipped)

#save(influencedata_all, file="influencedata.Rdata")

require(ggplot2)
ggplot(influencedata_all, aes(x = dimension, y = V1, fill=train_type))+
  stat_summary(fun.y = mean, geom="bar", position="dodge")+
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, colour="black", position=position_dodge(width=0.9)) +
  facet_grid(study ~ task)


