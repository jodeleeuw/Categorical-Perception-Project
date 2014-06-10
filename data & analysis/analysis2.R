#### SECTION: required packages ####

require(plyr)
require(sciplot)
require(ez)
require(ggplot2)
require(ggthemes)

#### SECTION: load all data ####

pilotdata <- read.csv2('pilot_flipped_dimensions.csv', quote = "''")

#### SECTION: get data columns in proper format ####

pilotdata$ts        <- strftime(as.POSIXlt(pilotdata$ts),format='%Y-%m-%d %H:%M:%S')
pilotdata$distance    <- as.numeric(as.character(pilotdata$distance))
pilotdata$xdist       <- as.numeric(as.character(pilotdata$xdist))
pilotdata$ydist       <- as.numeric(as.character(pilotdata$ydist))
pilotdata$correct     <- as.numeric(as.character(pilotdata$correct))
pilotdata$block       <- as.numeric(as.character(pilotdata$block))
pilotdata$sim_score   <- as.numeric(as.character(pilotdata$sim_score))
pilotdata$rt          <- as.numeric(as.character(pilotdata$rt))
pilotdata$trial_index <- as.numeric(as.character(pilotdata$trial_index))
pilotdata$trial_idx   <- as.numeric(as.character(pilotdata$trial_idx))

# add a column that indicates category_type (WITHIN v BETWEEN)
pilotdata$category_type <- sapply(pilotdata$comparison_type,function(ct) {
  if(ct=="ww" || ct=="pp") { return("WITHIN") }
  if(ct=="pw" || ct=="wp") { return("BETWEEN")}
  return(NA)
})

#### SECTION: subject count ####

n_subjects <- as.character(unique(pilotdata$mturk_id))

#### SECTION: filter data ####

### duplicate subjects
# somehow at least one subject did the experiment twice, even though
# this should have been impossible. let's remove all the subjects who
# did the experiment more than once

# filter data to look only at perceptual testing trials
test_data <- pilotdata[pilotdata$trial_type == "similarity" | pilotdata$trial_type == "xab" | pilotdata$trial_type == "same-different",]

# count how many trials each subject did
# subjects in XAB and SIM conditions should be 144
# subjects in SD condition should be 216
count_by_subj <- ddply(test_data, .(mturk_id), function(subset)with(subset,c(rows=nrow(subset))))

# find all subjects with more than 216 trials
repeat_subjects <- as.character(count_by_subj[count_by_subj$rows > 216,]$mturk_id)

# there are 8 subjects who did this

# filter those subjects out
filterdata <- pilotdata[!pilotdata$mturk_id %in% repeat_subjects,]
###

### subjects who failed training

# this function will figure out if someone failed training based on whether their 
# accuracy on five consecutive training blocks is below 60%
fail_detector <- function(acc_array){
  con_blocks_under = 0
  for(i in 1:length(acc_array)){
    if(acc_array[i] < 0.60){con_blocks_under = con_blocks_under+1}
    else { con_blocks_under = 0}
  }
  return(con_blocks_under>=5)
}

# this function runs the fail_detector function on each subject and builds a table to determine whether they
# passed or failed training
subject_fail_table <- function(d_block){
  b = ddply(d_block, .(block), function(subset)with(subset, c(acc = mean(as.numeric(as.character(correct))))))
  c = fail_detector(b$acc)
  return(c)
}

# run the subject_fail_table method on the data, fail_subjects will be a list of all the subjects that FAILED training
training_data <- filterdata[filterdata$trial_type == "adaptive_t" | filterdata$trial_type == "adaptive_train",]
df = ddply(training_data, .(mturk_id, exp_condition, stim_type), function(subset)subject_fail_table(subset))
fail_subjects = as.character(df[df$V1=="TRUE",]$mturk_id)

# one subject failed training

# filter out all subjects in the fail_subjects list
filterdata <- filterdata[!filterdata$mturk_id %in% fail_subjects,]
###

### filter out subjects with 0 sd on sim score
# these subjects should be removed because they did not complete
# the task as instructed
sd_by_subj_sim <- ddply(filterdata[filterdata$test_type=="SIM" & filterdata$trial_type=="similarity",], .(mturk_id), function(subset)with(subset,c(sd_sim=sd(sim_score))))
sd_0_subjects <- as.character(sd_by_subj_sim[sd_by_subj_sim$sd_sim==0,]$mturk_id)

# no subjects had this
###

### filter out subjects who pressed the same key for every test trial
# these subjects should be removed because they did not complete
# the task as instructed

sd_key_press <- ddply(filterdata[filterdata$trial_type=="xab" | filterdata$trial_type=="same-different",], .(mturk_id), function(subset)with(subset,c(sd_key=sd(key_press))))
sd_0_key_subjects <- as.character(sd_key_press[sd_key_press$sd==0,]$mturk_id)

# no subjects had this
###

#### SECTION: Subject by condition count ####
subcount <- ddply(filterdata, .(mturk_id, exp_condition), function(subset){return(1)})
table(subcount$exp_condition)

#### SECTION: Trials to complete training ####

# filter the data to look only at training trials
training_trials <- filterdata[filterdata$trial_type=="adaptive_t" | filterdata$trial_type=="adaptive_train",]

# count the number of rows associated with each mturk_id in this data set
training_trial_count <- ddply(training_trials, .(mturk_id, stim_type), function(subset)with(subset, c(len = nrow(subset))))

# visualize trials based on stim type
layout(1)
bargraph.CI(stim_type,len,data=training_trial_count)

# t test
t.test(len ~ stim_type, var.equal=T,data=training_trial_count)

# descriptive stats
sd(training_trial_count[training_trial_count$stim_type=="HD",]$len)
sd(training_trial_count[training_trial_count$stim_type=="LD",]$len)

#### SECTION: Post training category test ####

category_data <- filterdata[filterdata$trial_type=="singleimag",]

#### SECTION: Classic CP analysis (within v. between) ####
# all analyses in this section will restrict to pairs that are only 
# 1 or 2 units away on the relevant dimension
# can't use 3 unit pairs because they are ALL between category.

### SD data
sd_classic_cp <- ddply(filterdata[filterdata$trial_type=="same-different" & filterdata$xdist<3 & filterdata$ydist==0 & filterdata$distance>0,],
                       .(mturk_id, train_type, stim_type, category_type ),
                       function(subset)with(subset, c(mean_score = mean(correct))))
layout(matrix(1:2, nrow=1))
bargraph.CI(category_type, mean_score, train_type, data = sd_classic_cp[sd_classic_cp$stim_type=="HD",], ylim=c(0,1), main="HD stimuli", ylab="Mean Accuracy", xlab="Category Comparison Type")
bargraph.CI(category_type, mean_score, train_type, data = sd_classic_cp[sd_classic_cp$stim_type=="LD",], ylim=c(0,1), main="LD stimuli", legend=T, xlab="Category Comparison Type")

ezANOVA(data=sd_classic_cp,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(category_type),
        between = .(stim_type, train_type))

### SIM data
sim_classic_cp <- ddply(filterdata[filterdata$trial_type=="similarity" & filterdata$xdist<3 & filterdata$ydist==0,],
                        .(mturk_id, train_type, stim_type, category_type ),
                        function(subset)with(subset, c(mean_score = mean(sim_score))))
layout(matrix(1:2, nrow=1))
bargraph.CI(category_type, mean_score, train_type, data = sim_classic_cp[sim_classic_cp$stim_type=="HD",], ylim=c(0,75), main="HD stimuli", ylab="Mean Similarity Score", xlab="Category Comparison Type")
bargraph.CI(category_type, mean_score, train_type, data = sim_classic_cp[sim_classic_cp$stim_type=="LD",], ylim=c(0,75), main="LD stimuli", legend=T, xlab="Category Comparison Type")

ezANOVA(data=sim_classic_cp,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(category_type),
        between = .(stim_type, train_type))

# graphing the category_type x train type interaction
bargraph.CI(category_type, mean_score, train_type, data=sim_classic_cp)

### XAB data
xab_classic_cp <- ddply(filterdata[filterdata$trial_type=="xab" & filterdata$xdist<3 & filterdata$ydist==0,],
                        .(mturk_id, train_type, stim_type, category_type ),
                        function(subset)with(subset, c(mean_score = mean(correct))))
layout(matrix(1:2, nrow=1))
bargraph.CI(category_type, mean_score, train_type, data = xab_classic_cp[xab_classic_cp$stim_type=="HD",], ylim=c(0.5,0.9), main="HD stimuli", ylab="Mean Accuracy", xlab="Category Comparison Type")
bargraph.CI(category_type, mean_score, train_type, data = xab_classic_cp[xab_classic_cp$stim_type=="LD",], ylim=c(0.5,0.9), main="LD stimuli", legend=T, xlab="Category Comparison Type")

ezANOVA(data=xab_classic_cp,
        dv = .(mean_score),
        wid = .(mturk_id),
        within = .(category_type),
        between = .(stim_type, train_type))

#### SECTION: Influence of dimensions ####
# This section analyzes the influence of the relevant and irrelevant dimensions
# when the other dimension is held constant.

# arguments
# df - data frame with data from a single subject
# influence_dimension - name of the column in df that contains dimension information (distance measure)
# dv - name of the column in df that contains dependent variable
influence_measure <- function(df,influence_dimension, dv){
  total = 0
  count = 0
  
  for( i in max(df[,influence_dimension]):min(df[,influence_dimension]) ){
    for( j in max(df[,influence_dimension]):min(df[,influence_dimension]) ){
      if(i > j){
        total = total + (df[df[,influence_dimension]==i,][,dv] - df[df[,influence_dimension]==j,][,dv])
        count = count + 1
      }
    }
  }
  
  return(c(influence = total/count))
}

#### Same Different task

# add a column to indicate when the response was 'different' (independent of whether it was correct)
sd_diff_data <- filterdata[filterdata$trial_type=="same-different",]
sd_diff_data$diff <- sapply(sd_diff_data$key_press, function(k){
  if(k==68) { return(1); }
  else { return(0); }
})

sd_dimensions <- ddply(sd_diff_data, .(mturk_id, train_type, stim_type, xdist, ydist),
                       function(subset)with(subset,c(mean_score=mean(diff))))

## irrelevant dimension
sd_influence_irrelevant <- ddply(sd_dimensions, .(mturk_id, train_type, stim_type, xdist), function(subset){
  influence_measure(subset, "ydist", "mean_score")
})

# truncate the meaningless xdist = 3 rows
sd_influence_irrelevant <- sd_influence_irrelevant[sd_influence_irrelevant$xdist < 3,]

## relevant dimension
sd_influence_relevant <- ddply(sd_dimensions, .(mturk_id, train_type, stim_type, ydist), function(subset){
  influence_measure(subset, "xdist", "mean_score")
})

# truncate the meaningless ydist = 3 rows
sd_influence_relevant <- sd_influence_relevant[sd_influence_relevant$ydist < 3,]

# graph the results
layout(matrix(1:4,nrow=2, byrow=T))
bargraph.CI(xdist, influence, train_type,data=sd_influence_irrelevant[sd_influence_irrelevant$stim_type=="HD",])
bargraph.CI(xdist, influence, train_type,data=sd_influence_irrelevant[sd_influence_irrelevant$stim_type=="LD",])
bargraph.CI(ydist, influence, train_type,data=sd_influence_relevant[sd_influence_relevant$stim_type=="HD",])
bargraph.CI(ydist, influence, train_type,data=sd_influence_relevant[sd_influence_relevant$stim_type=="LD",])

# ANOVAs
sd_influence_irrelevant$xdist <- factor(sd_influence_irrelevant$xdist)
ezANOVA(data=sd_influence_irrelevant,
        dv=.(influence),
        wid=.(mturk_id),
        between=.(train_type,stim_type),
        within=.(xdist))

sd_influence_relevant$ydist <- factor(sd_influence_relevant$ydist)
ezANOVA(data=sd_influence_relevant,
        dv=.(influence),
        wid=.(mturk_id),
        between=.(train_type,stim_type),
        within=.(ydist))

#### Similarity task

# add a column to indicate when the response was 'different' (independent of whether it was correct)
sim_dimensions <- ddply(filterdata[filterdata$trial_type=="similarity",], .(mturk_id, train_type, stim_type, xdist, ydist),
                       function(subset)with(subset,c(mean_score=mean(sim_score))))

## irrelevant dimension
sim_influence_irrelevant <- ddply(sim_dimensions, .(mturk_id, train_type, stim_type, xdist), function(subset){
  influence_measure(subset, "ydist", "mean_score")
})

# truncate the meaningless xdist = 3 rows
sim_influence_irrelevant <- sim_influence_irrelevant[sim_influence_irrelevant$xdist < 3,]

## relevant dimension
sim_influence_relevant <- ddply(sim_dimensions, .(mturk_id, train_type, stim_type, ydist), function(subset){
  influence_measure(subset, "xdist", "mean_score")
})

# truncate the meaningless ydist = 3 rows
sim_influence_relevant <- sim_influence_relevant[sim_influence_relevant$ydist < 3,]

# graph the results
layout(matrix(1:4,nrow=2, byrow=T))
bargraph.CI(xdist, influence, train_type,data=sim_influence_irrelevant[sim_influence_irrelevant$stim_type=="HD",])
bargraph.CI(xdist, influence, train_type,data=sim_influence_irrelevant[sim_influence_irrelevant$stim_type=="LD",])
bargraph.CI(ydist, influence, train_type,data=sim_influence_relevant[sim_influence_relevant$stim_type=="HD",])
bargraph.CI(ydist, influence, train_type,data=sim_influence_relevant[sim_influence_relevant$stim_type=="LD",])

# ANOVAs
sim_influence_irrelevant$xdist <- factor(sim_influence_irrelevant$xdist)
ezANOVA(data=sim_influence_irrelevant,
        dv=.(influence),
        wid=.(mturk_id),
        between=.(train_type,stim_type),
        within=.(xdist))

sim_influence_relevant$ydist <- factor(sim_influence_relevant$ydist)
ezANOVA(data=sim_influence_relevant,
        dv=.(influence),
        wid=.(mturk_id),
        between=.(train_type,stim_type),
        within=.(ydist))

bargraph.CI(stim_type, influence, train_type,  data=sim_influence_irrelevant)


#### XAB task

xab_dimensions <- ddply(filterdata[filterdata$trial_type=="xab",], .(mturk_id, train_type, stim_type, xdist, ydist),
                       function(subset)with(subset,c(mean_score=mean(correct))))

## irrelevant dimension
xab_influence_irrelevant <- ddply(xab_dimensions, .(mturk_id, train_type, stim_type, xdist), function(subset){
  influence_measure(subset, "ydist", "mean_score")
})

# truncate the meaningless xdist = 3 rows
xab_influence_irrelevant <- xab_influence_irrelevant[xab_influence_irrelevant$xdist < 3,]

## relevant dimension
xab_influence_relevant <- ddply(xab_dimensions, .(mturk_id, train_type, stim_type, ydist), function(subset){
  influence_measure(subset, "xdist", "mean_score")
})

# truncate the meaningless ydist = 3 rows
xab_influence_relevant <- xab_influence_relevant[xab_influence_relevant$ydist < 3,]

# graph the results
layout(matrix(1:4,nrow=2, byrow=T))
bargraph.CI(xdist, influence, train_type,data=xab_influence_irrelevant[xab_influence_irrelevant$stim_type=="HD",])
bargraph.CI(xdist, influence, train_type,data=xab_influence_irrelevant[xab_influence_irrelevant$stim_type=="LD",])
bargraph.CI(ydist, influence, train_type,data=xab_influence_relevant[xab_influence_relevant$stim_type=="HD",])
bargraph.CI(ydist, influence, train_type,data=xab_influence_relevant[xab_influence_relevant$stim_type=="LD",])

# ANOVAs
xab_influence_irrelevant$xdist <- factor(xab_influence_irrelevant$xdist)
ezANOVA(data=xab_influence_irrelevant,
        dv=.(influence),
        wid=.(mturk_id),
        between=.(train_type,stim_type),
        within=.(xdist))

xab_influence_relevant$ydist <- factor(xab_influence_relevant$ydist)
ezANOVA(data=xab_influence_relevant,
        dv=.(influence),
        wid=.(mturk_id),
        between=.(train_type,stim_type),
        within=.(ydist))

#### SECTION: figures ####

### Dimension data in 'raw' form
require(grid)

sd_plot <- ggplot(sd_dimensions, aes(x=xdist, y=mean_score, fill=train_type, shape=factor(ydist), group=interaction(train_type, ydist))) +
  scale_shape_manual(values=c(22,23,24,25))+
  scale_fill_grey(labels=c("Control", "Category training"))+
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=0.4), width=0.3, mult=1, colour="black") +
  stat_summary(fun.y = mean, geom="point", size=3, position=position_dodge(width=0.4), aes(fill=train_type), colour="black")+
  labs(title="Same/different task", x="Distance on relevant dimension", y="Proportion of 'different' responses", shape="Distance on \nirrelevant dimension", linetype="Distance on irrelevant dimension", fill="Condition")+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  ylim(0,1.0)+
  facet_grid(. ~ stim_type)+
  theme_few()

xab_plot <- ggplot(xab_dimensions, aes(x=xdist, y=mean_score, fill=train_type, shape=factor(ydist), group=interaction(train_type, ydist))) +
  scale_shape_manual(values=c(22,23,24,25))+
  scale_fill_grey(labels=c("Control", "Category training"))+
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=0.4), width=0.3, mult=1, colour="black") +
  stat_summary(fun.y = mean, geom="point", size=3, position=position_dodge(width=0.4), aes(fill=train_type), colour="black")+
  labs(title="XAB task", x="Distance on relevant dimension", y="Proportion of correct responses", shape="Distance on \nirrelevant dimension", linetype="Distance on irrelevant dimension", fill="Condition")+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  ylim(0,1.0)+
  facet_grid(. ~ stim_type)+
  geom_hline(yintercept=0.5, linetype=2)+
  theme_few()

sim_plot <- ggplot(sim_dimensions, aes(x=xdist, y=mean_score, fill=train_type, shape=factor(ydist), group=interaction(train_type, ydist))) +
  scale_shape_manual(values=c(22,23,24,25))+
  scale_fill_grey(labels=c("Control", "Category training"))+
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=0.4), width=0.3, mult=1, colour="black") +
  stat_summary(fun.y = mean, geom="point", size=3, position=position_dodge(width=0.4), aes(fill=train_type), colour="black")+
  labs(title="Similarity task", x="Distance on relevant dimension", y="Similarity rating", shape="Distance on \nirrelevant dimension", linetype="Distance on irrelevant dimension", fill="Condition")+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  ylim(0,100)+
  facet_grid(. ~ stim_type)+
  theme_few()

grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
print(sd_plot, vp = viewport(layout.pos.row=1, layout.pos.col=1))
print(xab_plot, vp = viewport(layout.pos.row=2, layout.pos.col=1))
print(sim_plot, vp = viewport(layout.pos.row=3, layout.pos.col=1))

### Between/within

stimuli_names <- list(
  'HD'="High discriminability stimuli",
  'LD'="Low discriminability stimuli"
)

stimuli_labeller <- function(variable,value){
  return(stimuli_names[value])
}

# only produce the legend on the XAB plot, so that the plots can stack
# vertically for publication

sd_classic_cp_graph <- ggplot(sd_classic_cp, aes(x=category_type, y=mean_score, fill=train_type))+
  facet_grid(.~stim_type,labeller=stimuli_labeller) +
  geom_boxplot(labels=c("Between category", "Within category")) +
  stat_summary(geom="point", fun.y=mean, size=4, position=position_dodge(width=0.75), shape=3)+
  scale_fill_grey(start=0.6,end=0.9,labels=c("Control", "Category\ntraining")) +
  labs(title="Same/different task", x="Pair type", y="Proportion of 'different' responses", fill="Condition")+
  scale_x_discrete(labels=c("Between\ncategory", "Within\ncategory"))+
  ylim(0,1)+
  theme_few()+
  theme( text = element_text(size=28), 
         axis.title.y = element_text(vjust=0.4), 
         axis.title.x = element_blank(),
         plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"),
         legend.key.height=unit(3, "line"),
         legend.key.width=unit(2, "cm"),
         legend.direction="horizontal",
         legend.position="none")

sim_classic_cp_graph <- ggplot(sim_classic_cp, aes(x=category_type, y=mean_score, fill=train_type))+
  facet_grid(.~stim_type,labeller=stimuli_labeller) +
  geom_boxplot(notch=F, labels=c("Between category", "Within category")) +
  stat_summary(geom="point", fun.y=mean, size=4, position=position_dodge(width=0.75), shape=3)+
  scale_fill_grey(start=0.6,end=0.9,labels=c("Control", "Category\ntraining")) +
  labs(title="Similarity task", x="Pair type", y="Average similarity rating", fill="Condition")+
  scale_x_discrete(labels=c("Between\ncategory", "Within\ncategory"))+
  theme_few()+
  theme( text = element_text(size=28), 
         axis.title.y = element_text(vjust=0.4), 
         axis.title.x = element_blank(),
         plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"),
         legend.key.height=unit(3, "line"),
         legend.key.width=unit(2, "cm"),
         legend.direction="horizontal",
         legend.position="none")

xab_classic_cp_graph <- ggplot(xab_classic_cp, aes(x=category_type, y=mean_score, fill=train_type))+
  facet_grid(.~stim_type,labeller=stimuli_labeller) +
  geom_hline(yintercept=0.5, linetype=2)+
  geom_boxplot(notch=F, labels=c("Between category", "Within category")) +
  stat_summary(geom="point", fun.y=mean, size=6, position=position_dodge(width=0.75), shape=3)+
  scale_fill_grey(start=0.6,end=0.9,labels=c("Control    ", "Category    \ntraining")) +
  labs(title="XAB task", x="Pair type", y="Proportion of correct responses", fill="Condition    ")+
  scale_x_discrete(labels=c("Between\ncategory", "Within\ncategory"))+
  theme_few()+
  ylim(0,1)+
  theme( text = element_text(size=28), 
         axis.title.y = element_text(vjust=0.4), 
         axis.title.x = element_blank(),
         plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"),
         legend.key.height=unit(3, "line"),
         legend.key.width=unit(2, "cm"),
         legend.direction="horizontal",
         legend.position="none")


#options(bitmapType="cairo")
sd_classic_cp_graph
#ggsave(filename="sd_classic_cp.pdf", width=3.25, height=2, units=c("in"), dpi=300)

sim_classic_cp_graph
#ggsave(filename="sim_classic_cp.png", width=3.25, height=2, units=c("in"),dpi=300,scale=(300/72))

xab_classic_cp_graph
ggsave(filename="xab_classic_cp.bmp", width=3.25, height=2.9, units=c("in"))

pdf(file="xab_classic_cp.pdf", width=32.5, height=29)
print(xab_classic_cp_graph)
dev.off()

## Influence measure

colnames(sd_influence_irrelevant) <- c("mturk_id", "train_type", "stim_type", "dist", "influence")
colnames(sd_influence_relevant) <- c("mturk_id", "train_type", "stim_type", "dist", "influence")

sd_influence_irrelevant$type = "IRRELEVANT"
sd_influence_relevant$type = "RELEVANT"

sd_influence <- rbind(sd_influence_irrelevant, sd_influence_relevant)

colnames(xab_influence_irrelevant) <- c("mturk_id", "train_type", "stim_type", "dist", "influence")
colnames(xab_influence_relevant) <- c("mturk_id", "train_type", "stim_type", "dist", "influence")

xab_influence_irrelevant$type = "IRRELEVANT"
xab_influence_relevant$type = "RELEVANT"

xab_influence <- rbind(xab_influence_irrelevant, xab_influence_relevant)

colnames(sim_influence_irrelevant) <- c("mturk_id", "train_type", "stim_type", "dist", "influence")
colnames(sim_influence_relevant) <- c("mturk_id", "train_type", "stim_type", "dist", "influence")

sim_influence_irrelevant$type = "IRRELEVANT"
sim_influence_relevant$type = "RELEVANT"

sim_influence <- rbind(sim_influence_irrelevant, sim_influence_relevant)

ggplot(sd_influence, aes(x=dist, y=influence, fill=train_type, shape=type, group=interaction(train_type, type)))+
  facet_grid(.~stim_type,labeller=stimuli_labeller)+
  scale_fill_grey(start=0.2,end=0.8,labels=c("Control    ", "Category training    ")) +
  scale_shape_manual(values=c(22,23), labels=c("Irrelevant    ", "Relevant    "))+
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, colour="black") +
  stat_summary(geom="point", fun.y=mean, size=5, aes(fill=train_type))+
  labs(title="Same/different task", x="Distance between items on the fixed dimension", y="Influence of varying dimension (arbitrary units)", fill="Condition    ", shape="Dimension that is varying    ")+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  theme_few()+
  theme( text = element_text(size=24), 
         axis.title.y = element_text(vjust=0.3), 
         axis.title.x = element_text(vjust=-0.5), 
         plot.margin = unit(c(.1,.1,.1,.1), "in"),
         legend.direction="horizontal",
         legend.position="none")

ggplot(xab_influence, aes(x=dist, y=influence, fill=train_type, shape=type, group=interaction(train_type, type)))+
  facet_grid(.~stim_type,labeller=stimuli_labeller)+
  scale_fill_grey(start=0.2,end=0.8,labels=c("Control", "Category\ntraining")) +
  scale_shape_manual(values=c(22,23), labels=c("Irrelevant", "Relevant"))+
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, colour="black") +
  stat_summary(geom="point", fun.y=mean, size=4, aes(fill=train_type))+
  labs(title="XAB task", x="Distance between items on the fixed dimension", y="Influence of varying dimension (arbitrary units)", fill="Condition", shape="Dimension that\nis varying")+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  theme_few()+
  theme( text = element_text(size=24), 
         axis.title.y = element_text(vjust=0.3), 
         axis.title.x = element_text(vjust=-0.5), 
         plot.margin = unit(c(.1,.1,.1,.1), "in"),
         legend.direction="horizontal",
         legend.position="none")

ggplot(sim_influence, aes(x=dist, y=influence, fill=train_type, shape=type, group=interaction(train_type, type)))+
  facet_grid(.~stim_type,labeller=stimuli_labeller)+
  scale_fill_grey(start=0.2,end=0.8,labels=c("Control", "Category\ntraining")) +
  scale_shape_manual(values=c(22,23), labels=c("Irrelevant", "Relevant"))+
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, colour="black") +
  stat_summary(geom="point", fun.y=mean, size=4, aes(fill=train_type))+
  labs(title="Similarity task", x="Distance between items on the fixed dimension", y="Influence of varying dimension (arbitrary units)", fill="Condition", shape="Dimension that\nis varying")+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  theme_few()+
  theme( text = element_text(size=24), 
         axis.title.y = element_text(vjust=0.3), 
         axis.title.x = element_text(vjust=-0.5), 
         plot.margin = unit(c(.1,.1,.1,.1), "in"),
         legend.direction="horizontal",
         legend.position="none")

#### SECTION: XAB analysis in depth ####

data_extremes <- filterdata[filterdata$trial_type=="xab" & filterdata$xdist<3 & filterdata$ydist==0,]

data_extremes$extreme <- mapply(function(a_path, b_path){
  a <- as.numeric(strsplit(as.character(a_path),'_')[[1]][2])
  b <- as.numeric(strsplit(as.character(b_path),'_')[[1]][2])
  if(abs(3.5-a) > abs(3.5-b)){ return("target") }
  if(abs(3.5-a) < abs(3.5-b)){ return("foil") }
  return("equal")
}, data_extremes$a_path, data_extremes$b_path)

xab_extreme_cp <- ddply(data_extremes,
                        .(mturk_id, train_type, stim_type, category_type, extreme),
                        function(subset)with(subset, c(mean_score = mean(correct))))

bargraph.CI(extreme, mean_score, train_type, data=xab_extreme_cp[xab_extreme_cp$stim_type=="HD" & xab_extreme_cp$category_type=="WITHIN",])

t.test(mean_score ~ extreme, paired=T, data=xab_extreme_cp[xab_extreme_cp$stim_type=="HD" & xab_extreme_cp$category_type=="WITHIN" & xab_extreme_cp$train_type=="TRAIN",])
