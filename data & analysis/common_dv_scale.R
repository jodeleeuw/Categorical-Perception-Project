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


#### testing data ####

# remove testing items where both dimensions varied
onedimensiononly <- subset(alldata, (relevant_distance==0 | irrelevant_distance == 0) & distance > 0)
onedimensiononly$common_score <- mapply(function(s,c){
  if(!is.na(s)){ return(-s) }
  else { return(c) }
},onedimensiononly$sim_score, onedimensiononly$correct)

onedimensiononly$varying_dimension <- sapply(onedimensiononly$relevant_distance, function(d){
  if(d > 0) { return("relevant") }
  return("irrelevant")
})

testdata_norms <- ddply(onedimensiononly, .(trial_type), function(s){
  return(c(mean=mean(s$common_score), sd=sd(s$common_score)))
})

subject_test_data <- ddply(onedimensiononly, .(mturk_id, trial_type, stim_type, train_type, varying_dimension), function(s){
  m <- mean(s$common_score)
  test_m <- subset(testdata_norms, trial_type==s$trial_type[1])$mean
  test_s <- subset(testdata_norms, trial_type==s$trial_type[1])$sd
  z <- (m - test_m) / test_s
  return(c(z=z))
})

subject_test_data$trial_type <- factor(as.character(subject_test_data$trial_type))

ezANOVA(subject_test_data, wid=mturk_id, dv=z, within=.(varying_dimension), between=.(trial_type, stim_type, train_type))

bargraph.CI(trial_type, z, data=subject_test_data)
bargraph.CI(trial_type, z, varying_dimension, data=subject_test_data)

layout(c(1,2))
bargraph.CI(trial_type, z, varying_dimension, data=subset(subject_test_data, train_type=='CONTROL'), main="control group", ylab="normalized common score", legend=T)
bargraph.CI(trial_type, z, varying_dimension, data=subset(subject_test_data, train_type=='TRAIN'), main="train group", ylab="normalized common score")


bargraph.CI(varying_dimension, z, train_type, data=subject_test_data, legend=T)

## BETWEEN V WITHIN

subject_test_data <- ddply(subset(onedimensiononly, varying_dimension=="relevant" & distance < 3), .(mturk_id, trial_type, stim_type, train_type, category_type), function(s){
  m <- mean(s$common_score)
  test_m <- subset(testdata_norms, trial_type==s$trial_type[1])$mean
  test_s <- subset(testdata_norms, trial_type==s$trial_type[1])$sd
  z <- (m - test_m) / test_s
  return(c(z=z))
})

subject_test_data$trial_type <- factor(as.character(subject_test_data$trial_type))

layout(c(1,2))
bargraph.CI(trial_type, z, category_type, data=subset(subject_test_data, train_type=='CONTROL'), main="control group", ylab="normalized common score")
bargraph.CI(trial_type, z, category_type, data=subset(subject_test_data, train_type=='TRAIN'), main="train group", ylab="normalized common score", legend=T)

ezANOVA(subject_test_data, wid=mturk_id, dv=z, within=.(category_type), between=.(trial_type, stim_type, train_type))

bargraph.CI(category_type, z, train_type, data=subject_test_data, legend=T)
