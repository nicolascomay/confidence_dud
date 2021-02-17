library(MASS)
library(lestat)
library(ggplot2)
library(readxl)

# -------------------------------------------------------------------------------------------------

data <- read_xlsx('yourpath/data_experiment1.xlsx')
data$Correct = as.logical(data$Correct)

# ----------------------------------------------------------------------------------------------------

##################################
## Response times - type 1 task ##
##################################

type1_rt <- function(data){
  # this function returns a vector with response times in type 1 task for each level of difficulty
  
  listaprop <- sort(unique(data$StimVal))
  rt_prop   <- rep(NA, length(listaprop))
  
  for(i in seq_along(listaprop)){
    datosProp  <- data[data$StimVal == listaprop[i],]
    rt_prop[i] <- mean(datosProp$RT_type1)
    
  }
  
  return(rt_prop)
}

rt_prop07 <- c()
rt_prop08 <- c()
rt_prop09 <- c()
rt_prop93 <- c()
rt_prop95 <- c()
for(i in 1:max(data$Nsujeto)){
  sujeto <- data[data$Nsujeto == i,]
  rt_prop07 <- c(rt_prop07, type1_rt(sujeto)[1])
  rt_prop08 <- c(rt_prop08, type1_rt(sujeto)[2])
  rt_prop09 <- c(rt_prop09, type1_rt(sujeto)[3])
  rt_prop93 <- c(rt_prop93, type1_rt(sujeto)[4])
  rt_prop95 <- c(rt_prop95, type1_rt(sujeto)[5])
}

rtse07 <- mean_se(rt_prop07)
rtse08 <- mean_se(rt_prop08)
rtse09 <- mean_se(rt_prop09)
rtse93 <- mean_se(rt_prop93)
rtse95 <- mean_se(rt_prop95)

rt_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                          values = c(mean(rt_prop07), mean(rt_prop08), mean(rt_prop09), mean(rt_prop93), mean(rt_prop95)))

# plot

ggplot(rt_sujetos)+
  geom_bar(aes(x=names, y = values), stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(rtse07$ymin, rtse08$ymin, rtse09$ymin, rtse93$ymin, rtse95$ymin), ymax = c(rtse07$ymax, rtse08$ymax, rtse09$ymax, rtse93$ymax, rtse95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Response times (type 1 task) as a function of task difficulty')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Response time (ms)')+
  ylim(c(0,2500))

##################################
## Response times - type 2 task ##
##################################

confidence_RT <- function(data){
  # this function returns a vector with response times in type 2 task for each level of difficulty
  
  listaprop <- sort(unique(data$StimVal))
  rt_prop   <- rep(NA, length(listaprop))
  
  for(i in seq_along(listaprop)){
    datosProp  <- data[data$StimVal == listaprop[i],]
    rt_prop[i] <- mean(datosProp$RT_Confidence)
    
  }
  
  return(rt_prop)
}


rtconf_prop07 <- c()
rtconf_prop08 <- c()
rtconf_prop09 <- c()
rtconf_prop93 <- c()
rtconf_prop95 <- c()
for(i in 1:max(data$Nsujeto)){
  sujeto <- data[data$Nsujeto == i,]
  rtconf_prop07 <- c(rtconf_prop07, confidence_RT(sujeto)[1])
  rtconf_prop08 <- c(rtconf_prop08, confidence_RT(sujeto)[2])
  rtconf_prop09 <- c(rtconf_prop09, confidence_RT(sujeto)[3])
  rtconf_prop93 <- c(rtconf_prop93, confidence_RT(sujeto)[4])
  rtconf_prop95 <- c(rtconf_prop95, confidence_RT(sujeto)[5])
}

rtconfse07 <- mean_se(rtconf_prop07)
rtconfse08 <- mean_se(rtconf_prop08)
rtconfse09 <- mean_se(rtconf_prop09)
rtconfse93 <- mean_se(rtconf_prop93)
rtconfse95 <- mean_se(rtconf_prop95)

rtconf_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                              values = c(mean(rtconf_prop07), mean(rtconf_prop08), mean(rtconf_prop09), mean(rtconf_prop93), mean(rtconf_prop95)))

# plot

ggplot(rtconf_sujetos)+
  geom_bar(aes(x=names, y = values), stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(rtconfse07$ymin, rtconfse08$ymin, rtconfse09$ymin, rtconfse93$ymin, rtconfse95$ymin), ymax = c(rtconfse07$ymax, rtconfse08$ymax, rtconfse09$ymax, rtconfse93$ymax, rtconfse95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Response times (type 2 task) as a function of task difficulty')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Response time (ms)')+
  ylim(c(0,2500))

#-----------------------------------------------------------------------------

########################################################################################################
## Subtraction (type 1 task): response times with 3 alternatives - response times with 2 alternatives ##
########################################################################################################

resta_rt1 <- function(data, area2prop){
  # this function receives the data and size proportions of the second figure (task difficulty)
  # and returns the subtraction between mean response times with 3 alternatives and mean response times with 2 
  # alternatives for that level of difficulty. (size discrimination task)
  
  prop_alt3  <- data[data$StimVal == area2prop & data$Nalternativas == 3,]
  type1_rt3 <- mean(prop_alt3$RT_type1)
  
  prop_alt2  <- data[data$StimVal == area2prop & data$Nalternativas == 2,]
  type1_rt2 <- mean(prop_alt2$RT_type1)
  
  resta <- type1_rt3 - type1_rt2
  
  return(resta)}

restart1_prop07 <- c()
restart1_prop08 <- c()
restart1_prop09 <- c()
restart1_prop93 <- c()
restart1_prop95 <- c()
for(i in 1:max(data$Nsujeto)){
  sujeto <- data[data$Nsujeto == i,]
  restart1_prop07 <- c(restart1_prop07, resta_rt1(sujeto,0.7))
  restart1_prop08 <- c(restart1_prop08, resta_rt1(sujeto,0.8))
  restart1_prop09 <- c(restart1_prop09, resta_rt1(sujeto,0.9))
  restart1_prop93 <- c(restart1_prop93, resta_rt1(sujeto,0.93))
  restart1_prop95 <- c(restart1_prop95, resta_rt1(sujeto,0.95))
}

resta_rt1se07 <- mean_se(restart1_prop07)
resta_rt1se08 <- mean_se(restart1_prop08)
resta_rt1se09 <- mean_se(restart1_prop09)
resta_rt1se93 <- mean_se(restart1_prop93)
resta_rt1se95 <- mean_se(restart1_prop95)

restart1_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                                values = c(mean(restart1_prop07), mean(restart1_prop08), mean(restart1_prop09), mean(restart1_prop93), mean(restart1_prop95)))

# plot

ggplot(restart1_sujetos, aes(x=names, y = values))+
  geom_bar(stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(resta_rt1se07$ymin, resta_rt1se08$ymin, resta_rt1se09$ymin, resta_rt1se93$ymin, resta_rt1se95$ymin), ymax = c(resta_rt1se07$ymax, resta_rt1se08$ymax, resta_rt1se09$ymax, resta_rt1se93$ymax, resta_rt1se95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Response times (type 1 task): 3 alternatives - 2 alternatives')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Response time (ms)')+
  ylim(c(-500, 500))

########################################################################################################
## Subtraction (type 2 task): response times with 3 alternatives - response times with 2 alternatives ##
########################################################################################################

resta_rt2 <- function(data, area2prop){
  # this function receives the data and size proportions of the second figure (task difficulty)
  # and returns the subtraction between mean response times with 3 alternatives and mean response times with 2 
  # alternatives for that level of difficulty. (confidence report)
  
  prop_alt3  <- data[data$StimVal == area2prop & data$Nalternativas == 3,]
  type2_rt3 <- mean(prop_alt3$RT_Confidence)
  
  prop_alt2  <- data[data$StimVal == area2prop & data$Nalternativas == 2,]
  type2_rt2 <- mean(prop_alt2$RT_Confidence)
  
  resta <- type2_rt3 - type2_rt2
  
  return(resta)}

restart2_prop07 <- c()
restart2_prop08 <- c()
restart2_prop09 <- c()
restart2_prop93 <- c()
restart2_prop95 <- c()
for(i in 1:max(data$Nsujeto)){
  sujeto <- data[data$Nsujeto == i,]
  restart2_prop07 <- c(restart2_prop07, resta_rt2(sujeto,0.7))
  restart2_prop08 <- c(restart2_prop08, resta_rt2(sujeto,0.8))
  restart2_prop09 <- c(restart2_prop09, resta_rt2(sujeto,0.9))
  restart2_prop93 <- c(restart2_prop93, resta_rt2(sujeto,0.93))
  restart2_prop95 <- c(restart2_prop95, resta_rt2(sujeto,0.95))
}

resta_rt2se07 <- mean_se(restart2_prop07)
resta_rt2se08 <- mean_se(restart2_prop08)
resta_rt2se09 <- mean_se(restart2_prop09)
resta_rt2se93 <- mean_se(restart2_prop93)
resta_rt2se95 <- mean_se(restart2_prop95)

restart2_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                                values = c(mean(restart2_prop07), mean(restart2_prop08), mean(restart2_prop09), mean(restart2_prop93), mean(restart2_prop95)))

# plot

ggplot(restart2_sujetos, aes(x=names, y = values))+
  geom_bar(stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(resta_rt2se07$ymin, resta_rt2se08$ymin, resta_rt2se09$ymin, resta_rt2se93$ymin, resta_rt2se95$ymin), ymax = c(resta_rt2se07$ymax, resta_rt2se08$ymax, resta_rt2se09$ymax, resta_rt2se93$ymax, resta_rt2se95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Response times (type 2 task): 3 alternatives - 2 alternatives')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Response time (ms)')+
  ylim(c(-500, 500))

