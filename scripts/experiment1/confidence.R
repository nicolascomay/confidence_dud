library(MASS)
library(lestat)
library(ggplot2)
library(readxl)

# -------------------------------------------------------------------------------------------------

data <- read_xlsx('yourpath/data_experiment1.xlsx')
data$Correct = as.logical(data$Correct)

# ----------------------------------------------------------------------------------------------------

##################################################
## confidence as a function of 2nd figure sizes ##
##################################################

data_2alt <- data[data$Nalternativas ==2,] # 2 alternatives only
data_3alt <- data[data$Nalternativas ==3,] # 3 alternatives only

conf_prop <- function(data){
  # this function returns a vector with mean confidence for each level of difficulty
  
  listaprop <- sort(unique(data$StimVal))  
  confprop  <- rep(NA, length(listaprop))
  
  for(i in seq_along(listaprop)){
    select_prop <- data[data$StimVal == listaprop[i],]
    confprop[i] <- mean(select_prop$Confidence)
  }
  return(confprop) 
}

sujetos_conf_prop07 <- c()
sujetos_conf_prop08 <- c()
sujetos_conf_prop09 <- c()
sujetos_conf_prop93 <- c()
sujetos_conf_prop95 <- c()
for(i in 1:max(data$Nsujeto)){    
  sujeto <- data[data$Nsujeto == i,] # changing here and in the loop you can see data for 2 or 3 alternatives separately
  sujetos_conf_prop07 <- c(sujetos_conf_prop07, conf_prop(sujeto)[1])
  sujetos_conf_prop08 <- c(sujetos_conf_prop08, conf_prop(sujeto)[2])
  sujetos_conf_prop09 <- c(sujetos_conf_prop09, conf_prop(sujeto)[3])
  sujetos_conf_prop93 <- c(sujetos_conf_prop93, conf_prop(sujeto)[4])
  sujetos_conf_prop95 <- c(sujetos_conf_prop95, conf_prop(sujeto)[5])
}

confse07 <- mean_se(sujetos_conf_prop07)
confse08 <- mean_se(sujetos_conf_prop08)
confse09 <- mean_se(sujetos_conf_prop09)
confse93 <- mean_se(sujetos_conf_prop93)
confse95 <- mean_se(sujetos_conf_prop95)

conf_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                            values = c(mean(sujetos_conf_prop07), mean(sujetos_conf_prop08), mean(sujetos_conf_prop09), mean(sujetos_conf_prop93), mean(sujetos_conf_prop95)))

# plot

ggplot(conf_sujetos)+
  geom_bar(aes(x=names, y = values), stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(confse07$ymin, confse08$ymin, confse09$ymin, confse93$ymin, confse95$ymin), ymax = c(confse07$ymax, confse08$ymax, confse09$ymax, confse93$ymax, confse95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Confidence as a function of task difficulty')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Confidence')+
  ylim(c(0,1))

# --------------------------------------------------------------------------------------------------

#####################
## mean confidence ##
#####################

confianza_promedio <- function(data){
  # this function returns a vector with the mean confidence for each subject
  
  cantsuj <- max(data$Nsujeto) 
  confianza <- rep(NA, cantsuj)
  
  for(i in 1:cantsuj){
    sujeto <- data[data$Nsujeto == i,]
    confianza[i] <- mean(sujeto$Confidence)
  }
  
  return(confianza)
}

# ------------------------------------------------------------------------------------


######################################################
## confidence dissociated by number of alternatives ##
######################################################

confNalt <- function(data, alternativas){
  # the function receives as a second parameter the number of alternatives (2 or 3) and returns
  # the mean confidence with that amount of alternatives
  
  cantsuj   <- max(data$Nsujeto)
  confianza <- rep(NA, cantsuj)
  dataNalt  <- data[data$Nalternativas == alternativas,] 
  
  for(i in 1:cantsuj){
    sujeto <- dataNalt[dataNalt$Nsujeto == i,]
    confianza[i] <- mean(sujeto$Confidence)
  }
  return(confianza)
}

conf_n2 <- mean_se(confNalt(data,2))
conf_n3 <- mean_se(confNalt(data,3))

confidenceN <- data.frame( names = c('2', '3'),
                           values = c(mean(confNalt(data,2)), mean(confNalt(data,3))))


# plot

ggplot(confidenceN)+
  geom_bar(aes(x=names, y=values), stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(conf_n2$ymin, conf_n3$ymin), ymax = c(conf_n2$ymax, conf_n3$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Confidence according to the number of alternatives')+
  xlab('Alternatives')+
  ylab('Confidence')+
  ylim(c(0,1))


# -------------------------------------------------------------------------------------

##################################################################################
## Subtraction: confidence with 3 alternatives - confidence with 2 alternatives ##
##################################################################################

data_correct <- data[data$Correct == TRUE,]     # correct trials
data_incorrect <- data[data$Correct == FALSE,]  # incorrect trials

resta_conf <- function(data, area2prop){
  # this function receives the data and size proportions of the second figure (task difficulty)
  # and returns the subtraction between confidence with 3 alternatives and confidence with 2 
  # alternatives for that level of difficulty.
  
  prop_alt3  <- data[data$StimVal == area2prop & data$Nalternativas == 3,]
  confianza3 <- mean(prop_alt3$Confidence)
  
  prop_alt2  <- data[data$StimVal == area2prop & data$Nalternativas == 2,]
  confianza2 <- mean(prop_alt2$Confidence)
  
  resta <- confianza3 - confianza2
  
  return(resta)}

resta_conf_prop07 <- c()
resta_conf_prop08 <- c()
resta_conf_prop09 <- c()
resta_conf_prop93 <- c()
resta_conf_prop95 <- c()
for(i in 1:max(data$Nsujeto)){ # here and inside the loop you can change to data_correct or data_incorrect to see the subtraction for correct and incorrect trials   
  sujeto <- data[data$Nsujeto == i,] 
  resta_conf_prop07 <- c(resta_conf_prop07, resta_conf(sujeto,0.7))
  resta_conf_prop08 <- c(resta_conf_prop08, resta_conf(sujeto,0.8))
  resta_conf_prop09 <- c(resta_conf_prop09, resta_conf(sujeto,0.9))
  resta_conf_prop93 <- c(resta_conf_prop93, resta_conf(sujeto,0.93))
  resta_conf_prop95 <- c(resta_conf_prop95, resta_conf(sujeto,0.95))
}

resta_conf_prop07 <- na.omit(resta_conf_prop07)
resta_conf_prop08 <- na.omit(resta_conf_prop08)
resta_conf_prop09 <- na.omit(resta_conf_prop09)
resta_conf_prop93 <- na.omit(resta_conf_prop93)
resta_conf_prop95 <- na.omit(resta_conf_prop95)

resta_confse07 <- mean_se(resta_conf_prop07)
resta_confse08 <- mean_se(resta_conf_prop08)
resta_confse09 <- mean_se(resta_conf_prop09)
resta_confse93 <- mean_se(resta_conf_prop93)
resta_confse95 <- mean_se(resta_conf_prop95)

resta_conf_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                                  values = c(mean(resta_conf_prop07), mean(resta_conf_prop08), mean(resta_conf_prop09), mean(resta_conf_prop93), mean(resta_conf_prop95)))

# plot

ggplot(resta_conf_sujetos, aes(x=names, y = values))+
  geom_bar(stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(resta_confse07$ymin, resta_confse08$ymin, resta_confse09$ymin, resta_confse93$ymin, resta_confse95$ymin), ymax = c(resta_confse07$ymax, resta_confse08$ymax, resta_confse09$ymax, resta_confse93$ymax, resta_confse95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Confidence (3 alternatives - 2 alternatives)')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Confidence')+
  ylim(c(-0.11,0.1))

# ------------------------------------------------------------------------------