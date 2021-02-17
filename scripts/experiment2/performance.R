library(MASS)
library(lestat)
library(ggplot2)
library(readxl)

# -------------------------------------------------------------------------------------------------

data <- read_xlsx('yourpath/data_experiment2.xlsx')
data$Correct = as.logical(data$Correct)

# ----------------------------------------------------------------------------------------------------
###################################################
## performance as a function of 2nd figure sizes ##
###################################################

data_2alt <- data[data$Nalternativas ==2,] # 2 alternatives only
data_3alt <- data[data$Nalternativas ==3,] # 3 alternatives only
data_4alt <- data[data$Nalternativas ==4,] # 4 alternatives only
data_5alt <- data[data$Nalternativas ==5,] # 5 alternatives only

perf_prop <- function(data){
  # this function returns a vector with proportion of correct responses for each level of difficulty
  
  listaprop <- sort(unique(data$StimVal))   
  perfprop  <- rep(NA, length(listaprop))
  
  for(i in seq_along(listaprop)){
    select_prop <- data[data$StimVal == listaprop[i],]
    perfprop[i] <- mean(select_prop$Correct)
  }
  return(perfprop) 
}

sujetos_perf_prop07 <- c()
sujetos_perf_prop08 <- c()
sujetos_perf_prop09 <- c()
sujetos_perf_prop93 <- c()
sujetos_perf_prop95 <- c()
for(i in 1:max(data$Nsujeto)){
  sujeto <- data[data$Nsujeto == i,] # changing here and in the loop you can see data for 2, 3, 4 or 5 alternatives separately
  sujetos_perf_prop07 <- c(sujetos_perf_prop07, perf_prop(sujeto)[1])
  sujetos_perf_prop08 <- c(sujetos_perf_prop08, perf_prop(sujeto)[2])
  sujetos_perf_prop09 <- c(sujetos_perf_prop09, perf_prop(sujeto)[3])
  sujetos_perf_prop93 <- c(sujetos_perf_prop93, perf_prop(sujeto)[4])
  sujetos_perf_prop95 <- c(sujetos_perf_prop95, perf_prop(sujeto)[5])
}

se07 <- mean_se(sujetos_perf_prop07)
se08 <- mean_se(sujetos_perf_prop08)
se09 <- mean_se(sujetos_perf_prop09)
se93 <- mean_se(sujetos_perf_prop93)
se95 <- mean_se(sujetos_perf_prop95)

performance_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                                   values = c(mean(sujetos_perf_prop07), mean(sujetos_perf_prop08), mean(sujetos_perf_prop09), mean(sujetos_perf_prop93), mean(sujetos_perf_prop95)))

# plot

ggplot(performance_sujetos, aes(x=names, y = values))+
  geom_bar(stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(se07$ymin, se08$ymin, se09$ymin, se93$ymin, se95$ymin), ymax = c(se07$ymax,se08$ymax,se09$ymax, se93$ymax,se95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Performance as a function of task difficulty')+
  xlab('Task difficutly (stimulus2/stimulus1)')+
  ylab('Proportion of correct trials')

# --------------------------------------------------------------


###########################################################
## performance dissociated by the number of alternatives ##
###########################################################

perfNalt <- function(data, alternativas){
  # the function receives as a second parameter the number of alternatives (2, 3, 4 or 5) and returns
  # the proportion of correct responses with that amount of alternatives
  
  cantsuj   <- max(data$Nsujeto)
  correct   <- rep(NA, cantsuj)
  dataNalt  <- data[data$Nalternativas == alternativas,] 
  
  for(i in 1:cantsuj){
    sujeto <- dataNalt[dataNalt$Nsujeto == i,]
    correct[i] <- mean(sujeto$Correct)
  }
  return(correct)
}

perf_n2 <- mean_se(perfNalt(data,2))
perf_n3 <- mean_se(perfNalt(data,3))
perf_n4 <- mean_se(perfNalt(data,4))
perf_n5 <- mean_se(perfNalt(data,5))

performanceN <- data.frame( names = c('2', '3', '4', '5'),
                            values = c(mean(perfNalt(data,2)), mean(perfNalt(data,3)),
                                       mean(perfNalt(data,4)), mean(perfNalt(data,5))))
# plot

ggplot(performanceN)+
  geom_bar(aes(x=names, y=values), stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(perf_n2$ymin, perf_n3$ymin, perf_n4$ymin, perf_n5$ymin), ymax = c(perf_n2$ymax, perf_n3$ymax, perf_n4$ymax, perf_n5$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Performance according to the number of alternatives')+
  xlab('Alternatives')+
  ylab('Performance')+
  ylim(c(0,1))

# -----------------------------------------------------------------------------------------

####################################################################################
## Subtraction: performance with 3 alternatives - performance with 2 alternatives ##
####################################################################################

resta_perf <- function(data, area2prop){
  # this function receives the data and size proportions of the second figure (task difficulty)
  # and returns the subtraction between performance with N alternatives and performance with 2 
  # alternatives for that level of difficulty.
  
  prop_alt3  <- data[data$StimVal == area2prop & data$Nalternativas == 5,] # here you can modify the number in order to analize performance with 3, 4 or 5 alternatives
  correct3   <- mean(prop_alt3$Correct)
  
  prop_alt2  <- data[data$StimVal == area2prop & data$Nalternativas == 2,]
  correct2   <- mean(prop_alt2$Correct)
  
  resta <- correct3 - correct2
  
  return(resta)}

resta_perf_prop07 <- c()
resta_perf_prop08 <- c()
resta_perf_prop09 <- c()
resta_perf_prop93 <- c()
resta_perf_prop95 <- c()
for(i in 1:max(data$Nsujeto)){
  sujeto <- data[data$Nsujeto == i,]
  resta_perf_prop07 <- c(resta_perf_prop07, resta_perf(sujeto, 0.7))
  resta_perf_prop08 <- c(resta_perf_prop08, resta_perf(sujeto,0.8))
  resta_perf_prop09 <- c(resta_perf_prop09, resta_perf(sujeto,0.9))
  resta_perf_prop93 <- c(resta_perf_prop93, resta_perf(sujeto,0.93))
  resta_perf_prop95 <- c(resta_perf_prop95, resta_perf(sujeto,0.95))
}

restase07 <- mean_se(resta_perf_prop07)
restase08 <- mean_se(resta_perf_prop08)
restase09 <- mean_se(resta_perf_prop09)
restase93 <- mean_se(resta_perf_prop93)
restase95 <- mean_se(resta_perf_prop95)

resta_perf_sujetos <- data.frame( names = c('0.7','0.8', '0.9','0.93','0.95'),
                                  values = c(mean(resta_perf_prop07), mean(resta_perf_prop08), mean(resta_perf_prop09), mean(resta_perf_prop93), mean(resta_perf_prop95)))

# plot

ggplot(resta_perf_sujetos, aes(x=names, y = values))+
  geom_bar(stat = 'identity', fill = rgb(0,0.77,0))+
  geom_errorbar(aes(x=names, ymin = c(restase07$ymin, restase08$ymin, restase09$ymin, restase93$ymin, restase95$ymin), ymax = c(restase07$ymax, restase08$ymax, restase09$ymax, restase93$ymax, restase95$ymax)), width = 0.4, col = 'orange', size = 1.3)+
  ggtitle('Performance (N alternatives - 2 alternatives)')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Performance (N - 2 alternatives)')+
  ylim(c(-0.1,0.1))