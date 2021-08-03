library(tidyverse)

# -------------------------------------------------------------------------------------------------

data <- read.csv2('data_experiment2.csv')
data <- as_tibble(data)
data$Correct = as.logical(data$Correct)

# ----------------------------------------------------------------------------------------------------

####---- response times (type 1 task) by difficulty ----####

data %>%
  group_by(Nsujeto, StimVal) %>%  
  summarize(rt_type1 = mean(RT_type1)) %>% 
  ggplot(aes(x=as.factor(StimVal),
             y=rt_type1))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('RT (type 1)')+
  ggtitle('Response times (type 1 task) by difficulty')+
  theme_classic()

####---- rt (type 1) N alternatives - 2 alternatives ----####

levels <- sort(unique(data$StimVal))
nsubj  <- max(data$Nsujeto)
rt1_data <- matrix(NA, nsubj, 5) # 5 levels of difficulty

for(i in 1:nsubj){
  subj <- data[data$Nsujeto==i,]
  temp <- c()
  
  
  for(j in levels){
    indx3 <- which(subj$StimVal==j & subj$Nalternativas==3) # change here in order to see the subtraction between Nalt and 2alt
    indx2 <-  which(subj$StimVal==j & subj$Nalternativas==2)
    temp <- c(temp, mean(subj[indx3,]$RT_type1)-mean(subj[indx2,]$RT_type1))
  }
  
  rt1_data[i,] <- temp
  
}

# standard error of mean 
level1 <- mean_se(rt1_data[,1])
level2 <- mean_se(rt1_data[,2])
level3 <- mean_se(rt1_data[,3])
level4 <- mean_se(rt1_data[,4])
level5 <- mean_se(rt1_data[,5])

# plot
graphdata <- data.frame(names = levels, 
                        values = colMeans(rt1_data))

graphdata %>%
  ggplot(aes(x=as.factor(levels),y=values))+
  geom_col(fill='darkgreen')+
  geom_errorbar(aes(x=as.factor(levels), ymin=c(level1$ymin, level2$ymin, level3$ymin,
                                                level4$ymin, level5$ymin), 
                    ymax=c(level1$ymax,level2$ymax,level3$ymax, level4$ymax, level5$ymax)),
                width=0.4, size=1.3, color='orange')+
  ggtitle('Response times (type 1 task) Nalternatives-2alternatives')+
  ylab('RT Nalt-2alt')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  theme_classic()

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

####---- response times (type 2 task) by difficulty ----####

data %>%
  group_by(Nsujeto, StimVal) %>%  
  summarize(rt_conf = mean(RT_Confidence)) %>% 
  ggplot(aes(x=as.factor(StimVal),
             y=rt_conf))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('RT (type 2)')+
  ggtitle('Response times (type 2 task) by difficulty')+
  theme_classic()

####---- rt (type 2) 3 alternatives - 2 alternatives ----####

levels <- sort(unique(data$StimVal))
nsubj  <- max(data$Nsujeto)
rt2_data <- matrix(NA, nsubj, 5) # 5 levels of difficulty

for(i in 1:nsubj){
  subj <- data[data$Nsujeto==i,]
  temp <- c()
  
  
  for(j in levels){
    indx3 <- which(subj$StimVal==j & subj$Nalternativas==3) # change here in order to see the subtraction between Nalt and 2alt
    indx2 <-  which(subj$StimVal==j & subj$Nalternativas==2)
    temp <- c(temp, mean(subj[indx3,]$RT_Confidence)-mean(subj[indx2,]$RT_Confidence))
  }
  
  rt2_data[i,] <- temp
  
}

# standard error of mean 
level1 <- mean_se(rt2_data[,1])
level2 <- mean_se(rt2_data[,2])
level3 <- mean_se(rt2_data[,3])
level4 <- mean_se(rt2_data[,4])
level5 <- mean_se(rt2_data[,5])

# plot
graphdata <- data.frame(names = levels, 
                        values = colMeans(rt2_data))

graphdata %>%
  ggplot(aes(x=as.factor(levels),y=values))+
  geom_col(fill='darkgreen')+
  geom_errorbar(aes(x=as.factor(levels), ymin=c(level1$ymin, level2$ymin, level3$ymin,
                                                level4$ymin, level5$ymin), 
                    ymax=c(level1$ymax,level2$ymax,level3$ymax, level4$ymax, level5$ymax)),
                width=0.4, size=1.3, color='orange')+
  ggtitle('Response times (type 2 task) Nalternatives-2alternatives')+
  ylab('RT Nalt-2alt')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  theme_classic()
