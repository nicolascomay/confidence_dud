library(tidyverse)

# -------------------------------------------------------------------------------------------------

data <- read.csv2('data_experiment3.csv')
data$Correct = as.logical(data$Correct)
data <- as_tibble(data)

# ----------------------------------------------------------------------------------------------------


####---- rt (type 1 task) by difficulty ----####

data %>%
  group_by(Nsujeto, distance_ratio) %>%  
  summarize(rt_type1 = mean(RT_type1)) %>% 
  ggplot(aes(x=as.factor(distance_ratio),
             y=rt_type1))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  xlab('Likelihood ratio')+
  ylab('Response time (type 1 task)')+
  ggtitle('Response times (type 1 task) by difficulty')+
  theme_classic()


####---- rt (type 1 task) by amount of alternatives ----####

data %>%
  group_by(Nsujeto, Nalternativas) %>%
  summarize(rt_type1=mean(RT_type1)) %>%
  ggplot(aes(x=as.factor(Nalternativas),
             y=rt_type1))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  xlab('Number of alternatives')+
  ylab('Response time')+
  ggtitle('Response times (type 1 task) by the number alternatives')+
  theme_classic()

####---- type 1 rt 3alternatives - 2alternatives ----####

levels <- sort(unique(data$distance_ratio), decreasing = T)
nsubj  <- max(data$Nsujeto)
rt1_data <- matrix(NA, nsubj, 3) # 3 levels of difficulty

for(i in 1:nsubj){
  subj <- data[data$Nsujeto==i,]
  temp <- c()
  
  
  for(k in levels){
    indx3 <- which(subj$distance_ratio==k & subj$Nalternativas==3)
    indx2 <-  which(subj$distance_ratio==k & subj$Nalternativas==2)
    temp <- c(temp, mean(subj[indx3,]$RT_type1)-mean(subj[indx2,]$RT_type1))
  }
  
  
  rt1_data[i,] <- temp
  
}

# standard error of mean 
level1 <- mean_se(rt1_data[,1])
level2 <- mean_se(rt1_data[,2])
level3 <- mean_se(rt1_data[,3])

# plot
graphdata <- data.frame(names = levels, 
                        values = colMeans(rt1_data))

graphdata %>%
  ggplot(aes(x=as.factor(levels),y=values))+
  geom_col(fill='darkgreen')+
  geom_errorbar(aes(x=as.factor(levels), ymin=c(level1$ymin, level2$ymin, level3$ymin),
                    ymax=c(level1$ymax,level2$ymax,level3$ymax)), width=0.4, 
                size=1.3, color='orange')+
  ggtitle('Response times - type 1 task (3alternatives-2alternatives)')+
  ylab('RT 3alt-2alt')+
  xlab('Likelihood ratio')+
  theme_classic()

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------


####---- rt (type 2 task) by difficulty ----####

data %>%
  group_by(Nsujeto, distance_ratio) %>%  
  summarize(rt_type2 = mean(RT_Confidence)) %>% 
  ggplot(aes(x=as.factor(distance_ratio),
             y=rt_type2))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  xlab('Likelihood ratio')+
  ylab('Response time (type 2 task)')+
  ggtitle('Response times (type 2 task) by difficulty')+
  theme_classic()


####---- rt (type 2 task) by amount of alternatives ----####

data %>%
  group_by(Nsujeto, Nalternativas) %>%
  summarize(rt_type2=mean(RT_Confidence)) %>%
  ggplot(aes(x=as.factor(Nalternativas),
             y=rt_type2))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  xlab('Number of alternatives')+
  ylab('Response time')+
  ggtitle('Response times (type 2 task) by the number alternatives')+
  theme_classic()

####---- type 2 rt 3alternatives - 2alternatives ----####

rt2_data <- matrix(NA, nsubj, 3) # 3 levels of difficulty

for(i in 1:nsubj){
  subj <- data[data$Nsujeto==i,]
  temp <- c()
  
  
  for(k in levels){
    indx3 <- which(subj$distance_ratio==k & subj$Nalternativas==3)
    indx2 <-  which(subj$distance_ratio==k & subj$Nalternativas==2)
    temp <- c(temp, mean(subj[indx3,]$RT_Confidence)-mean(subj[indx2,]$RT_Confidence))
  }
  
  
  rt2_data[i,] <- temp
  
}

# standard error of mean 
level1 <- mean_se(rt2_data[,1])
level2 <- mean_se(rt2_data[,2])
level3 <- mean_se(rt2_data[,3])

# plot
graphdata <- data.frame(names = levels, 
                        values = colMeans(rt2_data))

graphdata %>%
  ggplot(aes(x=as.factor(levels),y=values))+
  geom_col(fill='darkgreen')+
  geom_errorbar(aes(x=as.factor(levels), ymin=c(level1$ymin, level2$ymin, level3$ymin),
                    ymax=c(level1$ymax,level2$ymax,level3$ymax)), width=0.4, 
                size=1.3, color='orange')+
  ggtitle('Response times - type 2 task (3alternatives-2alternatives)')+
  ylab('RT 3alt-2alt')+
  xlab('Likelihood ratio')+
  theme_classic()
