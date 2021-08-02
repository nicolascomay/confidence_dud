library(tidyverse)

# -------------------------------------------------------------------------------------------------

data <- read.csv2('data_experiment1.csv')
data$Correct = as.logical(data$Correct)
data <- as_tibble(data)

# ----------------------------------------------------------------------------------------------------

####---- confidence by difficulty ----####

data %>%
  group_by(Nsujeto, StimVal) %>%  
  summarize(conf = mean(Confidence)) %>% 
  ggplot(aes(x=as.factor(StimVal),
             y=conf))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  ylim(c(0,1))+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  ylab('Confidence')+
  ggtitle('Confidence by difficulty')+
  theme_classic()

####---- confidence by amount of alternatives ----####

data %>%
  group_by(Nsujeto, Nalternativas) %>%
  summarize(conf=mean(Confidence)) %>%
  ggplot(aes(x=as.factor(Nalternativas),
             y=conf))+
  stat_summary(fun.data = mean_se,
               geom='bar', fill='darkgreen')+
  stat_summary(fun.data = mean_se,
               geom='errorbar', color='orange', width=0.4, size=1.3)+
  ylim(c(0,1))+
  xlab('Number of alternatives')+
  ylab('Confidence')+
  ggtitle('Confidence by the number alternatives')+
  theme_classic()

####---- performance 3 alternatives - 2 alternatives ----####

levels <- sort(unique(data$StimVal))
nsubj  <- max(data$Nsujeto)
conf_data <- matrix(NA, nsubj, 5) # 5 levels of difficulty

for(i in 1:nsubj){
  subj <- data[data$Nsujeto==i,]
  temp <- c()
  
  
  for(j in levels){
    indx3 <- which(subj$StimVal==j & subj$Nalternativas==3)
    indx2 <-  which(subj$StimVal==j & subj$Nalternativas==2)
    temp <- c(temp, mean(subj[indx3,]$Confidence)-mean(subj[indx2,]$Confidence))
  }
  
  
  conf_data[i,] <- temp
  
}

# standard error of mean 
level1 <- mean_se(conf_data[,1])
level2 <- mean_se(conf_data[,2])
level3 <- mean_se(conf_data[,3])
level4 <- mean_se(conf_data[,4])
level5 <- mean_se(conf_data[,5])

# plot
graphdata <- data.frame(names = levels, 
                        values = colMeans(conf_data))

graphdata %>%
  ggplot(aes(x=as.factor(levels),y=values))+
  geom_col(fill='darkgreen')+
  geom_errorbar(aes(x=as.factor(levels), ymin=c(level1$ymin, level2$ymin, level3$ymin,
                                                level4$ymin, level5$ymin), 
                    ymax=c(level1$ymax,level2$ymax,level3$ymax, level4$ymax, level5$ymax)),
                width=0.4, size=1.3, color='orange')+
  ylim(c(-0.1,0.1))+
  ggtitle('Confidence (3alternatives-2alternatives)')+
  ylab('Confidence 3alt-2alt')+
  xlab('Task difficulty (stimulus2/stimulus1)')+
  theme_classic()
