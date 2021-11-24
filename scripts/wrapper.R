# 1) load needed libraries and functions
library(DirichletReg)
library(scales)
source('fit.R')
source('sim_fitted_pars.R')
source('prepare_data_to_fit.R')
source('MSE.R')

# 2) load data
data <- read.csv2('data_experiment1.csv')
diflevels <- sort(unique(data$StimVal)) # difficulty levels

# 3) specify the model to fit:
# 'diff' for fitting the difference model (Li & Ma, 2020)
# 'max' for fitting the max model (Meyniel et al., 2015; Sanders et al., 2016)
# 'contrast' for fitting the contrast model (Windschitl & Chambers, 2004; Charman et al., 2011; Hanczakowski et al., 2014) )
# 'average_residual' for fitting the average residual model (Windschitl & Chambers, 2004; Charman et al., 2011; Hanczakowski et al., 2014)

model <- 'contrast'

# 4) fit accross subjects
fitted.pars <- matrix(NA,max(data$Nsujeto),3)
sims <- matrix(NA,max(data$Nsujeto),10)

for(i in 1:max(data$Nsujeto)){
  subj <- data[data$Nsujeto==i,]
  data_to_fit <- prepare.data(subj)
  fitted.pars[i,] <- fit.model(data_to_fit,model,i,saving = F) # saving = T will save each subject values 
  sims[i,] <- sim.fitted.model(n=10000,fitted.pars[i,1:2],model,i)
}

# 5) compute mean squared error
e <- MSE(sims)

# 6) plot
par(bty='l',pty='s')
plot(1:5, colMeans(sims[,1:5],na.rm=T), lwd=3, col='darkgreen', ylim=c(0.5,0.9), type='l',
     ylab='Confidence', xlab='Task difficulty', xaxt='n', main=c(model,'model'))
lines(1:5, colMeans(sims[,6:10],na.rm=T), lwd=3, col='orange')
axis(1,at=1:5,labels=c(0.7,0.8,0.9,0.93,0.95))
legend(3.5,0.82,legend=c('2alt','3alt'),col=c('darkgreen','orange'),lwd=3, bty='n')
text(4.1,0.83,labels=paste('MSE:', round(e,4)))

