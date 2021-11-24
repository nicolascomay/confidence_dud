sim.fitted.model <- function(n, fittedpars, model, i){
  source('sim_fitted_model.R')
  source('sum_squared_error.R')
  
  cat('Simulating data with those parameters...', '\n')
    
  # fixed parameters from experimental design
  a1 <- 1
  a2 <- c(0.7,0.8,0.9,0.93,0.95)
  a3 <- 0.3
  
  # data
  data <- as.matrix(read.csv2('exp1datatofit.csv', header=F))
  data <- apply(data,2,as.numeric)

  # simulate data with fitted sigma and alpha
  sims <- rep(NA,10)
  temp <- simulate.model(n,a1,a2,a3,s=fittedpars[1],
                           alpha=fittedpars[2],model=model)
  sims <- rescale(temp, to=c(min(data[i,11:20]), max(data[i,11:20])))
  
  cat('Done!', '\n')

  return(sims)

}
