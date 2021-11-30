simulate.exp <- function(a1, a2, a3, s, alpha, min_d=0, max_d=1, model, data){
  # function that simulates empirical data according to a model.
  
  # n = number of trials to simulate for each condition (2 or 3 alt)
  # a1, a2, a3 = size of the alternatives
  # s = sensory noise
  # alpha = decision noise
  
  if(model == 'diff'){
    source('diff_model.R')
  }
  else if(model == 'contrast'){
    source('contrast_model.R')
  }
  else if(model == 'average_residual'){
    source('average_residual_model.R')
  }
  else{
    source('max_model.R')
  }
  
  c.sim <- c()
  for(i in 1:5){
    pred <- matrix(NA,100,length(data[[i]]))
    for(j in 1:length(data[[i]])){
      for(k in 1:100){
        pred[k,j]  <- simulate_trial(A1=a1,A2=a2[i],s=s,a=alpha)[2]
      }
    }
    pred <- colMeans(pred)
    c.sim <- c(c.sim, list(round(pred,2)))
  }
  for(i in 6:10){
    pred <- matrix(NA,100,length(data[[i]]))
    for(j in 1:length(data[[i]])){
      for(k in 1:100){
        if(i == 6)
          pred[k,j] <- simulate_trial(A1=a1,A2=a2[1],A3=a3,s=s,a=alpha)[2]
        if(i == 7)
          pred[k,j] <- simulate_trial(A1=a1,A2=a2[2],A3=a3,s=s,a=alpha)[2]
        if(i == 8)
          pred[k,j] <- simulate_trial(A1=a1,A2=a2[3],A3=a3,s=s,a=alpha)[2]
        if(i == 9)
          pred[k,j] <- simulate_trial(A1=a1,A2=a2[4],A3=a3,s=s,a=alpha)[2]
        if(i == 10)
          pred[k,j] <- simulate_trial(A1=a1,A2=a2[5],A3=a3,s=s,a=alpha)[2]
      }
    }
    pred <- colMeans(pred)
    c.sim <- c(c.sim, list(round(pred,2)))
  }
  

  
  return(c.sim)
}

