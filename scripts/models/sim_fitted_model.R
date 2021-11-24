simulate.model <- function(n,a1,a2,a3,s,alpha,model){
  
  modelpath <- gsub(" ","", paste(model, '_model.R'))
  source(modelpath)
  
  conf2 <- matrix(NA,n,5)
  for(i in 1:n){
    for(j in 1:length(a2)){
      conf2[i,j] <- simulate_trial(s,a1,a2[j],a=alpha)[2]
    }
  }
  conf3 <- matrix(NA,n,5)
  for(i in 1:n){
    for(j in 1:length(a2)){
      conf3[i,j] <- simulate_trial(s,a1,a2[j],A3=a3,a=alpha)[2]
    }
  }
  
  conf <- cbind(conf2,conf3)
  conf <- colMeans(conf)
  
  return(conf)
}
