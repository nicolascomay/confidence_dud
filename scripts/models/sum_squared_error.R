source('simulate_data.R')

computeSumSquaredError <- function(parameters, data, model){
  # function that computes the sum squared error between
  # model's predictions and data
  a1 <- 1
  a2 <- c(0.7, 0.8, 0.9, 0.93, 0.95)
  a3 <- 0.3
 
  model.prediction <- simulate.exp(a1,a2,a3,s=parameters[1],
                                   alpha=parameters[2],model=model,data=data)
  
  e <- c()
  for(i in 1:10){
    e <- c(e, data[[i]] - model.prediction[[i]])
    sse <- sum(e^2)
  }    
  
  return(sse)
  
}
