MSE <- function(sims){
  
  # data
  data <- as.matrix(read.csv2('exp1datatofit.csv', header=F))
  data <- apply(data,2,as.numeric)
  data <- data[,11:20] # only conf data
  
  # get MSE
  e <- sum((data-sims)^2)/length(sims)
  
  return(e)
}