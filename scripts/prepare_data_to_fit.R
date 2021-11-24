prepare.data <- function(subjdata){
  
  c2 <- c()
  c3 <- c()
  for(j in diflevels){
    c2 <- c(c2, list(round(subjdata[subjdata$StimVal == j & subjdata$Nalternativas==2,]$Confidence,2)))
    c3 <- c(c3, list(round(subjdata[subjdata$StimVal == j & subjdata$Nalternativas==3,]$Confidence,2)))
  }
  
  c <- c(c2, c3)
  
  return(c)
  
}