source('sum_squared_error.R')

fit.model <- function(expdata, model, subj, saving=F){
  
  fitted.pars <- rep(NA,3)
  
  j <- 1
  while(j < 2){
    error <- NULL
    error <- tryCatch({
      cat(c('Fitting sigma and alpha for subject n', subj, 'with', model, 'model', '...', '\n'))
      
      estimation1 <- optim(c(0.01,13), fn=computeSumSquaredError, data=expdata, model=model, method='L-BFGS-B',
                           lower=c(0.001, 1), upper=c(2,50))
      cat(c('Estimation n1 found a sigma of', estimation1$par[1], 
            'and an alpha of', estimation1$par[2], '\n'))
      
      estimation2 <- optim(c(0.3,12), fn=computeSumSquaredError, data=expdata, model=model, method='L-BFGS-B',
                           lower=c(0.001, 1), upper=c(2,50))
      cat(c('Estimation n2 found a sigma of', estimation2$par[1],
            'and an alpha of', estimation2$par[2], '\n'))
      
      estimation3 <- optim(c(0.5,17), fn=computeSumSquaredError, data=expdata, model=model, method='L-BFGS-B',
                           lower=c(0.001, 1), upper=c(2,50))
      cat(c('Estimation n3 found a sigma of', estimation3$par[1],
            'and an alpha of', estimation3$par[2], '\n'))
      
      
      best.estimation <- min(c(estimation1$value, estimation2$value, estimation3$value))
      if(best.estimation==estimation1$value){
        estimation <- estimation1
      }
      if(best.estimation==estimation2$value){
        estimation <- estimation2
      }
      if(best.estimation==estimation3$value){
        estimation <- estimation3
      }
      
      
      fitted.pars[1] <- estimation$par[1]
      fitted.pars[2] <- estimation$par[2]
      cat(c('Best values:', '\n'))
      cat(c('Sigma =',fitted.pars[1], '\n')) 
      cat(c('Alpha =',fitted.pars[2], '\n'))
      fitted.pars[3] <- estimation$value
      cat(c('SSE for this parameters =', estimation$value), '\n')},
      
      error=function(e){
        skip <- T
        cat(paste(c('Error:', e, '\n')))
        cat(c('Error fitting subject n', subj, 'Trying again...', '\n'))
        return(skip)
      })
    if(!is.null(error)){
      
    } else{
      j <- 2
      if(saving){
        name <- gsub(" ", "", paste('subject', subj, model, '.Rda'))
        temp <- fitted.pars
        save(temp, file=name)
      }
    }
  }
  return(fitted.pars)
}
