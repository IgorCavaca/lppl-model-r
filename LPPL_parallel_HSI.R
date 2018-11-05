#import packages
require(foreach)
require(doParallel)
require(lubridate)
require(quantmod)
require(minpack.lm)
require(plyr)
require(gplots)

# Attemptimg a 2007 crash prediction in the HSI = Hang Seng Index
startDate = as.Date("2004-05-01")
endDate = as.Date("2007-10-21")

getSymbols("^HSI", src="yahoo", from=startDate, to=endDate+21)
plot(HSI)

guess <- list(A=25000, B=-100, C=0.5, a=0.33, tc=length(GSPC[,6])+150, w=16.36, phi=0.1)
cm <- nls.lm.control(maxiter=250, maxfev=1600)
ubnd <- c(50000,0,5,1,length(GSPC[,6])+750,Inf,Inf)




# mestimate <- matrix(,nrow=50,ncol=50) # 1000 takes about 10 minutes

#setup parallel backend to use 8 processors
cl<-makeCluster(6)
registerDoParallel(cl)

#start time
timer0 <- proc.time()

fitter <- function(i,j,GSPC,ubnd,pars,cm) {
  
  d1 = length(GSPC[,6])-j
  d2 = d1-i
  
  t <- tail(head(1:length(GSPC[,6]),d1),d2)
  y <- tail(head(GSPC[,6],d1),d2) # adjusted close price
  
  lbnd <- c(0,-5000,0,0,d1+1,-Inf,-Inf)
  
  x <- tryCatch({ 
    
    mod <- nlsLM(y ~ A + B*((tc-t)^a)*(1+C*cos(w*log(tc-t)+phi)), start=pars, control=cm, 
                 lower=lbnd, upper=ubnd, model=TRUE)
    
  }, warning = function(war) {
    print(paste("warning: ", war))
  }, error = function(err) {
    print(paste("error: ", err))
  }, finally = {  
  })
}

#loop
model <- list()
models <- rep(list(list()),250)

models<-foreach( j = tail(seq(0,250,1),250), .packages="minpack.lm") %dopar% {
  
  pars <- guess
  model<-list()
  for (i in tail(seq(0,250,1),250) ) {
    
    temp <- fitter(i,j,GSPC,ubnd,pars,cm)
    pars <- tryCatch({ 
      
      x <- temp$m$getPars()
      model[[i]] = x[[5]]
      x
      
    }, warning = function(war) {
      print(paste("warning: ", war))
    }, error = function(err) {
      print(paste("error: ", err))
    }, finally = {  
    })
    
  }
  to.models<-model
  to.models
  
}

proc.time()-timer0
stopCluster(cl)

# turn results into a matrix
e2p <- do.call(cbind,models) # applies rbind to all lists within
dims <- dim(e2p)
e2p <- as.numeric(e2p)
dim(e2p) <- dims

e2p <- e2p - length(GSPC[,6])
e2p[is.na(e2p)] <- 0
plot(density(e2p))
tc_pred <- d$x[which.max(d$y)] # peak predictor
crashDate1 <- endDate + tc_pred*7/5
crashDate1

e3 <- e2p
e3[e3<0 | e3>250] <- 0
plot(density(e3[e3>0]))
tc_pred <- d$x[which.max(d$y)] # peak predictor
crashDate2 <- endDate + tc_pred*7/5
crashDate2

heatmap.2(e2p,dendrogram="none", Rowv=NULL, Colv=NULL, trace="none")
