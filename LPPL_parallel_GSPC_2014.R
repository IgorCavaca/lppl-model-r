#import packages
require(foreach)
require(doParallel)
require(lubridate)
require(quantmod)
require(minpack.lm)
require(plyr)
require(gplots)
require(fPortfolio)

# clear workspace
rm(list=ls())
# dev.off()

# 2014 crash prediction
startDate = as.Date("2012-01-30")
endDate = as.Date("2014-10-01")

getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
chartSeries(GSPC, theme='white')

guess <- list(A=2000, B=-100, C=0.5, a=0.33, tc=length(GSPC[,6])+150, w=16.36, phi=0.1)
cm <- nls.lm.control(maxiter=250, maxfev=1600)
ubnd <- c(5000,0,5,1,length(GSPC[,6])+750,Inf,Inf)



#setup parallel backend to use N processors
cl<-makeCluster(2)
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
e2 <- do.call(cbind,models) # applies rbind to all lists within
dims <- dim(e2)
e2 <- as.numeric(e2)
dim(e2) <- dims

# examine all dates
e2 <- e2 - length(GSPC[,6])
e2[is.na(e2)] <- 0
d2 <- density(e2,bw=2)
plot(d2)
tc_pred <- d2$x[which.max(d2$y)] # peak predictor
crashDate1 <- endDate + tc_pred*7/5
crashDate1

# examine all FUTURE dates
e3 <- e2
e3[e3<0 | e3>250] <- 0
d3 <- density(e3[e3>0],bw=1)
plot(d3)
tc_pred <- d3$x[which.max(d3$y)] # peak predictor
crashDate2 <- endDate + tc_pred*7/5
crashDate2

# examine all FUTURE dates with stability offset
e4 <- e3[2,]
d4 <- density(e4[e4>0],bw=1)
plot(d4)
tc_pred <- d4$x[which.max(d4$y)] # peak predictor
crashDate3 <- endDate + tc_pred*7/5
crashDate3


heatmap.2(e2,dendrogram="none", Rowv=NULL, Colv=NULL, trace="none")
plot(e2[1,])

heatmap.2(e3,dendrogram="none", Rowv=NULL, Colv=NULL, trace="none")
plot(e3[1,])
plot(e3[2,])
plot(e3[3,])


write.table(e2,"./parallel_250x250_20141231.csv",sep=",")