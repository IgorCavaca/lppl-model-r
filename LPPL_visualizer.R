require(lubridate)
require(quantmod)
require(gplots)
require(minpack.lm)

rm(list=ls())

startDate = as.Date("2012-11-30")
endDate = as.Date("2014-10-01")

getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
plot(GSPC[,6])

# mod$model$tc
# mod$model$tc - length(GSPC[,6])


# guess <- list(A=2000, B=-10, C=0.5, a=0.35, tc=length(GSPC[,6])+150, w=15, phi=0.5*pi)
# cm <- nls.lm.control(maxiter=200, maxfev=1500)
# ubnd <- c(5000,0,5,1,length(GSPC[,6])+750,Inf,2*pi)

guess <- list(A=2000, B=-100, C=0.5, a=0.33, tc=length(GSPC[,6])+150, w=3*16.36, phi=0)
cm <- nls.lm.control(maxiter=250, maxfev=1600)
ubnd <- c(5000,0,5,1,length(GSPC[,6])+750,Inf,Inf)

getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
plot(GSPC)

timer0 <- proc.time()

my_estimate <- matrix(,nrow=30,ncol=50) # 1000 takes about 10 minutes
my_model <- data.frame()

# rows i correspond to varying start of window, i=1 max window, i=100 is shortest
# cols j correspond to varying end of window, j=1 max window, j=10 ending 2wk ago

pars <- guess
for (j in tail(seq(0,50,1),50) ) {
#   pars <- guess
  for (i in tail(seq(0,60,2),30) ) {
    
    d1 = length(GSPC[,6])-j
    d2 = d1-i
    
    t <- tail(head(1:length(GSPC[,6]),d1),d2)
    y <- tail(head(GSPC[,6],d1),d2) # adjusted close price
    
    lbnd <- c(min(GSPC[,6]),-500,0,0,d1+1,0,0)
    
    result <- tryCatch({ 
      
      mod <- nlsLM(y ~ A + B*((tc-t)^a)*(1+C*cos(w*log(tc-t)+phi)), start=pars, control=cm, 
                   lower=lbnd, upper=ubnd, model=TRUE)
      plot(t,y,type="l")
      lines(t,fitted(mod),col=2, lwd=2)
      print(summary(mod))
      
      pars <- mod$m$getPars()
      ip <- i/2
      jp <- j
      my_estimate[ip,jp] = pars[[5]]-max(t)
      print(c(i,j,pars,my_estimate[ip,jp]*7/5))
      
    }, warning = function(war) {
      print(paste("warning: ", war))
    }, error = function(err) {
      print(paste("error: ", err))
    }, finally = {
      
    })
  }
}
proc.time()-timer0



me2 <- my_estimate - length(GSPC[,6])
me2[is.na(me2)]<- -100
me2[me2<0 | me2>250] <- -100
heatmap.2(me2,dendrogram="none", Rowv=NULL, Colv=NULL, trace="none")
d2 <- density(me2, bw=2)
plot(d2)
plot(me2[1,])

tc_pred <- d2$x[which.max(d2$y)] # peak predictor
crashDate1 <- endDate + tc_pred*7/5
crashDate1
