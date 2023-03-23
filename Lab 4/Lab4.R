#n is the number of trials
#p is the probability of success in each trial
#m3 is skewness (1st column)
#m4 is kurtosis (2nd column)
m34binomial = function(n,p){
  q = 1-p
  den = n*p*q
  m3 = (q-p)/sqrt(den)
  m4 = 3 + (1-6*p*q)/den
  cbind(m3, m4)
}


#r: experiment continues until r successes 
#p is the probability of success in each trial
#m3 is skewness (1st column)
#m4 is kurtosis (2nd column)
m34negbinomial = function(r,p){
  q = 1-p
  qr=q*r
  m3 = (1+q)/sqrt(qr)
  m4 = 3 + 6/r + p^2/qr
  cbind(m3, m4)
}

#computes the distance between Binom(n,p) and Hyper(b, t-b, n) with b = t*p
#plots the two pmfs for points around the mean = n*p
#blue dots are the binomial pmf and orange dots are the hypergeometric pmf
distBinomHyper = function(n,p,t){
  x=0:n
  b = floor(t*p)
  pr1=dbinom(x,n,p)
  pr2=dhyper(x,b,t-b,n)
  D = 0.5*sum(abs(pr1-pr2))
  #define the x values around the mean plus minus 5 st. deviations of the 
  #binomial distribution
  i.u=x < n*p + 5*sqrt(n*p*(1-p))
  i.l=x > n*p - 5*sqrt(n*p*(1-p))
  i.plot=i.u & i.l
  #round the distance down to first 3 digits after decimal point
  D1=floor(1000*D)/1000
  #plot the two pmf-s for x values around the mean plus minus 5 st. dev
  plot(x[i.plot],pr2[i.plot],type="p",col="orange",xlab="x", ylab="pmf", main=paste("D = ", D1), cex.main=1,lwd=2,cex=.5,ylim=c(0,max(c(pr1,pr2))))
  lines(x[i.plot],pr1[i.plot],type="p",col="blue",lwd=2,cex=.5)
  D
}