#EM algorithm
truemean1 = -5
truemean2 = 1
truesd1 = 2
truesd2 = 2.5

set.seed(200)
sample = c(rnorm(n = 200, mean = truemean1, sd = truesd1), rnorm(n = 30,mean = truemean2, sd = truesd2))


mean1 = 0
mean2 = 1
sd1 = 1
sd2 = 1
weight = 0.5

plot(sample,rep(0,length(sample)),ylim = c(0,1))
lines(seq(-6,6,0.01),dnorm(seq(-6,6,0.01),mean1,sd1))
lines(seq(-6,6,0.01),dnorm(seq(-6,6,0.01),mean2,sd2))



E = function(X, mean1, mean2, sd1, sd2,w) {
  w1 = weight * dnorm(X,mean1,sd1)/(weight*dnorm(X,mean1,sd1) + (1-weight)*dnorm(X,mean2,sd2))
  w2 = 1 - w1
  return(cbind(w1,w2))
}

M = function(X, w1, w2){
  mean1 = sum(X * w1)/sum(w1)
  mean2 = sum(X * w2)/sum(w2)
  sd1 = sqrt(sum((X - mean1)^2 * w1)/sum(w1))
  sd2 = sqrt(sum((X - mean2)^2 * w2)/sum(w2))
  weight = sum(w1)/(sum(w1)+sum(w2))
  return(c(mean1,mean2,sd1,sd2,weight))
}

for (i in 1:10000) {
w = E(sample,mean1, mean2, sd1, sd2,weight)
p = M(sample,w[,1],w[,2])
mean1 = p[1]
mean2 = p[2]
sd1 = p[3]
sd2 = p[4]
weight = p[5]
Sys.sleep(0.1)
#Sys.sleep(time = 0.1)
if (i%%1 == 0){
  print(i)
  print(p)
  plot(sample,rep(0,length(sample)),ylim = c(0,1))
  lines(seq(-10,20,0.01),dnorm(seq(-10,20,0.01),mean1,sd1))
  lines(seq(-10,20,0.01),dnorm(seq(-10,20,0.01),mean2,sd2))}
}
