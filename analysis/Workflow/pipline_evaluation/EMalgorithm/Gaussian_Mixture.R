
data = rmultinom(100,1,c(0.95,0.05,0))

mean_norm = runif(3,-5,5);mean_norm
sd_norm = 1

y = apply(data,2,function(x) {
  rnorm(1,mean_norm[which(x == 1)],sd_norm)})
plot(y,rep(0,length(y)))
lines(seq(-10,10,0.01),dnorm(seq(-10,10,0.01),mean_norm[1]))
lines(seq(-10,10,0.01),dnorm(seq(-10,10,0.01),mean_norm[2]))
lines(seq(-10,10,0.01),dnorm(seq(-10,10,0.01),mean_norm[3]))

likelihood_M = c()
for (i in y) {
  likelihood_M = cbind(likelihood_M,c(dnorm(i,mean_norm[1]),dnorm(i,mean_norm[2]),dnorm(i,mean_norm[3])))
}

pi = c(1/3,1/3,1/3)

for (i in 1:10000){
y_new = likelihood_M * pi
y_new = apply(y_new,2,function(x) x/sum(x))
pi = apply(y_new,1,sum)
pi = pi/sum(pi)
}

pi
true_pi = apply(data,1,function(x) sum(x==1));true_pi
prediction = apply(y_new, 2, which.max)
truth = apply(data, 2, which.max)
sum(prediction == truth)/length(prediction)
sum(prediction == 1)
sum(prediction == 2)
sum(prediction == 3)
