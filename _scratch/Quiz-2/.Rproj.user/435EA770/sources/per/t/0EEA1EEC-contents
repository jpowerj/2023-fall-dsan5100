# 1. a
1-pbinom(64,560,0.12)
#0.6317595

#FALSE
1-pbinom(65,560,0.12)
#0.581324

#FALSE
pbinom(65,560,0.12)

#b. 
pbinom(65,560,0.35)

#FALSE
1-pbinom(65,560,0.35)


#c.
np=560*0.35
sd=sqrt(560*0.35*(1-0.35)) #sqrt np(1-p)

pnorm(65,np,sd)        



#4.
#a. 
1-phyper(10,268,132,20)
#0.918

#b.
1-phyper(10,132,268,20)
#0.03141175

#FALSE
phyper(10,132,268,20)


#d. 
p=132/400
dbinom(15,40,p)

#FALSE
1-dbinom(15,40,p)

#c
dhyper(15,132,268,40)



#3. 

x <- runif(1000, 0.4,1)# make a sample from U(0,1)
 # keep only observations >= 0.5
plot.ecdf(x, ylab="F(x)",xlab="variable X", main="CDF of X")

y<-rnorm(1000, mean=5, sd=.15)
plot.ecdf(y, ylab="F(Y)",xlab="variable Y", main="CDF of Y")

ks.test(y,"pnorm",5,0.15)

t <- seq(0,5,by = .01)
plot(t,cdf.x(t), type = 'l', lwd = 2)
lines(t,cdf.y(t), col = 2)
lines(t,cdf.y.2(t), col = 4)
legend(c("X", "Y = X|X>2", "Y-2"), fill = c(1,2,4), x = 'bottomright')

df <- data.frame(height = runif(1000, 0.4,1))
library(ggplot2)
ggplot(df, aes(height)) + stat_ecdf(geom = "point")
ggplot(df, aes(height)) + stat_ecdf(geom = "step")


df <- data.frame(height = round(rnorm(200, mean=60, sd=15)))
library(ggplot2)
ggplot(df, aes(height)) + stat_ecdf(geom = "point")
ggplot(df, aes(height)) + stat_ecdf(geom = "step")



#d.

n = 20
p = 0.4
(x<- rbinom(1,n,p)) #one binomial distributed random variate with (n,p)
(y<- rbinom(5,n,p)) #five binomial distributed random variate with (n,p)

plot(0:n,pbinom(0:n,n,p), type = 's')
pbinom(8,n,p)


#5.

#b. 
1-pgeom(10,0.01)

#FALSE
1-pgeom(9,0.01)

#c.x = number of failures. size = target for number of successful trials, Must be strictly positive, need not be integer.
dnbinom(x = 96, size = 4, prob = 0.01)

#FALSE
dgeom(96,0.01)

#6.

pexp(30,2.5/60)
# 0.7134952

#2.
 ppois(2,5*7/14)
 
#9.
 x<-rnorm(1000,20,0.5)
 dnorm(5,3,5)
 
 
x<-  sample(1:65,10)
x 
mean(x)
sd(x)
var(x)

pnorm(60,43,sqrt(471))-pnorm(40,43,sqrt(471))


#9. 
1-pexp(2.5, 1/24)
