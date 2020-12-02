# Examples in the lecture
# Instructor : Young Joon Oh

library(ggplot2)
data(diamonds)

diamonds

head(diamonds)

summary(diamonds$carat)

hist(diamonds$carat)


#histogram by different width bars
ggplot(diamonds, aes(x=carat))+geom_histogram(binwidth=0.01)+labs(x="Carat")
ggplot(diamonds, aes(x=carat))+geom_histogram(binwidth=0.05)+labs(x="Carat")
ggplot(diamonds, aes(x=carat))+geom_histogram(binwidth=0.1)+labs(x="Carat")


boxplot(diamonds$carat)
#summary(diamonds$price)
#hist(diamonds$price)


# Scatter plot
plot(diamonds$carat,diamonds$price)


#line plot
ggplot(diamonds,aes(x=diamonds$carat, y=diamonds$price)) + geom_line()

######

a<- c(2,3,20, 21,23,24,25,29,33,34,35,54,65 )
length(a)
mean(a)

median(a)
summary(a) #(b<-quantile(a, probs = c(0.25,0.75)))
34-21
(out1_q1<-21-1.5*13)
(out2_q3<-34+1.5*13)

#hist(a)
boxplot(a)

#############################
#Quiz 1.
#############################

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#x<- sample(x=1:7, size = 15, replace = TRUE)
#x<-data.frame(x)
x<-c(7, 1, 7, 6, 1, 4, 3, 3, 1, 6, 4, 5, 6, 5, 6)


length(x)
summary(x)
hist(x)
Mode(x)
var(x)
sd(x)
IQR(x)
boxplot(x)

#a<-sample(x=1:6, size= 10, replace = TRUE)

a<-c(5, 6, 4, 6, 6, 5, 5, 1, 2, 6,8,1)
length(a)
quantile(a, probs = seq(0, 1, 0.25), na.rm = FALSE, type = 2)
summary(a)
IQR(a)
#boxplot(a) -> use STATA

############################
# week5
############################

set.seed(100)

r1 <- rnorm(10, 0, 1)
(d<-density(r1))
hist(r1)
plot(d)
abline(v=c(0,1,-1), col = "red")

r1 <- rnorm(20, 0, 1)
(d<-density(r1))
plot(d)
abline(v=c(0,1,-1), col = "red")


r1 <- rnorm(30, 0, 1)
(d<-density(r1))
plot(d)
abline(v=c(0,1,-1), col = "red")

r1 <- rnorm(50, 0, 1)
(d<-density(r1))
plot(d)
abline(v=c(0,1,-1), col = "red")


r1 <- rnorm(3000, 0, 1)
(d<-density(r1))
plot(d)
abline(v=c(0,1,-1), col = "red")


r1 <- rnorm(500000, 0, 1)
(d<-density(r1))
plot(d)
abline(v=c(0,1,-1), col = "red")


#########
# Week 5 Thur
########

set.seed(100)

population <- rnorm (10000, 5, 2)
summary(population)

sam1 <- sample(population, 20)
summary(sam1)

sam2 <- sample(population, 20)
summary(sam2)

sam3 <- sample(population, 20)
summary(sam3)

sam4 <- sample(population, 20)
summary(sam4)

sam5 <- sample(population, 20)
summary(sam5)

sam6 <- sample(population, 20)
summary(sam6)

sam7 <- sample(population, 20)
summary(sam7)

sam8 <- sample(population, 20)
summary(sam8)

sam9 <- sample(population, 20)
summary(sam9)


all <-c(5.352,3.9850,3.9930,5.342,4.895,4.878,4.995,5.562, 4.737 )
summary(all)
hist(all)



# plot

(d<-density(population))
plot(d)

sam10 <- sample(population, 50)
summary(sam10)
d1<-density(sam10)
lines(d1, col="red")


sam11 <- sample(population, 500)
summary(sam11)
d1<-density(sam11)
lines(d1, col="red")

sam12 <- sample(population, 5000)
d1<-density(sam12)
lines(d1, col="blue")
summary(sam12)



#########
## week6
#########

library(zoo)
library(psych)
set.seed(100)

##################
# For Z-statistics
##################

# making z-distribution
z <- seq( from=-3.5, to=+3.5, by=.01)
dens <- dnorm(z)
plot( z, dens, type="l", main="N(0,1) density" )

# creating random data set for male & female
m <-rnorm(100, 5,2)
f <-rnorm(100, 4,3)

dm <- density(m)
df <- density(f)
describe(m)
describe(f)


plot(dm)
lines(df, col="red")
abline(v=6, col = "blue")

val <-6

# For Male
xt <- diff(dm$x[dm$x<val]) # construct lengths and heights
yt <- rollmean(dm$y[dm$x<val],2)
sum(xt*yt) # This gives you the area

# z
mean <- 5.43 
sd <- 1.86
(z<-(val-mean)/sd)


# For female
xt <- diff(df$x[df$x<val])
yt <- rollmean(df$y[df$x<val],2)
sum(xt*yt)

# z
mean <- 3.8  
sd <- 2.78
(z<-(val-mean)/sd)



###########################
# For inferential statistics 


population <- rnorm (10000, 5, 2)
sam11 <- sample(population, 20)
sam12 <- sample(population, 100)



d0<-density(population)
d1<-density(sam11)
d2<-density(sam12)

describe(population)
describe(sam11)
describe(sam12)



plot(d0) # population
lines(d1, col=2) # sam11 , n=20
lines(d2, col=4) # sam12 , n=100
abline(v=c(mean(population),mean(sam11),mean(sam12)), col = c(1,2,4))



#for population to compare real area to z area
mean <- 5
sd <- 
(z<-(val-mean)/sd)


xt <- diff(d0$x[d0$x<val])
yt <- rollmean(d0$y[d0$x<val],2)
sum(xt*yt)


# for standard error

n<-20
val <- 4.33         # sample mean
mean <- mu <- 4.97
sd <- 2
(z<-(val-mean)/sd/n)  
  

n<-100
val <-  5.02       # sample mean
mean <- mu <- 4.97
sd <- 2
(z<-(val-mean)/sd/n)  # z increase


# larger n
sam13<- sample(population, 1000) 
describe(sam13)

n<-1000
val <-         # sample mean
mean <- mu <- 5
sd <- 
(z<-(val-mean)/sd/n) 
