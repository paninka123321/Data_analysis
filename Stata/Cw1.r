library(dplyr)
curve(dnorm(x,-1, 2), xlim = c(-3,4))
#add = T - dodaje wykres na tym samym wykresier 
# d - gęstość
par(mfrow = c(1,3))
# par dziala jak patchwork

#zadanie 1
#a
?dnorm()
curve(dnorm(x, 0, 1), xlim = c(-5,5), col =2, lwd = 3, lty = 2)
curve(dnorm(x, 1, 1), xlim = c(-5,5), add = T, col = 3)
curve(dnorm(x, 2, 1), xlim = c(-5,5), add = T)
title("Gęstość")

curve(pnorm(x, 0, 1), xlim = c(-5,5), col =2, lwd = 3, lty = 2)
curve(pnorm(x, 1, 1), xlim = c(-5,5), add = T, col = 3)
curve(pnorm(x, 2, 1), xlim = c(-5,5), add = T)
title("Dystrybuanta")

curve(1- pnorm(x, 0, 1), xlim = c(-5,5), col =2, lwd = 3, lty = 2)
curve(1 - pnorm(x, 1, 1), xlim = c(-5,5), add = T, col = 3)
curve(1 - pnorm(x, 2, 1), xlim = c(-5,5), add = T)
title("Funkcja przeżycia")

#b
curve(dnorm(x,0,1), xlim = c(-5,5), col = 2, lwd = 2, lty = 5)
curve(dnorm(x, 0, sqrt(0.5)), col = 3, add = T)
curve(dnorm(x, 0, sqrt(2)), col = 4, add = T)
title("Gęstość")

curve(pnorm(x, 0,1),xlim = c(-5,5), col = 2, lwd = 2, lty = 5)
curve(pnorm(x, 0, sqrt(0.5)), col = 3, add = T)
curve(pnorm(x, 0, sqrt(2)), col = 4, add = T)
title("Dystrybuanta")

curve(1 - pnorm(x, 0, 1),xlim = c(-5,5), col = 2, lwd = 2, lty = 5)
curve(1 - pnorm(x, 0, sqrt(0.5)), col = 3, add = T)
curve(1 - pnorm(x, 0, sqrt(2)), col = 4, add = T)
title("Funkcja przeżycia")
#zadanie 2 dla rozkladu N(0,1)
#za pomocą dystrybuanty
pnorm(1, 0, 1) - pnorm(-1, 0, 1)
pnorm(2,0,1) - pnorm(-2, 0, 1)
pnorm(3,0,1) - pnorm(-3, 0, 1)

#lub
1 - 2*pnorm(mi - sigma, mi, sigma)
1 - 2*pnorm(mi - 2*sigma, mi, sigma)
1 - 2*pnorm(mi - 3*sigma, mi, sigma)



#zadanie 3
pnorm(179, 173, 6)
pnorm(180, 173, 6) - pnorm(167, 173, 6)
1 - pnorm(181, 173, 6)
qnorm(0.6, 173, 6)


#zadanie 4
qnorm(0.95, 0,1)
qnorm(0.975, 0, 1)
qt(0.95,10)
qt(0.99, 20)
qchisq(0.9, 4)
qchisq(0.95, 10)
qf(0.95,2,10)
qf(0.99, 3, 18)


#zadanie5
par(mfrow = c(1,1))
curve(dgamma(x, 1,1), xlim = c(0,5), col =1)
curve(dgamma(x, 0.5, 1),xlim = c(0,5), col =2, add =T)
curve(dgamma(x, 2, 1), xlim = c(-0,5), col = 3, add =T)
curve(dgamma(x, 3, 1), xlim = c(0,5), col =4, add = T)


#zadanie 6
par(mfrow = c(1,1))
curve(dchisq(x, 5), col = "blue", xlim = c(-1, 100))
curve(dnorm(x, 5, sqrt(10)), add = T, xlim = c(-1, 100))
curve(dchisq(x, 10), col ="red", xlim = c(-1, 100))
curve(dnorm(x, 10, sqrt(20)), add = T, xlim = c(-1, 100))
curve(dchisq(x, 40), xlim = c(-1, 100))
curve(dnorm(x, 40, sqrt(80)), col = "dark green",add = T, xlim = c(-1, 100))

#zadanie 7
curve(dt(x, 1), col = "blue", xlim = c(-10, 10))
curve(dnorm(x, 5, sqrt(10)), add = T, xlim = c(-10, 10))
curve(dt(x, 5), col ="red", xlim = c(-10, 10))
curve(dnorm(x, 10, sqrt(20)), add = T, xlim = c(-10, 10))
curve(dt(x, 30), xlim = c(-10, 10), col = "dark green")
curve(dnorm(x, 40, sqrt(80)),add = T, xlim = c(-10, 10))


#zadanie 8
#c
curve(df(x, 2, 1), col = "blue", xlim = c(0,15))
curve(df(x, 2, 5), col = "dark green", xlim = c(0,15), add =T)
curve(df(x, 2, 10), col = "red", xlim = c(0,15), add = T)
curve(df(x, 2, 20), col = "orange", xlim = c(0,15), add = T)
curve(dexp(x, 1), xlim = c(0,15), add =T)


#Zadanie 9
curve(dbeta(x, 1,1), col = "red", xlim = c(-5,5))
curve(dbeta(x, 2,2), col ="blue", add = T)
curve(dbeta(x, 5,2), col = "dark green", add = T)
curve(dbeta(x, 2,5), col = "orange", add = T)

#zadanie 10
x <-0:15
barplot(dbinom(x,10, 0.5), xlim = c(0, 10), col = rgb(0,0,0, alpha = 0.5))
barplot(dbinom(x,10, 0.25), xlim = c(0, 10), col = rgb(1,0,0, alpha = 0.5), add =T)

#zadanie 11
pgeom(0, 0.1) # x = 1
dgeom(1, 0.1) # x =2
pgeom(1, 0.1) - pgeom(0.999, 0.1) # x =2
dgeom(2, 0.1)
dgeom(3, 0.1)
1 - pgeom(10, 0.1)

#zadanie 12
dhyper(0, 5, 195, 10)
?dhyper()
#zadanie 14
#Y ~ exp(4)
pexp(4)
#a
x <- c(0.33, 0.5, 0.66, 1)
pexp(x,4)
#b
EY = 1/4
VarY = 1/16
#c
pexp(0.5, 4)
#d
1 - pexp(1, 4)
dpois(0,4)

?dpois


#zadanie 15
x <- runif(1000, 0, 1)
y <- runif(1000, 0, 1)
z <- y < x^2
mean(z)
par(mfrow = c(1,1))
plot(x, y, pch = '.', xlim =c(0, 1), ylim = c(0,1))
points(x[z>0], y[z>0], col = 3)
curve(x^2, col =2, add = T, lwd =3)

#b
x <- runif(1000, 0, 1)
y <- runif(1000, 0, 1)
z <- y < 1 - x^2 & y > x^2
#opd:
mean(z)
#odp 2 sposob
length(z[z == TRUE])/length(z)
plot(x,y , pch = '.', xlim = c(0,1), ylim = c(0,1))
points(x[z>0], y[z>0], col = 4)
count(x[z>0] & y[z>0])
curve(x^2, col =2, add = T, lwd =3)

#Zadanie 1
wartosc_x <- -5:5
r <- -5:5
dystrybuanta <- pnorm(x)
a <- data.frame(wartosc_x, dystrybuanta)

#Zadanie 2
a<-qnorm(c,0,1)
c<-c(0.9,0.95, 0.975, 0.99, 0.995)
t(data.frame(c,a))

#Zadanie 3

c<-c(0.9,0.95, 0.975, 0.99, 0.995)
a1 <- qt(c, 10)
a2 <-qt(c,15)
a3 <- qt(c, 20)

t(data.frame(c,a1, a2, a3))

#Zadanie 4 
kwantyl <-c(0.005,0.01, 0.025, 0.05,0.1, 0.9,0.95,0.975,0.99, 0.995)
st_4 <- qchisq(kwantyl, 4)
st_10 <- qchisq(kwantyl, 10)
tablica_kwantyli_chikwadrat <- (data.frame(kwantyl, st_4, st_10))

#Zadanie5
#a
pnorm(1,0,1) - pnorm(-1,0,1)
#b
pnorm(2,0,1) - pnorm(-2,0,1)
#c
pnorm(3,0,1) - pnorm(-3,0,1)

probka <- as.data.frame(runif(1000, min = -7, max = 7))
names(probka)[1] <- "wartosc"
miedzy_3 <- probka %>% 
  filter(wartosc<3 & wartosc>-3)
425/1000

#zadanie6
pbinom(0,10, 0.5)
?pbinom
