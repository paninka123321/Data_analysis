dw_elementowa <- rnorm(20, 0, 1)
sto_elementowa <- rnorm(100, 0, 1)
pr <- rt(50, 5)

par(mfrow = c(1,3))
plot(ecdf(pr), main = "50 el")
curve(pnorm(x), -2, 2, main = "dystrybuanta")
#title("Dystrybuanta")
plot(ecdf(dw_elementowa), main = " 20")
#title("empiryczna 20 el")
plot(ecdf(sto_elementowa), main = "100 el")
par(mfrow = c(1,1))


#zad2
randomowa_cauchy <- rcauchy(500, 0, 1)
wektor <- 1:500
srednia <- c()
mediana <- c()
#1 sposoob
n <- 1/500
suma <- cumsum(randomowa_cauchy)
srednia <- suma/n
#2 spodsob
for (i in wektor){
  srednia[i] <- mean(randomowa_cauchy[1:i])
}

for (i in wektor) {
  mediana[i] <- median(randomowa_cauchy[1:i])
  
}

plot(wektor, srednia, type = "l")
lines(wektor, mediana, col = 2)
abline(h = 0, col =3)

#b
odch <- c()
odch_cw <- c()
wek_2 <- 2:500
for (i in wek_2){
  odch[i-1] <- sd(randomowa_cauchy[1:i])
}

for (i in wek_2) {
  odch_cw[i-1] <- IQR(randomowa_cauchy[1:i])/2
  
}
plot(wek_2, odch, type = "l", log = 'y')
lines(wek_2, odch_cw, lty = 1, col =2)
abline(h = 1, col =3)


#zadanie 3 
probka_norm <- rnorm(500, 0, 1)
sr_norm <- c()
med_norm <- c()
for (i in wek_2){
  sr_norm[i-1] <- mean(randomowa_cauchy[1:i])
  sr_med[i-1] <- median(randomowa_cauchy[1:i])
}
wek = 100
l = 1
u <- c()
for (i in 1: wek){
  proba = rpois(10, l)
  u[i] = mean(proba)
}
u
#zad4 
# X ~ U[O, theta]
#E[X] = theta/2
#est1 = 2*mean(X)
#est2 = max(X)
wektor <- 10000
theta <- 1
theta_M <- c()
theta_NW <- c()
u <- c()
for (i in 1:wektor){
  proba = runif(20, 0, theta)
  theta_M[i] <- 2*mean(proba)
  theta_NW <- max(proba)
}
b_M <- mean(theta_M)-theta
b_NW <- mean(theta_NW) - theta
mse_M <- mean((theta_M-theta)^2)
#inaczej: var(theta_M) + b_m^2
mse_NW <- mean((theta_NW-theta)^2)
#inaczej :var(theta_NW) + b_NW^2

#zad 5
N<- 1000
n<- 10
mi <- 9
sigma <- 3
alfa <- 0.05
q <- qt(1-alfa/2, n-1)
k<- 0 

for (i in 1:N){
  x < - rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  if(mi>= m - q*s/sqrt(n) & mi<= m + q*s/sqrt(n)) k <- k+1
}
k/N*100
#zad 7
temp <- scan(nlines = 3)
330.0 322.0 342.4 340.4 337.5 327.3
345.0 328.6 329.7 334.0 322.6 341.0
331.0 342.0 326.5 325.8 340.0 333.0
temp

funkcja <- function(x){
  y <- 2*x
  x+y
}
funkcja(3)

prz.ufn.mi <- function(x, alfa){
  
}