library(ggplot2)
library(dplyr)
library(readr)
#zadanie 1
panny <- 17364
mezatki <- 56128
wdowy <- 11239
rozwodki <- 8170

stan_cywilny <- data_frame(stan_cywilny = c("panny", "mezatki", "wdowy", "rozwodki"),
                           liczba = c(panny, mezatki, wdowy, rozwodki))
stan_cywilny = c("panny", "mezatki", "wdowy", "rozwodki")
liczba = c(panny, mezatki, wdowy, rozwodki)
wykres <- ggplot(stan_cywilny, aes(x ='', licznosc, fill = stan_cywilny)) +
  geom_col()
pie <- wykres + coord_polar("y", start =0)
pie(x = c(panny, mezatki, wdowy, rozwodki), labels =c("panny", "mezatki", "wdowy", "rozwodki") )
wykres_slupkowy <- ggplot(stan_cywilny, aes(x =stan_cywilny, licznosc, fill = stan_cywilny)) +
  geom_col()
wykres_slupkowy
pie(x = c(panny, mezatki, wdowy, rozwodki), paste(stan_cywilny, liczba, sep="\n"), cex=0.9, radius = 1.5)

#frakcja
proc <- prop.table(liczba)
liczba/sum(liczba)
pie(liczba, paste(stan_cywilny, paste(proc, '%', sep = ""), sep="\n"), cex = 0.7)
#zadanie 2
library(readr)
stacje <- read_csv("OneDrive - Politechnika Warszawska/R_studio/Stata/stacje.csv")
stacje_net <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")
head(stacje_net,3)
?getwd()
?setwd()
licznosc <- stacje %>% 
  group_by(Answers) %>% 
  summarise(Liczność = n())
#lub
licz <- table(stacje)
pie(licznosc$Liczność, labels = licznosc$Answers)
pie(licz, labels = paste(names(licz), licz, sep = "\n"))
barplot(licz, names.arg = (paste(names(licz))))

df <- as.data.frame(licznosc)
ggplot(df, aes(Answers, Liczność))+
  geom_col()
w2 <- ggplot(df, aes(fill = Answers, y = Liczność, x = " "))+
  geom_col()
wykres_kolowy <- w2 + coord_polar("y", start = 0)

#zadanie 3
ceny <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 25.20, 24.50, 24.60, 24.30, 26.10, 23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 25.20,
24.10, 26.80)
dzien <- 1:20
df3 <- data_frame(dzien, ceny) 
wykres3 <- ggplot(df3, aes(dzien, ceny)) +
  geom_line()

#zadanie 4 
butelki <- read_csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=butelki.csv")
butelki <- butelki %>% 
  mutate(ciśnienie = strength*0.0068947)
butelki$nowa <- butelki$strength*0.0068947
#b
hist(butelki$ciśnienie)
hist(butelki$ciśnienie, breaks = 4)
hist(butelki$ciśnienie, breaks = 14)
hist(butelki$ciśnienie, breaks = "FD") #jakis fajny algorytm zeby byly lkadne przedzialiki
#c
h <- hist(butelki$ciśnienie, breaks = 14)
lines(h$mids, h$counts)

#d
?stem() #wykres lodyga-liscie
stem(butelki$ciśnienie)

#e
boxplot(butelki$ciśnienie)
b <- boxplot(butelki$ciśnienie)
#f
b$stats
summary(butelki$ciśnienie)
#to samo na dwa sposoby

x <- butelki$strength
mean(x)
median(x)
mean(x, trim = 0.1) #sredni ucieta
quantile(x, 0.2)
var(x)
sd(x)
IQR(x)
range(x)
diff(range(x)) #roznica pomiedzy minimum a maksimum
sum(x)
length(x)

library(moments)

skewness(x) #skosnosc
kurtosis(x) #kurtoza

#g
quantile(butelki$strength, c(0.05,0.10,0.25,0.50,0.75,0.90,0.95))

#h
mean(x, trim = 0.1)
c <- seq(0.1, 0.9, 0.01)
plot(x, mean(x, trim = c))
ggplot(butelki, aes(butelki$ciśnienie, mean(butelki$ciśnienie, trim = c)))+
  geom_line()
trim_means <- lapply(0:49/100, function(y) mean(x, trim = y)) #lapply y jest przyjmowane jako pierwszy argument funkcji, i podstawia to pod y i zwraca potem liste
plot((1:length(trim_means)), trim_means)
plot(0:49/100, trim_means)

#zadanie 5
czynsz <- c(334,    436,352,    405,
384,    498,
425,    398,
392 ,   403,
374  ,  389,
424   , 429,
344    ,400,
367,457,
392,    428,
424 ,   443,
409  ,  454,
339   , 389,
378    ,387,
345    ,422)
length(czynsz)
c <- 1:30
df5 <- data_frame(c, czynsz)
summary(czynsz)
h5 <- hist(czynsz)
lines(h5$mids, h5$counts)

#zadanie 6
samochody <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")
head(samochody)
samochody$zp <- 1 * 3.785* 100/(samochody$mpg * 1609/1000)
stem(samochody$zp)
hist(samochody$zp)

mean(samochody$zp, na.rm = T)
zp1 <- na.omit(samochody$zp)
length(zp1)
as.vector(zp1)
#d
var <- c(0.5, 10, 15, 25, 50)
var[2]
for (i in length(var)){
  p1 <- plot(density(zp1, kernel = "epanechnikov",bw = var[i]))
  p1
}

plot(density(zp1, kernel = "epanechnikov", bw =0.5))
plot(density(zp1, kernel = "epanechnikov", bw =1))
plot(density(zp1, kernel = "gaussian", bw =0.5))
plot(density(zp1, kernel = "rectangular", bw =5))

#zad 7
samochody %>% mutate(paliwo  = case_when(
  zp <= 7 ~ "mało",
  zp >7 & zp <= 10 ~ "średnio",
  zp > 10 ~ "dużo"))
opis = c()
opis[zp1<=7] = 'malo'
opis[zp1>7 & zp1 <= 10] = 'średnio'
opis[zp1>10] = 'dużo'
table(opis)
opis2 = cut(zp1, c(-Inf, 7, 10, Inf), right = T, labels = c('malo', 'srednio' , 'duzo'))


barplot(table(opis)/100)

#zad 8 
?tapply
zp <- samochody$zp
prod <- factor(samochody$producent) #factor - super wektor R-owy
levels(prod) <- c("Ameryka", "Europa", "Japonia")
?tapply
tapply(zp, prod, mean, na.rm = TRUE)
tapply(zp, prod,var, na.rm = TRUE)
tapply(zp, prod, sd, na.rm = TRUE)
par(mfrow = c(1,3))
tapply(zp, prod, boxplot, na.rm = TRUE)
par(mfrow = c(1,1))
boxplot(zp~prod)
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)
library(ggplot2)
samochody
wykres <- samochody %>% 
  ggplot(aes(producent,zp))+
  geom_boxplot()

#zad 9 
samochody %>% 
  group_by(cylindry) %>% 
  summarise(zuz_pal = mean(zp, na.rm = T)) 
cyl <- samochody$cylindry
boxplot(zp~cyl)
#zad 10
samochody %>% 
  filter(waga < 2500) %>% 
  select(zp) %>% 
  summary()
#zad 11
v <- c(mean, var, sd)
samochody_79_81 <- samochody %>% 
  filter(rok >= 79 & rok <= 81) %>% 
  select(moc, rok) %>%
  group_by(rok) %>% 
  summarise() 
samochody <- na.omit(samochody)
samochody_79 <- samochody %>% 
  filter(rok == 79)
ggplot(samochody, aes(x =factor(rok), y = moc)) + 
  geom_boxplot()
moc_silnika_ktorej_nie_przekracza_95_aut <- quantile(samochody$moc, 0.95)


1 - quantile(samochody$przysp, 0.75)

#kartkowka
data.frame(samochody)
boxplot(samochody$cena)
h1 <- hist(samochody$cena)
quantile(samochody$cena, c(0.25, 0.75))
IQR(samochody$cena)
3287.5 - 1.5*2200
5487.5 + 1.5*2200
3287.5 - 3*2200
5487.5 + 3*2200
#zacisze domowe
pr_losowa_normalny <- rnorm(1000, 0, 1)
hist(pr_losowa_normalny)
curve(dnorm(x, 0, 1), xlim = c(-5,5), add = T)
d <- density(pr_losowa_normalny, bw = "nrd0", adjust = 1)
curve(d, add =T)

U <- 1:200
Z = sum(U[])

#jeszcze raz zadania
#zad1
c1 <- c(1736, 11239, 8170)
c2 <- c("panny", "męzatki", "rozwodki")
pie(c1, paste(c2, c1, sep = "\n"))
barplot(c1, names.arg = c2)
#zad2
stacje <- read_csv("OneDrive - Politechnika Warszawska/R_studio/Stata/stacje.csv")
a1<-table(stacje)
barplot(a1, names.arg = names(a1))
pie(a1, labels = paste(names(a1), licz))
#zad3
ceny <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 25.20, 24.50, 24.60, 24.30, 26.10, 23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 25.20,
          24.10, 26.80)
dni <- 1:20
plot(dni, ceny, type = "l")
lines(dni, ceny, type = "l")


#zad 2
wektor <- 1:100
u <- c()
for (i in wektor){
  u[i] = runif(200, 0, 1)
}
u
lapply(wektor, function(y) runif(200,0,1))

#kart 2
head(samochody)
samochody<- na.omit(samochody)
a <- samochody %>% 
  filter(cylindry == 4) 
sd(a$przysp)
