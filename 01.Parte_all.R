


# GENERATORE LINEARE DI NUMERI PSEUDO-CASUALI
### Metodo Congruenziale lineare
# Primo numero
a <- 65537
m <-34359738368
xini <- 47838
c <- 5
(a*xini + c)%%m
x1 <- (a*xini+c)%%m
x1/m
# Secondo numero
(a*x1 + c)%%m
x2 <- (a*x1+ c)%%m
x2/m
# Costruitto ioterativo
n <- 1857
random.n <- numeric(n)
for(j in 1:n){
    x1 <- (a*x1+c)%%m
    random.n[j] <- x1/m
    }
head(random.n)
tail(random.n)


### Valutazione della pseudo-casualita delle serie
# 1. Stat.Descritive
require(skimr)
skim_without_charts(random.n)
var(random.n)
# Test grafici
### a. Diagramma a dispersione
plot(
    random.n[1:1856],
    random.n[2:1857],
    main="Grafico (1,n-1)*(2,n)",
    ylab = "valori generati con metodo congruenziale")

# b. Istogramma
hist(
    random.n, 
    col = 'purple',
    breaks = 20,
    freq=FALSE,
    ylim =c(0,1.5),
    main='Istogramma',
    xlab='numeri pseudo-casuali',
    ylab='Densità di frequenza'
    )
abline(v=mean(random.n),
    col='purple',
    lwd=2,lty=2
    )
text(0.7,1.3,
    c("valore medio"),
    col="blue"
    )

### c.Funzione di ripartizione empirica
plot(ecdf(random.n),
    do.points=FALSE,
    main ='Funzione di ripartizione empirica vs teorica'
    )
curve(punif(x,0,1),
    lty='dashed',
    col='red',
    lwd='3',
    add=TRUE
        )
legend(0.8,
    0.4,
    col=c("black","red"),
    c("f.r. empirica","f.r. teorica"),
    lty=c(1,2),
    cex=0.7)

### Test di Kolmogorov Smirnov
ks.test(random.n, "punif")

### Test Chi Quadrato
n <- length(random.n)
int<-seq(0,1,by=0.1); int
foss<-table(cut(random.n, int))/n; foss

p<- rep(0.1,10); p
chisq.test(foss,p=p)
qchisq(0.95,df=9)

n <- length(random.n)
int<-seq(0,1,by=0.1); int

fossN<-table(cut(random.n, int)); fossN

chisq.test(fossN)

### Fumzione di autocorrelazione empirica
acf(random.n,
    main = " funzione di autocorrelazione")

n<-length(random.n)
acf(random.n, 
    main = " funzione di autocorrelazione", 
    lag.max =n)
######################################################
# Funzione runif
set.seed(3882)
n <- 2000
rand <- runif(n, min=0, max=1)
head(rand)
# diagramma a dispersione
plot(rand[1:(n-1)],
     rand[2:n],
     main="Grafico (1,n-1)*(2:n)",
     ylab = "valori generati con funzione runif")
plot(ecdf(rand),
    do.points=FALSE,
    main ='funzione di ripartizione empirica vs teorica'
    )
curve(punif(x,0,1),
    lty='dashed',
    col='red',
    lwd='3',
    add=TRUE
    )
legend(0.6,0.3,
    col=c("black","red"),
    c("f.r. empirica","f.r. teorica"),
    lty=c(1,2),
    cex=0.6)
# Istogramma
hist(rand, 
    col = 'yellow',
    breaks = 20,
    freq=FALSE,
    ylim =c(0,1.5),
    main='Istogramma',
    xlab='numeri pseudo-casuali (runif)',
    ylab='Densità di frequenza'
    )
abline(v=mean(random.n),
    col='purple',
    lwd=2,lty=2
    )
text(0.7,1.3,
    c("valore medio"),
    col="blue")
# Test Statistici
### Kolmogorov Smirnov
ks.test(rand, "punif")
### Chi Quadratto
foss<-table(cut(rand, int))/n; foss
chisq.test(foss,p=p)
#### Autocorelazione
acf(rand,
    main = " funzione di autocorrelazione")
acf(rand, lag.max = n,
    main = " funzione di autocorrelazione")

##############
# Esercizio 8:
gen <- function(a, c, m, x1, niter) {
    x <- numeric(length = niter)
    for (i in 1:niter) {
        x1 <- (a*x1 + c) %% m
        x[i] <- x1 / m
        }
    return(x)
    }
gen(a = 26, c = 5, m = 106, x1 = 36, niter = 10)
x <- gen(a = 26, c = 5, m = 106, x1 = 36, niter = 500)
plot(x)
length(unique(x))

x <- gen(a = 25, c = 5, m = 2^32, x1 = 36, niter = 500)
length(unique(x))

# Esercizio 9:
a1 <- 0
b1 <- 1
set.seed(163)
ran1 <- runif(1000, a1, b1)
a2 <- 2
b2 <- 3
set.seed(163)
ran2 <- runif(1000, a2, b2)

mean(ran1)
var(ran1)
mean(ran2)
var(ran2)

par(mfrow=c(1, 2))
hist(ran1, 
    col = "goldenrod4",
    breaks = 50,
    freq = FALSE,
    ylim = c(0, 1.2),
    main = "Istogramma",
    xlab = "Generazioni pseudo-casuali da U(0,1)",
    ylab = "Densità")
#aaaa
hist(ran2, 
    col = "red",
    breaks = 50,
    freq = FALSE,
    ylim = c(0,1.2),
    main = "",
    xlab = "Generazioni pseudo-casuali da U(2,3)",
    ylab = "Densità")

par(mfrow=c(1,2))
acf(ran1,
    main = "Autocorrelazione di U(0,1)")
acf(ran2,
    main = "Autocorrelazione di U(2,3)")


par(mfrow=c(1,2))
acf(ran1,
    lag.max = 1000,
    main = "Autocorrelazione di U(0,1) - lag.max = n")
acf(ran2,
    lag.max = 1000,
    main = "Autocorrelazione di U(2,3) - lag.max = n ")

# Esercizio 10:
rand1 <- read.csv("data/rand1.csv", header = FALSE)
require(skimr)
skim_without_charts(rand1)
ind <- which(rand1$V1 > 0.3 & rand1$V1 < 0.7)
length(ind)/100
# test grafici
plot(
    rand1$V1[1:99],
    rand1$V1[2:100],
    main = "Diagramma a dispersione nel piano (1, 99) x (2:100)",
    xlab = "x[1:99]", ylab = "x[2:100")
plot(ecdf(rand1$V1),
    do.points=FALSE,
    main = 'Funzioni di ripartizione empirica vs teorica')
curve(punif(x, 0, 1),
    lty = 'dashed',
    col = 'red',
    lwd = 3,
    add = TRUE)
legend(0.6, 0.3,
    col = c("black","red"),
    c("F.R. empirica", "F.R. teorica"),
    lty = c(1, 2),
    cex = 0.6)
hist(rand1$V1, col = 'orange',
    breaks = 20,
    freq = FALSE,
    ylim = c(0, 2),
    main = 'Istogramma',
    xlab = 'Generazioni pseudo-casuali',
    ylab = 'Densità di frequenza')
abline(v=mean(rand1$V1),
    col = 'blue',
    lwd=2, lty=2)
text(0.7, 1.3,
    c("Valore medio"),
    col = "blue")
par(mfrow = c(1, 2))
acf(rand1$V1, main = " Fun. di autocorrelazione")
acf(rand1$V1, main = " Fun. di autocorrelazione", lag.max = length(rand1$V1))
# test statistici
ks.test(rand1$V1, "punif")
foss <- table(cut(rand1$V1, seq(0, 1, by = 0.1)))
foss
p <- rep(0.1, 10)
chisq.test(foss, p = p)
qchisq(0.95, df = 9)



# Esercizio 11
data(lh, package = "datasets")
lh <- as.numeric(lh)
head(lh)
skim_without_charts(lh)

plot(lh,
    xlab = "Periodo temporale",
    main = "Ormone luteinizzante")
abline(h = mean(lh), col = "red")
legend(0.6, 0.3,
    col = c("black", "red"),
    c("F.R. empirica", "F.R. teorica"),
    lty = c(1, 2),
    cex = 0.6)



n <- length(lh)
par(mfrow = c(1, 3))
acf(lh, main = "ACF") 
acf(lh, lag.max = 1, main = "ACF - lag.max = n/2")
acf(lh, lag.max = n, main = "ACF - lag.max = n")

plot(ecdf(lh),
    do.points = FALSE,
    main = 'Funzione di ripartizione empirica di lh')
    lm <- mean(lh)
ls <- sd(lh)
curve(pnorm(x, mean = lm, sd = ls),
    lty = 'dashed',
    col = 'red',
    add = TRUE)
legend(2.5, 0.3,
    col = c("black", "red"),
    c("F.R. empirica", "F.R. teorica"),
    lty = c(1, 2),
    cex = 0.6)

ks.test(lh,
    "pnorm",
    lm,
    ls, exact = FALSE)

table(lh)



# Esercizio 13:
load("data/dataeNo.Rdata")
head(dataeNo)
require(skimr)
skim_without_charts(dataeNo[,2:4])
plot(
    dataeNo[,2],
    type = "p",
    main = "Prima, 5 min. dopo, 20 min. dopo",
    ylim=c(0,150),
    ylab = "Costrizione dei bronchi")
par(new=TRUE)
plot(dataeNo[,3],
    type="p",
    col="blue",
    main=" ",
    ylim=c(0,150),
    ylab = "")
par(new=TRUE)
plot(dataeNo[,4],
    type="p",
    col="red",
    main=" ",
    ylim=c(0,150),
    ylab = "")
legend("topright",
    col=c("black","blue", "red"),
    c("prima","5m dopo", "20m dopo"),
    lty=c(1,1,1),
    cex=0.7)
apply(dataeNo[, 2:4], 2, IQR)

plot(ecdf(dataeNo[, 2]),
    col = "black",
    do.points = FALSE,
    main = "Funz. ripartizione empiriche asma")
plot(ecdf(dataeNo[, 3]),
    col = "red",
    do.points = FALSE,
    add = TRUE)
plot(ecdf(dataeNo[, 4]),
    col = "blue",
    do.points = FALSE,
    add = TRUE)
legend("bottomright",
    col = c("black", "red", "blue"),
    c("Prima", "5 min. dopo", "20 min. dopo"),
    lty = c(1, 1, 1),
    cex = 0.6)
abline(v = 60, lty = "dotted", col = "green")

# GENERAZIONE DI DETERMINAZIONI DA VIARIABILI CASUALI











## BINOMIALE NEGATIVA
Crabs <- read.table("http://stat4ds.rwth-aachen.de/data/Crabs.dat", header=TRUE)
head(Crabs)
require(skimr)
skimr::skim_without_charts(Crabs)
table(Crabs$sat)
hist(Crabs$sat, 
    breaks=c(0:16)-0.5,
    ylim = c(0,70),
    col = "pink", ylab = "Frequenze",
    xlab = "Numero satelliti",
    main = " ")
library(MASS)
stima <- glm.nb(sat ~ weight + factor(color), link=log, data=Crabs)
summary(stima)
1/0.9596



# Analisidella storica dei conteggi COVID
# Modello autoregressivo di Poisson non omogeneo
repository <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/"
overall.dataset <- "dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
overall.filename<-paste(repository,overall.dataset,sep="")
Italy<-read.csv(overall.filename)
names(Italy)
library(dplyr)
dataItaly<- Italy %>%
    select(data, 
        dimessi_guariti,
        isolamento_domiciliare,
        ricoverati_con_sintomi,
        terapia_intensiva,deceduti)
mydate <- as.Date(as.POSIXct(dataItaly$data, format="%Y-%m-%dT %H:%M:%S"))
df<-data.frame(mydate,dataItaly$isolamento_domiciliare)
colnames(df)<-c("date", "Isolati")
df1 <- subset(df, date >= as.Date('2022-08-01') & date <= as.Date('2022-10-20'))
min(df$date)
max(df$date)
require(skimr)
skimr::skim_without_charts(df1$Isolati)
n<-length(df1$Isolati)
ma<-max(df1$Isolati)
mi<-min(df1$Isolati)
ytick<-c(mi,409247,ma)
xtick<-c(1,11, 21, 31, 41, 51, 61, 71, 81)
plot(df1$Isolati,
    ylab=expression("Isolamento ("*italic(y[t])*")"),
    xlab=expression("Giorni ("*italic(t)*")"),
    yaxt="n",
    xaxt="n",
    xlim =c(1,n+5),
    ylim = c(0,ma+10),
    lwd = 0.5,
    lty = 1,col ="black" )
axis(side=2, at=ytick, labels = FALSE,
    cex.lab = 0.5, padj = 2,tck=-0.009)
text(par("usr")[1],
    ytick,
    labels = ytick, srt = 45,
    pos = 2, xpd = TRUE, cex.lab = 0.1)
axis(side=1, at=xtick,
    labels = FALSE, cex=0.1,tck=-0.009)
text(x=xtick,
    par("usr")[3],
    labels = xtick,
    cex.lab = 0.5,
    pos = 1, xpd = TRUE)


regressors1Italy <- cbind(
    linearTrend=seq(along=df1$Isolati),
    quadTrend = seq(along=df1$Isolati)^2/100,
    linlogTrend = log(seq(along=df1$Isolati)))
head(regressors1Italy)



options(scipen = 100)
require(tscount)
M3Italy <- tsglm(ts=df1$Isolati,
    link = "log",
    model=list(past_obs=1),
    xreg=regressors1Italy,
    distr = "poisson")

summary(M3Italy)

go <- 5
TT <- length(df1$date)
P3Italy <-predict(M3Italy,
    newxreg = data.frame(linearTrend = ((TT+1):(TT+go)),
    quadTrend = ((TT+1):(TT+go))^2/100,
    linlogTrend = log((TT+1):(TT+go))),
    n.ahead=go,
    method="bootstrap" )



pred<-data.frame(cbind(P3Italy$pred,P3Italy$interval))
colnames(pred)<-c("previsti", "PIinf", "PIsup")
pred

ytick<-c(mi,409247,ma)
xtick<-c(1,11, 21, 31, 41, 51, 61, 71, 81)
plot(df1$Isolati,
    ylab=expression("Isolamento ("*italic(y[t])*")"),
    xlab=expression("Giorni ("*italic(t)*")"),
    yaxt="n",
    xaxt="n",
    xlim =c(1,n+5),
    ylim = c(0,ma),
    lwd = 0.5,
    lty = 1,col ="black" )
abline(v=240, col = "gray")
axis(side=2, at=ytick, labels = FALSE,
    cex.lab = 0.5, padj = 2,tck=-0.009)
text(par("usr")[1],
    ytick,
    labels = ytick, srt = 45,
    pos = 2, xpd = TRUE, cex.lab = 0.1)
axis(side=1, at=xtick,
    labels = FALSE, cex=0.1,tck=-0.009)
text(x=xtick,
    par("usr")[3],
    labels = xtick,
    cex.lab = 0.5,
    pos = 1, xpd = TRUE)
lines(c(M3Italy$fitted.values,
    pred$previsti),
    lwd=3, col = 3, lty = 4)
legend("bottomleft",
    pch = c(20,NA,NA,NA,NA),
    lty = c(NA,2),
legend = c("osservati", "interpolati e previsti"),
    col = c("black", "green"),
    bty = "n",
    x.intersp = 0.1,
    cex= 0.6, pt.cex = .5,
    xpd = TRUE,
    text.width = 0.0001)


### Modello autoregressivo con distribuzione Binomiale Negativa
options(scipen = 100)
require(tscount)
M4Italy <- tsglm(ts=df1$Isolati,
    link = "log",
    model=list(past_obs=1),
    xreg=regressors1Italy,
    distr = "nbinom")
summary(M4Italy)


go <- 5
TT <- length(df1$date)
P4Italy <-predict(M4Italy,
    newxreg = data.frame(
        linearTrend = ((TT+1):(TT+go)),
        quadTrend = ((TT+1):(TT+go))^2/100,
        linlogTrend = log((TT+1):(TT+go))),
        n.ahead=go,
        method="bootstrap" )
pred<-cbind(P4Italy$pred,P4Italy$interval)
colnames(pred)<-c("previsti", "PIinf", "PIsup")
pred


plot(df1$Isolati, 
    lwd = 0.5,
    lty = 1,col ="black",
    xlab = "Giorni",
    ylab= "Conteggio Isolati",
    ylim = c(mi,ma+2), xlim = c(0,n+5))
    abline(v=81, col = "gray")
lines(c(M4Italy$fitted.values,P4Italy$pred),
    lwd=2, col = 2, lty = 2)
    legend("bottomleft",
    lty = c(1,2),
    legend = c("osservati", "interpolati e previsti"),
    col = c("black", "red"),
    bty = "n",
    x.intersp = 0.1,
    cex= 0.6, pt.cex = .5,
    xpd = TRUE,
    text.width = 0.0001)

### Valutazione della prevalenza nel tempo
prev<-c(M4Italy$fitted.values,P4Italy$pred)/60317000
prev<-prev*1000
    plot(prev, type = "l",
    xlab = "Giorni",
    ylim = c(0,20),
    lwd=2,
    col = "blue", 
    ylab = "(prevalenza isolati)*1000")


    
##############
# Esercizio 19:
repository <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/"
overall.dataset <- "dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
overall.filename <- paste(repository, overall.dataset,sep = "")
Italy <- read.csv(overall.filename)
df <- Italy[, c(1, 4)]
df$data <- as.Date(as.POSIXct(df$data, format = "%Y-%m-%dT %H:%M:%S"))
df <- subset(df, data <= as.Date('2022-10-26'))
colnames(df) <- c("Data", "Terapia")
rownames(df) <- 1:nrow(df)

require(skimr)
skim_without_charts(df)  
ind_max <- which.max(df$Terapia)
df[ind_max, ]
ind_min <- which.min(df$Terapia)
df[ind_min, ]
tail(df, 1)
n <- nrow(df)
m1 <- min(df$Terapia)
m2 <- max(df$Terapia)
ytick <- seq(m1, m2, length.out = 5)
xtick <- seq(1, n, by = 90)
plot(df$Terapia,
    ylab = expression("Terapia Intensiva ("*italic(y[t])*")"),
    xlab = expression("Giorni ("*italic(t)*")"),
    yaxt = "n",
    xaxt = "n",
    xlim = c(1, n+5),
    ylim = c(m1-10, m2+10),
    lwd = 0.5,
    lty = 1,
    col = "black")
axis(side = 2, at = ytick, labels = FALSE,
    cex.lab = 0.5, padj = 2, tck = -0.009)
text(par("usr")[1],
    ytick,
    labels = ytick, srt = 45,
    pos = 2, xpd = TRUE, cex.lab = 0.1)
axis(side = 1, at = xtick,
    labels = FALSE, cex = 0.1, tck = -0.009)
text(x = xtick,
    par("usr")[3],
    labels = xtick,
    cex.lab = 0.5,
    pos = 1, xpd = TRUE)


Regressors <- cbind(
    linearTrend = seq(along = df$Terapia),
    quadTrend = seq(along = df$Terapia)^2/100,
    linlogTrend = log(seq(along = df$Terapia)))


options(scipen = 100)
require(tscount)
M3Italy <- tsglm(ts = df$Terapia,
    link = "log",
    model = list(past_obs=1),
    xreg = Regressors,
    distr = "poisson")
summary(M3Italy)


A <- cbind(df$Terapia, M3Italy$fitted.values)
colnames(A) <- c("Osservati", "Interpolati")
A <- data.frame(A)
head(A)
tail(A)

# 3.
go <- 5
TT <- length(df$Data)
newRegressors <- data.frame(
    linearTrend = ((TT+1):(TT+go)),
    quadTrend = ((TT+1):(TT+go))^2/100,
    linlogTrend = log((TT+1):(TT+go)))
P3Italy <- predict(M3Italy,
    newxreg = newRegressors,
    n.ahead = go,
    method = "bootstrap")
pred <- cbind(
    P3Italy$pred, 
    P3Italy$interval,
    P3Italy$interval[, 2] - P3Italy$interval[, 1])
colnames(pred) <- c("Stime puntuali", "PIinf", "PIsup", "Ampiezza")
rownames(pred) <- format(df$Data[TT] + (1:go), "%d %b")
pred <- data.frame(pred)
pred


ytick <- seq(m1, m2, length.out = 5)
xtick <- seq(1, n, by = 90)
plot(df$Terapia,
    ylab = expression("Terapia intensiva ("*italic(y[t])*")"),
    xlab = expression("Giorni ("*italic(t)*")"),
    yaxt = "n",
    xaxt = "n",
    xlim = c(1, n+5),
    ylim = c(m1-10, m2+10),
    lwd = 0.5,
    lty = 1,
    col ="black" )
axis(side=2, at=ytick, labels = FALSE,
    cex.lab = 0.5, padj = 2,tck=-0.009)
text(par("usr")[1],
    ytick,
    labels = ytick, srt = 45,
    pos = 2, xpd = TRUE, cex.lab = 0.1)
axis(side=1, at=xtick,
    labels = FALSE, cex=0.1,tck=-0.009)
text(x=xtick,
    par("usr")[3],
    labels = xtick,
    cex.lab = 0.5,
    pos = 1, xpd = TRUE)
lines(c(M3Italy$fitted.values, P3Italy$pred),
    lwd = 1, col = 2, lty = 2)

legend(x = 750, y = m2,
    pch = c(20, NA),
    lty = c(NA, 2),
    legend = c("Valori osservati", "Valori interpolati e previsti"),
    col = c("black", "red"),
    bty = "n",
    x.intersp = 0.1,
    cex = 0.6, pt.cex = .5,
    xpd = TRUE,
    text.width = 0.0001)



# Esercizio 21:
repository <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/"
overall.dataset <- "dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
overall.filename <- paste(repository, overall.dataset,sep = "")
Italy <- read.csv(overall.filename)
df <- Italy[, c(1, 4)]
df$data <- as.Date(as.POSIXct(df$data, format = "%Y-%m-%dT %H:%M:%S"))
df <- subset(df, data <= as.Date('2022-10-26'))
colnames(df) <- c("Data", "Terapia")
rownames(df) <- 1:nrow(df)

options(scipen = 100)
require(tscount)
Regressors <- cbind(linearTrend = seq(along = df$Terapia),
quadTrend = seq(along = df$Terapia)^2/100,
linlogTrend = log(seq(along = df$Terapia)))
M4Italy <- tsglm(ts = df$Terapia,
    link = "log",
    model = list(past_obs = 1),
    xreg = Regressors,
    distr = "nbinom")
summary(M4Italy)

pop_Ita <- 60317000
prev <- M4Italy$fitted.values/pop_Ita
prev <- prev*1000

plot(prev, type = "l",
    xlab = "Giorni",
    lwd = 2,
    col = "blue", 
    ylab = "(Prevalenza terapia intensiva)*1000")



