# Projekt 2

library(actuar)


# 1. naloga: 

# 1a
vzorec <- scan("vzorec1.txt")
graf1 <- hist(vzorec, main ="Histogram odškodnin", xlab ="Višina odškodnine", col = "cyan1")

# 1b
a <- mde(vzorec, ppareto1, start = list( shape = 1, min=3 ), measure = "CvM")
shape <- a$estimate[1]
min <- a$estimate[2]

# 1c

gostota <- dpareto1(vzorec, shape, min)
hist(vzorec, main ="Histogram odškodnin", xlab ="Višina odškodnine", col = "cyan2",probability = TRUE,ylim=c(0,2),breaks=30) 
curve(dpareto1(x,shape,min), col ="red", add = TRUE, lwd=2)

graf3 <- plot(ecdf(vzorec), main = "Porazdelitvena funkcija odškodnin", xlab="Višina odškodnine", ylab = "Porazdelitvena funkcija")
curve(ppareto1(x, shape, min), add=TRUE, col="red", lwd=2)
legend(8, 0.8, legend=c("Empirična porazdelitev", "Paretova porazdelitev"), bty = "n", col = c("black", "red"), lwd=2)

# 1d

# N porazdeljen Binomsko s p = 1/12 in n= 20

En <- 1/2 * 20
Ey <- (shape * min) / (shape - 1)
Varn <- (20*(1/2)) * 1/2
Vary <- (min * min * shape) / ((shape-1)*(shape-1)*(shape-2))

Es <- En * Ey
VarS = (En * Vary) + (Ey * Ey * Varn)

# 2. Naloga

# 2a
h = 0.25
n = 32
dis <- discretize(ppareto1(x, shape, min), from = 0, to = n*h, step = h, method = "rounding" )

# 2b
graf4 <- plot(stepfun(seq(0, (n-1)*h,h), diffinv(dis)),main = "Paretova porazdelitev", xlab= "x", ylab = "Porazdelitvena funkcija", col = "gold", lwd = 2 )
curve(ppareto1(x, shape, min), add = TRUE)

# 2c 

lala <- aggregateDist("recursive", model.freq = "binom", model.sev = dis, prob = 1/2, size = 20, maxit = 1000, x.scale = h, tol = 0.1)

Es <- mean(lala)

