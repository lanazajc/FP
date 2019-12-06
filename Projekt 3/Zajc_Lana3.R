library(combinat)
library(Rlab)


# Prva naloga
# A

S0 <- 50
u <- 1.05
d <- 0.95
T <- 5
R <- 0.03
W <- c(1,2,3,4,5,6)

#tabela_cen_delnic
pot1 <- c(50.00, 52.50, 49.88, 47.38, 45.01, 47.26)
pot2 <- c(50.00, 52.50, 55.12, 57.88, 60.78, 63.81)
pot3 <- c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
pot4 <- c(50.00, 47.50, 45.12, 47.38, 45.01, 47.26)
pot5 <- c(50.00, 52.50, 49.88, 52.37, 54.99, 52.24)

izplacilo <- function(vrsta, W, type){
  povp <- sum(vrsta*W) / sum(W)
  if(type == "call"){
    rezultat <- max(vrsta[length(vrsta)] - povp, 0)
  }
  if(type == "put"){
    rezultat <- max(povp - vrsta[length(vrsta)], 0)
  }
  return(rezultat)
}

#Izpla?ila X - nakupne
izplacilo.xpot1 <- izplacilo(pot1, W, "call")
izplacilo.xpot2 <- izplacilo(pot2, W, "call")
izplacilo.xpot3 <- izplacilo(pot3, W, "call")
izplacilo.xpot4 <- izplacilo(pot4, W, "call")
izplacilo.xpot5 <- izplacilo(pot5, W, "call")

#Izpla?ila Y - prodajne
izplacilo.ypot1 <- izplacilo(pot1, W, "put")
izplacilo.ypot2 <- izplacilo(pot2, W, "put")
izplacilo.ypot3 <- izplacilo(pot3, W, "put")
izplacilo.ypot4 <- izplacilo(pot4, W, "put")
izplacilo.ypot5 <- izplacilo(pot5, W, "put")

x <- c(izplacilo.xpot1, izplacilo.xpot2, izplacilo.xpot3, izplacilo.xpot4, izplacilo.xpot5)
y <- c(izplacilo.ypot1, izplacilo.ypot2, izplacilo.ypot3, izplacilo.ypot4, izplacilo.ypot5)

matrika_izplacil <- t(matrix(data = c(pot1, pot2, pot3, pot4, pot5), nrow = 6, ncol = 5))
tabela_izplacil <- data.frame(matrika_izplacil)
colnames(tabela_izplacil) <- c("S0", "S1", "S2","S3","S4", "S5")
tabela_izplacil <- cbind(tabela_izplacil, x, y)
colnames(tabela_izplacil) <- c("S0", "S1", "S2","S3","S4", "S5", "Izpla?ilo X", "Izpla?ilo Y")

#testi

izplacilo(c(50,52.5,49.88,52.37,49.75,52.24),c(1,0,1,0,1,0),"call")
izplacilo(c(50,52.5,55.12,57.88,60.78,63.81),1:6,"put")
izplacilo(c(60,61.2,59.98,58.78,57.6,58.75,57.58),rep(1,7),"put")
izplacilo(c(60,58.8,57.62,58.78,59.95,61.15,62.37),7:1,"call")
izplacilo(c(70,66.5,69.83,73.32,76.98,73.13,69.48),c(0,1,2,1,3,2,3),"put")

# Druga naloga

#Binomska

binomski <- function(S0,u,d,R,T,W,type){
  q <- (1+R-d)/(u-d) #
  kocka <- hcube(rep(2, T)) - 1 #
  
  dobicek_poti <- u ** kocka * (d **(1 - kocka))
  
  drevo <- cbind(rep(S0, 2**T), dobicek_poti)
  
  kom_prod <- t(apply(drevo, 1, cumprod))

  vektor_izplacil <- apply(kom_prod, 1, izplacilo, W=W, type=type)
  
  verjetnosti <- q ** rowSums(kocka) * (1 - q) ** (T - rowSums(kocka))

  premija <- sum(vektor_izplacil * verjetnosti) / (1 + R) ** T
  
  return(premija)
  }

#testi
binomski(50,1.05,0.95,0.03,5,rep(1,6),"put")
binomski(50, 1.05, 0.9 , 0.03, 10,0:10 , "call")
binomski(60, 1.05, 0.95, 0.01, 8,c(0,rep(1,8)),"put" )
binomski(70, 1.05, 1   ,    0,  7, rep(1,8), "call")
binomski(80, 1.1 , 0.95, 0.05,  9, 12:3, "put" )
binomski(90, 1.15, 0.8 , 0.01, 11,rep(c(1,0),6), "call")

# MonteCarlo

monte <- function(S0, u, d, R, T, W, type, N){
  q <- (1+R-d)/(u-d)
  
  drevo <- matrix(rbinom(N*T, 1, q), N, T)
  drugo_drevo <- d**(1 - drevo) * u ** drevo
  
  verjetnost <- (q ** rowSums(drevo)) * ((1 - q) ** (T - rowSums(drevo)))
  
  drugo_drevo <- t(apply(drugo_drevo, 1, cumprod))
  vrednosti <- cbind(S0, S0 * drugo_drevo)
  
  vektor_izplacil <- apply(vrednosti, 1, izplacilo, W=W, type=type)
  mat_upanje <- sum(vektor_izplacil) / length((vektor_izplacil))
  
  return(mat_upanje / (1 + R) ** T)
}

#Testi 
N1 <- 10
N2 <- 100
N3 <- 1000
monte(50,1.05,0.9,0.03,10,0:10,"call",100)
monte(70, 1.05, 1   , 0,7,c(0,rep(1,7)), "put",2000)
monte(90, 1.15, 0.8 , 0.01, 10,11:1, "call",50000)

S0 = 60
u = 1.05
d = 0.95
R = 0.01
T = 15
W = rep(1, 16)
type = "put"

monte_carlo1 = monte(S0, u, d, R, T, W, type, N1)
monte_carlo2 = monte(S0, u, d, R, T, W, type, N2)
monte_carlo3 = monte(S0, u, d, R, T, W, type, N3)

# 3. naloga

monte1 <- c()
monte2 <- c()
monte3 <- c()

for (i in 1:100){
  monte1 <- c(monte1,monte(60, 1.05, 0.95, 0.01, 15, rep(1,16),"put", 10) )
  monte2 <- c(monte2,monte(60, 1.05, 0.95, 0.01,15, rep(1,16), "put", 100) )
  monte3 <- c(monte3,monte(60, 1.05, 0.95, 0.01, 15, rep(1,16),"put",1000) )
}
cena <- binomski (60,1.05, 0.95,0.01, 15, rep(1,16), "put")

minimum <-floor(min(monte1, monte2, monte3))
maksimum <- ceiling((max(monte1, monte2, monte3)))

# Histogram N = 10

povprecje_monte1 <- mean(monte1)
odklon_monte1 <- sqrt(var(monte1))

Odklon_levo_monte1 <- cena - odklon_monte1
odklon_desno_monte1 <- cena + odklon_monte1

histogram_monte1 <- hist(monte1, breaks = 7,
                         main = 'Monte Carlo: N=10',
                         xlab = 'Premija',
                         xlim = c(minimum, maksimum),
                         ylim= c(0,40),
                         col='lightblue')
abline(v=povprecje_monte1, col='orange')
abline (v = cena, col = "red", lty = "dashed")
arrows (x0 = cena, y0=0, x1=odklon_desno_monte1, col='orange', length = 0.1)
arrows(x0 = cena, y0=0, x1=Odklon_levo_monte1, col='orange', length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('orange', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

# N = 100

povprecje_monte2 <- mean(monte2)
odklon_monte2 <- sqrt(var(monte2))

Odklon_levo_monte2 <- cena - odklon_monte2
odklon_desno_monte2 <- cena + odklon_monte2

histogram_monte1 <- hist(monte2, breaks = 7,
                         main = 'Monte Carlo: N=100',
                         xlab = 'Premija',
                         xlim = c(minimum, maksimum),
                         ylim= c(0,40),
                         col='lightblue')
abline(v=povprecje_monte2, col='orange')
abline (v = cena, col = "red", lty = "dashed")
arrows (x0 = cena, y0=0, x1=odklon_desno_monte2, col='orange', length = 0.1)
arrows(x0 = cena, y0=0, x1=Odklon_levo_monte2, col='orange', length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('orange', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

# N = 1000

povprecje_monte3 <- mean(monte3)
odklon_monte3 <- sqrt(var(monte3))

Odklon_levo_monte3 <- cena - odklon_monte3
odklon_desno_monte3 <- cena + odklon_monte3

histogram_monte3 <- hist(monte3, breaks = 7,
                         main = 'Monte Carlo: N=1000',
                         xlab = 'Premija',
                         xlim = c(minimum, maksimum),
                         ylim= c(0,40),
                         col='lightblue')
abline(v=povprecje_monte3, col='orange')
abline (v = cena, col = "red", lty = "dashed")
arrows (x0 = cena, y0=0, x1=odklon_desno_monte3, col='orange', length = 0.1)
arrows(x0 = cena, y0=0, x1=Odklon_levo_monte3, col='orange', length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('orange', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))





