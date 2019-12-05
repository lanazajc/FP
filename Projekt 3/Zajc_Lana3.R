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

#Izplaèila X - nakupne
izplacilo.xpot1 <- izplacilo(pot1, W, "call")
izplacilo.xpot2 <- izplacilo(pot2, W, "call")
izplacilo.xpot3 <- izplacilo(pot3, W, "call")
izplacilo.xpot4 <- izplacilo(pot4, W, "call")
izplacilo.xpot5 <- izplacilo(pot5, W, "call")

#Izplaèila Y - prodajne
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
colnames(tabela_izplacil) <- c("S0", "S1", "S2","S3","S4", "S5", "Izplaèilo X", "Izplaèilo Y")

# Druga naloga

#Binomska

binomski <- function(S0, u, d, R, T, W, type){
  q <- (1+R)/(u-d)
  poti <- hcube(rep(2,T),1,-1)
  verjetnosti <- q ** rowSums(poti) * (1 - q) ** (T - rowSums(poti))
  razvoji <- u ** poti * d * (1 - poti)
  S0 <- rep(S0, nrow(razvoji))
  cene <- cbind(S0, razvoji)
  matrika <- t(apply(cene, 1, cumprod))
  vektor_izplacil <- apply(matrika, 1, izplacilo, W=W, type=type)
  premija <- sum(vektor_izplacil * verjetnosti) / (1 + R) ** T
  return(premija)
  }










