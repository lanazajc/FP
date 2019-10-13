library(rvest)
library(gsubfn)
library(readr)
library(dplyr)

hist2015 = read.csv("hist2015.csv", row.names = 1)
hist2016 = read.csv("hist2016.csv", row.names = 1)
hist2017 = read.csv("hist2017.csv", row.names = 1)

ozko2015 <- hist2015[,c(1, 22, 42, 64, 84, 104, 126, 149, 170, 192, 214, 235)]
ozko2016 <- hist2016[, c(1, 21, 42, 63, 84, 106, 128, 149, 172, 194, 215, 237)]
ozko2017 <- hist2017[, c(1,23, 43, 66, 84, 106, 128, 149, 172, 193, 215, 237)]

skupna <- t(cbind(ozko2015,ozko2016,ozko2017))

# 1 del - Euribor

a <-ts(skupna[,6], start=c(2015,1), frequency = 12)
b <- ts(skupna[, 8], start=c(2015, 1), frequency = 12)

ts.plot(a,b ,col=c("red","blue"), main="Euribor", xlab="Time", ylab="%")
 legend("topright",c("6m", "12m"), col=c("red", "blue"),lwd=1.5)
 
# 2 del - Časovna struktura obrestnih mer
 
 datumi <- t(skupna[c(1, 15, 36),])
 rownames(datumi) <- c(0.25, 0.50, 1, 2, 3, 6, 9, 12)
 dospetja = rownames(datumi)
 datumi <- data.frame(datumi)

graf2 <- plot(x = dospetja, y = datumi[, c(1)],ylim=c(min(-0.4), max(0.5)), type="b", main="Časovna struktura Euribor", xlab="Dospetje [mesec]", ylab="%")
lines(datumi[, c(1)], x = dospetja, col="green", type="o", text(11,0.4 , "02.01.2015", col="green"))
lines(datumi[, c(2)], x = dospetja, col="red", type="o", text(11, 0.05, "01.03.2016", col="red"))
lines(datumi[, c(3)], x =dospetja, col="blue", type="o", text(11, -0.3, "01.12.2017", col="blue"))

# OPIS krivulj: Opazimo, da z dospetjem narašča tudi obrestna mera. Obrestne krivulje spominjajo na linearno funkcijo. 

# 3 del - Hipoteza pričakovanj trga

