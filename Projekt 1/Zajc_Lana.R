library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
library(ggplot2)

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

terminske <- data.frame()
# T = 6, U = 12

for (i in 1: nrow(skupna)){
L012 <- skupna[i, "12m"]
L06 <- skupna[i, "6m"]
Ter0612 = ( 1 / (12-6) ) *((1 + 12 * L012)/(1 + 6* L06) - 1)

terminske[i, 1] <- Ter0612 }

terminske <- cbind(terminske, skupna[, c(6,8)])
terminske <- terminske[, c(2,3,1)]
colnames(terminske) <- c("Euribor6m", "Euribor12m", "Napoved6m")
rownames(terminske) <- rownames(skupna)

# 3C:

#dodamo v tabelo terminske še stolpec z letnico: 
stolpecLeto <- as.vector(cbind(seq(2015, 2015, length.out = 12), (seq(2016, 2016, length.out = 12)), seq(2017, 2017, length.out = 12))) %>% as.factor()
terminske <- as.data.frame(cbind(terminske, stolpecLeto))
names(terminske)[4] <- "Leto"

graf3 <- ggplot(terminske, aes(x = terminske$Napoved6m, y = terminske$Euribor6m)) + 
        geom_point(aes(color = terminske$Leto), size = 5) + 
        geom_smooth(method='lm',se = FALSE, color = "gray") + 
        geom_abline(slope = 1, intercept = 0)+ 
        coord_cartesian(xlim=c(-1.0,1.5),ylim=c(-1.0,1.5)) + 
        labs(title = "6m Euribor 2015-2017", x = "Napoved", y = "Opazovano", color = "Leto" )
print(graf3)

#Grafi po letih 3d:

#2015

terminske2015 <- terminske[c(1:12),]

graf2015 <- ggplot(terminske2015, aes(x = terminske2015$Napoved6m, y = terminske2015$Euribor6m)) + 
        geom_point(color = "red", size = 5) + 
        geom_smooth(method='lm',se = FALSE, color = "gray") + 
        geom_abline(slope = 1, intercept = 0)+ 
        coord_cartesian(xlim=c(-0.1,0.3),ylim=c(-0.1,0.3)) + 
        labs(title = "6m Euribor 2015", x = "Napoved", y = "Opazovano", color = "Leto" )
print(graf2015)

#2016

terminske2016 <- terminske[c(13:24),]

graf2016 <- ggplot(terminske2016, aes(x = terminske2016$Napoved6m, y = terminske2016$Euribor6m)) + 
        geom_point(color = "green", size = 5) + 
        geom_smooth(method='lm',se = FALSE, color = "gray") + 
        geom_abline(slope = 1, intercept = 0)+ 
        coord_cartesian(xlim=c(-1.0,1.5),ylim=c(-1.0,1.5)) + 
        labs(title = "6m Euribor 2016", x = "Napoved", y = "Opazovano", color = "Leto" )
print(graf2016)

#2017

terminske2017 <- terminske[c(25:36),]

graf2017 <- ggplot(terminske2017, aes(x = terminske2017$Napoved6m, y = terminske2017$Euribor6m)) + 
        geom_point(color = "blue", size = 5) + 
        geom_smooth(method='lm',se = FALSE, color = "gray") + 
        geom_abline(slope = 1, intercept = 0)+ 
        coord_cartesian(xlim=c(-0.5,0.25),ylim=c(-0.5,0.25)) + 
        labs(title = "6m Euribor 2017", x = "Napoved", y = "Opazovano", color = "Leto" )
print(graf2017)

