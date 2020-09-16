#Alexandrea Ramnarine - September 2, 2020
#2019 big pharma competitive landscape - revenue vs. r&d spending
#Source: macrotrends.net

#Data build
#competitors
rd1 <- data.frame(c("Bayer","GSK","AstraZeneca","Pfizer","Novartis","Johnson & Johnson","Roche","Biogen"))
#R&D spending (billions USD)
rd1 <- cbind(rd1, c(5.983,5.833,6.059,8.65,9.402,11.355,12.857,14.378))
#Revenue (billions USD)
rd1 <- cbind(rd1, c(46.6,45,25.2,49.2,48.2,80.5,0.8056,10.9))
colnames(rd1) <- c("Company","RD","Rev")

#plot Revenue vs Spending
plot(rd1$RD, rd1$Rev, ylim=c(0,90), xlim=c(0,15), cex=2, pch=ifelse(rd1$RD==5.983, 17, 19), col=ifelse(rd1$RD==5.983, "blue", "black"), xlab="Research & Development Spending ($Billions/year)", ylab="Revenue ($Billions/year)", main="2019 Pharma Competitive Landscape", cex.lab=1.3)
#text(Rev~RD, data=rd1, labels=Company, pos=ifelse(rd1$RD==5.983, 3, 1))
axis(1, at=seq(0, 15, 1), labels=TRUE) #x
axis(2, at=seq(0, 85, 10), labels=TRUE) #y

##
##
##

#Bayer R&D expenses projection (billions)

#Data build
#year
rd2 <- data.frame(2005:2019)
#R&D Expenditures (billions USD) 
rd2 <- cbind(rd2, c(2.349,2.885,3.534,3.903,3.830,4.054,4.084,3.875,4.237,4.751,4.754,5.163,5.091,6.196,5.983))
colnames(rd2) <- c("Year","Expenditures")

#Fit linear model
lm <- lm(Expenditures~Year, data=rd2)

#Plot & Predict expenses from 2020-2032
library(plotrix)
plot(rd2$Expenditures~rd2$Year, ylim=c(0,10), xlim=c(2004,2033), las=2, cex=1, pch=19, main="Bayer Annual R&D Expenditures, 2005-2009", xlab="Year", ylab="Expenditures ($Billions)", cex.lab=1.3)
#predict using linear model
ablineclip(coef=lm$coefficients, lty=2, lwd=2, x1=2020, x2=2032)
axis(1, at=seq(2005, 2032, 1), labels=TRUE, las=2) #x

#calculate amount of 2032 sales expected to be spent on R&D 
#(%revenue spent on R&D/R&D expenditures)
#(.128/5.983)=(x/9)
(.128/5.983)*9
