#############################
# Use kernel density estimation in order to estimate quantiles of the underlying distribution of age- and cause-specific death rates across states
# Adrien Remund, May 10, 2016, based on a suggestion by Jutta Gampe
#############################

# idea: based on Jutta's suggestion, estimate the kernel density of the mxc's across states, one cause and age at a time, and extract quantiles
# advantages : deals with zeros, avoids smoothing, and allows studying other quantiles than "zero" = min

####################################################
# example with random deviates of a normal density
#x <- rnorm(2e2, 100, 10)
#pdf <- density(x, n = 1e4) # use large n for finer estimate (small granularity)
#
## density
#hist(x, probability = T, las = 1, main = paste(length(x),"random deviates from a normal distribution"), col = 8, border = "grey90", xlab = "pdf")
#lines(pdf$x, pdf$y, lwd=2)
#lines(50:150, dnorm(x = 50:150, mean = 100, sd = 10), lwd=2, col=2)
#legend("topright", legend = c("observed","estimated","true"), lwd = c(NA,2,2), col = c(NA,1,2), fill = c(8,NA,NA), border = c("grey90",NA,NA))
#
## estimated vs. theoretical quantiles
#max(pdf$x[which(cumsum(pdf$y)/sum(pdf$y) < 0.01)]) ; qnorm(p = 0.01, mean = 100, sd = 10)
#max(pdf$x[which(cumsum(pdf$y)/sum(pdf$y) < 0.50)]) ; qnorm(p = 0.50, mean = 100, sd = 10)
#max(pdf$x[which(cumsum(pdf$y)/sum(pdf$y) < 0.99)]) ; qnorm(p = 0.99, mean = 100, sd = 10)
#

####################################################
# real data
getwd()
# NB: should we weight the distribution by the exposure?
setwd("/home/tim/Dropbox/BPLE")
load("Data/LTC1.Rdata")

#AK <- local(get(load("/home/tim/git/BPLE/BPLE/Data/cod.fractions.Rdata")))
#dimnames(AK)


#LTC2 <- LTC1[,c("State","Sex","Age","Exposure","Deaths")]
head(LTC1[LTC1$cprop>0,])
library(reshape2)
# OK, try males, Cause == 1, Age~Year~State
TestDens <- acast(LTC1[LTC1$Cause == 1 & LTC1$Sex == "m", ], Age~Year~State, value.var = "mxc5")
TestExp <- acast(LTC1[LTC1$Cause == 1 & LTC1$Sex == "m", ], Age~Year~State, value.var = "Exposure")
TestDens <- acast(DAT[DAT$Cause == 1 & DAT$Sex == "m", ], Age~Year~State, value.var = "Mxc5raw")
TestExp <- acast(DAT[DAT$Cause == 1 & DAT$Sex == "m", ], Age~Year~State, value.var = "Exposure")


rm(LTC1)
#MeanMat <- apply(TestDens, c(1,2), mean, na.rm=TRUE)
#MeanMat[MeanMat==0] <- NA
#library(LexisUtils)
#getwd()
#pdf("Figures_Bin/MeanCause1Males.pdf")
#LexisMap(MeanMat)
#dev.off()
#

# densfun
densquant <- function(mxc, nx, p=.5){
	mxc[is.na(mxc)] <- 0
	if (all(mxc==0)){
		return(0)
	}
	
	pdf <- density(mxc, 
			n = 1e4, 
			from = 0, 
			to = max(mxc, na.rm = TRUE), 
			weights = sqrt(nx))
	max(pdf$x[cumsum(pdf$y)/sum(pdf$y) < p])
}

Q.01 <-Q.99 <-Q.5 <- Q.1 <- Q.9 <- is.na(TestDens[,,1]) * 0

#densquant(TestDens[30,1,], TestExp[30,1,])
TestExp[is.na(TestExp)] <- 0
for (y in 1:ncol(Q.5)){
	for (a in 1:nrow(Q.5)){
		Q.5[a,y]  <- densquant(TestDens[a, y,], TestExp[a, y,], .5)
		Q.1[a,y]  <- densquant(TestDens[a, y,], TestExp[a, y,], .1)
		Q.9[a,y]  <- densquant(TestDens[a, y,], TestExp[a, y,], .9)
		Q.01[a,y] <- densquant(TestDens[a, y,], TestExp[a, y,], .01)
		Q.99[a,y] <- densquant(TestDens[a, y,], TestExp[a, y,], .99)
	}
}


na0 <- function(x){
	x[x==0] <- NA
	x
}
graphics.off()
pdf("Figures_Bin/Q.01Cause1Males.pdf")
LexisMap(na0(Q.01), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("Figures_Bin/Q.1Cause1Males.pdf")
LexisMap(na0(Q.1), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("Figures_Bin/Q.5Cause1Males.pdf")
LexisMap(na0(Q.5), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("Figures_Bin/Q.9Cause1Males.pdf")
LexisMap(na0(Q.9), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("Figures_Bin/Q.99Cause1Males.pdf")
LexisMap(na0(Q.99), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
library(reshape2)
TestLong <- melt(TestDens,varnames=c("Age","Year","State"),value.name="Mx")
ExpLong <- melt(TestExp,varnames=c("Age","Year","State"),value.name="Exp")
TestLong$Exp <- ExpLong$Exp
library(quantreg)
library(splines)
library(Epi)
head(TestLong)
TestLong$Mx2 <- TestLong$Mx
TestLong$Mx2[TestLong$Mx2==0]<- 1e-7



# this is an issue w 0s, work on this more
mod1 <- rq(log(Mx2) ~ 
				ns(Age,knots=c(.5,1,4,10,15,20,25,seq(30,90,by=10))) * 
				ns(Year, knots = seq(1965,2005,by=10)), 
		weights = sqrt(Exp),
		data = TestLong,
		tau = c(.01,.1,.25,.5,.75,.9,.99))
mod2 <- rq(Mx ~ 
				ns(Age,knots=c(.5,1,4,10,15,20,25,seq(30,90,by=10))) * 
				ns(Year, knots = seq(1965,2005,by=10)), 
		weights = sqrt(Exp),
		data = TestLong,
		tau = c(.01,.1,.25,.5,.75,.9,.99))
library(LexisUtils)
newdat <- expand.grid(Age = 0:100, Year = 1959:2010)

mxq <- cbind(newdat,exp(predict(mod1,newdat)))
LexisMap(acast(mxq,Age~Year,value.var = "exp(predict(mod1, newdat))"))
mxq2 <- cbind(newdat,predict(mod2,newdat))

matplot(mxq2)


mxq2[mxq2 < 1e-7] <- NA
LexisMap(acast(mxq2,Age~Year,value.var = "predict(mod2, newdat)"))


summary(mod1)
mxq <- exp(mod1$fitted.values)
matplot(0:110, mxq, type = 'l', lwd =1, lty=1,log='y')

# if all states have Dx == 0

DM <- reshape2::acast(LTC1[LTC1$Cause == 1 & LTC1$sex == "m", ], Age~Year, value.var = "Deaths",sum)


image(matrix(unlist(by(data = LTC1$Deaths, INDICES = list(LTC1$Cause, LTC1$Age), FUN = sum)), nrow = 30)) # the same values seem to repeat again and again...

year <- 2010
sex <- "m"
cause <- 1
age <- 10
mxc <- LTC1$Mx[LTC1$Sex == sex & LTC1$Age == age & LTC1$Cause == cause & LTC1$Year == year]
nx  <- LTC1$Exposure[LTC1$Sex == sex & LTC1$Age == age & LTC1$Cause == cause & LTC1$Year == year]

hist(mxc, las = 1, xlab = "mxc by state", ylab = "pdf", main = paste("sex = ",sex,", age = ",age,", year = ",year,", cause = ",cause), probability = T, col = 8, border = "grey90", breaks = 12)
pdf <- density(na.omit(mxc), n = 1e4, from = 0, to = max(mxc, na.rm = T), weights = nx / sum(nx)) # 'from' is important to avoid negative values, use 'bw' to adjust "wigliness"
lines(pdf$x, pdf$y, lwd = 2)

q01 <- max(pdf$x[which(cumsum(pdf$y)/sum(pdf$y) < 0.01)])
q01 ; min(mxc, na.rm = T)  # 1% quantile =/= minimum observed  = 0
q50 <- max(pdf$x[which(cumsum(pdf$y)/sum(pdf$y) < 0.50)]) # median
q99 <- max(pdf$x[which(cumsum(pdf$y)/sum(pdf$y) < 0.99)]) # 99% quantile
m   <- pdf$x[which.max(pdf$y)]

abline(v = c(q01, q50, q99), lwd = 2, col = 4, lty = 2)
abline(v = m, lwd = 2, col = 2, lty = 2)
mtext(side = 3, at = m, text = "mode", col = 2)
mtext(side = 1, at = c(q01, q50, q99), text = c("1%", "50%", "99%"), col = 4)
legend("topright", legend = c("observed","estimated","quantiles"), lwd = c(NA,2,2), lty = c(NA,1,2), col = c(NA,1,4), fill = c(8,NA,NA), border = c("grey90",NA,NA))

