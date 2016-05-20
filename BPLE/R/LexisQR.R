
# Author: tim
###############################################################################
setwd("/home/tim/git/BPLE/BPLE")
# getwd()
library(parallel)
library(splines)
library(quantreg)
# do this cause by cause
#LTC <- local(get(load("/home/tim/Dropbox/BPLE/Data/LTC1.Rdata")))
#LTC$Mxc5raw <- LTC$Mx * LTC$cprop
#LTC$Dxc5 <- NULL
#LTC$mx2 <- NULL
#LTC$mxs <- NULL
#LTC$Age5 <- NULL
#LTC$mxc5 <- NULL
#Causes <- unique(LTC$Cause)
##
#for (cs  in Causes){
#	path <- paste0("Data/CauseChunks/Cause",cs,".Rdata")
#	obj <- LTC[LTC$Cause == cs, ]
#	save(obj, file = path)
#}
#rm(LTC)




probs <- c(.99,.9,.75,.5,.25,.1,.01)
cs <- 1;sex<-"m";.probs = probs
Causeslog <- lapply(Causes, function(cs,.probs){
			path <- paste0("Data/CauseChunks/Cause",cs,".Rdata")
			DAT  <- local(get(load(path)))
			
			sexes <- unique(DAT$Sex)
			
			DAT <- DAT[!is.na(DAT$Mxc5raw) & !is.na(DAT$Exposure) & DAT$Exposure > 0,]
			
			out <- list()
			for (sex in sexes){
				mod1 <- rq(Mxc5raw ~ 
								ns(Age,knots=c(.5,1,4,10,15,20,25,seq(35,105,by=10))) * 
								ns(Year, knots = seq(1960,2009,length=5)), 
							weights = sqrt(Exposure),
							data = subset(DAT,Sex==sex),
							tau = .probs,
							method = 'fn')
				newdat           <- expand.grid(Age = 0:110, Year = 1959:2010) 
				mypred           <- exp(predict(mod1,newdat))
				colnames(mypred) <- .probs
				# put 0s back where needed
				mypred[ mypred < 1e-7 ] <- 0 
				out[[sex]] <- cbind(Sex = sex, newdat, mypred)
			}
			rm(DAT);gc()
			do.call(rbind,out)
		}, .probs = probs) #

# first try on log rates, giving 1e-7 for 0.
# in prediction whenever 
newdat <- expand.grid(Age = 0:100, Year = 1959:2010)

hm <- do.call(rbind,out)
head(hm)


library(LexisUtils)
library(reshape2)

mat <- acast(hm,Age~Year,value.var = "0.5")
mat[mat==0] <- NA
LexisMap(mat)
na0 <- function(x){
	x[x==0] <- NA
	x
}

graphics.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.01Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.01")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.1Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.1")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.5Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.5")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.9Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.9")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.99Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.99")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()

# retest without log
#----------------------------------
path <- paste0("Data/CauseChunks/Cause",cs,".Rdata")
DAT  <- local(get(load(path)))

sexes <- unique(DAT$Sex)

#DAT$Mxc5raw[DAT$Mxc5raw == 0] <- 1e-7
#DAT$Mxc5raw[DAT$Exposure==0]  <- NA
DAT <- DAT[!is.na(DAT$Mxc5raw) & !is.na(DAT$Exposure) & DAT$Exposure > 0,]
DAT <- DAT[DAT$Exposure!=0,]
boxplot(DAT$Mxc5raw)
mod1 <- rq(Mxc5raw ~ 
				ns(Age,knots=c(.5,1,4,10,15,20,25,seq(35,105,by=10))) * 
				ns(Year, knots = seq(1960,2009,length=5)), 
		weights = sqrt(Exposure),
		data = subset(DAT,Sex==sex),
		tau = probs,
		method = 'fn')
newdat           <- expand.grid(Age = 0:110, Year = 1959:2010) 
mypred           <- exp(predict(mod1,newdat))
colnames(mypred) <- .probs
hm <- mypred

pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.01Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.01")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.1Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.1")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.5Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.5")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.9Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.9")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.99Cause1MalesQRlog.pdf")
LexisMap(na0(acast(hm,Age~Year,value.var = "0.99")), zlim = c(1e-7,.1), contour = TRUE)
dev.off()
