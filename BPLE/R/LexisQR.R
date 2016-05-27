
# Author: tim
###############################################################################
#setwd("/hdir/0/triffe/COMMONS/git/VarSC/VarSC")
#library(parallel)
#library(splines)
#library(quantreg)
# do this cause by cause
#LTC <- local(get(load("/home/tim/Dropbox/BPLE/Data/LTC1.Rdata")))
# LTC <- local(get(load("/hdir/0/triffe/COMMONS/Dropbox/BPLE/Data/LTC1.Rdata")))
# LTC$Mxc5raw <- LTC$Mx * LTC$cprop
# LTC$Dxc5 <- NULL
# LTC$mx2 <- NULL
# LTC$mxs <- NULL
# LTC$Age5 <- NULL
# LTC$mxc5 <- NULL
# head(LTC)
# sum(is.na(LTC$mxc5raw ))
# #
# Causes <- unique(LTC$Cause)
# head(LTC)
# # cs <- 1; sex = 1
# for (cs  in Causes){
# 	path <- paste0("Data/CauseChunks/Cause",cs,".Rdata")
# 	obj <- LTC[LTC$Cause == cs, ]
# 	save(obj, file = path)
# }
#rm(obj);rm(LTC);gc()

#detectCores()
# 
#probs <- c(.99,.9,.75,.5,.25,.1,.01)
#Causes <- 1:30
# The following chunk was run on Keyfitz server, using 3 cores, over the weekend
# May 20. It may or may not finish. Went with unlogged mort and impute 0 for negatives...

#Causeslog <- mclapply(Causes, function(cs,.probs){
#			path <- paste0("Data/CauseChunks/Cause",cs,".Rdata")
#			DAT  <- local(get(load(path)))
#			
#			sexes <- unique(DAT$Sex)
#			
#			DAT <- DAT[!is.na(DAT$Mxc5raw) & !is.na(DAT$Exposure) & DAT$Exposure > 0,]
#			out <- list()
#			for (sex in sexes){
#				# take quantiles over unlogged data
#				mod1 <- rq(Mxc5raw ~ 
#								ns(Age,knots=c(.5,1,2,4,10,15,20,25,30,seq(35,105,by=10))) * 
#								ns(Year, knots = seq(1965,2005,by=10)), 
#						weights = sqrt(Exposure),
#						data = subset(DAT,Sex==sex),
#						tau = .probs,
#						method = 'fn')
#				newdat           <- expand.grid(Age = 0:110, Year = 1959:2010) 
#				mypred           <- predict(mod1,newdat)
#				colnames(mypred) <- .probs
#				# put 0s back where needed; this is ok
#				mypred[ mypred < 0 ] <- 0 
#				out[[sex]] <- cbind(Cause = cs, Sex = sex, newdat, mypred)
#			}
#			rm(DAT);gc()
#			do.call(rbind,out)
#		}, mc.cores = 3, .probs = probs) # This nr of cores only on the big machine
#Causeslog <- do.call(rbind, Causeslog)
#setwd("/hdir/0/triffe/COMMONS/git/VarSC/VarSC")
setwd("/home/tim/git/BPLE/BPLE")
library(parallel)
library(splines)
library(quantreg)

# quantiles
probs <- c(.99,.9,.75,.5,.25,.1,.01)

# first cause
cs    <- 1

# read in cause chunk
path  <- paste0("Data/CauseChunks/Cause",cs,".Rdata")
DAT   <- local(get(load(path)))

sex   <- "m"

DAT   <- DAT[!is.na(DAT$Mxc5raw) & !is.na(DAT$Exposure) & DAT$Exposure > 0,]

# take quantiles over unlogged data
system.time(
mod1 <- rq(Mxc5raw ~ 
				ns(Age,knots=c(.5,1,2,4,10,15,20,25,30,seq(35,105,by=10))) * 
				ns(Year, knots = seq(1965,2005,by=10)), 
		weights = sqrt(Exposure),
		data = subset(DAT,Sex==sex),
		tau = probs,
		method = 'fn')
)
sessionInfo()
newdat           <- expand.grid(Age = 0:110, Year = 1959:2010) 
mypred           <- predict(mod1,newdat)
colnames(mypred) <- .probs
# put 0s back where needed; this is ok
mypred[ mypred < 0 ] <- 0 
out <-  cbind(Cause = cs, Sex = sex, newdat, mypred)

rm(DAT);gc()
#save(Causeslog, file = "/hdir/0/triffe/COMMONS/Dropbox/BPLE/Data/QRlog.Rdata")
library(LexisUtils)
library(reshape2)

# Test results when they are in, should be available in DropBox.
Test1 <- local(get(load("/home/tim/Dropbox/BPLE/Data/QRresults.Rdata")))

mat <- acast(Test1[Test1$Cause == 1 & Test1$Sex == "m", ],Age~Year,value.var = "0.99")
mat[mat==0] <- NA
LexisMap(mat, zlim=c(1e-6,.1))


source("/home/tim/git/BPLE/BPLE/R/LTuniform.R")

probsc <- colnames(Test1)[-c(1:4)]
e0QR <- expand.grid(Year = 1959:2010,Sex=c("f","m"),Quant = probs, ex=NA)
plot(NULL, type = "n", xlim=c(1959,2010),ylim=c(50,95))
for (pr in 1:length(probsc)){
	Mi <- acast(Test1[Test1$Sex == "m", ],Age~Year,sum,value.var = probsc[pr])
	Fi <- acast(Test1[Test1$Sex == "f", ],Age~Year,sum,value.var = probsc[pr])
	
	e0fi <- apply(Fi,2,LTuniformvecminimal,sex="f")
	e0mi <- apply(Mi,2,LTuniformvecminimal,sex="m")
	
	e0QR$ex[with(e0QR,Sex == "m" & Quant == rev(probs)[pr])] <- e0mi
	e0QR$ex[with(e0QR,Sex == "f" & Quant == rev(probs)[pr])] <- e0fi
	
	lines(1959:2010, e0mi, col = "blue")
	lines(1959:2010, e0fi, col = "red")
	
	text(1958,e0mi["1959"],rev(probs)[pr],col="blue",xpd=TRUE)
	text(2011,e0fi["2010"],rev(probs)[pr],col="red",xpd=TRUE)
}



Test2 <- local(get(load("/home/tim/Dropbox/BPLE/Data/CausesDensresults.Rdata")))

plot(NULL, type = "n", xlim=c(1959,2010),ylim=c(50,95))
e0Density <- expand.grid(Year = 1959:2010,Sex=c("f","m"),Quant = probs, ex=NA)
for (pr in probs){
	Mi <- acast(Test2[Test2$Sex == "m" & Test2$Quant == pr, ],Age~Year,sum,value.var = "Mxc")
	Fi <- acast(Test2[Test2$Sex == "f" & Test2$Quant == pr, ],Age~Year,sum,value.var = "Mxc")
	
	Mi[is.infinite(Mi)] <- 0
	Fi[is.infinite(Fi)] <- 0
	
	e0fi <- apply(Fi,2,LTuniformvecminimal,sex="f")
	e0mi <- apply(Mi,2,LTuniformvecminimal,sex="m")
    e0Density$ex[with(e0Density,Sex == "m" & Quant == pr)] <- e0mi
	e0Density$ex[with(e0Density,Sex == "f" & Quant == pr)] <- e0fi
	lines(1959:2010, e0mi, col = "blue")
	lines(1959:2010, e0fi, col = "red")
	
	text(1958,e0mi["1959"],pr,col="blue",xpd=TRUE)
	text(2011,e0fi["2010"],pr,col="red",xpd=TRUE)
}










#na0 <- function(x){
#	x[x==0] <- NA
#	x
#}
#
#graphics.off()
#pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.01Cause1MalesQRlog.pdf")
#LexisMap(na0(acast(hm,Age~Year,value.var = "0.01")), zlim = c(1e-7,.1), contour = TRUE)
#dev.off()
#pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.1Cause1MalesQRlog.pdf")
#LexisMap(na0(acast(hm,Age~Year,value.var = "0.1")), zlim = c(1e-7,.1), contour = TRUE)
#dev.off()
#pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.5Cause1MalesQRlog.pdf")
#LexisMap(na0(acast(hm,Age~Year,value.var = "0.5")), zlim = c(1e-7,.1), contour = TRUE)
#dev.off()
#pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.9Cause1MalesQRlog.pdf")
#LexisMap(na0(acast(hm,Age~Year,value.var = "0.9")), zlim = c(1e-7,.1), contour = TRUE)
#dev.off()
#pdf("/home/tim/Dropbox/BPLE/Figures_Bin/Q.99Cause1MalesQRlog.pdf")
#LexisMap(na0(acast(hm,Age~Year,value.var = "0.99")), zlim = c(1e-7,.1), contour = TRUE)
#dev.off()

