
# Author: tim
###############################################################################
setwd("/hdir/0/triffe/COMMONS/git/VarSC/VarSC")
library(parallel)
library(splines)
library(quantreg)
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
probs <- c(.99,.9,.75,.5,.25,.1,.01)
Causes <- 1:30
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


#mat <- acast(hm,Age~Year,value.var = "0.01")
#mat[mat==0] <- NA
#LexisMap(mat, zlim=c(1e-6,.1))
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

