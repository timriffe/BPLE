
# Author: tim
###############################################################################

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
getwd()
library(data.table)
source("/home/tim/git/BPLE/BPLE/R/LTuniform.R")

LTC <- local(get(load("Data/LTC1.Rdata")))
head(LTC)
hm <- tapply(LTC$mxcs2*LTC$Exposure, list(LTC$Cause, LTC$Sex), sum, na.rm=TRUE)
hm <- as.data.frame(hm)
hm$f[9] <- NA
library(xtable)
print(xtable(100*t(t(hm) / colSums(hm,na.rm=TRUE))))