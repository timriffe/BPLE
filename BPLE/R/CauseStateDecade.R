
# Author: tim
###############################################################################


# find total contribution from Cause C, State S by decade.

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}

fem <- local(get(load("Data/fcontrib.Rdata")))
mal <- local(get(load("Data/mcontrib.Rdata")))

names(fem)
names(fem[[1]])
names(fem)
ftot <- lapply(names(fem),function(nm,fem){
			do.call(rbind,lapply(fem,colSums))
		}, fem = fem)

sts     <- names(fem[[1]])
decades <- (1960:2004) - (1960:2004) %% 10
decs    <- data.frame(Year = 1960:2004,decade = decades)
decs$Year
StateCauseYear <- do.call(rbind, lapply(sts, function(st,decs){
					decs$State <- st
					decs
				}, decs=decs))
MSCY <- FSCY <- StateCauseYear

Causes <- colnames(fem[[1]][[1]])
Causes <- matrix(0,ncol=length(Causes),nrow=nrow(StateCauseYear), dimnames=list(NULL, Causes))
MSCY <- cbind(MSCY, Causes)
FSCY <- cbind(FSCY, Causes)
MSCY$Uterus <- NULL

for (i in 1:nrow(MSCY)){
	yr   <- as.character(MSCY$Year[i])
	St   <- MSCY$State[i]
	MSums <- colSums(mal[[yr]][[St]])
	FSums <- colSums(fem[[yr]][[St]])
	MSCY[i,names(MSums)]  <- MSums
	FSCY[i,names(FSums)] <- FSums
}

save(MSCY, file = "Data/MSCY.Rdata")
save(FSCY, file = "Data/FSCY.Rdata")







