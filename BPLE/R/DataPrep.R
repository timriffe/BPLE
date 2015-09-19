
# Author: tim
###############################################################################

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
library(reshape2)
library(magrittr)
library(data.table)
library(MortalitySmooth)

LT 			<- local(get(load("Data/MilaStatesSingleAge.Rdata")))

# clean mxc, reshape
mxc 		<- local(get(load("Data/mxcod.Rdata")))
mxc 		<- melt(mxc, varnames = c("State","Year","Sex","Age","Cause"), value.name = "Mxc")
mxc$State 	<- as.character(mxc$State)
mxc$Sex   	<- as.character(mxc$Sex)
mxc$Age   	<- as.character(mxc$Age)
mxc$Cause 	<- as.character(mxc$Cause)

# fix age
# I rather dislike this little operation,
# but more legible with pipes
mxc$Age %>% 
		strsplit(split="-") %>% 
		lapply("[[",1) %>% 
		unlist %>% 
		gsub(pattern="Over ", replacement = "") %>% 
		as.integer -> mxc$Age
mxc$Sex <- ifelse(mxc$Sex == "male","m","f")

# convert mxc to proportions
makeprop <- function(Mxc){
	prop <- Mxc 
	if (sum(prop) > 0){
		prop <- prop / sum(prop)
	}
	prop
}
mxc <- data.table(mxc)
mxc[,prop := makeprop(Mxc), by = list(Year, State, Sex, Age)]

###################################################
# apply proportions to single age mx 
LT <- LT[LT$Year <=2004, ]
head(LT)
sum(LT$Mx==0 & !is.na(LT$Mx))

LT$Dxhat <- LT$Exposure * LT$mx
# given a matrix od DX and EX, lets smooth rates!
sm.mat <- function(DX, EX){
	ages  		<- 0:110
	years 		<- 1959:2004
	W     		<- EX
	W[W > 0] 	<- 1
	W[DX < 1 & W == 1] 	<- .1
	fit   <- Mort2Dsmooth(
			     x = ages, 
				 y = years, 
				 Z = DX,
			     offset = log(EX), 
				 W = W,
				 control=list(MAX.IT=200))
	mxs <- exp(fit$logmortality)
	mxs <- melt(mxs, varnames=c("Age","Year"), value.name = "mxs")
    mxs
}
# the iterator function for data.table
sm.chunk <- function(.SD){
	# using the Dx that came from the HMD smoothed
	# mx, keeps upper ages under control a bit more.
	DX      <- acast(.SD, Age~Year, value.var = "Dxhat")
	EX      <- acast(.SD, Age~Year, value.var = "Exposure")
	mxs     <- sm.mat(DX,EX)
	.SD$mxs <- mxs$mxs
	.SD
}
# takes a while
LT <- LT[,sm.chunk(.SD),by=list(State,Sex)]

# now we have a smoothed mx, very nice.
###############################################
# next step: attach proportions:
# 1) make age groups for LT

LT$Age5                            <- LT$Age - LT$Age %% 5
LT$Age5[LT$Age5 == 0 & LT$Age > 0] <- 1
LT$Age5[LT$Age5 > 100]             <- 100 # because they'll all get same proportions...

# now attach mxs to mxc, easier.

# State, Year, Age, Sex...
mxs <- as.data.frame(mxs)
LT  <- as.data.frame(LT)

# make an LT for each Cause...
LTC <- do.call(rbind,lapply(unique(mxc$Cause),function(cause, LT){
		LT$Cause <- cause
		LT
		}, LT=LT))

# use the recvec method, easiest to remember...
# add prop to LT

values        <- mxc$prop
names(values) <- with(mxc,paste(State, Year, Sex, Age, Cause))
LTC$cprop     <- values[with(LTC, paste(State, Year, Sex, Age5, Cause))]

# ok mxc is the product:
LTC$mxc5 <- LTC$mxs * LTC$cprop

# now it's best to smooth the cause props in nearly the same way: 2d
# then reconstrain.
any(LTC$mxc5[LTC$Sex == "f" & LTC$Cause == "Prostate"] > 0)
LTC <- LTC[!(LTC$Sex == "f" & LTC$Cause == "Prostate"), ]
any(LTC$mxc5[LTC$Sex == "m" & LTC$Cause == "Uterus"] > 0)
LTC <- LTC[!(LTC$Sex == "m" & LTC$Cause == "Uterus"), ]
LTC$Dxc5 <- LTC$mxc5 * LTC$Exposure

.SD <- LTC[LTC$Sex == "f" & LTC$State == "AK" & LTC$Cause == "Cardio.", ]
sm.chunk.cause <- function(.SD){
	# using the Dx that came from the HMD smoothed
	# mx, keeps upper ages under control a bit more.
	DX      <- acast(.SD, Age~Year, value.var = "Dxc5")
	EX      <- acast(.SD, Age~Year, value.var = "Exposure")
	if (sum(DX > 0) > 100){
		mxs     <- sm.mat(DX,EX)
		.SD$mxcs <- mxs$mxs
	} else {
		.SD$mxcs <- .SD$mxc5
	}
	.SD
}
# takes a long time
head(LTC)
LTC <- LTC[,sm.chunk.cause(.SD),by=list(State,Sex,Cause)]


# saver intermediary because this takes so long, just in case.
save(LTC, file = "Data/LTC1.Rdata")

# next step is to convert to props and multiply back into mxs.
LTC[,prop2 := makeprop(mxcs), by = list(Year, State, Sex, Age)]
LTC$mxcs2 <- LTC$prop2 * LTC$mxs

# take look at example for small state...
#plot(with(LTC, mxcs2[Sex == "f" & Year == 1959 & Cause == "Breast"& State == "AK"]), log = 'y', type = 'l')
#points(with(LTC, mxcs[Sex == "f" & Year == 1959 & Cause == "Breast"& State == "AK"]))
#points(with(LTC, mxc5[Sex == "f" & Year == 1959 & Cause == "Breast"& State == "AK"]),pch=3)

# so depending on whether the result is smooth or not, may be due for a second round of smoothing
# within cause. Or maybe we accept this? Probably enough juice for now.
save(LTC, file = "Data/LTC1.Rdata")
