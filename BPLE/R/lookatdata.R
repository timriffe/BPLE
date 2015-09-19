# Author: triffe
###############################################################################
# compiles long-format all-cause complete lifetables from HMD.
# should only run once at beginning and again once at end 
# (for final data version in paper, to be saved with metadata)
# for Tim, this will choke

#hm<- local(get(load("/home/tim/Desktop/Ken-decompos/mxcod.Rdata")))
#dimnames(hm)

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}
getwd()

States <- local(get(load("Data/LTC1.Rdata")))
LT     <- local(get(load("Data/MilaStatesSingleAge.Rdata")))

fRLE <- tapply(LT$ex[LT$Age == 0 & LT$Sex == "f"], LT$Year[LT$Age == 0& LT$Sex == "f"], max)
mRLE <- tapply(LT$ex[LT$Age == 0 & LT$Sex == "m"], LT$Year[LT$Age == 0& LT$Sex == "m"], max)

plot(unique(LT$Year), fRLE, type = 'l', main = "US Record e(0)",ylim=c(65,85))
lines(unique(LT$Year), mRLE)

library(data.table)
States <- data.table(States)
Mins   <- States[,min(mxcs2),by=list(Year, Sex, Age, Cause)]
Agg <- Mins[,sum(V1),by=list(Year, Sex, Age)]

library(reshape2)
Males <- acast(Agg[Agg$Sex == "m",], Age~Year, value.var = "V1")
Females <- acast(Agg[Agg$Sex == "f",], Age~Year, value.var = "V1")

mx2ex <- function(mx){
  lx <- c(1,exp(-cumsum(mx)),0)
  Lx <- (lx[-1] + lx[-length(lx)]) / 2
  sum(Lx)
}
me0 <- apply(Males, 2, mx2ex)
fe0 <- apply(Females, 2, mx2ex)

me0st <- acast(LT[LT$Sex == "m" & LT$Age == 0, ], Year ~ State, value.var = "ex")
fe0st <- acast(LT[LT$Sex == "f" & LT$Age == 0, ], Year ~ State, value.var = "ex")

getwd()
png("/hdir/0/triffe/workspace/other/Decomposition/StateDivergence/Figures/ExampleBestPractices.png")
plot(1959:2010, fRLE, type = 'l', main = "US States\nvanguard and best practices e(0)", col = "red", lty = 2, 
  ylim = c(55,90), xlab = "year", ylab = "e(0)")
lines(1959:2010, mRLE, col = "blue", lty = 2)
lines(1959:2004, me0,col="blue",lty=1,lwd=2)
lines(1959:2004, fe0,col="red",lty=1,lwd=2)
matplot(1959:2010, me0st, type='l',col="#0000FF20",lwd=2,lty=1,add=TRUE)
matplot(1959:2010, fe0st, type='l',col="#FF000020",lwd=2,lty=1,add=TRUE)
legend("bottomright", lty=c(1,1,2,2), col=c("red","blue","red","blue"),
legend=c("Female Best Prac.","Male Best Prac.","Female vanguard","Male vanguard"))
dev.off()



