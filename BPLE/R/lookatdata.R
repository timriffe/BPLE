# Author: triffe
###############################################################################


States <- local(get(load("/hdir/0/triffe/workspace/other/Decomposition/StateDivergence/Data/MXClong.Rdata")))
LT     <- local(get(load("/hdir/0/triffe/workspace/other/Decomposition/StateDivergence/Data/MilaStatesSingleAge.Rdata")))
head(LT)
fRLE <- tapply(LT$ex[LT$Age == 0 & LT$Sex == "f"], LT$Year[LT$Age == 0& LT$Sex == "f"], max)
mRLE <- tapply(LT$ex[LT$Age == 0 & LT$Sex == "m"], LT$Year[LT$Age == 0& LT$Sex == "m"], max)

plot(unique(LT$Year), RLE, type = 'l', main = "US Record e(0)")
States$Cause <- as.character(States$Cause)

library(data.table)
States <- data.table(States)
Mins <- States[,min(mxc),by=list(Year, Sex, Age, Cause)]
Agg <- Mins[,sum(V1),by=list(Year, Sex, Age)]

library(reshape2)
Males <- acast(Agg[Agg$Sex == "male",], Age~Year, value.var = "V1")
Females <- acast(Agg[Agg$Sex == "female",], Age~Year, value.var = "V1")

mx2ex <- function(mx){
  lx <- c(1,exp(-cumsum(mx)),0)
  Lx <- (lx[-1] + lx[-length(lx)]) / 2
  sum(Lx)
}
me0 <- apply(Males, 2, mx2ex)
fe0 <- apply(Females, 2, mx2ex)
getwd()
png("/hdir/0/triffe/workspace/other/Decomposition/StateDivergence/Figures/ExampleBestPractices.png")
plot(1959:2010, fRLE, type = 'l', main = "US States\nvanguard and best practices e(0)", col = "red", lty = 2, 
  ylim = c(65,90), xlab = "year", ylab = "e(0)")

lines(1959:2010, mRLE, col = "blue", lty = 2)
lines(1959:2004, me0,col="blue")
lines(1959:2004, fe0,col="red")
legend("bottomright", lty=c(1,1,2,2), col=c("red","blue","red","blue"),
legend=c("Female Best Prac.","Male Best Prac.","Female vanguard","Male vanguard"))
dev.off()
