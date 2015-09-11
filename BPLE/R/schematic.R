####################
# Adrien Remund


# Oeppen & Vaupel 2002 (data from EDSD)
rle <- read.table("C:/Users/REMUND/Dropbox/BPLE/Data/Jims.txt",header=T)
pchs <- match(rle$country,levels(rle$country))
plot(rle$year,rle$rle,ylim=c(70,90),xlim=c(1950,2000),pch=pchs,xlab="",ylab="e0",las=1,main="Expected evolution of the RLE and BPLE")
legend("bottomright",ncol=2,legend=levels(rle$country),pch=1:8)

# schematical
library(RColorBrewer)
cols <- brewer.pal(3,"Set1")
mat <- matrix(NA,ncol=4,nrow=length(1900:2020))
mat[,1] <- 1900:2020
mat[,2] <- 60+0.25*(0:120)
mat[,3] <- c(55+0.32*(0:55),55+0.32*55+0.23*(0:64))
breaks <- c(1900,1950,1970,1990,2020)
jumps <- c(0,5,3,2)
mat[1,4] <- 70
for(i in 2:5){mat[mat[,1] %in% breaks[i-1]:breaks[i],4] <- mat[mat[,1] == breaks[i-1],4]+jumps[i-1]+(breaks[i-1]:breaks[i])*0.1-breaks[i-1]*0.1}
matplot(mat[,1],mat[,2:4],las=1,ylim=c(50,100),col=cols,lty=1,lwd=2,type="s",xlab="time",ylab="e0",main="schematic evolution of life expectancy")
legend("bottomright",legend=c("Oeppen & Vaupel 2002","Vallin & Meslé 2010","BPLE"),col=cols,lwd=2)
