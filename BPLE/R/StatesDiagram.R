# TODO: Add comment
# 
# Author: tim
###############################################################################
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/BPLE/BPLE")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/BPLE/BPLE"))
}

ST <- matrix(c("AK",rep("",10),"ME",
		rep("",10),"VT","NH",
		"","WA","ID","MT","ND","MN","IL","WI","MI","NY","RI","MS",
		"","OR","NV","WY","SD","IA","IN","OH","PA","NJ","CT","",
		"","CA","UT","CO","NB","MO","KY","WV","VA","MD","DL","",
		"","","AZ","NM","KS","AR","TN","NC","SC","DC","","",
		"","","","","OK","LA","MS","AL","GA","","","",
		"HA","","","","TX","","","","","FL","",""),ncol = 12, nrow = 8,byrow=TRUE)

ST[ST == ""] <- NA
Color <- matrix(NA,nrow=nrow(ST),ncol=ncol(ST))
Color[!is.na(ST[8:1,])] <- gray(.5)

x1 <- col(ST) - 1
x2 <- col(ST)
y1 <- row(ST) - 1
y2 <- row(ST)

#coords <- centercoords(x1,x2,y1,y2)

pdf("Figures/StatesDiagram.pdf",width=12,height=9)
par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
plot(NULL, xlim = c(0,12),ylim=c(0,9),xlab = "", ylab = "", type= "n", axes = FALSE,asp=1)
rect(x1,y1,x2,y2,border="white",col = Color)
text((x1+x2)/2,(y1+y2)/2,ST[8:1,],col="white",cex=2)
dev.off()
