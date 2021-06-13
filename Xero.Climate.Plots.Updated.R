
setwd("/Users/will1809/OneDrive - purdue.edu/TCD Xerotolerance/Climate Analyses Dec 2020")

source("Xero.MACA.Survival.Map.functions.R")

load("00s.GmSurv.12.6.20.RData")

load("90s.GmSurv.12.6.20.RData")

load("Future.GmSurv.12.7.20.Scenarios.RData")

surv.95.04 <- mean(surv.90s, surv.00s)

files

surv.stack

# multipanel

	#quartz(width=16,height=5)
	pdf("Climate.Composite.EMC.Gm.Feb2.2021.pdf",width=15,height=5)
	par(oma=c(0,0,0,0), xpd=T)

	par(fig=c(0,0.5,0,1), mar=c(0,0,0,0))
	plot(rotate(surv.95.04), box=F, axes=F, zlim=c(0,1), legend=F, main="\n\n(A) Historical 1995 - 2004")
	par(fig=c(0,0.5,0,1), mar=c(0,0,0,0))
	map(database="state", add=T)
	par(fig=c(0,0.5,0,0.65), mar=c(0,0,0,0))
	plot(surv.95.04, legend.only=TRUE, horizontal =T, zlim=c(0,1))
	
	par(fig=c(0.5,0.75,0.5,1), mar=c(0,0,0,0),new=TRUE)
	plot(rotate(subset(surv.stack, which(names(surv.stack)=="ESM2M.45.2031"))), box=F, axes=F, zlim=c(0,1), legend=F, main="\n(B) Low-emission 2031")
	par(fig=c(0.5,0.75,0.5,1), mar=c(0,0,0,0))
	map(database="state", add=T)

	par(fig=c(0.75,1,0.5,1), mar=c(0,0,0,0),new=TRUE)
	plot(rotate(subset(surv.stack, which(names(surv.stack)=="ESM2M.45.2071"))), box=F, axes=F, zlim=c(0,1), legend=F, main="\n(C) Low-emission 2071")
	par(fig=c(0.75,1,0.5,1), mar=c(0,0,0,0), xpd=T, oma=c(0,0,0,0))
	map(database="state", add=T)
	
	par(fig=c(0.5,0.75,0,0.5), mar=c(0,0,0,0),new=TRUE)
	plot(rotate(subset(surv.stack, which(names(surv.stack)=="ESM2M.85.2031"))), box=F, axes=F, zlim=c(0,1), legend=F, main="\n(D) High-emission 2031")
	par(fig=c(0.5,0.75,0,0.5), mar=c(0,0,0,0))
	map(database="state", add=T)

	par(fig=c(0.75,1,0,0.5), mar=c(0,0,0,0),new=TRUE)
	plot(rotate(subset(surv.stack, which(names(surv.stack)=="ESM2M.85.2071"))), box=F, axes=F, zlim=c(0,1), legend=F, main="\n(E) High-emission 2071")
	par(fig=c(0.75,1,0,0.5), mar=c(0,0,0,0), xpd=T, oma=c(0,0,0,0))
	map(database="state", add=T)

dev.off()
