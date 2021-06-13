# libraries

library(sp)
library(rgdal)

setwd('/scratch/brown/will1809')

source("Xero.MACA.Survival.Map.functions.R")

# loads m$models$Gm.1st
load("Xeromodel.RData")

# set up climate data files

tmax90s <- "macav2metdata_tasmax_bcc-csm1-1_r1i1p1_historical_1995_1999_CONUS_daily.nc"
tmin90s <- "macav2metdata_tasmin_bcc-csm1-1_r1i1p1_historical_1995_1999_CONUS_daily.nc"
hmax90s <- "macav2metdata_rhsmax_bcc-csm1-1_r1i1p1_historical_1995_1999_CONUS_daily.nc"
hmin90s <- "macav2metdata_rhsmin_bcc-csm1-1_r1i1p1_historical_1995_1999_CONUS_daily.nc"
tmax00s <- "macav2metdata_tasmax_bcc-csm1-1_r1i1p1_historical_2000_2004_CONUS_daily.nc"
tmin00s <- "macav2metdata_tasmin_bcc-csm1-1_r1i1p1_historical_2000_2004_CONUS_daily.nc"
hmax00s <- "macav2metdata_rhsmax_bcc-csm1-1_r1i1p1_historical_2000_2004_CONUS_daily.nc"
hmin00s <- "macav2metdata_rhsmin_bcc-csm1-1_r1i1p1_historical_2000_2004_CONUS_daily.nc"

China.tmax.rcp45.2031 <- "macav2metdata_tasmax_bcc-csm1-1_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"
China.tmin.rcp45.2031 <- "macav2metdata_tasmin_bcc-csm1-1_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"
China.hmax.rcp45.2031 <- "macav2metdata_rhsmax_bcc-csm1-1_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"
China.hmin.rcp45.2031 <- "macav2metdata_rhsmin_bcc-csm1-1_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"

China.tmax.rcp45.2036 <- "macav2metdata_tasmax_bcc-csm1-1_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"
China.tmin.rcp45.2036 <- "macav2metdata_tasmin_bcc-csm1-1_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"
China.hmax.rcp45.2036 <- "macav2metdata_rhsmax_bcc-csm1-1_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"
China.hmin.rcp45.2036 <- "macav2metdata_rhsmin_bcc-csm1-1_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"

GDFL.tmax.rcp45.2031 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"
GDFL.tmin.rcp45.2031 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"
GDFL.hmax.rcp45.2031 <- "macav2metdata_rhsmax_GFDL-ESM2M_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"
GDFL.hmin.rcp45.2031 <- "macav2metdata_rhsmix_GFDL-ESM2M_r1i1p1_rcp45_2031_2035_CONUS_daily.nc"

GDFL.tmax.rcp45.2036 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"
GDFL.tmin.rcp45.2036 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"
GDFL.hmax.rcp45.2036 <- "macav2metdata_rhsmax_GFDL-ESM2M_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"
GDFL.hmin.rcp45.2036 <- "macav2metdata_rhsmix_GFDL-ESM2M_r1i1p1_rcp45_2036_2040_CONUS_daily.nc"

GDFL.tmax.rcp45.2051 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2051_2055_CONUS_daily.nc"
GDFL.tmin.rcp45.2051 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2051_2055_CONUS_daily.nc"
GDFL.hmax.rcp45.2051 <- "macav2metdata_rhsmax_GFDL-ESM2M_r1i1p1_rcp45_2051_2055_CONUS_daily.nc"
GDFL.hmin.rcp45.2051 <- "macav2metdata_rhsmix_GFDL-ESM2M_r1i1p1_rcp45_2051_2055_CONUS_daily.nc"

GDFL.tmax.rcp45.2056 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2056_2040_CONUS_daily.nc"
GDFL.tmin.rcp45.2056 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2056_2040_CONUS_daily.nc"
GDFL.hmax.rcp45.2056 <- "macav2metdata_rhsmax_GFDL-ESM2M_r1i1p1_rcp45_2056_2040_CONUS_daily.nc"
GDFL.hmin.rcp45.2056 <- "macav2metdata_rhsmix_GFDL-ESM2M_r1i1p1_rcp45_2056_2040_CONUS_daily.nc"

GDFL.tmax.rcp45.2071 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2071_2075_CONUS_daily.nc"
GDFL.tmin.rcp45.2071 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2071_2075_CONUS_daily.nc"
GDFL.hmax.rcp45.2071 <- "macav2metdata_rhsmax_GFDL-ESM2M_r1i1p1_rcp45_2071_2075_CONUS_daily.nc"
GDFL.hmin.rcp45.2071 <- "macav2metdata_rhsmix_GFDL-ESM2M_r1i1p1_rcp45_2071_2075_CONUS_daily.nc"

GDFL.tmax.rcp45.2076 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2076_2040_CONUS_daily.nc"
GDFL.tmin.rcp45.2076 <- "macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2076_2040_CONUS_daily.nc"
GDFL.hmax.rcp45.2076 <- "macav2metdata_rhsmax_GFDL-ESM2M_r1i1p1_rcp45_2076_2040_CONUS_daily.nc"
GDFL.hmin.rcp45.2076 <- "macav2metdata_rhsmix_GFDL-ESM2M_r1i1p1_rcp45_2076_2040_CONUS_daily.nc"

files <- expand.grid(m=c("ESM2M","ESM2G"), co2=c(45,85), y=c(2031,2036,2051,2056,2071,2076))

registerDoParallel(24)

surv.stack <- foreach (i = 1:24, .combine=stack, .packages="raster") %dopar% {
		
	file.suffix <- with(files[i,], paste(m, "_r1i1p1_rcp", as.character(co2), "_", as.character(y), "_", as.character(y+4), "_CONUS_daily.nc", sep=""))
	tmin <- paste("macav2metdata_tasmin_GFDL-", file.suffix, sep="")
	tmax <- paste("macav2metdata_tasmax_GFDL-", file.suffix, sep="")
	hmin <- paste("macav2metdata_rhsmin_GFDL-", file.suffix, sep="")
	hmax <- paste("macav2metdata_rhsmax_GFDL-", file.suffix, sep="")

	surv <- Gm.survival.year.raster(model=m$models$Gm.1st, file_min_temp=tmin, file_max_temp=tmax, file_min_rh=hmin, file_max_rh=hmax, year=files$y[i], months=c(5:9), parallel=FALSE)
	names(surv) <- with(files[i,],paste(m,co2,y,sep="."))

	pdf(with(files[i,],paste("ClimateFigs/Average.Cum.Yrly.Exp.Gm.Surv.", as.character(y), ".RCP", as.character(co2), ".", m, ".pdf", sep="")))
	plot(rotate(surv), axes=F, box=F, zlim=c(0,1))
	map(database="state", add=T)
	dev.off()

	surv
}


save.image("Future.GmSurv.12.6.20.Scenarios.RData")

#surv.rcp45.2031 <- Gm.survival.year.raster(model=m$models$Gm.1st, file_min_temp=tmin.rcp45.2031, file_max_temp=tmax.rcp45.2031, file_min_rh=hmin.rcp45.2031, file_max_rh=hmax.rcp45.2031, year=2031, months=c(5:9))
#surv.rcp45.2035 <- Gm.survival.year.raster(model=m$models$Gm.1st, file_min_temp=tmin.rcp45.2031, file_max_temp=tmax.rcp45.2031, file_min_rh=hmin.rcp45.2031, file_max_rh=hmax.rcp45.2031, year=2035, months=c(5:9))
#surv.rcp45.2040 <- Gm.survival.year.raster(model=m$models$Gm.1st, file_min_temp=tmin.rcp45.2036, file_max_temp=tmax.rcp45.2036, file_min_rh=hmin.rcp45.2036, file_max_rh=hmax.rcp45.2036, year=2040, months=c(5:9))

#pdf("Average cumulative yearly expected Gm survival 2031 RCP 4.5 noaxes.pdf")
#plot(rotate(surv.rcp45.2031), axes=F, box=F)
#map(database="state", add=T)
#dev.off()

#pdf("Average cumulative yearly expected Gm survival 2035 RCP 4.5 noaxes.pdf")
#plot(rotate(surv.rcp45.2035), axes=F, box=F)
#map(database="state", add=T)
#dev.off()

#pdf("Average cumulative yearly expected Gm survival 2040 RCP 4.5 noaxes.pdf")
#plot(rotate(surv.rcp45.2040), axes=F, box=F)
#map(database="state", add=T)
#dev.off()

#save.image("RCP4.5.GmSurv.12.6.20.RData")

#surv.00s <- Gm.survival.avg(m$models$Gm.1st, file_min_temp=tmin00s, file_max_temp=tmax00s, file_min_rh=hmin00s, file_max_rh=hmax00s, years=c(2000:2004), months=c(5:9))

#pdf("Average cumulative yearly expected Gm survival 2000-2004 USA Map.pdf")
#plot(rotate(surv.00s))
#map(database="state", add=T)
#dev.off()

#save.image("00s.GmSurv.12.6.20.RData")

#load("00s.GmSurv.12.6.20.RData")
#load("90s.GmSurv.12.6.20.RData")

#surv.95.04 <- mean(surv.90s, surv.00s)

#pdf("Average cumulative yearly expected Gm survival 1995-2004 USA Map.pdf")
#plot(rotate(surv.95.04))
#map(database="state", add=T)
#dev.off()

# Gm.survival.avg calls Gm.survival.year.raster
# Gm.survival.year.raster calls Gm.survival.month.raster
# Gm.survival.month.raster calls emc.map.MACA.month.raster
# emc.map.MACA.month.raster calls map.variable.avg.MACA.raster

# first test map.variable.avg.MACA

#days.in.a.month <- apply(expand.grid(1:31,"5","1995")[,c(3,2,1)], 1, paste, collapse="-") %>% as.Date %>% na.omit %>% format("%Y-%m-%d")

#test.temp <- map.variable.avg.MACA.raster(tmin90s, tmax90s, "air_temperature", days.in.a.month)

#plot avg temp
#pdf("Avg air temperature May 1995 USA Map.pdf")
#plot(rotate(test.temp))
#map(database="state", add=T)
#dev.off()

# Test emc.map.MACA.month
#emc.1995.5 <- emc.map.MACA.month.raster (tmin90s, tmax90s, hmin90s, hmax90s, 1995, 5)
#emc.1995.5
#plot EMC
#pdf("Expected average wood moisture content May 1995 USA Map.pdf")
#plot(rotate(emc.1995.5))
#map(database="state", add=T)
#dev.off()

# plot G. morbida survival for May 1995

#Gm.surv.1995.5 <- calc(emc.1995.5, fun = function(x) predict(m$models$Gm.1st, newdata=data.frame(mc=x)) %>% odds.to.rate)

#pdf("Expected Gm survival May 1995 USA Map.pdf")
#plot(rotate(Gm.surv.1995.5))
#map(database="state", add=T)
#dev.off()

# find cumulative survival rate across years and average them

#plot(s)
#cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
#plt <- levelplot(s, margin = F, at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme,
#  main="January temperature")
#plt + layer(sp.lines(world_outline, col="black", lwd=1.0))

# Try running emc.max.map.MACA

#emc.max.1995 <- emc.max.map.MACA  (tmin90s, tmax90s, hmin90s, hmax90s, 1995)