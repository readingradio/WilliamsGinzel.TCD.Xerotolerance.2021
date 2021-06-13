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

registerDoParallel(24)

surv.90s <- Gm.survival.avg(model, file_min_temp=tmin90s, file_max_temp=tmax90s, file_min_rh=hmin90s, file_max_rh=hmax90s, years=c(1995:1999), months=c(5:9))

pdf("Average cumulative yearly expected Gm survival 1995-1999 USA Map.pdf")
plot(rotate(surv.90s))
map(database="state", add=T)
dev.off()

save.image("90s.GmSurv.12.6.20.RData")

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