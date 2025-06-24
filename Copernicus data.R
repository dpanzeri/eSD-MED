#•	Download data from Copernicus



download_from_cop <- function(service, product, depth_min = 1.472, depth_max = 1.472, x_min, x_max, y_min, y_max, t0, t_max, variable, folder, file.name){
  system(paste("C:/Users/diego/anaconda3/Scripts/copernicusmarine.exe   subset --username dpanzeri --password DiegoCMEMS2017 --force-download --overwrite ",  
               "-i ",product, 
               "-z ",depth_min," -Z ", depth_max," -x ",x_min," -X ", x_max," -y ",y_min," -Y ", y_max," -t ", t0," -T ", t_max," --variable ", variable," -o ",folder," -f ",file.name), wait=T)
  
}
###############################################################################
#----------------------------------bottomSo------------------------------------
###############################################################################

download_from_cop(product = "med-cmcc-sal-rean-m", depth_min = 0, depth_max =  1000,
                  x_min = min(stack.point$x), x_max = max(stack.point$x), y_min = min(stack.point$y), y_max = max(stack.point$y), variable = "so", t0 = '1994-01-01',
                  t_max = '2023-12-31',folder = "C:/Users/diego/Downloads", file.name = "bottomSo.nc")

SoStack <- rast("bottomSo.nc")

full.set <- NULL

for (i in unique(stack.point$Cop_reanalysis_deptholvl_mask)) {
  sub.raster <- subset(stack.point, Cop_reanalysis_deptholvl_mask == i)
  so <- raster::brick("bottomSo.nc", level= i)
  so.value <- data.frame(x = sub.raster$x, y = sub.raster$y , raster::extract(so, sub.raster[,1:2]))
  full.set <- rbind(full.set, so.value)
  print(paste0("Processed deptho lvl: ",i))
}
num.date <- gsub("X","",names(full.set)[3:ncol(full.set)],fixed = T)
colnames(full.set)[3:ncol(full.set)] <- substr(as.POSIXct(as.numeric(num.date)*60, origin="1900-01-01", format="%Y-%m-%d", tz = "UTC"),1,10)

sal.bot <- data.table(melt(full.set,id.vars=c('x','y')))
colnames(sal.bot)[3:4] <- c('time','sal_bot')
saveRDS(sal.bot,'sal.bot.rds')

saveRDS(full.set,'df.bottom_sal_99_22.rds')

for (i in 3:ncol(full.set)) {
  temp.subset <- data.frame(x = full.set$x, y = full.set$y, z = full.set[,i])
  raster_new <- rasterFromXYZ(temp.subset, res = 0.04166667, crs = CRS("+proj=longlat +datum=WGS84") )
  raster::writeRaster(raster_new, filename = paste0("Output_nc/bottomSo_mean_",gsub("-","_",substr(time(full.set),1,7)),".tif"))
}

grid.neccton <- readRDS('sal.bot.rds')
grid.neccton$x <- round(grid.neccton$x,4)
grid.neccton$y <- round(grid.neccton$y,4)

grid.neccton$code <- apply(grid.neccton[,c('x','y','time')],1,paste0,collapse='_')
saveRDS(grid.neccton,'grid.neccton.rds')

###############################################################################
#-----------------------------THETAO--------------
###############################################################################

setwd("G:/Il mio Drive/NECCTON/NECCTON_prj/Output_nc")

download_from_cop(product = "med-cmcc-tem-rean-m", depth_min = 1.0182366371154785, depth_max =  1.0182366371154785,
                  x_min = min(stack.point$x), x_max = max(stack.point$x), y_min = min(stack.point$y), y_max = max(stack.point$y), variable = "thetao", t0 = '1994-01-01',
                  t_max = '2023-12-31',folder = "C:/Users/diego/Downloads", file.name = "thetao.nc")

thetaoStack <- rast("thetao.nc") # file impilati verticalmente 
thetaoStack.df <-  terra::as.data.frame(thetaoStack, xy = TRUE, na.rm = T) 
tmp.sur <- data.table(melt(thetaoStack.df,id.vars=c('x','y')))
tmp.sur$x <- round(tmp.sur$x,4)
tmp.sur$y <- round(tmp.sur$y,4)
tmp.sur$time <-  grid.neccton$time
tmp.sur$code <- apply(tmp.sur[,c('x','y','time')],1,paste0,collapse='_')

grid.neccton$tmp.sur <- tmp.sur$value[match(grid.neccton$code,tmp.sur$code )]


for (i in 1:nlyr(thetaoStack)) {
  sub.rast <- terra::subset(thetaoStack, i)
  writeRaster(sub.rast, filename = paste0("Output_nc/thetao_mean_",gsub("-","_",substr(time(sub.rast),1,7)),".tif"))
}

###############################################################################
#-------------------------------------------bottomT-------------
###############################################################################

download_from_cop(product = "med-cmcc-tem-rean-m", depth_min = 1.0182366371154785, depth_max =  1.0182366371154785,
                  x_min = min(stack.point$x), x_max = max(stack.point$x), y_min = min(stack.point$y), y_max = max(stack.point$y), variable = "bottomT", t0 = '1994-01-01',
                  t_max = '2022-12-31',folder = "C:/Users/diego/Downloads", file.name = "bottomT.nc")


bottomTStack <- rast("bottomT.nc")

for (i in 1:nlyr(bottomTStack)) {
  sub.rast <- terra::subset(bottomTStack, i)
  writeRaster(sub.rast, filename = paste0("Output_nc/bottomT_mean_",gsub("-","_",substr(time(sub.rast),1,7)),".tif"))
}


bottomTStack.df <-  as.data.frame(bottomTStack,xy = TRUE) 

#bottomT <- as.data.frame(bottomTStack, xy = TRUE, na.rm = T)


tmp_bot <- na.omit(data.table(melt(bottomTStack.df,id.vars=c('x','y'))))
tmp_bot$x <- round(tmp_bot$x,4)
tmp_bot$y <- round(tmp_bot$y,4)
tmp_bot$time <-  grid.neccton$time
tmp_bot$code <- apply(tmp_bot[,c('x','y','time')],1,paste0,collapse='_')

grid.neccton$tmp_bot <- tmp_bot$value[match(grid.neccton$code,tmp_bot$code )]


saveRDS(grid.neccton,'grid.neccton.rds')

###############################################################################
#----------------------------------bottomO2------------
###############################################################################

download_from_cop(product = "med-ogs-bio-rean-m", depth_min = 0, depth_max =  1000,
                  x_min = min(stack.point$x), x_max = max(stack.point$x), y_min = min(stack.point$y), y_max = max(stack.point$y), variable = "o2", t0 = '1999-01-01',
                  t_max = '2022-12-31',folder = "C:/Users/diego/Downloads", file.name = "o2.nc")

full.set <- NULL##


for (i in unique(stack.point$Cop_reanalysis_deptholvl_mask)) {
  sub.raster <- subset(stack.point, Cop_reanalysis_deptholvl_mask == i)
  o2 <- raster::brick("o2.nc", level= i)
  o2.value <- data.frame(x = sub.raster$x, y = sub.raster$y , raster::extract(o2, sub.raster[,1:2]))
  full.set <- rbind(full.set, o2.value)
  print(paste0("Processed deptho lvl: ",i))
}
num.date <- gsub("X","",names(full.set)[3:ncol(full.set)],fixed = T)
colnames(full.set)[3:ncol(full.set)] <- gsub(".","-", num.date, fixed = T)

bottomO2 <- full.set


O2_bot <- data.table(melt(bottomO2,id.vars=c('x','y')))
O2_bot$x <- round(O2_bot$x,4)
O2_bot$y <- round(O2_bot$y,4)
#O2_bot$time <-  grid.neccton$time
O2_bot$code <- apply(O2_bot[,c('x','y','variable')],1,paste0,collapse='_')

grid.neccton$O2_bot <- O2_bot$value[match(grid.neccton$code,O2_bot$code)]

setwd('C:/Users/diego/Downloads/Output_EOVs')
saveRDS(grid.neccton,'grid.neccton.rds')


for (i in 3:ncol(full.set)) {
  temp.subset <- data.frame(x = full.set$x, y = full.set$y, z = full.set[,i])
  raster_new <- rasterFromXYZ(temp.subset, res = 0.04166667, crs = CRS("+proj=longlat +datum=WGS84") )
  raster::writeRaster(raster_new, filename = paste0("Output_EOVs_o2Bot",gsub("-","_", substr(colnames(full.set)[i], 1,7)),".tif"))
}


####################################################################################################################
#------------------------------------ChlIntegrato 
####################################################################################################################
library(RNetCDF)
setwd("G:/Il mio Drive/NECCTON/Ares_2023_1674196/Ares_2023_1674196/CHL_0-200")
files= list.files(pattern='*.nc',full.names=TRUE)

res <- list()
for(i in 1:length(files)){
  nc =  raster(files[i])
  t <- paste(substr(files[i],7,10),'-',substr(files[i],11,12),'-',substr(files[i],13,14),sep='')
  IntChl <- as.data.frame(nc, xy = TRUE, na.rm = T)
  IntChl$time <- t
  res[[i]] <- IntChl
}
IntChl.f <- data.table(list.rbind(res))

IntChl.f$x <- round(IntChl.f$x,4)
IntChl.f$y <- round(IntChl.f$y,4)
IntChl.f$code <- apply(IntChl.f[,c('x','y','time')],1,paste0,collapse='_')

grid.neccton$chl_int <- IntChl.f$P_l[match(grid.neccton$code,IntChl.f$code)]

setwd('C:/Users/diego/Downloads/Output_EOVs')
saveRDS(grid.neccton,'grid.neccton.rds')


###############################################################################
#------------------------------POC Integrato 
###############################################################################

setwd("G:/Il mio Drive/NECCTON/Ares_2023_1674196/Ares_2023_1674196/POC_0-200")
files= list.files(pattern='*.nc',full.names=TRUE)

res <- list()
for(i in 1:length(files)){
  nc =  raster(files[i])
  t <- paste(substr(files[i],7,10),'-',substr(files[i],11,12),'-',substr(files[i],13,14),sep='')
  IntPoc <- as.data.frame(nc, xy = TRUE, na.rm = T)
  IntPoc$time <- t
  res[[i]] <- IntPoc
}
IntPoc.f <- data.table(list.rbind(res))



IntPoc.f$x <- round(IntPoc.f$x,4)
IntPoc.f$y <- round(IntPoc.f$y,4)
IntPoc.f$code <- apply(IntPoc.f[,c('x','y','time')],1,paste0,collapse='_')

grid.neccton$poc_int <- IntPoc.f$POC[match(grid.neccton$code,IntPoc.f$code)]

setwd('C:/Users/diego/Downloads/Output_EOVs')
saveRDS(grid.neccton,'grid.neccton.rds')


###############################################################################
#---------------------------------POC bottom GIORGIO----------
###############################################################################
setwd("G:/Il mio Drive/NECCTON/Ares_2023_1674196/Ares_2023_1674196/POC_bottom")
files= list.files(pattern='*.nc',full.names=TRUE)

res <- list()
for(i in 1:length(files)){
  nc =  raster(files[i])
  t <- paste(substr(files[i],7,10),'-',substr(files[i],11,12),'-',substr(files[i],13,14),sep='')
  IntPoc <- as.data.frame(nc, xy = TRUE, na.rm = T)
  IntPoc$time <- t
  res[[i]] <- IntPoc
}
BotPoc <- list.rbind(res)

BotPoc$x <- round(BotPoc$x,4)
BotPoc$y <- round(BotPoc$y,4)
BotPoc$code <- apply(BotPoc[,c('x','y','time')],1,paste0,collapse='_')

grid.neccton$poc_bot <- BotPoc$POC[match(grid.neccton$code,BotPoc$code)]

setwd('C:/Users/diego/Downloads/Output_EOVs')
saveRDS(grid.neccton,'grid.neccton.rds')



###############################################################################
#----------------------------------------------------------bottom NO2----------
###############################################################################

download_from_cop(product = "med-ogs-nut-rean-m", depth_min = 0, depth_max =  1000,
                  x_min = min(stack.point$x), x_max = max(stack.point$x), y_min = min(stack.point$y), y_max = max(stack.point$y), variable = "no3", t0 = '1999-01-01',
                  t_max = '2022-12-31',folder = "C:/Users/diego/Downloads", file.name = "no3.nc")

full.set <- NULL##

setwd("C:/Users/diego/Downloads")

for (i in unique(stack.point$Cop_reanalysis_deptholvl_mask)) {
  sub.raster <- subset(stack.point, Cop_reanalysis_deptholvl_mask == i)
  no3 <- raster::brick("no3.nc", level= i)
  no3.value <- data.frame(x = sub.raster$x, y = sub.raster$y , raster::extract(no3, sub.raster[,1:2]))
  full.set <- rbind(full.set, no3.value)
  print(paste0("Processed deptho lvl: ",i))
}
num.date <- gsub("X","",names(full.set)[3:ncol(full.set)],fixed = T)
colnames(full.set)[3:ncol(full.set)] <- gsub(".","-", num.date, fixed = T)

bottomNo3 <- full.set


NO3_bot <- data.table(melt(bottomNo3,id.vars=c('x','y')))
NO3_bot$x <- round(NO3_bot$x,4)
NO3_bot$y <- round(NO3_bot$y,4)
#O2_bot$time <-  grid.neccton$time
NO3_bot$code <- apply(NO3_bot[,c('x','y','variable')],1,paste0,collapse='_')

grid.neccton$NO3_bot <- NO3_bot$value[match(grid.neccton$code,NO3_bot$code)]

setwd('C:/Users/diego/Downloads/Output_EOVs')
saveRDS(grid.neccton,'grid.neccton.rds')


###############################################################################
#--------------------------------------------------bottom PO4----------
###############################################################################

download_from_cop(product = "med-ogs-nut-rean-m", depth_min = 0, depth_max =  1000,
                  x_min = min(stack.point$x), x_max = max(stack.point$x), y_min = min(stack.point$y), y_max = max(stack.point$y), variable = "po4", t0 = '1999-01-01',
                  t_max = '2022-12-31',folder = "C:/Users/diego/Downloads", file.name = "po4.nc")

full.set <- NULL##

setwd("C:/Users/diego/Downloads")

for (i in unique(stack.point$Cop_reanalysis_deptholvl_mask)) {
  sub.raster <- subset(stack.point, Cop_reanalysis_deptholvl_mask == i)
  po4 <- raster::brick("po4.nc", level= i)
  po4.value <- data.frame(x = sub.raster$x, y = sub.raster$y , raster::extract(po4, sub.raster[,1:2]))
  full.set <- rbind(full.set, po4.value)
  print(paste0("Processed deptho lvl: ",i))
}
num.date <- gsub("X","",names(full.set)[3:ncol(full.set)],fixed = T)
colnames(full.set)[3:ncol(full.set)] <- gsub(".","-", num.date, fixed = T)

bottomPo4 <- full.set


PO4_bot <- data.table(melt(bottomPo4,id.vars=c('x','y')))
PO4_bot$x <- round(PO4_bot$x,4)
PO4_bot$y <- round(PO4_bot$y,4)
PO4_bot$code <- apply(PO4_bot[,c('x','y','variable')],1,paste0,collapse='_')

grid.neccton$PO4_bot <- PO4_bot$value[match(grid.neccton$code,PO4_bot$code)]

grid.neccton$year <- substr(grid.neccton$time,1,4)
grid.neccton$month <- substr(grid.neccton$time,6,7)


grid.neccton <- readRDS('grid.neccton.rds')
depth.noNa <- readRDS("C:/Users/dpanzeri/OneDrive - OGS/googleDrive/NECCTON/NECCTON_prj/depth.noNa.rds")
depth.noNa$Var1 <- format(round(as.numeric(depth.noNa$Var1), 3), nsmall = 3)
depth.noNa$Var2 <- format(round(as.numeric(depth.noNa$Var2), 3), nsmall = 3)

depth.noNa$codeXY <- apply(depth.noNa[,c('Var1','Var2')],1,paste0,collapse='_')


grid.neccton$x3 <- format(round(grid.neccton$x, 3), nsmall = 3)
grid.neccton$y3 <- format(round(grid.neccton$y, 3), nsmall = 3)

grid.neccton$codeXY <- apply(grid.neccton[,c('x3','y3')],1,paste0,collapse='_')
grid.neccton$depth <- depth.noNa$value[match(grid.neccton$codeXY,depth.noNa$codeXY)]


"C:/Users/dpanzeri/OneDrive - OGS/googleDrive/NECCTON/NECCTON_prj"
saveRDS(grid.neccton,'grid.neccton.rds')
