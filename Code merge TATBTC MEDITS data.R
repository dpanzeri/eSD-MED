#	Code for merging TA and TB file
formatLat = function(latmed, lonmed, quadrant){
  frac <- function(x) abs( x - trunc(x)) # to get a fractional part of the number
  lonDeg = floor(lonmed/100) # degress
  lonMin = lonmed/100
  lonMin = frac(lonMin)/60*100
  lon = lonDeg+lonMin
  lon = ifelse(quadrant==1, lon, -1*lon) # getting right sign
  latDeg = floor(latmed/100) # degress
  latMin = latmed/100
  latMin = frac(latMin)/60*100
  lat = latDeg+latMin
  
  return(rbind(lat,lon))
  
}

'TA'
setwd("G:/Il mio Drive/NECCTON/Ares_2023_1674196/Ares_2023_1674196")
A <-  read_csv("MED_TA.csv")#fread("G:\\Il mio Drive\\PhD\\data\\MEDITS\\Medits_TableA.csv")
converted_coord <- formatLat(A$hauling_latitude, A$hauling_longitude, A$hauling_quadrant)
A$new_latitude <- converted_coord[1,]
A$new_longitude <- converted_coord[2,]
#mapview(A, xcol = "new_longitude", ycol="new_latitude", crs= 4326, grid=F)
A$swept.area <- (A$distance * (A$wing_opening/10))/1000000
A$date <- paste0(A$year,"-",A$month,"-",A$day)
A$code <- paste0(A$country,"_",A$area,"_",A$year,"_",A$haul_number)# non posso costruire il codice solo con l'anno perchè ci sarebbero delle repliche
A_v2 <- A[,c("area","code","date","new_longitude","new_latitude","hauling_depth","swept.area","validity")]
length(unique(A_v2$code))# 30871 cale
colnames(A_v2) <- c("area","code","date",'X','Y','depth',"swept.area","validity")
A_v2$X <- ifelse(A_v2$area ==c(1,2),A_v2$X* -1,A_v2$X)
A_v2 <- A_v2[A_v2$X>-10,]
A_v2$X <- as.numeric(format(round(as.numeric(A_v2$X), 4), nsmall = 4))
A_v2$Y <- as.numeric(format(round(as.numeric(A_v2$Y), 4), nsmall = 4))

grid.neccton.sub <- grid.neccton[grid.neccton$time=='2022-01-01' & grid.neccton$depth<=800,]

index <- c()
for(i in 1:nrow(A_v2)){   # estraggo il numero riga in cui la differenza tra le lat è minore
  index[i] <- which.min(distGeo(A_v2[i,c('X','Y')],grid.neccton.sub[,c('x','y')],a=6378137))
}

A_v2$index <- index


A_v2$year <- substr(A_v2$date,1,4)
A_v2$month <- substr(A_v2$date,6,7)

A_v2$month <- ifelse(A_v2$month== "5-",'05',
                     ifelse(A_v2$month=="6-",'06',
                            ifelse(A_v2$month=="8-",'08',
                                   ifelse(A_v2$month=="7-",'07',
                                          ifelse(A_v2$month=="9-",'09',
                                                 ifelse(A_v2$month=="2-",'02',
                                                        ifelse(A_v2$month=="4-",'04',A_v2$month)))))))

A_v2$codeIndex <- apply(A_v2[,c('index','year','month')],1,paste0,collapse='')
updated_uniques <- str_replace_all(A_v2$codeIndex, " ", "")
A_v2$codeIndex <- updated_uniques

setwd("C:/Users/dpanzeri/OneDrive - OGS/googleDrive/NECCTON/NECCTON_prj")
saveRDS(A_v2,'A_v2.rds')

grid.neccton.800 <- grid.neccton[grid.neccton$depth<=800,] 

spl <- split(grid.neccton.800,grid.neccton.800$time)
spl2 <-lapply(spl, function (x) {
  x$nrow <- seq(1:nrow(x))
  return(x)
})

grid.neccton.800 <- list.rbind(spl2)
grid.neccton.800$index <- apply(grid.neccton.800[,c('nrow','year','month')],1,paste0,collapse='')
updated_string.msfd <- str_replace_all(grid.neccton.800$index, " ", "")
grid.neccton.800$index <- updated_string.msfd


A_v2$tmp_bot <-grid.neccton.800$tmp_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$sal_bot <- grid.neccton.800$sal_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$tmp.sur <- grid.neccton.800$tmp.sur[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$O2_bot <- grid.neccton.800$O2_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$chl_int <- grid.neccton.800$chl_int[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$poc_int <- grid.neccton.800$poc_int[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$poc_bot <- grid.neccton.800$poc_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$NO3_bot <- grid.neccton.800$NO3_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$PO4_bot <- grid.neccton.800$PO4_bot[match(A_v2$codeIndex,grid.neccton.800$index)]


grid.neccton.800 <- grid.neccton[grid.neccton$depth<=800,] 

spl <- split(grid.neccton.800,grid.neccton.800$time)
spl2 <-lapply(spl, function (x) {
  x$nrow <- seq(1:nrow(x))
  return(x)
})

grid.neccton.800 <- list.rbind(spl2)
grid.neccton.800$index <- apply(grid.neccton.800[,c('nrow','year','month')],1,paste0,collapse='')
updated_string.msfd <- str_replace_all(grid.neccton.800$index, " ", "")
grid.neccton.800$index <- updated_string.msfd


A_v2$tmp_bot <-grid.neccton.800$tmp_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$sal_bot <- grid.neccton.800$sal_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$tmp.sur <- grid.neccton.800$tmp.sur[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$O2_bot <- grid.neccton.800$O2_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$chl_int <- grid.neccton.800$chl_int[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$poc_int <- grid.neccton.800$poc_int[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$poc_bot <- grid.neccton.800$poc_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$NO3_bot <- grid.neccton.800$NO3_bot[match(A_v2$codeIndex,grid.neccton.800$index)]
A_v2$PO4_bot <- grid.neccton.800$PO4_bot[match(A_v2$codeIndex,grid.neccton.800$index)]

A_v2.99 <- A_v2[A_v2$year>=1999,]
spl.A_v2.y <- split(A_v2.99,A_v2.99$year)


'TB'
B <- read_csv("MED_TB.csv") #fread("G:\\Il mio Drive\\PhD\\data\\MEDITS\\Medits_TableB.csv")
B$code <- paste0(B$country,"_",B$area,"_",B$year,"_",B$haul_number)
B$SPECIE <- paste0(B$genus,"-",B$species)
B_v2 <- B[,c("code",'year',"SPECIE","catfau","nbtot","ptot")]
B_v2.99 <- B_v2[B_v2$year>=1999,] 
length(unique(B_v2.99$code))# 30764 cale
saveRDS(B_v2.99,'B_v2.99.rds')
B_v2.99 <- readRDS('B_v2.99.rds')

spl.B <- split(B_v2.99,B_v2.99$SPECIE)
spl.TATB.99 <- lapply(spl.B, function (x){
  AB <- merge(A_v2.99,x, by="code", all.x=T)
  
  AB$PA <- ifelse(is.na(AB$ptot)==T,0,1)
  AB$ptot <- ifelse(AB$PA==0,0,AB$ptot/1000)
  AB$kg_km2 <-  AB$ptot/AB$swept.area
  AB$SPECIE <- unique(na.omit(AB$SPECIE))
  AB$year <- as.numeric(AB$year.x)
  AB$month <- as.numeric(AB$month)
  
  return(AB)
})
saveRDS(spl.TATB.99,'spl.TATB.99.rds') # ptot già in kg
spl.TATB.99 <- readRDS('spl.TATB.99.rds')


spl.TATB.99_spNeccton<- list()

spl.TATB.99_spNeccton [['ENGR_ENC']] <- spl.TATB.99$`ENGR-ENC`
spl.TATB.99_spNeccton [['SARD_PIL']] <- spl.TATB.99$`SARD-PIL`
spl.TATB.99_spNeccton [['MULL_BAR']] <- spl.TATB.99$`MULL-BAR`
spl.TATB.99_spNeccton [['MERL_MER']] <- spl.TATB.99$`MERL-MER`
spl.TATB.99_spNeccton [['ARIS_FOL']] <- spl.TATB.99$`ARIS-FOL`
spl.TATB.99_spNeccton [['PAPE_LON']] <- spl.TATB.99$`PAPE-LON`

#UTM coordinates
f.utm <- function(data){
  data <- add_utm_columns(
    data,
    ll_names = c("X", "Y"),
    utm_names = c("X.utm", "Y.utm"))
  return(data)
}

spl.TATB.99_spNeccton <- lapply(spl.TATB.99_spNeccton, f.utm)
spl.TATB.99_spNeccton$`ENGR-ENC`$bati <- 'pelagic'
spl.TATB.99_spNeccton$`SARD-PIL`$bati <- 'pelagic'
spl.TATB.99_spNeccton$`MULL-BAR`$bati <- 'pelagic'
spl.TATB.99_spNeccton$`MERL-MER`$bati <- 'demersal'
spl.TATB.99_spNeccton$`ARIS-FOL`$bati <- 'demersal'
spl.TATB.99_spNeccton$`PAPE-LON`$bati <- 'demersal'

saveRDS(spl.TATB.99_spNeccton,' spl.TATB.99_spNeccton.rds')


'TC'
MED_TC <- read_csv("C:/Users/dpanzeri/OneDrive - OGS/googleDrive/NECCTON/Ares_2023_1674196/Ares_2023_1674196/MED_TC.csv")

MED_TC$code <- paste0(MED_TC$country,"_",MED_TC$area,"_",MED_TC$year,"_",MED_TC$haul_number)
MED_TC$SPECIE <- paste0(MED_TC$genus,"-",MED_TC$species)
TC_v2 <- MED_TC[,c("code",'year',"SPECIE","catfau",'length_class',"nblon","pfrac","pechan")]
TC_v2.99 <- TC_v2[TC_v2$year>=1999,]
saveRDS(TC_v2.99,'TC_v2.99.rds')
TC_v2.99 <- readRDS('TC_v2.99.rds')

'merge TA TC'
spl.TC_99 <-  split(TC_v2.99,TC_v2.99$SPECIE)

spl.TATC <-  lapply(spl.TC_99, function (x){
  AC.2 <- merge(A_v2.99,x, by="code", all.x=T)
  AC.2$PA <- ifelse(is.na(AC.2$nblon)==T,0,1)
  AC.2$nblon <- ifelse(AC.2$PA==0,0,AC.2$nblon)
  AC.2$N_km2 <- AC.2$nblon/AC.2$swept.area
  AC.2$SPECIE <- unique(na.omit(AC.2$SPECIE))
  AC.2$length_class <- ifelse(is.na(AC.2$length_class)==T,0,AC.2$length_class)
  AC.2$year <- as.numeric(AC.2$year.x)
  AC.2$month <- as.numeric(AC.2$month)
  return(AC.2)
})
saveRDS(spl.TATC,'spl.TATC.rds')
spl.TATC <- readRDS('spl.TATC.rds')
