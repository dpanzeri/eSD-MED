tinyVAST per size
f.data.size_nblon <- function(data,size,bati){
  
  data <- data[data$name_of_survey.x=='MEDITS',]
  data <- data[data$year<=2022,]
  data$length_class <- ifelse(is.na(data$length_class),0,data$length_class)
  data$size <- ifelse(data$length_class<size,'juv','ad')  
  
  data <- data[data$length_class<= quantile(data$length_class,0.9999),]
  data <- combine_bins(data)
  
  data$sc.depth <- scale(data$depth)
  data$sc.tmp_bot <- scale(data$tmp_bot)
  data$sc.sal_bot <- scale(data$sal_bot)
  data$sc.tmp.sur <- scale(data$tmp.sur)
  data$sc.O2_bot <- scale(data$O2_bot)
  data$sc.chl_int <- scale(data$chl_int)
  data$sc.poc_int <- scale(data$poc_int)
  data$sc.poc_bot <- scale(data$poc_bot)
  #data$sc.NO3_bot <- scale(data$NO3_bot)
  #data$sc.PO4_bot <- scale(data$PO4_bot)
  
  data$fyear <- as.factor(data$year)
  
  colnames(data)[4:5] <- c('lon','lat')
  data <- sdmTMB::add_utm_columns(data, c("lon", "lat"))
  data$log.swept.area <- log(data$swept.area)
  data$N_km2 <- data$nblon/data$swept.area
  data$bati <- bati
  
  
  return(data)
} # function to aggregate adults and juveniles


eng.ad_juv <- f.data.size_nblon(spl.TATC$ENGRENC[spl.TATC$ENGRENC$year>=2012,],90,'pelagic')
saveRDS(eng.ad_juv,'eng.ad_juv.rds')
#eng.ad_juv <- readRDS('eng.ad_juv.rds')

sard.ad_juv <- f.data.size_nblon(spl.TATC$SARDPIL,110,'pelagic') #https://www.sciencedirect.com/science/article/abs/pii/S0003347212002382
saveRDS(sard.ad_juv,'sard.ad_juv.rds')
#sard.ad_juv <- readRDS('sard.ad_juv.rds')

mull.ad_juv <- f.data.size_nblon(spl.TATC$MULLBAR,100,'pelagic')
saveRDS(mull.ad_juv,'mull.ad_juv.rds')
#mull.ad_juv <- readRDS('mull.ad_juv.rds')

merl.ad_juv <- f.data.size_nblon(spl.TATC$MERLMER,330,'demersal') 
saveRDS(merl.ad_juv,'merl.ad_juv.rds')
#merl.ad_juv <- readRDS('merl.ad_juv.rds')

pape.ad_juv <- f.data.size_nblon(spl.TATC$PAPELON,20,'demersal')
saveRDS(pape.ad_juv,'pape.ad_juv.rds')
#pape.ad_juv<-  readRDS('pape.ad_juv.rds')

aris.ad_juv <- f.data.size_nblon(spl.TATC$ARISFOL,30,'demersal')
saveRDS(aris.ad_juv,'aris.ad_juv.rds')
#aris.ad_juv <- readRDS('aris.ad_juv.rds')

eng.ad_juv
sard.ad_juv 
mull.ad_juv
merl.ad_juv
pape.ad_juv
aris.ad_juv


space_term = "
    juv <-> juv, sd
    ad <-> ad, sd
    ad <-> juv, sd"

spacetime_term = "
    juv -> ad, 1, ar
    ad -> juv, 1, ar
    "


Family = list(
  "juv" = tweedie(link='log'),
  "ad" = tweedie(link='log')
)

f.SpaceTime <- function(d,space_term,Family){
  
  if (unique(d$bati) =='pelagic'){
    
    d <- d[d$depth<=300,]
  } else {
    
    d <- d
  }
  
  d = droplevels(d)
  
  fit_space = tinyVAST(
    # Specification
    data = d,
    formula = N_km2 ~  0 + interaction(year, size) + s(sc.tmp_bot) + s(sc.depth)+fyear,
    space_term = space_term,
    #spacetime_term = spacetime_term,
    family = Family,
    
    # Indicators
    variable_column = "size",
    time_column = "year",
    times = min(d$year):max(d$year),
    distribution_column = "size",
    space_columns = c("X", "Y"),
    
    # Settings
    control = tinyVASTcontrol(
      trace = 1
    ),
    spatial_domain = mesh
  )
  
  
  y_ir_space = replicate( n = 100,
                          expr = fit_space$obj$simulate()$y_i )
  
  res_space = DHARMa::createDHARMa(simulatedResponse = y_ir_space,
                                   observedResponse = d$N_km2,
                                   fittedPredictedResponse = fitted(fit_space) )
  
  #'data prediction'
  pred_space_data <- data.table(data=fit_space$data$N_km2,pred=predict(fit_space, newdata = fit_space$data))#, offset = fit$data$log.swept.area)
  
  
  pred_grid_space <- list()
  
  for( bin_i in seq_along(unique(d$size)) ){
    #
    gr = subset( grid_loschi_xGSA9, year %in% unique(d$year) )
    gr = gr[gr$month=='07' & gr$depth <= max(d$depth),]
    gr$size = unique(d$size)[bin_i]
    
    #grid = remove_all_zeros( d, grid )
    colnames(gr)[1:2] <- c('lon','lat')
    gr <- sdmTMB::add_utm_columns(gr, c("lon", "lat"),
                                  utm_names = c("X", "Y"))
    
    grid_sf = st_as_sf( x = gr, coords = c("X","Y"), crs = st_crs(32633) )
    grid_sf = st_geometry(grid_sf)
    
    
    gr$year <- as.numeric(gr$year)
    newdata = cbind( gr,size = unique(d$size)[bin_i] )
    
    newdata = newdata[,c('lon','lat','sc.depth','sc.tmp_bot','X','Y','size','year')]
    newdata$fyear <- as.factor(newdata$year)
    
    # Remove same zeros
    #newdata = remove_all_zeros( d, new )
    #newdata = cbind( newdata, log.swept.area = log(1) )
    #
    log_d = predict(fit_space, newdata = newdata,   type='response')
    pred_grid_space[[bin_i]] <- log_d
    
    #log_d = ifelse( log_d < max(log_d - log(1e3)), NA, log_d )
    
    #plot_sf = st_sf( grid_sf, log_d = log_d, year = gr$year )
    #saveRDS(log_d,paste0('log_d_se','_',bin_i,'.rds',collapse='_'))
    
    
  }
  
  grid_plot_space <- cbind(newdata,ad=pred_grid_space[[1]],juv=pred_grid_space[[2]])
  
  
  l <- list()
  
  l[['fit']] <- fit_space
  l[['residuals']] <- res_space
  l[['pred_space_data']] <-  pred_space_data
  l[['grid_plot_space']] <- grid_plot_space
  
  return(l)
  
}


vast_merl_ad_juv <- f.SpaceTime(merl.ad_juv,space_term,Family)#yes
#saveRDS(vast_merl_ad_juv,'vast_merl_ad_juv.rds')
vast_merl_ad_juv <- readRDS('vast_merl_ad_juv.rds')

vast_eng_ad_juv <- f.SpaceTime(eng.ad_juv,space_term,Family)#yes
saveRDS(vast_eng_ad_juv,'vast_eng_ad_juv.rds')
vast_eng_ad_juv <- readRDS('vast_eng_ad_juv.rds')

vast_sard_ad_juv <- f.SpaceTime(sard.ad_juv,space_term,Family)#almost
saveRDS(vast_sard_ad_juv,'vast_sard_ad_juv.rds')

vast_mull_ad_juv <- f.SpaceTime(mull.ad_juv,space_term,Family)#almost
saveRDS(vast_mull_ad_juv,'vast_mull_ad_juv.rds')

vast_pape.ad_juv <- f.SpaceTime(pape.ad_juv,space_term,Family)#yes
saveRDS(vast_pape.ad_juv,'vast_pape_ad_juv.rds')

vast_aris_ad_juv <- f.SpaceTime(aris.ad_juv,space_term,Family)#yes
saveRDS(vast_aris_ad_juv,'vast_aris_ad_juv.rds')
