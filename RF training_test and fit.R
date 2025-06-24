#random forest

res.rf_trts <- lapply(trts.TATB.99_spNeccton, function(x){
  
  res <- lapply(x,f_fit_rf_trts)
  return(res)
})
#saveRDS(res.rf_trts,'res.rf_trts.rds')

diag.res.rf_trts <- lapply(lapply(res.rf_trts, function(x) {
  res <- lapply(x,f.diagnostic_GBM)
  return(res)
}),f.mean_diag_RF)
#saveRDS(diag.res.rf_trts,'diag.res.rf_trts.rds')

g.fold.engr.RF <- lapply(metrics_trts$`metrics RF`,f.plot_diag.GBM)


# def fit GBM all
setwd('C:/Users/diego/OneDrive - OGS/NECCTON_tinyVast/NEW MERGE 2025')
f.RF_def <- function(data){
  
  if (unique(data$bati)=='pelagic'){
    
    data <- data[data$depth<=300,]
    grid <- grid[grid$depth<=300,]
    fit <- randomForest(log(kg_km2+1) ~X.utm+Y.utm+depth.sc+tmp.sur.sc+chl_int.sc,data=data,ntree = 500,mtry=2)
    residuals <- ((log(data$kg_km2+1) - fit$predicted))
    pred.data <- predict(fit,data,type="response")
    cor.data <- cor(data$kg_km2,exp(pred.data))  
    rmse.data <- rmse(data$kg_km2,exp(pred.data))
    pred.grid <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=exp(predict(fit,grid,type="response")))
    print(unique(data$SPECIE))
    
    l <- list()
    l[['fit']] <- fit
    l[['residuals']] <- residuals
    l[['cor.data']] <- cor.data
    l[['rmse.data']] <- rmse.data
    l[['pred.grid']] <- pred.grid
    return(l)
    
  } else{
    fit <- randomForest(log(kg_km2+1) ~X.utm+Y.utm+depth.sc+tmp.sur.sc+chl_int.sc+fyear,data=data,ntree = 500,mtry=2)
    residuals <- ((log(data$kg_km2+1) - fit$predicted))
    pred.data <- predict(fit,data,type="response")
    cor.data <- cor(data$kg_km2,exp(pred.data))  
    rmse.data <- rmse(data$kg_km2,exp(pred.data))
    pred.grid <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=exp(predict(fit,grid,type="response")))
    print(unique(data$SPECIE))
    
    l <- list()
    l[['fit']] <- fit
    l[['residuals']] <- residuals
    l[['cor.data']] <- cor.data
    l[['rmse.data']] <- rmse.data
    l[['pred.grid']] <- pred.grid
    return(l)
  } 
}



RF_def_fit <- lapply(spl.TATB.99_spNeccton.99,f.RF_def)
saveRDS(RF_def_fit,'RF_def_fit.rds')
RF_def_fit <- readRDS('RF_def_fit.rds')

RF_def_fit<-  lapply(RF_def_fit, function (x){
  x$pred.grid$mod <- 'RF'
  return(x)
})

#residuals
par(mfrow=c(3,2))
lapply(RF_def_fit, function(x){
  qqnorm(x$residuals);abline(0, 1)
})

#rmse
lapply(RF_def_fit, function(x){
  (x$rmse.data)
})

#variance explained (R2)
lapply(RF_def_fit, function(x){
  (x$cor.data)^2
})

#map density
dens_mean_RF <- lapply(RF_def_fit, function(x){
  agg <- aggregate(est ~ lon+lat,FUN=mean,data=x$pred.grid)
  g <- ggplot(data=world) + #
    #geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
    geom_sf() +
    coord_sf(xlim = c(-7.5, 38), ylim = c(30,46), expand = FALSE)+
    geom_tile(data=agg,aes(x=lon,y=lat, fill=est))+
    geom_raster(data=agg,aes(x=lon,y=lat, fill=est))+
    scale_fill_viridis_c(trans = "sqrt")+#,limits = c(0, quantile(agg$est, 0.99)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  l <- list()
  l[['data_dens_mean']] <- agg
  l[['plot_dens_mean']] <- g
  return(l)
})
#saveRDS(dens_mean_RF,'dens_mean_RF.rds')


dens_mean_RF$ENGR_ENC$plot_dens_mean+ggtitle('Mean RF kg/km^2: anchovies')
ggsave("RF_kg_ENGR.png", width = 10, height = 5 )
dens_mean_RF$SARD_PIL$plot_dens_mean+ggtitle('Mean RF kg/km^2: sardines')
ggsave("RF_kg_SARD.png", width = 10, height = 5 )
dens_mean_RF$MULL_BAR$plot_dens_mean+ggtitle('Mean RF kg/km^2: mullet')
ggsave("RF_kg_MULL.png", width = 10, height = 5 )
dens_mean_RF$MERL_MER$plot_dens_mean+ggtitle('Mean RF kg/km^2: European hake')
ggsave("RF_kg_MERL.png", width = 10, height = 5 )
dens_mean_RF$ARIS_FOL$plot_dens_mean+ggtitle('Mean RF kg/km^2: giant red shrimp')
ggsave("RF_kg_ARIS.png", width = 10, height = 5 )
dens_mean_RF$PAPE_LON$plot_dens_mean+ggtitle('Mean RF kg/km^2: deep rose shrimp')
ggsave("RF_kg_PAPE.png", width = 10, height = 5 )

