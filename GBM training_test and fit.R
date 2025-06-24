trts.TATB.99_spNeccton <- lapply(spl.TATB.99_spNeccton,f.tr_ts)

#UTM coordinates
f.utm <- function(data){
  data <- add_utm_columns(
    data,
    ll_names = c("X", "Y"),
    utm_names = c("X.utm", "Y.utm"))
  return(data)
}

spl.TATB.99_spNeccton.99 <- lapply(spl.TATB.99_spNeccton.99, f.utm)
spl.TATB.99_spNeccton.99$`ENGR-ENC`$bati <- 'pelagic'
spl.TATB.99_spNeccton.99$`SARD-PIL`$bati <- 'pelagic'
spl.TATB.99_spNeccton.99$`MULL-BAR`$bati <- 'pelagic'
spl.TATB.99_spNeccton.99$`MERL-MER`$bati <- 'demersal'
spl.TATB.99_spNeccton.99$`ARIS-FOL`$bati <- 'demersal'
spl.TATB.99_spNeccton.99$`PAPE-LON`$bati <- 'demersal'



'---------------------------MODEL TRAINING and TEST---------------------------------------------------'

# fit GBM
# fit GBM
# fit GBM

res.gbm_trts <- lapply(trts.TATB.99_spNeccton, function(x){
  
  res <- lapply(x,f_fit_gbm_trts)
  return(res)
})
saveRDS(res.gbm_trts,'res.gbm_trts.rds')
res.gbm_trts <- readRDS('res.gbm_trts.rds')

diag.res.gbm_trts <- lapply(lapply(res.gbm_trts, function(x) {
  res <- lapply(x,f.diagnostic_GBM)
  return(res)
}),f.mean_diag_GBM)

g.fold.engr.GBM <- lapply(diag.res.gbm_trts,f.plot_diag.GBM)

'--------------------------- MODEL def---------------------------------------------------'
# def fit GBM all
GBM_def_fit <- lapply(spl.TATB.99_spNeccton.99,f.gbm_def)

GBM_def_fit <-  lapply(GBM_def_fit, function (x){
  x$pred.grid$mod <- 'GBM'
  return(x)
})

#residuals
par(mfrow=c(3,2))
lapply(GBM_def_fit, function(x){
  qqnorm(x$residuals);abline(0, 1)
  
})

#rmse
lapply(GBM_def_fit, function(x){
  (x$rmse.data)
})

#variance explained (R2)
lapply(GBM_def_fit, function(x){
  (x$cor.data)^2
})


#map density
dens_mean_GBM <- lapply(GBM_def_fit, function(x){
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
saveRDS(dens_mean_GBM,'dens_mean_GBM.rds')

dens_mean_GBM$`ENGR-ENC`$plot_dens_mean+ggtitle('Mean GBM kg/km^2: anchovies')
ggsave( "GBM_kg_engr.png", width = 10, height = 5 )
dens_mean_GBM$`SARD-PIL`$plot_dens_mean+ggtitle('Mean GBM kg/km^2: sardines')
ggsave( "GBM_kg_sard.png", width = 10, height = 5 )
dens_mean_GBM$`MERL-MER`$plot_dens_mean+ggtitle('Mean GBM kg/km^2: European hake')
ggsave( "GBM_kg_hake.png", width = 10, height = 5 )
dens_mean_GBM$`MULL-BAR`$plot_dens_mean+ggtitle('Mean GBM kg/km^2: red mullet')
ggsave( "GBM_kg_mullet.png", width = 10, height = 5 )
dens_mean_GBM$`ARIS-FOL`$plot_dens_mean+ggtitle('Mean GBM kg/km^2: giant red shrimp')
ggsave( "GBM_kg_RedShrimp.png", width = 10, height = 5 )
dens_mean_GBM$`PAPE-LON`$plot_dens_mean+ggtitle('Mean GBM kg/km^2: deep rose shrimp')
ggsave( "GBM_kg_deepShrimp.png", width = 10, height = 5 )

