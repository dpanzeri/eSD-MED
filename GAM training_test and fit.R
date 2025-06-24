res.GAM_trts <- lapply(trts.TATB.99_spNeccton, function(x){
  
  res <- lapply(x,f_fit_GAM_trts)
  return(res)
})
#saveRDS(res.GAM_trts,'res.GAM_trts.rds')

diag.res.GAM_trts <- lapply(lapply(res.GAM_trts, function(x) {
  res <- lapply(x,f.diagnostic_GAM)
  return(res)
}),f.gam.meanfold)
diag.res.GAM_trts <- readRDS('diag.res.GAM_trts.rds')

g.fold.engr.GAM <- lapply(metrics_trts$`metrics GAM`,f.plot_diag_GAM)

GAM_def_fit <- lapply(spl.TATB.99_spNeccton.99,f.GAM_def)
#saveRDS(GAM_def_fit,'GAM_def_fit.rds')
GAM_def_fit <- readRDS('GAM_def_fit.rds')

GAM_def_fit <-  lapply(GAM_def_fit, function (x){
  x$pred.grid$mod <- 'GAM'
  return(x)
})

#residuals
par(mfrow=c(2,2))
lapply(GAM_def_fit, function(x){
  gam.check(x$fit)
  
})

#r2
lapply(GAM_def_fit, function(x){
  (x$cor.data)
})

#rmse
lapply(GAM_def_fit, function(x){
  (x$rmse.data)
})
#summary
lapply(GAM_def_fit, function(x){
  summary(x$fit)
})


dens_mean_GAM <- lapply(GAM_def_fit, function(x){
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
saveRDS(dens_mean_GAM,'dens_mean_GAM.rds')
dens_mean_GAM <- readRDS('dens_mean_GAM.rds')

dens_mean_GAM$`ENGR-ENC`$plot_dens_mean+ggtitle('Mean GAM kg/km^2: anchovies')
ggsave( "GAM_kg_engr.png", width = 10, height = 5 )
dens_mean_GAM$`SARD-PIL`$plot_dens_mean+ggtitle('Mean GAM kg/km^2: sardines')
ggsave( "GAM_kg_sard.png", width = 10, height = 5 )
dens_mean_GAM$`MERL-MER`$plot_dens_mean+ggtitle('Mean GAM kg/km^2: European hake')
ggsave( "GAM_kg_merl.png", width = 10, height = 5 )
dens_mean_GAM$`MULL-BAR`$plot_dens_mean+ggtitle('Mean GAM kg/km^2: red mullet')
ggsave( "GAM_kg_mullet.png", width = 10, height = 5 )
dens_mean_GAM$`ARIS-FOL`$plot_dens_mean+ggtitle('Mean GAM kg/km^2: giant red shrimp')
ggsave( "GAM_kg_ARIS.png", width = 10, height = 5 )
dens_mean_GAM$`PAPE-LON`$plot_dens_mean+ggtitle('Mean GAM kg/km^2: deep rose shrimp')
ggsave( "GAM_kg_PAPE.png", width = 10, height = 5 )

