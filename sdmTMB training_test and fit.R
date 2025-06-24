# fit sdmTMB run on other cluster
# fit sdmTMB run on other cluster
res.TMB_trts <- lapply(trts.TATB.99_spNeccton, function(x){
  
  res <- lapply(x,f.fit_trts_TMB)
  return(res)
})
#saveRDS(res.TMB_trts,'res.TMB_trts.rds')

diag.res.TMB_trts <- readRDS('diag.res.TMB_trts.rds')
g.fold.engr.TMB <- lapply(diag.res.TMB_trts,f.plot_diag_GAM)

res.def_TMB <- readRDS("C:/Users/diego/OneDrive - OGS/NECCTON_tinyVast/NEW MERGE 2025/res.def_TMB.rds")

res.def_TMB <-  lapply(res.def_TMB, function (x){
  x$pred$mod <- 'TMB'
  return(x)
})



sanity(res.def_TMB$ENGR_ENC$fit) #ok
sanity(res.def_TMB$SARD_PIL$fit) #ok
sanity(res.def_TMB$MULL_BAR$fit) #ok
sanity(res.def_TMB$MERL_MER$fit) #ok
sanity(res.def_TMB$PAPE_LON$fit) #ok
sanity(res.def_TMB$ARIS_FOL$fit) #ok

par(mfrow=c(3,3))

qq.residual_TMB <- lapply(res.def_TMB,f.residual_TMB)
qqnorm(qq.residual_TMB$ENGR_ENC) # NO!
qqnorm(qq.residual_TMB$SARD_PIL) # ok
qqnorm(qq.residual_TMB$MULL_BAR)
qqnorm(qq.residual_TMB$MERL_MER) # ok
# NO!
qqnorm(qq.residual_TMB$`ARIS-FOL`) # NO!
qqnorm(qq.residual_TMB$`PAPE-LON`) # NO!



TMB_def_fit_resid <- lapply(res.def_TMB,function(x){
  x$data$resid <- residuals(x, type = "mle-eb")
  g <- ggplot(x$data, aes(X, Y, col = resid)) +
    scale_colour_gradient2() +
    geom_point() +
    facet_wrap(~year) +
    coord_fixed()
  l <- list()
  l[['data']] <- x
  l[['g.resid']] <- g
  return(l)
})
TMB_def_fit_resid$`ENGR-ENG`$g.resid
TMB_def_fit_resid$`SARD-PIL`$g.resid
TMB_def_fit_resid$`MULL-BAR`$g.resid
TMB_def_fit_resid$`ARIS-FOL`$g.resid



TMB_pred_data <- lapply(res.def_TMB, function(x){
  pred.data <- predict(x$fit,x$fit$data,type='response')
  return(pred.data)
})
saveRDS(TMB_pred_data,'TMB_pred_data.rds')

lapply(TMB_pred_data, function(x){
  cor(x$kg_km2,x$est)  
})

lapply(TMB_pred_data, function(x){
  rmse(x$kg_km2,x$est)  
})


dens_mean_TMB <- lapply(res.def_TMB, function(x){
  agg <- aggregate(est ~ lon+lat,FUN=mean,data=x$pred)
  g <- ggplot(data=world) + #
    #geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
    geom_sf() +
    coord_sf(xlim = c(-7.5, 38), ylim = c(30,46), expand = FALSE)+
    geom_tile(data=agg,aes(x=lon,y=lat, fill=est))+
    geom_raster(data=agg,aes(x=lon,y=lat, fill=est))+
    scale_fill_viridis_c(trans = "sqrt",limits = c(0, quantile(agg$est, 0.99)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle("Prediction (fixed effects + all random effects)",
            subtitle = paste("maximum estimated biomass density =", round(max(agg$est))))
  
  l <- list()
  l[['data_dens_mean']] <- agg
  l[['plot_dens_mean']] <- g
  return(l)
})
