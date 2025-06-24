'ENSEMBLE'
sp <- c('ENGR ENG','SARD PIL','MERL MER','MULL BAR','ARIS FOL','PAPE LON')
rmse.GBM <- data.frame(rmse=list.rbind(lapply(GBM_def_fit, function(x){
  (x$rmse.data)
})),sp=sp,mod='GBM')
rmse.RF <- data.frame(rmse=list.rbind(lapply(RF_def_fit, function(x){
  (x$rmse.data)
})),sp=sp,mod='RF')
rmse.GAM <- data.frame(rmse=list.rbind(lapply(GAM_def_fit, function(x){
  (x$rmse.data)
})),sp=sp,mod='GAM')
rmse.TMB <- data.frame(rmse=list.rbind(lapply(TMB_pred_data, function(x){
  rmse(x$kg_km2,x$est) 
})),sp=sp,mod='TMB')


rmse.tot <- rbind(rmse.GBM,rmse.RF,rmse.GAM,rmse.TMB)#,rmse.TMB
f.weight <- function(d) {
  d <- na.omit(d)
  d$perc <- 1-(d$rmse / sum(d$rmse))
  d$rev <- d$perc/sum(d$perc)
  return(d)
}

rmse.tot$rmse <-  ifelse(rmse.tot$sp %in% c('ENGR ENG','ARIS FOL','PAPE LON') & rmse.tot$mod=='TMB',NA,rmse.tot$rmse)

weigth.rmse <- list.rbind(lapply(split(rmse.tot,rmse.tot$sp),f.weight))

ggplot()+
  geom_bar(data=weigth.rmse,aes(x=mod,y=rev,fill=mod),stat = "identity")+
  facet_wrap(~sp)+theme_bw()


dens_mean_GBM <- lapply(dens_mean_GBM, function(x){
  x$data_dens_mean$mod <- 'GBM'
  return(x)
}) 
dens_mean_GAM <- lapply(dens_mean_GAM, function(x){
  x$data_dens_mean$mod <- 'GAM'
  return(x)
}) 
dens_mean_TMB <- lapply(dens_mean_TMB, function(x){
  x$data_dens_mean$mod <- 'TMB'
  return(x)
}) 
dens_mean_RF <- lapply(dens_mean_RF, function(x){
  x$data_dens_mean$mod <- 'RF'
  return(x)
}) 


'anchovy'
engr <- rbind(GBM_def_fit$ENGR_ENC$pred.grid,GAM_def_fit$ENGR_ENC$pred.grid,
              RF_def_fit$ENGR_ENC$pred.grid) #no sdmTMB
engr$sp <- 'ENGR ENG' 

engr$ens <- ifelse(engr$mod=='GBM',engr$est* weigth.rmse$rev[weigth.rmse$sp=='ENGR ENG' & weigth.rmse$mod=='GBM'],
                   ifelse(engr$mod=='GAM',engr$est* weigth.rmse$rev[weigth.rmse$sp=='ENGR ENG' & weigth.rmse$mod=='GAM'],
                          ifelse(engr$mod=='RF',engr$est* weigth.rmse$rev[weigth.rmse$sp=='ENGR ENG' & weigth.rmse$mod=='RF'],NA)))

engr.ens <- aggregate(ens ~ lat+lon+year+sp,FUN=sum,data=engr)

'sardine YES sdmTMB'
sard <- rbind(GBM_def_fit$SARD_PIL$pred.grid,GAM_def_fit$SARD_PIL$pred.grid,
              RF_def_fit$SARD_PIL$pred.grid,res.def_TMB$SARD_PIL$pred[,c('lon','lat','year','est','mod')])

sard$sp <- 'SARD PIL'


sard$ens <- ifelse(sard$mod=='GBM',sard$est* weigth.rmse$rev[weigth.rmse$sp=='SARD PIL' & weigth.rmse$mod=='GBM'],
                   ifelse(sard$mod=='GAM',sard$est* weigth.rmse$rev[weigth.rmse$sp=='SARD PIL' & weigth.rmse$mod=='GAM'],
                          ifelse(sard$mod=='RF',sard$est* weigth.rmse$rev[weigth.rmse$sp=='SARD PIL' & weigth.rmse$mod=='RF'],
                                 ifelse(sard$mod=='TMB',sard$est* weigth.rmse$rev[weigth.rmse$sp=='SARD PIL' & weigth.rmse$mod=='TMB'],NA))))

sard.ens <- aggregate(ens ~ lat+lon+year+sp,FUN=sum,data=sard)

'hake YES sdmTMB'
hake <- rbind(GBM_def_fit$MERL_MER$pred.grid,GAM_def_fit$MERL_MER$pred.grid,
              RF_def_fit$MERL_MER$pred.grid, res.def_TMB$MERL_MER$pred[,c('lon','lat','year','est','mod')])

hake$sp <- 'MERL MER'


hake$ens <- ifelse(hake$mod=='GBM',hake$est* weigth.rmse$rev[weigth.rmse$sp=='MERL MER' & weigth.rmse$mod=='GBM'],
                   ifelse(hake$mod=='GAM',hake$est* weigth.rmse$rev[weigth.rmse$sp=='MERL MER' & weigth.rmse$mod=='GAM'],
                          ifelse(hake$mod=='RF',hake$est* weigth.rmse$rev[weigth.rmse$sp=='MERL MER' & weigth.rmse$mod=='RF'],
                                 ifelse(hake$mod=='TMB',hake$est* weigth.rmse$rev[weigth.rmse$sp=='MERL MER' & weigth.rmse$mod=='TMB'],NA)
                          )))
hake.ens <- aggregate(ens ~ lat+lon+year+sp,FUN=sum,data=hake)

'red mullet yes sdmTMB'
mullet <- rbind(GBM_def_fit$MULL_BAR$pred.grid,GAM_def_fit$MULL_BAR$pred.grid,
                RF_def_fit$MULL_BAR$pred.grid,res.def_TMB$MULL_BAR$pred[,c('lon','lat','year','est','mod')])

mullet$sp <- 'MULL BAR'


mullet$ens <- ifelse(mullet$mod=='GBM',mullet$est* weigth.rmse$rev[weigth.rmse$sp=='MULL BAR' & weigth.rmse$mod=='GBM'],
                     ifelse(mullet$mod=='GAM',mullet$est* weigth.rmse$rev[weigth.rmse$sp=='MULL BAR' & weigth.rmse$mod=='GAM'],
                            ifelse(mullet$mod=='RF',mullet$est* weigth.rmse$rev[weigth.rmse$sp=='MULL BAR' & weigth.rmse$mod=='RF'],
                                   ifelse(mullet$mod=='TMB',mullet$est* weigth.rmse$rev[weigth.rmse$sp=='MULL BAR' & weigth.rmse$mod=='TMB'],NA)
                            )))

mull.ens <- aggregate(ens ~ lat+lon+year+sp,FUN=sum,data=mullet)

'aris'
aris <- rbind(GBM_def_fit$ARIS_FOL$pred.grid,GAM_def_fit$ARIS_FOL$pred.grid,
              RF_def_fit$ARIS_FOL$pred.grid) # dens_mean_TMB$ARIS_FOL$data_dens_mean,

aris$sp <- 'ARIS FOL'

aris$ens <- ifelse(aris$mod=='GBM',aris$est* weigth.rmse$rev[weigth.rmse$sp=='ARIS FOL' & weigth.rmse$mod=='GBM'],
                   ifelse(aris$mod=='GAM',aris$est* weigth.rmse$rev[weigth.rmse$sp=='ARIS FOL' & weigth.rmse$mod=='GAM'],
                          ifelse(aris$mod=='RF',aris$est* weigth.rmse$rev[weigth.rmse$sp=='ARIS FOL' & weigth.rmse$mod=='RF'],NA)
                   ))

aris.ens <- aggregate(ens ~ lat+lon+year+sp,FUN=sum,data=aris)


'pape'
pape <- rbind(GBM_def_fit$PAPE_LON$pred.grid,GAM_def_fit$PAPE_LON$pred.grid,
              RF_def_fit$PAPE_LON$pred.grid) #dens_mean_TMB$PAPE_LON$data_dens_mean,

pape$sp <- 'PAPE LON'

pape$ens <- ifelse(pape$mod=='GBM',pape$est* weigth.rmse$rev[weigth.rmse$sp=='PAPE LON' & weigth.rmse$mod=='GBM'],
                   ifelse(pape$mod=='GAM',pape$est* weigth.rmse$rev[weigth.rmse$sp=='PAPE LON' & weigth.rmse$mod=='GAM'],
                          ifelse(pape$mod=='RF',pape$est* weigth.rmse$rev[weigth.rmse$sp=='PAPE LON' & weigth.rmse$mod=='RF'],NA)
                   ))

pape.ens <- aggregate(ens ~ lat+lon+year+sp,FUN=sum,data=pape)


all_sp_kg_ens <- rbind(engr.ens,sard.ens,hake.ens,mull.ens,aris.ens,pape.ens)
saveRDS(all_sp_kg_ens,'all_sp_kg_ens.rds')
all_sp_kg_ens_mean <- aggregate(ens ~ lon+lat+sp,data=all_sp_kg_ens,FUN=mean)

g_ens <- ggplot(data=world) + #
  #geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  geom_sf() +
  coord_sf(xlim = c(-7.5, 38), ylim = c(30,46), expand = FALSE)+
  geom_tile(data=all_sp_kg_ens_mean,aes(x=lon,y=lat, fill=ens))+
  geom_raster(data=all_sp_kg_ens_mean,aes(x=lon,y=lat, fill=ens))+
  scale_fill_viridis_c(trans = "sqrt",limits = c(0, quantile(all_sp_kg_ens$ens, 0.995)))+
  theme_bw()+
  scale_y_continuous(breaks = c(32, 36, 40,44))+
  xlab('Longitude')+ylab('Latitude')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~sp,ncol=2)
