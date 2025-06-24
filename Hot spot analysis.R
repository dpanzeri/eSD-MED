'Getis'
'GETIS'
spl.dens <- split(all_sp_kg_ens,all_sp_kg_ens$sp)

spl.dens <- lapply(split(all_sp_kg_ens,all_sp_kg_ens$sp), function(x){
  res <- split(x,x$year)
  return(res)
} )

K= 8
gGetis.sp_ens <- list.rbind(lapply(spl.dens, function (x){
  res <- list.rbind(lapply(x,f.getis))
  return(res)
}))
saveRDS(gGetis.sp_ens,'gGetis.sp_ens.rds')

gGetis.sp_ens <-  readRDS('gGetis.sp_ens.rds')

spl.gGetis.sp_ens <- split(gGetis.sp_ens,gGetis.sp_ens$sp)

spl.gGetis.sp_ens.G <- list.rbind(lapply(spl.gGetis.sp_ens,function(x){
  
  x <- x[x$res>0,]
  #x <- x[x$res> quantile(x$res,0.5),]
  return(x)
}))

library(colorRamps)
ggplot(data=world) + #
  #geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  geom_sf() +
  coord_sf(xlim = c(-7.5, 38), ylim = c(30,46), expand = FALSE)+
  geom_raster(data=spl.gGetis.sp_ens.G,aes(x=lon,y=lat,fill=res))+
  geom_tile(data=spl.gGetis.sp_ens.G,aes(x=lon,y=lat,fill=res))+
  theme_minimal()+
  scale_fill_gradientn(colours = matlab.like(9),limits = c(0, quantile(spl.gGetis.sp_ens.G$res, 0.995)))+
  scale_y_continuous(breaks = c(32, 36, 40,44))+
  xlab('Longitude')+ylab('Latitude')+
  theme_bw()+
  facet_wrap(~sp,ncol=2)+
  labs(fill = "G*")

ggsave("g_getis_allSP.png", width = 20, height = 15, units = "cm")
