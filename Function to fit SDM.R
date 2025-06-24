#	Functions


f.tatb_sp <- function(tatb){
  
  #tatb <- data[data$kg_km2 <= quantile(data$kg_km2,0.95),]
  #tatb <- add_utm_columns(
  # tatb,
  #ll_names = c("X", "Y"),
  #utm_names = c("X.utm", "Y.utm"))
  
  tatb$depth.sc <- scale(tatb$depth)
  tatb$tmp.sur.sc <- scale(tatb$tmp.sur)
  tatb$chl_int.sc <- scale(tatb$chl_int)
  tatb$poc_int.sc <- scale(tatb$poc_int)
  tatb$fyear <- as.factor(tatb$year)
  
  tatb$O2_bot.sc <- scale(tatb$O2_bot)
  tatb$tmp_bot.sc <- scale(tatb$tmp_bot)
  tatb$sal_bot.sc <- scale(tatb$sal_bot)
  #tatb$poc_bot.sc <- scale(tatb$poc_bot)
  tatb$poc_int.sc <- scale(tatb$poc_int)
  tatb$PO4_bot.sc <- scale(tatb$PO4_bot)
  tatb$NO3_bot.sc <- scale(tatb$NO3_bot)
  
  return(tatb)
}

# Function to split in train and test
f.tr_ts <- function(data){
  trts <- list()
  for (i in 1:10){
    
    train <- data[sb$folds_list[[i]][[1]],]
    train$type <- 'train'
    test <- data[sb$folds_list[[i]][[2]],]
    test$type <- 'test'
    r <- rbind(train,test)
    r$fold <- i
    trts[[i]] <-   r
    
  }
  
  return(trts)
} # sb from CV_blocks.R

# function to fit GBM model on train and test dataset
f_fit_gbm_trts <- function(data){
  
  if (unique(data$bati)=='pelagic'){
    
    data <- data[data$depth<=300,]
    #pelagic  
    gbm0 <- gbm(log(kg_km2+1) ~X.utm*Y.utm,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm0 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm0,data[data$type=='test',],type="response"))  
    
    gbm.pr1 <- gbm(log(kg_km2+1) ~X.utm*Y.utm+depth.sc,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm1 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm.pr1,data[data$type=='test',],type="response"))  
    
    gbm.pr2 <- gbm(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp.sur.sc,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm2 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm.pr2,data[data$type=='test',],type="response"))  
    
    gbm.pr3 <- gbm(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp.sur.sc+chl_int.sc+fyear,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm3 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm.pr3,data[data$type=='test',],type="response"))  
    
    l <- list()
    
    l[['fit0']] <- gbm0
    l[['pred0']] <- pred.gbm0
    l[['fit1']] <- gbm.pr1
    l[['pred1']] <- pred.gbm1
    l[['fit2']] <- gbm.pr2
    l[['pred2']] <- pred.gbm2
    l[['fit3']] <- gbm.pr3
    l[['pred3']] <- pred.gbm3
    return(l)
    
    #demrsal
  }else{
    
    gbm0 <- gbm(log(kg_km2+1) ~X.utm*Y.utm,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm0 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm0,data[data$type=='test',],type="response"))  
    
    gbm.pr1 <- gbm(log(kg_km2+1) ~X.utm*Y.utm+depth.sc,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm1 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm.pr1,data[data$type=='test',],type="response"))  
    
    gbm.pr2 <- gbm(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp_bot.sc,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm2 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm.pr2,data[data$type=='test',],type="response"))  
    
    gbm.pr3 <- gbm(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp_bot.sc+O2_bot.sc+fyear,data=data[data$type=='train',],n.trees = 8000,interaction.depth=10,shrinkage=0.01)
    pred.gbm3 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(gbm.pr3,data[data$type=='test',],type="response"))  
    
    l <- list()
    
    l[['fit0']] <- gbm0
    l[['pred0']] <- pred.gbm0
    l[['fit1']] <- gbm.pr1
    l[['pred1']] <- pred.gbm1
    l[['fit2']] <- gbm.pr2
    l[['pred2']] <- pred.gbm2
    l[['fit3']] <- gbm.pr3
    l[['pred3']] <- pred.gbm3
    return(l)
    
    
    
  }
  
}

#function to exctract diagnostic from GBM train and test model
f.diagnostic_GBM <- function(data){
  
  cor.r2 <- as.data.frame(t(data.frame(mod0=cor(data$pred0$kg_km2,data$pred0$pred),
                                       mod1=cor(data$pred1$kg_km2,data$pred1$pred),
                                       mod2=cor(data$pred2$kg_km2,data$pred2$pred),
                                       mod3=cor(data$pred3$kg_km2,data$pred3$pred))))
  
  model_name <- rownames(cor.r2)
  
  cor.r2$fit <-  model_name
  colnames(cor.r2)[1] <- 'r2'
  
  rmse.mod <- as.data.frame(t(data.frame(mod0=rmse(data$pred0$kg_km2,data$pred0$pred),
                                         mod1=rmse(data$pred1$kg_km2,data$pred1$pred),
                                         mod2=rmse(data$pred2$kg_km2,data$pred2$pred),
                                         mod3=rmse(data$pred3$kg_km2,data$pred3$pred))))
  
  rmse.mod$fit <- model_name
  colnames(rmse.mod)[1] <- 'rmse'
  
  l <- list()
  l[['cor.r2']] <- cor.r2
  l[['rmse.mod']] <- rmse.mod
  
  
  return(l)
}

#apply a mean and sd trough fold of train and test GBM model
f.mean_diag_GBM <- function(data){
  r2_allFold <- list.rbind(lapply(data, function(x) {
    x$cor.r2
  }))
  
  r2_allFold$fit <- rep(1:4,5)
  agg.r2.allFold <- aggregate(r2 ~ fit,data=r2_allFold,FUN=mean)
  agg.r2.allFold.sd <- aggregate(r2 ~ fit,data=r2_allFold,FUN=sd)
  agg.r2.allFold$sd <- agg.r2.allFold.sd$r2
  agg.r2.allFold$mod <- 'GBM'
  
  rmse_allFold <- list.rbind(lapply(data, function(x) {
    x$rmse.mod
  }))
  
  rmse_allFold$fold <- rep(1:4,5)
  agg.rmse.allFold <- aggregate(rmse ~ fit,data=rmse_allFold,FUN=mean)
  agg.rmse.allFold.sd <- aggregate(rmse ~ fit,data=rmse_allFold,FUN=sd)
  agg.rmse.allFold$sd <- agg.rmse.allFold.sd$rmse
  agg.rmse.allFold$mod <- 'GBM'
  
  l <- list()
  l[['mean r2']] <- agg.r2.allFold
  l[['mean rmse']] <- agg.rmse.allFold
  
  return(l)
}

#function to plot diagnostic from GBM model on traina nd test fold
f.plot_diag.GBM <- function(data){
  
  g.r2 <- ggplot()+
    geom_pointrange(data$`mean r2`,mapping=aes(x=fit,y=r2,ymin=r2-sd,ymax=r2+sd))+
    theme_bw()+ggtitle('r2')
  
  g.rmse <- ggplot()+
    geom_pointrange(data$`mean rmse`,mapping=aes(x=fit,y=rmse,ymin=rmse-sd,ymax=rmse+sd))+
    theme_bw()+ggtitle('RMSE')
  
  grid.fin <- grid.arrange(g.r2,g.rmse)
  return(grid.fin)
  
}

# function to apply best GBM model on data
f.gbm_def <- function(data){
  
  if (unique(data$bati)=='pelagic'){
    
    data <- data[data$depth<=300,]
    grid <- grid[grid$depth<=300,]
    fit <- gbm(log(kg_km2+1) ~(X.utm*Y.utm)+depth.sc+tmp.sur.sc+chl_int.sc+fyear,data=data,n.trees = 5000,interaction.depth=3,shrinkage=0.01)
    residuals <- ((log(data$kg_km2+1) - fit$fit))
    pred.data <- predict(fit,data,type="response")
    cor.data <- cor(data$kg_km2,exp(pred.data))  
    rmse.data <- rmse(data$kg_km2,exp(pred.data))
    pred.grid <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=exp(predict(fit,grid,type="response")))
    print(unique(data$SPECIE))
    
    l <- list()
    l[['fit']] <- fit
    l[['pred.data']] <- pred.data
    l[['residuals']] <- residuals
    l[['cor.data']] <- cor.data
    l[['rmse.data']] <- rmse.data
    l[['pred.grid']] <- pred.grid
    return(l)
    
  } else{
    fit <- gbm(log(kg_km2+1) ~(X.utm*Y.utm)+depth.sc+tmp_bot.sc+O2_bot.sc+fyear,data=data,n.trees = 5000,interaction.depth=3,shrinkage=0.01)
    residuals <- ((log(data$kg_km2+1) - fit$fit))
    pred.data <- predict(fit,data,type="response")
    cor.data <- cor(data$kg_km2,exp(pred.data))  
    rmse.data <- rmse(data$kg_km2,exp(pred.data))
    pred.grid <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=exp(predict(fit,grid,type="response")))
    print(unique(data$SPECIE))
    
    l <- list()
    l[['fit']] <- fit
    l[['pred.data']] <- pred.data
    l[['residuals']] <- residuals
    l[['cor.data']] <- cor.data
    l[['rmse.data']] <- rmse.data
    l[['pred.grid']] <- pred.grid
    return(l)
  } 
}

#GAM
#GAM
# function to fit GAM model on train and test dataset
f_fit_GAM_trts <- function(data){
  
  if (unique(data$bati)=='pelagic'){
    
    data <- data[data$depth<=300,]
    #pelagic  
    fit0 <- gam(kg_km2 ~ s(X.utm,Y.utm),data=data[data$type=='train' & data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit0.pa <- gam(PA ~ s(X.utm,Y.utm),data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred0 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit0,data[data$type=='test',],type="response")*
                          predict(fit0.pa,data[data$type=='test',],type="response"))  
    
    fit1 <- gam(kg_km2 ~ s(X.utm,Y.utm)+s(depth.sc),data=data[data$type=='train'& data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit1.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc),data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred1 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit1,data[data$type=='test',],type="response")*
                          predict(fit1.pa,data[data$type=='test',],type="response"))  
    
    fit2 <- gam(kg_km2 ~s(X.utm,Y.utm)+s(depth.sc)+s(tmp.sur.sc)+fyear,data=data[data$type=='train'& data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit2.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc)+s(tmp.sur.sc)+fyear,data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred2 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit2,data[data$type=='test',],type="response")*
                          predict(fit2.pa,data[data$type=='test',],type="response"))  
    
    fit3 <- gam(kg_km2 ~s(X.utm,Y.utm)+s(depth.sc)+s(tmp.sur.sc)+s(chl_int.sc)+fyear,data=data[data$type=='train'& data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit3.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc)+s(tmp.sur.sc)+s(chl_int.sc)+fyear,data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred3 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit3,data[data$type=='test',],type="response")*
                          predict(fit3.pa,data[data$type=='test',],type="response"))  
    
    
    l <- list()
    
    l[['fit0']] <- fit0
    l[['pred0']] <- pred0
    l[['fit1']] <- fit1
    l[['pred1']] <- pred1
    l[['fit2']] <- fit2
    l[['pred2']] <- pred2
    l[['fit3']] <- fit3
    l[['pred3']] <- pred3
    return(l)
    
    #demersal
  }else{
    
    fit0 <- gam(kg_km2 ~ s(X.utm,Y.utm),data=data[data$type=='train' & data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit0.pa <- gam(PA ~ s(X.utm,Y.utm),data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred0 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit0,data[data$type=='test',],type="response")*
                          predict(fit0.pa,data[data$type=='test',],type="response"))  
    
    fit1 <- gam(kg_km2 ~ s(X.utm,Y.utm)+s(depth.sc),data=data[data$type=='train'& data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit1.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc),data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred1 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit1,data[data$type=='test',],type="response")*
                          predict(fit1.pa,data[data$type=='test',],type="response"))  
    
    fit2 <- gam(kg_km2 ~s(X.utm,Y.utm)+s(depth.sc)+s(tmp_bot.sc)+fyear,data=data[data$type=='train'& data$kg_km2>0,],method='REML',family=Gamma(link='log'))
    fit2.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc)+s(tmp_bot.sc)+fyear,data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred2 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit2,data[data$type=='test',],type="response")*
                          predict(fit2.pa,data[data$type=='test',],type="response"))  
    
    fit3 <- gam(kg_km2 ~s(X.utm,Y.utm)+s(depth.sc)+s(tmp_bot.sc)+s(O2_bot.sc)+fyear,data=data[data$type=='train'& data$kg_km2>0,],family=Gamma(link='log'))
    fit3.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc)+s(tmp_bot.sc)+s(O2_bot.sc)+fyear,data=data[data$type=='train',],method='REML',family=binomial(link='logit'))
    pred3 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(fit3,data[data$type=='test',],type="response")*
                          predict(fit3.pa,data[data$type=='test',],type="response"))  
    
    l <- list()
    
    l[['fit0']] <- fit0
    l[['pred0']] <- pred0
    l[['fit1']] <- fit1
    l[['pred1']] <- pred1
    l[['fit2']] <- fit2
    l[['pred2']] <- pred2
    l[['fit3']] <- fit3
    l[['pred3']] <- pred3
    
    return(l)
    
  }
  
}

#function to exctract diagnostic from GAM train and test model
f.diagnostic_GAM <- function(data){
  
  aic.df <- data.frame(AIC(data$fit0,data$fit1,data$fit2,data$fit3))
  model_name <- substr(rownames(aic.df),6,9)
  
  aic.df$fit <- model_name
  
  cor.r2 <- as.data.frame(t(data.frame(mod0=cor(data$pred0$kg_km2,data$pred0$pred),
                                       mod1=cor(data$pred1$kg_km2,data$pred1$pred),
                                       mod2=cor(data$pred2$kg_km2,data$pred2$pred),
                                       mod3=cor(data$pred3$kg_km2,data$pred3$pred))))
  cor.r2$fit <-  model_name
  colnames(cor.r2)[1] <- 'r2'
  
  rmse.mod <- as.data.frame(t(data.frame(mod0=rmse(data$pred0$kg_km2,data$pred0$pred),
                                         mod1=rmse(data$pred1$kg_km2,data$pred1$pred),
                                         mod2=rmse(data$pred2$kg_km2,data$pred2$pred),
                                         mod3=rmse(data$pred3$kg_km2,data$pred3$pred))))
  
  rmse.mod$fit <- model_name
  colnames(rmse.mod)[1] <- 'rmse'
  
  l <- list()
  l[['aic.df']] <- aic.df
  l[['cor.r2']] <- cor.r2
  l[['rmse.mod']] <- rmse.mod
  
  
  return(l)
}

#apply a mean and sd trough fold of train and test GAM model
f.gam.meanfold <- function(data){
  
  aic.allFold <- list.rbind(lapply(data, function(x) {
    x$aic.df
  }))
  
  aic.allFold$fold <- rep(1:4,10)
  agg.aic.allFold <- aggregate(AIC ~ fit,data=aic.allFold,FUN=mean)
  agg.aic.allFold.sd <- aggregate(AIC ~ fit,data=aic.allFold,FUN=sd)
  agg.aic.allFold$sd <- agg.aic.allFold.sd$AIC
  agg.aic.allFold$mod <- 'GAM'
  
  
  
  r2_allFold <- list.rbind(lapply(data, function(x) {
    x$cor.r2
  }))
  
  r2_allFold$fold <- rep(1:4,10)
  agg.r2.allFold <- aggregate(r2 ~ fit,data=r2_allFold,FUN=mean)
  agg.r2.allFold.sd <- aggregate(r2 ~ fit,data=r2_allFold,FUN=sd)
  agg.r2.allFold$sd <- agg.r2.allFold.sd$r2
  agg.r2.allFold$mod <- 'GAM'
  
  
  rmse_allFold <- list.rbind(lapply(data, function(x) {
    x$rmse.mod
  }))
  
  rmse_allFold$fold <- rep(1:4,10)
  agg.rmse.allFold <- aggregate(rmse ~ fit,data=rmse_allFold,FUN=mean)
  agg.rmse.allFold.sd <- aggregate(rmse ~ fit,data=rmse_allFold,FUN=sd)
  agg.rmse.allFold$sd <- agg.rmse.allFold.sd$rmse
  agg.rmse.allFold$mod <- 'GAM'
  
  l <- list()
  l[['mean AIC']] <- agg.aic.allFold
  l[['mean r2']] <- agg.r2.allFold
  l[['mean rmse']] <- agg.rmse.allFold
  
  return(l)
}

#function to plot diagnostic from GAM model on traina nd test fold
f.plot_diag_GAM <- function(data){
  
  g.aic <- ggplot()+
    geom_pointrange(data$`mean AIC`,mapping=aes(x=fit,y=AIC,ymin=AIC-sd,ymax=AIC+sd))+
    theme_bw()+ggtitle('AIC')
  
  g.r2 <- ggplot()+
    geom_pointrange(data$`mean r2`,mapping=aes(x=fit,y=r2,ymin=r2-sd,ymax=r2+sd))+
    theme_bw()+ggtitle('r^2')
  
  g.rmse <- ggplot()+
    geom_pointrange(data$`mean rmse`,mapping=aes(x=fit,y=rmse,ymin=rmse-sd,ymax=rmse+sd))+
    theme_bw()+ggtitle('RMSE')
  
  grid.fin <- grid.arrange(g.aic,g.r2,g.rmse)
  return(grid.fin)
  
}

# function to apply best GAM model on data
f.GAM_def <- function(data){
  
  if (unique(data$bati)=='pelagic'){
    
    data <- data[data$depth<=300,]
    grid <- grid[grid$depth<=300,]
    fit <- gam(kg_km2 ~s(X.utm,Y.utm)+s(depth.sc)+s(tmp.sur.sc)+fyear,data=data[data$kg_km2>0,],family=Gamma(link='log'),method='REML')
    fit.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc)+s(tmp.sur.sc)+fyear,data=data,method='REML',family=binomial(link='logit'))
    
    residuals <- fit$residuals
    pred.data <- predict(fit,data,type="response")
    pred.data.pa <- predict(fit.pa,data,type="response")
    
    cor.data <- cor(data$kg_km2,pred.data*pred.data.pa)  
    rmse.data <- rmse(data$kg_km2,pred.data*pred.data.pa)
    pred.grid <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=predict(fit,grid,type="response")*predict(fit.pa,grid,type="response"))
    pred.grid.se <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=predict(fit,grid,type="response",se.fit=T)$se.fit+predict(fit.pa,grid,type="response",se.fit=T)$se.fit)
    
    print(unique(data$SPECIE))
    
    l <- list()
    l[['fit']] <- fit
    l[['fit.pa']] <- fit.pa
    l[['residuals']] <- residuals
    l[['cor.data']] <- cor.data
    l[['rmse.data']] <- rmse.data
    l[['pred.grid']] <- pred.grid
    l[['pred.grid.se']] <- pred.grid.se
    
    return(l)
    
  } else{
    fit <- gam(kg_km2 ~s(X.utm,Y.utm)+s(depth.sc)+s(tmp_bot.sc)+fyear,data=data[data$kg_km2>0,],family=Gamma(link='log'),method='REML')
    fit.pa <- gam(PA ~ s(X.utm,Y.utm)+s(depth.sc)+s(tmp_bot.sc)+fyear,data=data,method='REML',family=binomial(link='logit'))
    
    residuals <- fit$residuals
    pred.data <- predict(fit,data,type="response")
    pred.data.pa <- predict(fit.pa,data,type="response")
    
    cor.data <- cor(data$kg_km2,pred.data*pred.data.pa)  
    rmse.data <- rmse(data$kg_km2,pred.data*pred.data.pa)
    pred.grid <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=predict(fit,grid,type="response")*predict(fit.pa,grid,type="response"))
    pred.grid.se <- data.table(lon=grid$lon,lat=grid$lat,year=grid$year,est=predict(fit,grid,type="response",se.fit=T)$se.fit+predict(fit.pa,grid,type="response",se.fit=T)$se.fit)
    print(unique(data$SPECIE))
    
    l <- list()
    l[['fit']] <- fit
    l[['fit.pa']] <- fit.pa
    l[['residuals']] <- residuals
    l[['cor.data']] <- cor.data
    l[['rmse.data']] <- rmse.data
    l[['pred.grid']] <- pred.grid
    l[['pred.grid.se']] <- pred.grid.se
    
    return(l)
  } 
}


#sdmTMB
#sdmTMB
l <- list()

#function to fit sdmTMB on train and test fold for each species
f.fit_trts_TMB <- function(pr){
  
  if (unique(pr$bati)=='pelagic'){
    
    pr <- pr[pr$depth<=300,]
    
    
    mesh_trts <- make_mesh(pr[pr$type=='train',],xy_cols = c('X','Y'),cutoff = 10)
    
    fit0 <- sdmTMB(
      kg_km2 ~ 0,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    
    pred.data_0 <- predict(fit0,pr[pr$type=='test',],type='response')
    
    
    fit1 <- sdmTMB(
      kg_km2 ~ 0+depth +fyear,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    pred.data_1 <- predict(fit1,pr[pr$type=='test',],type='response')
    
    fit2 <- sdmTMB(
      kg_km2 ~ 0+depth +tmp.sur.sc+fyear,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    pred.data_2 <- predict(fit2,pr[pr$type=='test',],type='response')
    
    fit3 <- sdmTMB(
      kg_km2 ~ 0+depth +tmp.sur.sc+chl_int.sc +fyear,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    
    pred.data_3 <- predict(fit3,pr[pr$type=='test',],type='response')
    
    
    l <- list()
    l[['fit0']] <- fit0
    l[['fit1']] <- fit1
    l[['fit2']] <- fit2
    l[['fit3']] <- fit3
    l[['pred.data_0']] <- pred.data_0
    l[['pred.data_1']] <- pred.data_1
    l[['pred.data_2']] <- pred.data_2
    l[['pred.data_3']] <- pred.data_3
    return(l)
    
  } else{
    
    mesh_trts <- make_mesh(pr[pr$type=='train',],xy_cols = c('X','Y'),cutoff = 10)
    
    fit0 <- sdmTMB(
      kg_km2 ~ 0,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    
    pred.data_0 <- predict(fit0,pr[pr$type=='test',],type='response')
    
    
    fit1 <- sdmTMB(
      kg_km2 ~ 0+depth +fyear,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    pred.data_1 <- predict(fit1,pr[pr$type=='test',],type='response')
    
    fit2 <- sdmTMB(
      kg_km2 ~ 0+depth +tmp_bot.sc+fyear,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    pred.data_2 <- predict(fit2,pr[pr$type=='test',],type='response')
    
    fit3 <- sdmTMB(
      kg_km2 ~ 0+depth +tmp_bot.sc+O2_bot.sc +fyear,
      data = pr[pr$type=='train',],#[pr$year %in% 2012:2016,],
      mesh = mesh_trts,
      time = "year",
      family = tweedie(link = "log"),
      #spatial = "on",
      spatiotemporal = "ar1")
    
    pred.data_3 <- predict(fit3,pr[pr$type=='test',],type='response')
    
    
    l <- list()
    l[['fit0']] <- fit0
    l[['fit1']] <- fit1
    l[['fit2']] <- fit2
    l[['fit3']] <- fit3
    l[['pred.data_0']] <- pred.data_0
    l[['pred.data_1']] <- pred.data_1
    l[['pred.data_2']] <- pred.data_2
    l[['pred.data_3']] <- pred.data_3
    
    return(l)
    
    
    
  }
} #function for one fold 

f.residual_TMB <- function(fit){
  rq_res <- residuals(fit$fit, type = "mle-eb") #mle-mvn
  rq_res <- rq_res[is.finite(rq_res)] # some Inf
  qqnorm(rq_res);abline(0, 1)
  return(rq_res)
}


#Random Forest
#Random Forest
# function to fit RF model on train and test dataset
f_fit_rf_trts <- function(data){
  
  if (unique(data$bati)=='pelagic'){
    
    data <- data[data$depth<=300,]
    #pelagic  
    rf0 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf0 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf0,data[data$type=='test',],type="response"))  
    
    rf1 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm+depth.sc,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf1 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf1,data[data$type=='test',],type="response"))  
    
    rf2 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp.sur.sc,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf2 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf2,data[data$type=='test',],type="response"))  
    
    rf3 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp.sur.sc+chl_int.sc+fyear,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf3 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf3,data[data$type=='test',],type="response"))  
    
    l <- list()
    
    l[['fit0']] <- rf0
    l[['pred0']] <- pred.rf0
    l[['fit1']] <- rf1
    l[['pred1']] <- pred.rf1
    l[['fit2']] <- rf2
    l[['pred2']] <- pred.rf2
    l[['fit3']] <- rf3
    l[['pred3']] <- pred.rf3
    return(l)
    
    #demrsal
  }else{
    
    rf0 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf0 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf0,data[data$type=='test',],type="response"))  
    
    rf1 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm+depth.sc,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf1 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf1,data[data$type=='test',],type="response"))  
    
    rf2 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp_bot.sc,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf2 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf2,data[data$type=='test',],type="response"))  
    
    rf3 <- randomForest(log(kg_km2+1) ~X.utm*Y.utm+depth.sc+tmp_bot.sc+O2_bot.sc+fyear,data=data[data$type=='train',],ntree = 500,mtry=2)
    pred.rf3 <- data.table(kg_km2=data[data$type=='test',]$kg_km2,pred=predict(rf3,data[data$type=='test',],type="response"))  
    
    l <- list()
    
    l[['fit0']] <- rf0
    l[['pred0']] <- pred.rf0
    l[['fit1']] <- rf1
    l[['pred1']] <- pred.rf1
    l[['fit2']] <- rf2
    l[['pred2']] <- pred.rf2
    l[['fit3']] <- rf3
    l[['pred3']] <- pred.rf3
    return(l)
    
    
    
  }
  
}

# function to apply mean trough fold of train and test of RF model
f.mean_diag_RF <- function(data){
  r2_allFold <- list.rbind(lapply(data, function(x) {
    x$cor.r2
  }))
  
  r2_allFold$fit <- rep(1:4,10)
  agg.r2.allFold <- aggregate(r2 ~ fit,data=r2_allFold,FUN=mean)
  agg.r2.allFold.sd <- aggregate(r2 ~ fit,data=r2_allFold,FUN=sd)
  agg.r2.allFold$sd <- agg.r2.allFold.sd$r2
  agg.r2.allFold$mod <- 'RF'
  
  rmse_allFold <- list.rbind(lapply(data, function(x) {
    x$rmse.mod
  }))
  
  rmse_allFold$fold <- rep(1:4,10)
  agg.rmse.allFold <- aggregate(rmse ~ fit,data=rmse_allFold,FUN=mean)
  agg.rmse.allFold.sd <- aggregate(rmse ~ fit,data=rmse_allFold,FUN=sd)
  agg.rmse.allFold$sd <- agg.rmse.allFold.sd$rmse
  agg.rmse.allFold$mod <- 'RF'
  
  l <- list()
  l[['mean r2']] <- agg.r2.allFold
  l[['mean rmse']] <- agg.rmse.allFold
  
  return(l)
}


# hot spot Getis
f.getis <- function(data){
  data <- data[!is.na(data$ens),]
  coordinates(data) <- ~ lon + lat
  knea <- knearneigh(coordinates(data), longlat = TRUE,k=K)
  neib <- knn2nb(knea)
  neib <- nb2listw(neib , glist=NULL, style="W", zero.policy=NULL)
  G <-(localG(data$ens,neib))
  df <- data.frame(cbind(sp=as.character(unique(data$sp)),lon=as.numeric(data$lon),lat=as.numeric(data$lat),res=as.numeric(G),clust=as.character(attr(G, "cluster"))))
  df$res <- as.numeric(df$res)
  df$lon <- as.numeric(df$lon)
  df$lat <- as.numeric(df$lat)
  df$clust <- as.factor(df$clust)
  
  return(df)
}  
