library(data.table)
library(dplyr)
library(fitdistrplus)

library(foreach)
library(doParallel)
library(parallel)

library(patchwork)

accel_beta = function(dat,target){
  fit<-fitdist(data = dat^2,distr  = "beta")
  pval <- 1-pbeta(target^2,shape1 = fit$estimate[1], shape2 = fit$estimate[2]) 
  return(pval)
}


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")



region_regressions = fread( file = paste(base_dir,"Final/all_region_regressions.csv",sep=""),data.table = F)
region_regressions =  region_regressions %>% dplyr::filter(contrast == 3)
perm.out = fread(paste(base_dir,"Final/maps.all.permuted.csv",sep=""),data.table = F)

task_all_full.map = fread(paste(base_dir,"fulltaskmap.csv",sep=""),data.table = F)

variables = unique(region_regressions$y)

out = data.frame(y = variables,cor=NA,p_robust = NA)
task_all_full.map.temp = task_all_full.map %>% dplyr::select(c("region","t"))


cl = makeCluster(10)
registerDoParallel(cl)

out = foreach(i = 1: length(variables),.packages = c("dplyr","fitdistrplus"),.combine = rbind) %dopar% {
  out.temp = data.frame(y = variables[i],cor=NA,p_robust = NA)
  
  temp =region_regressions %>% dplyr::filter(y == variables[i]) 
  temp2 = merge(temp,task_all_full.map.temp)
  true.cor = cor(temp2$Estimate,temp2$t)
  out.temp$cor = true.cor
  
  null_cors = data.frame(rep = c(1:10000),cor = NA)
  
  for(ii in 1:10000){
    t1 = perm.out[ii,] %>% t() %>% as.data.frame()
    t1$region =rownames(t1)
    colnames(t1)[1] = "t"
    temp3 = merge(temp,t1)
    null_cors$cor[ii] = cor(temp3$Estimate,temp3$t)
    
  }
  
  p1 = accel_beta(dat = null_cors$cor ,target = true.cor)
  
  out.temp$p_robust =  p1
  return(out.temp)
  
  
}

stopCluster(cl)
registerDoSEQ()

out_orig = out

out$pfdr = p.adjust(out$p_robust,method = "fdr")

names = fread( file = paste(base_dir,"Final/VariableNames.csv",sep=""),data.table = F)

names$y2 = as.factor(names$Expanded)
names$y2 = factor(names$y2,levels = (names$Expanded))


out = merge(out,names)

region_regressions = merge(region_regressions,names)

# round(out$cor,2)
# scales::scientific(x = out$p_robust,digits = 2)

out$caption = paste("r = ",round(out$cor,3),", p-robust = ",scales::scientific(x = out$p_robust,digits = 2),sep="")
#

task_all_full.map = fread(paste(base_dir,"fulltaskmap.csv",sep=""),data.table = F)


dat.plot = merge(region_regressions,task_all_full.map,by = "region")

dat.plot = dat.plot[order(dat.plot$Order),]

dat1a = dat.plot[grep("^tfmri.*rate$|nihtbx",x = dat.plot$y),]
dat2a = dat.plot[grep("cbcl|pps",x = dat.plot$y),]

v1 = unique(dat1a$y2)
v2 = unique(dat2a$y2)


plotlist1 = list()

for(i in 1:length(v1)){
  
  temp = dat1a %>% dplyr::filter(y2 == v1[i])
  caption = out %>% dplyr::filter(out$y2 == v1[i]) %>% dplyr::select("caption") %>% unname() 
  
plotlist1[[i]] =  ggplot(data = temp,aes(x = Estimate,y=beta))+
    geom_hline(yintercept = 0,color="darkgrey")+
    geom_vline(xintercept = 0,color="darkgrey")+
    geom_point(shape=21,fill="lightblue")+
    geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
    xlab(label = "Individual difference associations")+
    ylab(label = "Main effect of task")+
    theme_bw()+
    theme(aspect.ratio=1) +
    labs(caption = caption)+
    ggtitle(label = v1[i])
    
  
}

plotlist2= list()

for(i in 1:length(v2)){
  
  temp = dat2a %>% dplyr::filter(y2 == v2[i])
  caption = out %>% dplyr::filter(out$y2 == v2[i]) %>% dplyr::select("caption") %>% unname() 
  
  plotlist2[[i]] =  ggplot(data = temp,aes(x = Estimate,y=beta))+
    geom_hline(yintercept = 0,color="darkgrey")+
    geom_vline(xintercept = 0,color="darkgrey")+
    geom_point(shape=21,fill="lightblue")+
    geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
    xlab(label = "Individual difference associations")+
    ylab(label = "Main effect of task")+
    theme_bw()+
    theme(aspect.ratio=1) +
    labs(caption = caption)+
    ggtitle(label = v2[i])
  
  
}

p1 = patchwork::wrap_plots(plotlist1,ncol = 3)
ggsave(plot = p1,filename = paste(base_dir,"Final/map_corr1.jpeg",sep = ""),dpi = 500,width = 10,height =15 )

p2 = patchwork::wrap_plots(plotlist2,ncol = 6)
ggsave(plot = p2,filename = paste(base_dir,"Final/map_corr2.jpeg",sep = ""),dpi = 1000,width = 20,height =15 )


