library(data.table)
library(dplyr)
library(fitdistrplus)

library(foreach)
library(doParallel)
library(parallel)

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


cl = makeCluster(20)
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

out$pfdr = p.adjust(out$p_robust,method = "fdr")

# 
# for(i in 1: length(variables)){
#   print(i)
#   temp =region_regressions %>% dplyr::filter(y == variables[i]) 
#   temp2 = merge(temp,task_all_full.map.temp)
#   true.cor = cor(temp2$Estimate,temp2$t)
#   out$cor[i] = true.cor
#   
#   null_cors = data.frame(rep = c(1:10000),cor = NA)
# # 
# #  for(ii in 1:10000){
# #    t1 = perm.out[ii,] %>% t() %>% as.data.frame()
# #    t1$region =rownames(t1)
# #    colnames(t1)[1] = "t"
# #    temp3 = merge(temp,t1)
# #    null_cors$cor[ii] = cor(temp3$Estimate,temp3$t)
# # 
# #  }
# 
# 
#  p1 = accel_beta(dat = null_cors$cor ,target = true.cor)
# 
#  out$p_robust[i] =  p1
#   
# }


n.sig = data.frame(y = variables,uncor=NA,fdr = NA,fwe = NA)

for(i in 1: length(variables)){
  temp =region_regressions %>% dplyr::filter(y == variables[i]) 
  n.sig$uncor[i] = sum(temp$p<0.05)/167*100
  n.sig$fdr[i] = sum(p.adjust(temp$p,method = "fdr")<0.05)/167*100
  n.sig$fwe[i] = sum(p.adjust(temp$p,method = "bonferroni")<0.05)/167*100
  }

ML_regressions = fread( file = paste(base_dir,"ML_regressions_log3.csv",sep=""),data.table = F)
ML_regressions = merge(ML_regressions,out)
ML_regressions = merge(ML_regressions,n.sig)

ML_regressions = ML_regressions[-grep("mrt|diff",x = ML_regressions$y),]


names = fread( file = paste(base_dir,"Final/VariableNames.csv",sep=""),data.table = F)

names$y2 = as.factor(names$Expanded)
names$y2 = factor(names$y2,levels = rev(names$Expanded))

region_regressions = merge(region_regressions,names)
ML_regressions = merge(ML_regressions,names)

ML_regressions$pfdr = p.adjust(p = ML_regressions$p,method = "fdr")

plot(ML_regressions$Estimate,ML_regressions$cor)
plot(ML_regressions$Estimate,ML_regressions$fdr)


p1=ggplot(ML_regressions,aes(x = Estimate,y = fdr,fill=Group))+
  geom_hline(yintercept = 0,linetype="dashed")+
  geom_vline(xintercept = 0,linetype="dashed")+
  
  scale_x_continuous(limits = c(-.15,.46))+
  geom_point(shape=21,size=2)+
  
  xlab(label = "Signature association")+
  ylab(label = "Percent fdr-significant regions")+
  theme_bw()+theme(legend.title = element_blank(),aspect.ratio=1)
p1
p2=ggplot(ML_regressions,aes(x = (Estimate),y = (cor) ,fill=Group))+
  geom_point(shape=21,size=2)+
  scale_x_continuous(limits = c(-.15,.46))+
  scale_y_continuous(limits = c(-1,1))+
  
  xlab(label = "Signature association")+
  ylab(label = "Map correlation")+
  
  geom_vline(xintercept = 0,linetype="dashed")+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme_bw()+theme(legend.position = "none",legend.title = element_blank(),aspect.ratio=1)
p2
p3=ggplot(ML_regressions,aes(x = rank(Estimate),y = rank(cor) ,fill=Group))+
  geom_point(shape=21,size=2)+
  #scale_x_continuous(limits = c(-.15,.45))+
  geom_smooth(formula = 'y~x',method = 'lm',se = FALSE,color="black",inherit.aes = FALSE,aes(x = rank(Estimate),y = rank(cor)))+
  xlab(label = "Signature association\nrank ordered")+
  ylab(label = "Map correlation\nrank ordered")+
  theme_bw()+theme(legend.title = element_blank(),aspect.ratio=1)
p3

p_all = ggarrange(p2,p3,p1,ncol=3,legend = "top",common.legend = TRUE)
p_all

cor(x = rank(ML_regressions$Estimate),y = rank(ML_regressions$cor) )
plot(ML_regressions$cor,ML_regressions$fdr)


dat2b = ML_regressions[grep("cbcl|pps",x = ML_regressions$y),]
plot(dat2b$Estimate,dat2b$cor)
cor(dat2b$Estimate,dat2b$cor,method = "spear")
dat3b = ML_regressions[grep("nihtbx|^tfmri.*rate$",x = ML_regressions$y),]
plot(dat3b$Estimate,dat3b$cor)
cor(dat3b$Estimate,dat3b$cor,method = "spear")


###################################


library(ggseg)
library(ggsegDesterieux)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggpubr)

atlas = behav.dat = fread(paste(base_dir,"Atlas_regions.csv",sep=""),data.table = F)
colnames(atlas)[2] = "region"

dat.plot = merge(atlas,region_regressions %>% dplyr::filter(contrast == 3,y == "cbcl_scr_syn_attention_r" ) )
colnames(dat.plot)[1] = "region_orig"


# J<-colorRampPalette(colors = c(rgb_gsea()[2], "#FFFFFF", rgb_gsea(reverse = T)[1]),
#                     space = "rgb",interpolate = "linear")
J<-colorRampPalette(colors = paletteer_d("colorBlindness::Blue2DarkRed12Steps"),
                    space = "rgb",interpolate = "linear")
task_effects = ggplot(data = dat.plot,aes(fill = Estimate))+
  
  geom_brain(atlas = ggsegDesterieux::desterieux,
             #position = position_brain(hemi ~ side),
             color="black"
  )+
  scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                       limits = c(-0.04,0.04),breaks=seq(-0.04,0.04,by=0.02),
                       guide = guide_colorbar( frame.colour = "black",
                                                barwidth = 12,
                                               # barheight =1.5,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black"
                                               # frame.linewidth = .5,
                                               # label.theme = element_text(size=10),
                                               # ticks.linewidth = 1)
                       ))+
  
  theme_void()+
  theme(legend.position = "top")+
  labs(fill =   "Association with Attention Problems" )
task_effects

task_effects2 = ggplot(data = dat.plot,aes(fill = Estimate))+
  
  geom_brain(atlas = ggseg::aseg,side = "coronal",
             #position = position_brain(  hemi ~ side),
             color="black"
  )+
  scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                       limits = c(-0.04,0.04),breaks=seq(-0.04,0.04,by=0.02),
                       guide = guide_colorbar( frame.colour = "black",
                                                barwidth = 12,
                                               # barheight =1.5,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black"
                                               # frame.linewidth = .5,
                                               # label.theme = element_text(size=10),
                                               # ticks.linewidth = 1)
                       ))+
  
  theme_void()+
  theme(legend.position = "top")+
  labs(fill =   "Association with Attention Problems" )
task_effects2

task_effects_all   = ggarrange(task_effects,task_effects2,ncol = 2,common.legend = T,widths = c(6,1))
task_effects_all

task_all_full.map = fread(paste(base_dir,"fulltaskmap.csv",sep=""),data.table = F)
#colnames(task_all_full.map)[1] = "region_orig"

d1 = region_regressions %>% dplyr::filter(contrast == 3,y == "cbcl_scr_syn_attention_r" )

dat.plot = merge(d1,task_all_full.map,by = "region")


attn_comparison1 = ggplot(data = dat.plot,aes(x = Estimate,y=beta))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
 # geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  # scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  # scale_x_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  
  #ggtitle(label = "Run 1 vs. Run 2: Baseline")+
   xlab(label = "Association with Attention Problems")+
   ylab(label = "Main effect of task")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
 # labs(caption = expression(paste("r = 0.95, ",p[robust] ,"= 1.1x",10^{-8},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1) 
attn_comparison1

#################

atlas = behav.dat = fread(paste(base_dir,"Atlas_regions.csv",sep=""),data.table = F)
colnames(atlas)[2] = "region"
colnames(task_all_full.map)[1] = "region"

dat.plot = merge(atlas,task_all_full.map )
colnames(dat.plot)[1] = "region_orig"


# J<-colorRampPalette(colors = c(rgb_gsea()[2], "#FFFFFF", rgb_gsea(reverse = T)[1]),
#                     space = "rgb",interpolate = "linear")
J<-colorRampPalette(colors = paletteer_d("colorBlindness::Blue2DarkRed12Steps"),
                    space = "rgb",interpolate = "linear")
task_effects = ggplot(data = dat.plot,aes(fill = beta))+
  
  geom_brain(atlas = ggsegDesterieux::desterieux,
             #position = position_brain(hemi ~ side),
             color="black"
  )+
  scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                       limits = c(-0.25,0.25),breaks=seq(-0.2,0.2,by=0.1),
                       guide = guide_colorbar( frame.colour = "black",
                                               barwidth = 12,
                                               # barheight =1.5,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black"
                                               # frame.linewidth = .5,
                                               # label.theme = element_text(size=10),
                                               # ticks.linewidth = 1)
                       ))+
  
  theme_void()+
  theme(legend.position = "top")+
  labs(fill =   "Main effect of task - 2-back > 0-back" )
task_effects

task_effects2 = ggplot(data = dat.plot,aes(fill = beta))+
  
  geom_brain(atlas = ggseg::aseg,side = "coronal",
             #position = position_brain(  hemi ~ side),
             color="black"
  )+
  scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                       limits = c(-0.25,0.25),breaks=seq(-0.2,0.2,by=0.1),
                       guide = guide_colorbar( frame.colour = "black",
                                               barwidth = 12,
                                               # barheight =1.5,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black"
                                               # frame.linewidth = .5,
                                               # label.theme = element_text(size=10),
                                               # ticks.linewidth = 1)
                       ))+
  
  theme_void()+
  theme(legend.position = "top")+
  labs(fill =   "Main effect of task - 2-back > 0-back" )
task_effects2

task_effects_main   = ggarrange(task_effects,task_effects2,ncol = 2,common.legend = T,widths = c(6,1))
task_effects_main





#################
p4a = ggarrange(p3,p1,ncol=2,nrow=1,common.legend = TRUE,labels = c("E","F"))
p4a
p4b = ggarrange(attn_comparison1,p2,ncol=2,nrow=1,labels = c("C","D"))

p4 = ggarrange(p4b,p4a,nrow=2,heights = c(1,1.2))
p4
example1  =  ggarrange(task_effects_main,task_effects_all,p4,nrow = 3,heights = c(1,1,3),labels = c("A","B",""))
example1


ggsave(plot = example1,filename = paste(base_dir,"Final/map_comparisons.jpeg",sep = ""),dpi = 500,width = 6,height =8 )
ggsave(plot = example1,filename = paste(base_dir,"Final/map_comparisons.pdf",sep = ""),dpi = 500,width = 6,height =8 )
ggsave(plot = example1,filename = paste(base_dir,"Final/map_comparisons.png",sep = ""),dpi = 500,width = 6,height =8 )
ggsave(plot = example1,filename = paste(base_dir,"Final/map_comparisons.bmp",sep = ""),dpi = 500,width = 6,height =8 )
ggsave(plot = example1,filename = paste(base_dir,"Final/map_comparisons.tiff",sep = ""),dpi = 500,width = 6,height =8 )
