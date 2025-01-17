# compare ML variable importance



library(data.table)
library(stringr)
library(dplyr)


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

weight.out.final = fread(paste(base_dir,"ML_classification_out_weights_updated.csv",sep=""))

split1 =weight.out.final %>% dplyr::filter(split == 1)
split2 =weight.out.final %>% dplyr::filter(split == 2)


perm_split1 = fread(paste(base_dir,"Final/maps.split1.permuted.csv",sep=""))
perm_split2 = fread(paste(base_dir,"Final/maps.split2.permuted.csv",sep=""))

perm_weights =  fread(paste(base_dir,"Final/maps.weights.perm.csv",sep=""))
perm_haufe =  fread(paste(base_dir,"Final/maps.haufe.perm.csv",sep=""))


out.null = matrix(data = NA,nrow = dim(perm_split1)[1],ncol = 6) %>% as.data.frame()
colnames(out.null)= c("split1_weight_r","split1_haufe_r","split2_weight_r","split2_haufe_r","split_comp_enet","split_comp_haufe")

for(i in 1:dim(perm_split1)[1]){
  
  perm1 = perm_split1[i,] %>% t() %>% as.data.frame()
  perm1$regions = rownames(perm1)
  
  perm2 = perm_split1[i,] %>% t() %>% as.data.frame()
  perm2$regions = rownames(perm2)
  
  weights_perm_2 = perm_weights[i,] %>% t() %>% as.data.frame()
  weights_perm_2$regions = rownames(weights_perm_2)
  
  weights_haufe_2 = perm_haufe[i,] %>% t() %>% as.data.frame()
  weights_haufe_2$regions = rownames(weights_haufe_2)
  
  temp1 = merge(split1,perm1)
  temp2 = merge(split2,perm2)

  temp2_enet =  merge(split2,weights_perm_2)
  temp2_haufe =  merge(split2,weights_haufe_2)
  

out.null$split1_weight_r[i] = cor(temp1$enet_weights,temp1$V1)
out.null$split1_haufe_r[i] = cor(temp1$haufe_weight,temp1$V1)
out.null$split2_weight_r[i] = cor(temp2$enet_weights,temp2$V1)
out.null$split2_haufe_r[i] = cor(temp2$haufe_weight,temp2$V1)

out.null$split_comp_enet[i] = cor(temp2_enet$enet_weights,temp2_enet$V1)
out.null$split_comp_haufe[i] = cor(temp2_haufe$haufe_weight,temp2_haufe$V1)


}


true_activation = fread(paste(base_dir,"task_split_map.csv",sep=""))
colnames(true_activation)[1] = "regions"

s1_compare = merge(true_activation,weight.out.final %>% dplyr::filter(split == 1))
s2_compare = merge(true_activation,weight.out.final %>% dplyr::filter(split == 2))

s1_haufe_r = cor(s1_compare$t_1,s1_compare$haufe_weight)
s1_weights_r = cor(s1_compare$t_1,s1_compare$enet_weights)

s2_haufe_r = cor(s2_compare$t_2,s2_compare$haufe_weight)
s2_weights_r = cor(s2_compare$t_2,s2_compare$enet_weights)


cor_haufe = cor(split1$haufe_weight,split2$haufe_weight)
cor_weights = cor(split1$enet_weights,split2$enet_weights)


library(fitdistrplus)

accel_beta = function(dat,target){
  fit<-fitdist(data = dat^2,distr  = "beta")
  pval <- 1-pbeta(target^2,shape1 = fit$estimate[1], shape2 = fit$estimate[2]) 
  return(pval)
}


p1 = accel_beta(dat = out.null$split1_weight_r ,target = s1_weights_r)
p2 = accel_beta(dat = out.null$split1_haufe_r ,target = s1_haufe_r)
p3 = accel_beta(dat = out.null$split2_weight_r ,target = s2_weights_r)
p4 = accel_beta(dat = out.null$split2_haufe_r ,target = s2_haufe_r)


out.null$split_comp_haufe2 =out.null$split_comp_haufe
out.null$split_comp_haufe2[is.na(out.null$split_comp_haufe2)] = 0

p5 = accel_beta(dat = na.omit(out.null$split_comp_haufe) %>% as.vector() ,target = cor_haufe)
p6 = accel_beta(dat = na.omit(out.null$split_comp_enet) %>% as.vector() ,target = cor_weights)



fit<-fitdist(data = ( na.omit(out.null$split_comp_enet) %>% as.vector())^2,distr  = "gamma")
plot(fit)

accel_gamma = function(dat,target){
  fit<-fitdist(data = dat^2,distr  = "gamma")
  pval <- 1-pgamma(target^2, fit$estimate[1], fit$estimate[2]) 
  return(pval)
}
p6 = accel_gamma(dat = na.omit(out.null$split_comp_enet) %>% as.vector() ,target = cor_weights)



########################################
# Plots

library(ggseg)
library(ggsegDesterieux)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggpubr)

Split1_haufe = ggplot(data = s1_compare,aes(x = haufe_weight,y=t_1))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
#  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
  
  ggtitle(label = "Split 1: Haufe weights vs. Activation")+
  ylab(label = "2-back > 0-back t-statistic")+
  xlab(label = "Haufe-transformed variable importance")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 7.5x",10^{-14},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
Split1_haufe

Split1_weights = ggplot(data = s1_compare,aes(x = enet_weights,y=t_1))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  #  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-.8,.8),breaks = seq(-.8,.8,.2))+
  
  ggtitle(label = "Split 1: Elastic net weights vs. Activation")+
  ylab(label = "2-back > 0-back t-statistic")+
  xlab(label = "Elastic net variable importance")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.57, ",p[robust] ,"= 2.1x",10^{-4},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
Split1_weights

Split2_haufe = ggplot(data = s2_compare,aes(x = haufe_weight,y=t_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  #  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
  
  ggtitle(label = "Split 2: Haufe weights vs. Activation")+
  ylab(label = "2-back > 0-back t-statistic")+
  xlab(label = "Haufe-transformed variable importance")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 1.0x",10^{-13},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
Split2_haufe

Split2_weights = ggplot(data = s2_compare,aes(x = enet_weights,y=t_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  #  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-.8,.8),breaks = seq(-.8,.8,.2))+
  
  ggtitle(label = "Split 2: Elastic net weights vs. Activation")+
  ylab(label = "2-back > 0-back t-statistic")+
  xlab(label = "Elastic net variable importance")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.56, ",p[robust] ,"= 1.9x",10^{-4},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
Split2_weights




#########################


weight.out.final = fread(paste(base_dir,"ML_classification_out_weights_updated.csv",sep=""))

split1 =weight.out.final %>% dplyr::filter(split == 1) %>% dplyr::select(c("regions","haufe_weight","enet_weights"))
split2 =weight.out.final %>% dplyr::filter(split == 2) %>% dplyr::select(c("regions","haufe_weight","enet_weights"))

colnames(split1)[2] = "haufe_weight_1"
colnames(split2)[2] = "haufe_weight_2"

colnames(split1)[3] = "enet_weights_1"
colnames(split2)[3] = "enet_weights_2"

all.haufe = merge(split1,split2)

cor(all.haufe$haufe_weight_1,all.haufe$haufe_weight_2)
cor(all.haufe$enet_weights_1,all.haufe$enet_weights_2)

haufe_comparison = ggplot(data = all.haufe,aes(x = haufe_weight_1,y=haufe_weight_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
  scale_x_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
  
  ggtitle(label = "Haufe weights: Split 1 vs. Split 2")+
  ylab(label = "Split 2")+
  xlab(label = "Split 1")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 1.8x",10^{-7},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
haufe_comparison

ENET_comparison = ggplot(data = all.haufe,aes(x = enet_weights_1,y=enet_weights_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-.8,.8),breaks = seq(-.8,.8,.2))+
  scale_x_continuous(limits = c(-.8,.8),breaks = seq(-.8,.8,.2))+
  
  ggtitle(label = "Elastic net weights: Split 1 vs. Split 2")+
  ylab(label = "Split 2")+
  xlab(label = "Split 1")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.91, ",p[robust] ,"= 1.1x",10^{-16},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
ENET_comparison

#######################


task_split.map = fread(paste(base_dir,"task_split_map.csv",sep=""))

task_comparison_splits = ggplot(data = task_split.map,aes(x = t_1,y=t_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  
  ggtitle(label = "Split 1 vs. Split 2")+
  xlab(label = "Split 1 - t-statistic")+
  ylab(label = "Split 2 - t-statistic")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 2.7x",10^{-12},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
task_comparison_splits




########################################################
weight_comparison = ggarrange(labels = c("B","C","D","E","F","G"),
                              Split1_weights,Split1_haufe,Split2_weights,Split2_haufe,haufe_comparison,ENET_comparison,ncol = 2,nrow = 3)
weight_comparison

all.plots = ggarrange(task_comparison_splits,weight_comparison,nrow = 2,heights = c(1,3),labels = c("A",""))

ggsave(plot = all.plots,filename = paste(base_dir,"Final/comparison_plots.jpeg",sep = ""),
       dpi = 500,width = 9,height = 15)
ggsave(plot = all.plots,filename = paste(base_dir,"Final/comparison_plots.png",sep = ""),
       dpi = 500,width = 9,height = 15)
ggsave(plot = all.plots,filename = paste(base_dir,"Final/comparison_plots.pdf",sep = ""),
       dpi = 500,width = 9,height = 15)
ggsave(plot = all.plots,filename = paste(base_dir,"Final/comparison_plots.bmp",sep = ""),
       dpi = 500,width = 9,height = 15)
ggsave(plot = all.plots,filename = paste(base_dir,"Final/comparison_plots.tiff",sep = ""),
       dpi = 500,width = 9,height = 15)
