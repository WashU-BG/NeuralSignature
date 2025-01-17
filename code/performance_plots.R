
library(data.table)
library(dplyr)
library(lme4)
library(DescTools)
library(pROC)
library(epiR)
library(ggplot2)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")


train.out.all.final = fread(paste(base_dir,"ML_classification_out_train_updated.csv",sep=""),data.table = F)

test.out.all.final = fread(paste(base_dir,"ML_classification_out_test_updated.csv",sep=""),data.table = F)


split1.train.out = train.out.all.final %>% dplyr::filter(split == 1, run == 3)
split2.train.out = train.out.all.final %>% dplyr::filter(split == 2, run == 3)

split1.test.out = test.out.all.final %>% dplyr::filter(split == 1, run == 3)
split2.test.out = test.out.all.final %>% dplyr::filter(split == 2, run == 3)

split1_train_AUC = pROC::roc(split1.train.out$con,split1.train.out$ENET)
split2_train_AUC = pROC::roc(split2.train.out$con,split2.train.out$ENET)

split1_test_AUC = pROC::roc(split1.test.out$con,split1.test.out$ENET)
split2_test_AUC = pROC::roc(split2.test.out$con,split2.test.out$ENET)

#########################

auc_perm =  fread(paste(base_dir,"Final/all.auc.perm.csv",sep=""),data.table = F)
auc_perm=unique(auc_perm)

max(auc_perm %>% dplyr::filter(train == 1) %>% dplyr::select("auc.train") %>% as.matrix())
max(auc_perm %>% dplyr::filter(train == 2) %>% dplyr::select("auc.train") %>% as.matrix())
max(auc_perm %>% dplyr::filter(train == 1) %>% dplyr::select("auc.test") %>% as.matrix())
max(auc_perm %>% dplyr::filter(train == 2) %>% dplyr::select("auc.test") %>% as.matrix())



pROC::roc(c(rep(0,10),rep(1,10)),
          c(runif(n = 10,min = 0,max = .5),runif(n = 10,min = .51,max = 1))) %>% plot()


##############################
roc_train = rbind(data.frame(split=1,sp = split1_train_AUC$specificities,se = split1_train_AUC$sensitivities),
                  data.frame(split=2,sp = split2_train_AUC$specificities,se = split2_train_AUC$sensitivities))
roc_train$Split = as.factor(roc_train$split)

roc_test = rbind(data.frame(split=1,sp = split1_test_AUC$specificities,se = split1_test_AUC$sensitivities),
                  data.frame(split=2,sp = split2_test_AUC$specificities,se = split2_test_AUC$sensitivities))
roc_test$Split = as.factor(roc_test$split)

ROC_train_plot = ggplot(data = roc_train,aes(x = sp,y = se,color=Split ))+
  scale_color_viridis_d(option = "D",begin = .3,end = .7)+
  scale_y_continuous(limits = c(0,1))+
  scale_x_reverse(limits = c(1,0))+
  geom_abline(slope = 1,intercept = 1,color="darkgrey",linetype="dashed")+
  geom_line(linewidth=1)+
  ggtitle(label = "",subtitle =   "Performance in training data")+
  ylab(label = "Sensitivity\n(true positive rate)")+
  xlab(label = "Specificity\n(true negative rate)")+
  # annotate("text",x = .75,y = .95,label = "AUC = 0.88",size=4,color =viridis::viridis(n = 1,begin = .2) )+
  # annotate("text",x = .75,y = .7,label = "AUC = 0.87",size=4,color =viridis::viridis(n = 1,begin = .8) )+
  # 
  theme_bw()+
  theme(aspect.ratio=1,legend.position = "right",plot.title = element_blank())+
  guides(color=guide_legend(nrow=2,byrow=TRUE))
ROC_train_plot


ROC_test_plot = ggplot(data = roc_test,aes(x = sp,y = se,color=Split ))+
  scale_color_viridis_d(option = "D",begin = .3,end = .7)+
  scale_y_continuous(limits = c(0,1))+
  scale_x_reverse(limits = c(1,0))+
  geom_abline(slope = 1,intercept = 1,color="darkgrey",linetype="dashed")+
  geom_line(linewidth=1)+
  ggtitle(label = "",subtitle =  "Out-of-sample performance")+
  ylab(label = "Sensitivity\n(true positive rate)")+
  xlab(label = "Specificity\n(true negative rate)")+
  # annotate("text",x = .75,y = .95,label = "AUC = 0.87",size=4,color =viridis::viridis(n = 1,begin = .2) )+
  # annotate("text",x = .75,y = .7,label = "AUC = 0.86",size=4,color =viridis::viridis(n = 1,begin = .8) )+

  theme_bw()+
  theme(aspect.ratio=1,legend.position = "right",plot.title = element_blank())+
  guides(color=guide_legend(nrow=2,byrow=TRUE))

ROC_test_plot

##############

all.test.out = test.out.all.final %>% dplyr::filter( run == 3)
all.test.out$Condition = ifelse(all.test.out$con == 0,"0-back","2-back")

histogram_plot = ggplot(all.test.out,aes(x = ENET,color = Condition,fill=Condition))+
  scale_color_viridis_d(option = "C",begin = .2,end = .8)+
  scale_x_continuous(breaks = c(0,.25,.5,.75,1),labels = c("0.00\n0-back","0.25","0.50","0.75","1.00\n2-back"))+
  scale_fill_viridis_d(option = "C",begin = .2,end = .8)+
  geom_histogram(binwidth = .01,position = "identity",alpha=.5)+
 # ggtitle(label = "Neural Signature of the Emotional N-back (NSEN)")+
  xlab(label = "Predicted n-back condition probability")+
  ylab(label = "Count")+
  theme_bw()
histogram_plot


classification.pred = fread(paste(base_dir,"ML_classification_out_test.csv",sep=""),data.table=F)
twoback = classification.pred %>% dplyr::filter(run == 3, con == 1) %>% dplyr::select(-c("con"))
zeroback = classification.pred %>% dplyr::filter(run == 3, con == 0)%>% dplyr::select(-c("con"))

colnames(twoback)[6] = "twoback"
colnames(zeroback)[6] = "zeroback"

nsen = merge(zeroback,twoback)
nsen$diff = nsen$twoback - nsen$zeroback

var(nsen$zeroback)
var(nsen$twoback)

library(ggpointdensity)
 

nsen_plot = ggplot(nsen,aes(x =twoback,y = zeroback))+
  xlab(label = "Predicted n-back probability\n2-back condition")+
  ylab(label = "Predicted n-back probability\n0-back condition")+
 # ggtitle(label = "",subtitle = "2-back probability")+
 # scale_color_viridis_c(option = "B")+
  ggpointdensity::geom_pointdensity(aes( color = after_stat(log(density))  ),adjust=.005,size=1) + 
  geom_smooth(method = 'lm',se = F,formula = 'y~x',color="white",lwd = 1.5,linetype="solid")+
  theme_bw()+
  theme(aspect.ratio = 1,legend.position = "right",legend.title.position = "top")+
  labs(color =   "Point density" )+
scale_color_gradientn(colours = viridisLite::magma(n = 512),
                    # limits = c(0,5),
                     breaks=c(0,log(c(2,4,8,16,32,64))),labels = c(0,2,4,8,16,32,64),
                     guide = guide_colorbar( frame.colour = "black",
                                             # barwidth = 12,
                                             # barheight =1.5,
                                             draw.ulim = T,
                                             title.position = "top",
                                             ticks.colour = "black"))
                                             # ,
                                             # frame.linewidth = .5,
                                             # label.theme = element_text(size=10),
                                             # ticks.linewidth = 1))
nsen_plot

#####
# 
# x= nsen$twoback
# y= nsen$zeroback
# adjust = 0.005
# 
# count_neighbors_r <- function(x, y,adjust) {
#   
#   
#   xrange <- diff(range(x)) * adjust
#   yrange <- diff(range(y)) * adjust
#   r2 <- (xrange + yrange) / 70
#   r2
#   
#   xy <- xrange / yrange
#   
#   yx <- 1 / xy
#   sapply(1:length(x), function(i) {
#     sum((yx * (x[i] - x) ^ 2) + (xy * (y[i] - y) ^ 2) < r2)
#   })
#   
# }
# 
# 



#####
###########

histogram_plot_2 = ggplot(nsen,aes(x = diff))+
  geom_histogram(binwidth = .05,position = "identity",alpha=.5,
                 color = viridis::plasma(n = 1,begin = .5),
                 fill = viridis::plasma(n = 1,begin = .5))+
  xlab(label = "Difference score: 2-back > 0-back")+
  ylab(label = "Count")+
  theme_bw()
histogram_plot_2

###################

weight.out.final = fread(paste(base_dir,"ML_classification_out_weights_updated.csv",sep=""),data.table = F)

split1 = weight.out.final %>% dplyr::filter(split == 1) %>% dplyr::select(c("regions","haufe_weight","enet_weights"))
split2 = weight.out.final %>% dplyr::filter(split == 2) %>% dplyr::select(c("regions","haufe_weight","enet_weights"))

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
  labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 7.5x",10^{-14},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
#haufe_comparison

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
  labs(caption = expression(paste("r = 0.91, ",p[robust] ,"= 7.5x",10^{-14},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
#ENET_comparison

all.haufe$mean = apply(all.haufe[,c(2,4)],1,mean)

########################################################


library(ggseg)
library(ggsegDesterieux)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggpubr)

atlas = behav.dat = fread(paste(base_dir,"Atlas_regions.csv",sep=""),data.table = F)
colnames(atlas)[2] = "regions"

dat.plot = merge(atlas,all.haufe )
colnames(dat.plot)[1] = "region_orig"

library(paletteer)


# J = colorRampPalette(colors = c(viridisLite::turbo(n = 1,begin = .1),
#                                 "#FFFFFF", 
#                                 viridisLite::turbo(n = 1,begin = .9)),
#                      space = "rgb",interpolate = "linear")


J<-colorRampPalette(colors = paletteer_d("colorBlindness::Blue2DarkRed12Steps"),
                    space = "rgb",interpolate = "linear")

# 
# J = colorRampPalette(colors = c(paletteer_d("colorBlindness::Blue2DarkRed12Steps")[2],
#                                 "#FFFFFF", 
#                                 paletteer_d("colorBlindness::Blue2DarkRed12Steps")[11]),
#                      space = "rgb",interpolate = "linear")
# 
# 
# J<-colorRampPalette(colors = c(rgb_gsea()[2], "#FFFFFF", rgb_gsea(reverse = T)[2]),
#                     space = "rgb",interpolate = "linear")
# 
# J<-colorRampPalette(colors = c("#002fa7", "#FFFFFF", "#FF3E41"),
#                     space = "rgb",interpolate = "linear")


task_effects = ggplot(data = dat.plot,aes(fill = mean))+
  
  geom_brain(atlas = ggsegDesterieux::desterieux,
             #position = position_brain(hemi ~ side),
             color="black"
  )+
  scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                       limits = c(-.2,.2),breaks=seq(-.2,.2,by=.1),
                       guide = guide_colorbar( frame.colour = "black",
                                               barwidth = 12,
                                               barheight =1.5,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black",
                                               frame.linewidth = .5,
                                               label.theme = element_text(size=10),
                                               ticks.linewidth = 1))+
  
  theme_void()+
  theme(legend.position = "top")+
  labs(fill =   "Haufe-transformed variable weights" )
task_effects

task_effects2 = ggplot(data = dat.plot,aes(fill = mean))+
  
  geom_brain(atlas = ggseg::aseg,side = "coronal",
             #position = position_brain(  hemi ~ side),
             color="black"
  )+
  scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                       limits = c(-.2,.2),breaks=seq(-.2,.2,by=.1),
                       guide = guide_colorbar( frame.colour = "black",
                                               barwidth = 12,
                                               barheight =1.5,
                                               draw.ulim = T,
                                               title.position = "top",
                                               ticks.colour = "black",
                                               frame.linewidth = .5,
                                               label.theme = element_text(size=10),
                                               ticks.linewidth = 1))+
  
  theme_void()+
  theme(legend.position = "top")+
  labs(fill =   "Haufe-transformed variable weights" )
task_effects2

haufe_all   = ggarrange(task_effects,task_effects2,ncol = 2,common.legend = T,widths = c(6,1))
haufe_all


#####################################

p1 = ggarrange(ROC_train_plot,ROC_test_plot,widths = c(1,1),labels=c("A","B"))
p2 = ggarrange(nsen_plot,histogram_plot_2,widths = c(1.5,1),labels=c("E","F"))

  p3 = ggarrange(p1,haufe_all,histogram_plot,p2,labels = c("","C","D",""),nrow = 4,heights = c(1,.75,.75,1))

  ggsave(plot = p3,filename = paste(base_dir,"Final/ML_performance.jpeg",sep = ""),dpi = 500,width = 8,height = 10)
  ggsave(plot = p3,filename = paste(base_dir,"Final/ML_performance.pdf",sep = ""),dpi = 500,width = 8,height = 10)
  ggsave(plot = p3,filename = paste(base_dir,"Final/ML_performance.png",sep = ""),dpi = 500,width = 8,height = 10)
  ggsave(plot = p3,filename = paste(base_dir,"Final/ML_performance.bmp",sep = ""),dpi = 500,width = 8,height = 10)
  ggsave(plot = p3,filename = paste(base_dir,"Final/ML_performance.tiff",sep = ""),dpi = 500,width = 8,height = 10)
  
####################################################
  
  
  library(ggseg)
  library(ggsegDesterieux)
  library(ggplot2)
  library(dplyr)
  library(ggsci)
  library(ggpubr)
  

  task_all_full.map = read.csv( paste(base_dir,"fulltaskmap.csv",sep=""))
  atlas = behav.dat = fread(paste(base_dir,"Atlas_regions.csv",sep=""),data.table = F)
  colnames(atlas)[2] = "region"
  dat.plot = merge(atlas,task_all_full.map )
  colnames(dat.plot)[1] = "region_orig"
  
  
  # J<-colorRampPalette(colors = c(rgb_gsea()[2], "#FFFFFF", rgb_gsea(reverse = T)[1]),
  #                     space = "rgb",interpolate = "linear")
  
  task_effects = ggplot(data = dat.plot,aes(fill = t))+
    
    geom_brain(atlas = ggsegDesterieux::desterieux,
               #position = position_brain(hemi ~ side),
               color="black"
    )+
    scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                         limits = c(-60,60),breaks=seq(-60,60,by=20),
                         guide = guide_colorbar( frame.colour = "black",
                                                 barwidth = 12,
                                                 barheight =1.5,
                                                 draw.ulim = T,
                                                 title.position = "top",
                                                 ticks.colour = "black",
                                                 frame.linewidth = .5,
                                                 label.theme = element_text(size=10),
                                                 ticks.linewidth = 1))+
    
    theme_void()+
    theme(legend.position = "top")+
    labs(fill =   "Main effect of task: 2-back > 0-back (t)" )
  task_effects
  
  task_effects2 = ggplot(data = dat.plot,aes(fill = t))+
    
    geom_brain(atlas = ggseg::aseg,side = "coronal",
               #position = position_brain(  hemi ~ side),
               color="black"
    )+
    scale_fill_gradientn(colours = J(n = 512),na.value = "white",
                         limits = c(-60,60),breaks=seq(-60,60,by=20),
                         guide = guide_colorbar( frame.colour = "black",
                                                 barwidth = 12,
                                                 barheight =1.5,
                                                 draw.ulim = T,
                                                 title.position = "top",
                                                 ticks.colour = "black",
                                                 frame.linewidth = .5,
                                                 label.theme = element_text(size=10),
                                                 ticks.linewidth = 1))+
    
    theme_void()+
    theme(legend.position = "top")+
    labs(fill =   "Main effect of task: 2-back > 0-back (t)" )
  task_effects2
  
  task_effects_all   = ggarrange(task_effects,task_effects2,ncol = 2,common.legend = T,widths = c(6,1))
  task_effects_all

  ####################################################
  
  
  weight.out.final = fread(paste(base_dir,"ML_classification_out_weights_updated.csv",sep=""))
  colnames(weight.out.final)[1] = "region"
  split1 =weight.out.final %>% dplyr::filter(split == 1)
  split2 =weight.out.final %>% dplyr::filter(split == 2)

  colnames(dat.plot)[1] = "region"
    
  split1 = merge(split1,dat.plot)
  split2 = merge(split2,dat.plot)
  
  weight.out.final = merge(weight.out.final,dat.plot)
  
  ggplot(data = split1,aes(x = haufe_weight,y=t))+
    geom_hline(yintercept = 0,color="darkgrey")+
    geom_vline(xintercept = 0,color="darkgrey")+
    geom_point(shape=21,fill="lightblue")+
    geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
    scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
    scale_x_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
    
    ggtitle(label = "Split 1")+
    ylab(label = "2-back > 0-back t-statistic")+
    xlab(label = "Haufe-transformed variable importance")+
    theme_bw()+
    theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
  
  
  Split1_haufe = ggplot(data = split1,aes(x = haufe_weight,y=t))+
    geom_hline(yintercept = 0,color="darkgrey")+
    geom_vline(xintercept = 0,color="darkgrey")+
    #  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
    geom_point(shape=21,fill="lightblue")+
    geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
    scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
    scale_x_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
    
    ggtitle(label = "Split 1")+
    ylab(label = "2-back > 0-back t-statistic")+
    xlab(label = "Haufe-transformed variable importance")+
    #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
    #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
   # labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 7.5x",10^{-14},sep="")))+
    
    theme_bw()+
    theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
  Split1_haufe
  
  
  Split2_haufe = ggplot(data = split2,aes(x = haufe_weight,y=t))+
    geom_hline(yintercept = 0,color="darkgrey")+
    geom_vline(xintercept = 0,color="darkgrey")+
    #  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
    geom_point(shape=21,fill="lightblue")+
    geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
    scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
    scale_x_continuous(limits = c(-.2,.2),breaks = seq(-.2,.2,.1))+
    
    ggtitle(label = "Split 2")+
    ylab(label = "2-back > 0-back t-statistic")+
    xlab(label = "Haufe-transformed variable importance")+
    #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
    #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
    # labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 7.5x",10^{-14},sep="")))+
    
    theme_bw()+
    theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
  Split2_haufe

  
    
  
  p1 = ggarrange(ROC_train_plot,ROC_test_plot,widths = c(1,1),labels=c("B","C"))
  p4 = ggarrange(Split1_haufe,Split2_haufe,widths = c(1,1),labels=c("E","F"))
  
  p2 = ggarrange(nsen_plot,histogram_plot_2,widths = c(1.5,1),labels=c("H","I"))
  
  p3 = ggarrange(task_effects_all,p1,haufe_all,p4,histogram_plot,p2,
                 labels = c("A","","D","","G",""),nrow = 6,heights = c(.75,1,.75,1,.75,1))
  
  ggsave(plot = p3,filename = paste(base_dir,"Final/ML_performance2.jpeg",sep = ""),dpi = 500,width = 7,height = 14)
  
  
  
  
