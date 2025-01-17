


library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(see)
library(tidyr)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

all.boots.aic = fread( file = paste(base_dir,"all.boots.aic.csv",sep=""),data.table = F)

findp = function(p,dat){
  interval = quantile(dat,probs = c(0 + p/2,1 - p/2))
  if(interval[1] < 0 & interval[2] < 0 | interval[1] > 0 & interval[2] > 0){return(p)}else{return(1)}
}

ML_diff_p = apply(all.boots.aic,2,function(X){optimise(findp,interval = c(0,1),dat =X)$minimum}) %>% as.data.frame()
ML_diff_p$y = rownames(ML_diff_p)
colnames(ML_diff_p)[1] = "p"

ML_diff_p$pfdr  = p.adjust(ML_diff_p$p,method = "fdr")
ML_diff_p$stroke = .5
ML_diff_p$stroke[ML_diff_p$pfdr < 0.05] = 1

names = fread( file = paste(base_dir,"Final/VariableNames.csv",sep=""),data.table = F)

reg.long = fread( file = paste(base_dir,"ML_regressions_compare_ML_fixed.csv",sep=""),data.table = F) 

reg.long2 = reg.long %>% dplyr::filter(var == "neuralsig" | var == "ML") %>% 
  dplyr::select(c("y","var","Estimate")) %>% 
  tidyr::pivot_wider(names_from = var,values_from = Estimate )
reg.long2 = merge(reg.long2,names)
reg.long2 = merge(reg.long2,ML_diff_p)

reg.long2 = reg.long2[order(reg.long2$p,decreasing = T),]

  p1 = ggplot(reg.long2,aes(x = abs(neuralsig),y = abs(ML) ,fill=Group))+
    # geom_vline(xintercept = 0,linetype="dashed")+
    # geom_hline(yintercept = 0,linetype="dashed")+
    geom_abline(intercept = 0,slope = 1,linetype="dashed")+
    # geom_smooth(formula = 'y~x',method = "lm",
    #             se=F,inherit.aes = F,color="black",aes(x = abs(neuralsig),y = abs(ML)))+
  geom_point(shape=21,size=2,aes(stroke = stroke))+
  #  geom_point(shape=21,size=2)+
    scale_x_continuous(limits = c(0,.5))+
  scale_y_continuous(limits = c(0,.5))+
  
  xlab(label = "Neural Signature\n|standardized estimate|")+
  ylab(label = "ML prediction\n|standardized estimate|")+
  theme_bw()+
  theme(legend.position = "right",legend.title = element_blank(),aspect.ratio=1)
 p1 
 #################
  
  reg.long3 = reg.long %>% dplyr::filter(var == "neuralsig.con" | var == "ML.con")
 
 names$y2 = as.factor(names$Expanded)
 names$y2 = factor(names$y2)
 names$y2 = factor(names$y2, levels = rev(names$y2[c(1,2,24:33,3:23)]))
  
 reg.long3 = merge(reg.long3,names)
  
 levels(reg.long3$y2)
  
 
 reg.long3$P_fdr = p.adjust(p = reg.long3$p,method = "fdr")
  
  reg.long3$strk = .5
  
  reg.long3$strk[reg.long3$P_fdr < 0.05] = 1
  
  
  reg.long3$var = as.factor(reg.long3$var)
  reg.long3$var = factor(reg.long3$var,levels = c("neuralsig.con","ML.con"))
  levels(reg.long3$var) = c("Neural signature - controlling for ML prediction",
                            "ML prediction - controlling for Neural signature")
  
 p2 =  ggplot(reg.long3,aes(x = y2,y = Estimate,color=var,fill=var))+
    
    scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = -1)+
    scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = -1)+
   
    geom_hline(yintercept = 0,linetype="dashed")+
    ylab(label = "Standardized regression estimate")+
    #ggtitle(label = "N-back task performance")+

       geom_linerange( aes(xmin=y2, xmax=y2, ymin=L_CI, ymax=U_CI),alpha=.5,
                    stat = "identity",position = position_dodge(width = .25),size=1,
                    show.legend = F)+
    
    geom_point(shape=21,size=2,position = position_dodge(.25),color="black",aes(stroke = strk))+
    coord_flip()+
    theme_bw()+
    #scale_y_continuous(limits = c(-.15,.5))+
    theme(axis.title.y = element_blank(),
          legend.key.size = unit(13,"points"),legend.title = element_blank(),
          legend.position = "top",
          legend.title.position = "top",legend.text = element_text(size=10))+
   guides(fill = guide_legend(nrow=2),color = guide_legend(nrow=2))

 p2
 p_all = ggarrange(p1,p2,nrow = 2,heights = c(1,2.5),labels = c("A","B"))  
p_all 
ggsave(plot = p_all,filename = paste(base_dir,"Final/regression_plots_compML3.jpeg",sep = ""),   dpi = 500,width = 6,height = 9)
ggsave(plot = p_all,filename = paste(base_dir,"Final/regression_plots_compML3.pdf",sep = ""),   dpi = 500,width = 6,height = 9)
ggsave(plot = p_all,filename = paste(base_dir,"Final/regression_plots_compML3.png",sep = ""),   dpi = 500,width = 6,height = 9)
ggsave(plot = p_all,filename = paste(base_dir,"Final/regression_plots_compML3.bmp",sep = ""),   dpi = 500,width = 6,height = 9)
ggsave(plot = p_all,filename = paste(base_dir,"Final/regression_plots_compML3.tiff",sep = ""),   dpi = 500,width = 6,height = 9)


###############################


reg.long4 = reg.long %>% dplyr::filter(var == "neuralsig" | var == "ML"| var == "ML2")

# names$y2 = as.factor(names$Expanded)
# names$y2 = factor(names$y2)
# names$y2 = factor(names$y2, levels = rev(names$y2[c(1,2,24:33,3:23)]))

reg.long4 = merge(reg.long4,names)

# levels(reg.long3$y2)
# 
# 
# reg.long3$P_fdr = p.adjust(p = reg.long3$p,method = "fdr")
# 
# reg.long3$strk = .5
# 
# reg.long3$strk[reg.long3$P_fdr < 0.05] = 1
# 

reg.long4$var = as.factor(reg.long4$var)
reg.long4$var = factor(reg.long4$var,levels = c("neuralsig","ML","ML2"))
levels(reg.long4$var) = c("Neural signature",
                          "ML prediction","ML prediction including signature")

p3 =  ggplot(reg.long4,aes(x = y2,y = Estimate,color=var,fill=var))+
  
  scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = -1)+
  scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = -1)+
  
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "Standardized regression estimate")+
  #ggtitle(label = "N-back task performance")+
  
  geom_linerange( aes(xmin=y2, xmax=y2, ymin=L_CI, ymax=U_CI),alpha=.5,
                  stat = "identity",position = position_dodge(width = .25),size=1,
                  show.legend = F)+
  
  geom_point(shape=21,size=2,position = position_dodge(.25),color="black")+
  coord_flip()+
  theme_bw()+
  #scale_y_continuous(limits = c(-.15,.5))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "top",
        legend.title.position = "top",legend.text = element_text(size=10))+
  guides(fill = guide_legend(nrow=2),color = guide_legend(nrow=2))

p3

