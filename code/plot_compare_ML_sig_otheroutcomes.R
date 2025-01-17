library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(see)
library(tidyr)
library(ggpp)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

all.boots.aic.cog = fread( file = paste(base_dir,"all.boots.cog.csv",sep=""),data.table = F)
all.boots.aic.2back = fread( file = paste(base_dir,"all.boots.2back.csv",sep=""),data.table = F)

findp = function(p,dat){
  interval = quantile(dat,probs = c(0 + p/2,1 - p/2))
  if(interval[1] < 0 & interval[2] < 0 | interval[1] > 0 & interval[2] > 0){return(p)}else{return(1)}
}

aic_p  = function(aic){

ML_diff_p = apply(aic,2,function(X){optimise(findp,interval = c(0,1),dat =X)$minimum}) %>% as.data.frame()
ML_diff_p$y = rownames(ML_diff_p)
colnames(ML_diff_p)[1] = "p"

ML_diff_p$pfdr  = p.adjust(ML_diff_p$p,method = "fdr")
ML_diff_p$stroke = .5
ML_diff_p$stroke[ML_diff_p$pfdr < 0.05] = 1
return(ML_diff_p)
}

aic.cog.p = aic_p(all.boots.aic.cog)
aic.2back.p = aic_p(all.boots.aic.2back)

aic.cog.p$x = "nihtbx_totalcomp_agecorrected_ML_base"
aic.2back.p$x = "tfmri_nb_all_beh_c2b_rate_ML_base"

aic.diff = data.frame(y = aic.cog.p$y,x = "diff",stroke = .5)

all.aic = rbind(aic.diff,aic.cog.p %>% dplyr::select(c("y","x","stroke")), aic.2back.p %>%  dplyr::select(c("y","x","stroke")))

names = fread( file = paste(base_dir,"Final/VariableNames.csv",sep=""),data.table = F)
names$y2 = as.factor(names$Expanded)
names$y2 = factor(names$y2)
names$y2 = factor(names$y2, levels = rev(names$y2[c(1,2,24:33,3:23)]))

ML_regressions_compare_MLmodels_toeachother = fread( file = paste(base_dir,"ML_regressions_compare_MLmodels_toeachother_fixed.csv",sep=""),data.table = F) 


reg1 = fread( file = paste(base_dir,"ML_regressions_log3.csv",sep=""),data.table = F) 
reg2 = fread( file = paste(base_dir,"ML_regressions_log3_corperf.csv",sep=""),data.table = F) 

reg1$mod = "base"
reg2$mod = "con.perf"

reg.all = rbind(reg1,reg2)

ML_regressions_compare_MLmodels_toeachother = merge(ML_regressions_compare_MLmodels_toeachother,names)
reg.all = merge(reg.all,names)


ML_regressions_compare_MLmodels_toeachother$mod = as.factor(ML_regressions_compare_MLmodels_toeachother$mod)
reg.all$mod = as.factor(reg.all$mod)

ML_regressions_compare_MLmodels_toeachother$U_CI[ML_regressions_compare_MLmodels_toeachother$Estimate <0] = ML_regressions_compare_MLmodels_toeachother$U_CI[ML_regressions_compare_MLmodels_toeachother$Estimate <0] * -1
ML_regressions_compare_MLmodels_toeachother$L_CI[ML_regressions_compare_MLmodels_toeachother$Estimate <0] = ML_regressions_compare_MLmodels_toeachother$L_CI[ML_regressions_compare_MLmodels_toeachother$Estimate <0] * -1
ML_regressions_compare_MLmodels_toeachother$Estimate[ML_regressions_compare_MLmodels_toeachother$Estimate <0] = ML_regressions_compare_MLmodels_toeachother$Estimate[ML_regressions_compare_MLmodels_toeachother$Estimate <0] * -1


reg.all$U_CI[reg.all$Estimate <0] = reg.all$U_CI[reg.all$Estimate <0] * -1
reg.all$L_CI[reg.all$Estimate <0] = reg.all$L_CI[reg.all$Estimate <0] * -1
reg.all$Estimate[reg.all$Estimate <0] = reg.all$Estimate[reg.all$Estimate <0] * -1


ML2 = ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(x == "tfmri_nb_all_beh_c2b_rate_ML_base" | x == "nihtbx_totalcomp_agecorrected_ML_base" )
ML3 = ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(x != "tfmri_nb_all_beh_c2b_rate_ML_base" & x != "nihtbx_totalcomp_agecorrected_ML_base" )

ML2 = ML2 %>% dplyr::select(-c("df","t"))
reg.all = reg.all %>% dplyr::select(-c("t_z"))

plot_ci = rbind(reg.all,ML2)

plot_ci = plot_ci %>% dplyr::filter(mod == "base")
ML3 = ML3 %>% dplyr::filter(mod == "base")

plot_ci = merge(all.aic,plot_ci)

plot_ci$x = as.factor(plot_ci$x)
levels(plot_ci$x) = c("Neural Signature", "ML - Total Composite","ML - 2-back Accuracy" )

plot2 = ggplot(ML3,aes(x = y2,y = Estimate))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "|Standardized regression estimate|")+

  
    geom_violinhalf(scale = "width",aes(fill = "All other ML models") ,position = position_identity(),alpha=.5 )+
  
  
  geom_linerange( inherit.aes = TRUE,
                  aes(xmin=y2, xmax=y2, ymin=L_CI, ymax=U_CI,color=x),
                  data = plot_ci,
                  stat = "identity",
                  #position = position_dodge(width = .5),
                  position = position_dodgenudge(width = .5,x = .25),
                  show.legend = F)+
  
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = plot_ci,color="black",
             mapping = aes(x = y2,y = Estimate,fill =x ,stroke = stroke),
             #position = position_dodge(width = .5)
             position = position_dodgenudge(width = .5,x = .25)
             
             )+
  
  scale_fill_manual(values = viridis::turbo(n = 4,begin = .1,end = .9)[c(1,4,3,2)])+
  scale_color_manual(values = viridis::turbo(n = 4,begin = .1,end = .9)[c(1,4,3)])+
  
  coord_flip()+
  
  theme_bw()+
  #  scale_y_continuous(limits = c(-.11,.05))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.title.position = "top")+
  guides(colour = guide_legend(nrow = 2),fill = guide_legend(nrow = 2))
plot2


ggsave(plot = plot2,filename = paste(base_dir,"Final/regression_plots_compML_otheroutcomes_2.jpeg",sep = ""),   dpi = 500,width = 6,height = 7)
ggsave(plot = plot2,filename = paste(base_dir,"Final/regression_plots_compML_otheroutcomes_2.png",sep = ""),   dpi = 500,width = 6,height = 7)
ggsave(plot = plot2,filename = paste(base_dir,"Final/regression_plots_compML_otheroutcomes_2.pdf",sep = ""),   dpi = 500,width = 6,height = 7)
ggsave(plot = plot2,filename = paste(base_dir,"Final/regression_plots_compML_otheroutcomes_2.bmp",sep = ""),   dpi = 500,width = 6,height = 7)
ggsave(plot = plot2,filename = paste(base_dir,"Final/regression_plots_compML_otheroutcomes_2.tiff",sep = ""),   dpi = 500,width = 6,height = 7)



# samexy = which(ML_regressions_compare_MLmodels_toeachother$y == stringr::str_replace(ML_regressions_compare_MLmodels_toeachother$x,pattern = "_ML",replacement = ""))
# ML_regressions_compare_MLmodels_toeachother = ML_regressions_compare_MLmodels_toeachother[-samexy,]

# ################
# 
# plot1 = ggplot(ML_regressions_compare_MLmodels_toeachother,aes(x = y2,y = Estimate,fill = mod))+
#   geom_hline(yintercept = 0,linetype="dashed")+
#   ylab(label = "Standardized regression estimate")+
# #  ggtitle(label = "Psychopathology")+
#   geom_violinhalf(scale = "width",adjust = .75,position = position_identity(),alpha=.5 )+
#   
#   geom_segment(inherit.aes = TRUE,data = reg.all,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2,color = mod),
#                lwd=1,position = position_nudge(x = .25))+
# 
#   geom_point(shape=21,size=2,inherit.aes = FALSE,data = reg.all,color="black",
#              mapping = aes(x = y2,y = Estimate,fill=mod),position = position_nudge(x = .25))+
#   
#    scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
#   scale_color_manual(values = viridis::turbo(n = 20)[c(4,16)])+
#   
#   coord_flip()+
#   
#   theme_bw()+
# #  scale_y_continuous(limits = c(-.11,.05))+
#   theme(axis.title.y = element_blank(),
#         legend.key.size = unit(13,"points"),legend.title = element_blank(),
#         legend.position = "top",
#         legend.title.position = "top")
# plot1
# #########################

ML_regressions_compare_MLmodels_toeachother = ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(mod == "base")
reg.all = reg.all %>% dplyr::filter(mod == "base")

plot2 = ggplot(ML_regressions_compare_MLmodels_toeachother,aes(x = y2,y = Estimate))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "|Standardized regression estimate|")+
  #  ggtitle(label = "Psychopathology")+
  geom_violinhalf(scale = "width",aes(fill = "ML models trained on each outcome") ,position = position_identity(),alpha=.5 )+
  
  geom_segment(inherit.aes = TRUE,data = reg.all,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2),
               color=viridisLite::turbo(n = 1,begin = .8,end = .8),
               lwd=1,position = position_nudge(x = .25))+
  
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = reg.all,color="black",
             mapping = aes(x = y2,y = Estimate,fill = "Neural signature"),position = position_nudge(x = .25))+
  
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  scale_color_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  coord_flip()+
  
  theme_bw()+
  #  scale_y_continuous(limits = c(-.11,.05))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "top",
        legend.title.position = "top")
plot2





comp = rbind(ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(mod == "base") %>% 
               dplyr::select(c("x","y","Estimate")),reg1 %>% dplyr::select(c("x","y","Estimate")) )

best = comp %>% 
 # dplyr::select(c("x","y","Estimate")) %>% 
  dplyr::group_by(y) %>% 
  reframe(E_rank = rank(abs(Estimate)),y=y,x=x,Estimate = Estimate) %>% 
  dplyr::group_by(x) %>% 
  summarise(sum_rank = mean(E_rank))

#######################################


ML_regressions_compare_MLmodels_toeachother = ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(mod == "base")

two_back = ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(x == "tfmri_nb_all_beh_c2b_rate_ML_base" )
ML_plot = ML_regressions_compare_MLmodels_toeachother %>% dplyr::filter(x != "tfmri_nb_all_beh_c2b_rate_ML_base" )



plot3 = ggplot(ML_plot,aes(x = y2,y = Estimate))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "|Standardized regression estimate|")+
  #  ggtitle(label = "Psychopathology")+
  geom_violinhalf(scale = "width",aes(fill = "ML models trained on each outcome") ,position = position_identity(),alpha=.5 )+
  
  geom_segment(inherit.aes = TRUE,data = reg.all,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2),
               color=viridisLite::turbo(n = 1,begin = .8,end = .8),
               lwd=1,position = position_nudge(x = .25))+
  
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = reg.all,color="black",
             mapping = aes(x = y2,y = Estimate,fill = "Neural signature"),position = position_nudge(x = .25))+
  
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = two_back,color="black",
             mapping = aes(x = y2,y = Estimate,fill = "Two back"),position = position_nudge(x = .25))+
  
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16,8)])+
  scale_color_manual(values = viridis::turbo(n = 20)[c(4,16,8)])+
  
  coord_flip()+
  
  theme_bw()+
  #  scale_y_continuous(limits = c(-.11,.05))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "top",
        legend.title.position = "top")
plot3
