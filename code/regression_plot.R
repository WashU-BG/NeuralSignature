
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(see)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

region_regressions = fread( file = paste(base_dir,"Final/all_region_regressions.csv",sep=""),data.table = F)
ML_regressions = fread( file = paste(base_dir,"ML_regressions_log3.csv",sep=""),data.table = F)

region_regressions = region_regressions %>% dplyr::filter(contrast == 3)
  

names = fread( file = paste(base_dir,"Final/VariableNames.csv",sep=""),data.table = F)

names$y2 = as.factor(names$Expanded)
names$y2 = factor(names$y2,levels = rev(names$Expanded))

############################
variables = unique(region_regressions$y)

for(i in 1: length(variables)){
  temp = region_regressions %>% dplyr::filter(y == variables[i])
  temp$pfdr = p.adjust(temp$p,method = "fdr")
  temp$fdr = (temp$pfdr<0.05)*1
if(i == 1){out = temp}else{out = rbind(out,temp)}
  }
region_regressions = out

region_regressions$fdr = as.factor(region_regressions$fdr)
region_regressions$fdr = factor(region_regressions$fdr,levels = c(1,0))

levels(region_regressions$fdr) = c("Significant","Non-significant")
#############
region_regressions = merge(region_regressions,names)
ML_regressions = merge(ML_regressions,names)

ML_regressions$pfdr = p.adjust(ML_regressions$p,method = "fdr")

ML_regressions %>% group_by(Group) %>% summarise(mean = mean(Estimate),sd = sd(Estimate))
region_regressions %>% group_by(y) %>% summarise(max = max(abs(Estimate)),Group = Group) %>% unique() %>% group_by(Group) %>% summarise(mean = mean(max),sd = sd(max))

##########################
# variables = unique(region_regressions$y)
# 
# n.sig = data.frame(y = variables,uncor=NA,fdr = NA,fwe = NA)
# 
# for(i in 1: length(variables)){
#   temp =region_regressions %>% dplyr::filter(y == variables[i]) 
#   n.sig$uncor[i] = sum(temp$p<0.05)/167*100
#   n.sig$fdr[i] = sum(p.adjust(temp$p,method = "fdr")<0.05)/167*100
#   n.sig$fwe[i] = sum(p.adjust(temp$p,method = "bonferroni")<0.05)/167*100
# }
# 
# region_regressions = merge(region_regressions,n.sig)
########################


dat1a = region_regressions[grep("^tfmri.*rate$",x = region_regressions$y),]
dat1b = ML_regressions[grep("^tfmri.*rate$",x = ML_regressions$y),]


plot1 = ggplot(dat1a,aes(x = y2,y = Estimate))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "Standardized regression estimate")+
  ggtitle(label = "N-back task performance")+
  geom_violinhalf(scale = "width",adjust = .5,position = position_identity(),aes(fill = "Individual regions"))+
  
  geom_segment(inherit.aes = FALSE,data = dat1b,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2), 
               color=viridisLite::turbo(n = 1,begin = .8,end = .8),
               lwd=1,position = position_nudge(x = .25))+
  
    geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat1b,
             mapping = aes(x = y2,y = Estimate,fill = "Neural signature"),position = position_nudge(x = .25))+
  
  # scale_fill_viridis_d(option = "H",begin = .8,end = .2,direction = -1)+
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  
  coord_flip()+
  
  theme_bw()+
  scale_y_continuous(limits = c(-.15,.5))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "top",
        legend.title.position = "top")

plot1 

legend = get_legend(plot1) %>% as_ggplot()
plot1 = plot1 + theme(legend.position = "none")
##################################



dat2a = region_regressions[grep("cbcl|pps",x = region_regressions$y),]
dat2b = ML_regressions[grep("cbcl|pps",x = ML_regressions$y),]

# 
# plot2 = ggplot(dat2a,aes(x = y2,y = Estimate))+
#   geom_hline(yintercept = 0,linetype="dashed")+
#   ylab(label = "Standardized regression estimate")+
#   ggtitle(label = "Psychopathology")+
#   geom_violinhalf(scale = "width",alpha=.5,fill="red")+
#   geom_segment(inherit.aes = FALSE,data = dat2b,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2),color="blue",
#                lwd=1,position = position_nudge(x = .25))+
#   geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat2b,fill = "blue",
#              mapping = aes(x = y2,y = Estimate),position = position_nudge(x = .25))+
#   coord_flip()+
#   theme_bw()+
#   scale_y_continuous(limits = c(-.11,.05))+
#   theme(axis.title.y = element_blank())
# plot2 

plot2 = ggplot(dat2a,aes(x = y2,y = Estimate))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "Standardized regression estimate")+
  ggtitle(label = "Psychopathology")+
  geom_violinhalf(scale = "width",adjust = .5,position = position_identity(),aes(fill = "Individual regions") )+
  
  geom_segment(inherit.aes = FALSE,data = dat2b,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2), 
               color=viridisLite::turbo(n = 1,begin = .8,end = .8),
               lwd=1,position = position_nudge(x = .25))+
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat2b,
             mapping = aes(x = y2,y = Estimate,fill = "Neural signature"),position = position_nudge(x = .25))+
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  coord_flip()+
  
  theme_bw()+
  scale_y_continuous(limits = c(-.11,.05))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "none",
        legend.title.position = "top")
plot2
##################################


dat3a = region_regressions[grep("nihtbx",x = region_regressions$y),]
dat3b = ML_regressions[grep("nihtbx",x = ML_regressions$y),]

# 
# plot3 = ggplot(dat3a,aes(x = y2,y = Estimate))+
#   geom_hline(yintercept = 0,linetype="dashed")+
#   ylab(label = "Standardized regression estimate")+
#   
#   ggtitle(label = "Cognition")+
#   geom_violinhalf(scale = "width",fill = "red",alpha=.5)+
#   geom_segment(inherit.aes = FALSE,data = dat3b,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2),color="blue",
#                lwd=1,position = position_nudge(x = .25))+
#   geom_point(shape=21,size=2,fill = "blue",inherit.aes = FALSE,data = dat3b,
#              mapping = aes(x = y2,y = Estimate),position = position_nudge(x = .25))+
#   coord_flip()+
#   theme_bw()+
#   scale_y_continuous(limits = c(-.15,.5))+
#   theme(axis.title.y = element_blank())
# plot3 

plot3 = ggplot(dat3a,aes(x = y2,y = Estimate))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "Standardized regression estimate")+
  ggtitle(label = "Cognition")+
  geom_violinhalf(scale = "width",adjust = .5,position = position_identity(),aes(fill = "Individual regions")  )+
  
  geom_segment(inherit.aes = FALSE,data = dat3b,mapping = aes(y=L_CI,yend=U_CI,x = y2,xend = y2), 
               color=viridisLite::turbo(n = 1,begin = .8,end = .8),
               lwd=1,position = position_nudge(x = .25))+
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat3b,
             mapping = aes(x = y2,y = Estimate,fill = "Neural signature"),position = position_nudge(x = .25))+
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  coord_flip()+
  
  theme_bw()+
  scale_y_continuous(limits = c(-.15,.5))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "none",
        legend.title.position = "top")
plot3
#############
# 
# plot_all = ggarrange(legend,plot1,plot3,plot2,align = "v",nrow=4,heights = c(.1,.35,.7,1.2),labels = c("","A","B","C"))
# plot_all

p1 =  ggarrange(plot1,plot3,align = "v",nrow=2,heights = c(.35,.7),labels = c("A","B"))
p2 =  ggarrange(p1,plot2,ncol=2,labels = c("","C"),widths = c(1,1.1))
plot_all = ggarrange(legend,p2,nrow=2,heights = c(.1,1))
plot_all


ggsave(plot = plot_all,filename = paste(base_dir,"Final/regression_plots.jpeg",sep = ""),   dpi = 500,width = 10,height = 5)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/regression_plots.pdf",sep = ""),   dpi = 500,width = 10,height = 5)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/regression_plots.png",sep = ""),   dpi = 500,width = 10,height = 5)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/regression_plots.bmp",sep = ""),   dpi = 500,width = 10,height = 5)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/regression_plots.tiff",sep = ""),   dpi = 500,width = 10,height = 5)
