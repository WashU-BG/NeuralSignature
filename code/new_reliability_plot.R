library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(see)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

region_reliabilities = fread( file = paste(base_dir,"Final/region_reliabilities.csv",sep=""),data.table = F)
ML_reliabilities = fread( file = paste(base_dir,"Final/ML_reliability.csv",sep=""),data.table = F,nrows = 9)


region_reliabilities$con = as.factor(region_reliabilities$con)
levels(region_reliabilities$con) = c("0-back","2-back","2-back > 0-back")

ML_reliabilities$measure = as.factor(ML_reliabilities$measure)
levels(ML_reliabilities$measure) = c("0-back","2-back","2-back > 0-back")

region_reliabilities$con = as.factor(region_reliabilities$con)
region_reliabilities$con = factor(region_reliabilities$con,levels = rev(levels(region_reliabilities$con)))

ML_reliabilities$measure = as.factor(ML_reliabilities$measure)
ML_reliabilities$measure = factor(ML_reliabilities$measure,levels = rev(levels(ML_reliabilities$measure)))


region_reliabilities %>% group_by(con,wave) %>% summarise(median = median(rel),max = max(rel))

####################################################


dat1a = region_reliabilities %>% dplyr::filter(wave == "baseline")
dat1b = ML_reliabilities %>% dplyr::filter(type == "baseline")


task_all_full.map =  fread(paste(base_dir,"fulltaskmap.csv",sep=""),data.table = F) 
dat.plot = merge(dat1a,task_all_full.map )
dat.plot = dat.plot %>% dplyr::filter(con == "2-back > 0-back")

plot(abs(dat.plot$t),dat.plot$rel)
  

plot1 = ggplot(dat1a,aes(x = con,y = rel ))+
#  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "Split-half reliability (ICC)")+
  ggtitle(label = "Baseline wave")+
  geom_violinhalf(scale = "width",alpha=.75,aes(fill = "Individual regions"))+
  
  geom_segment(inherit.aes = FALSE,data = dat1b,mapping = aes(y=rel.LCI,yend=rel.UCI,x = measure,xend = measure), 
               color=viridisLite::turbo(n = 1,begin = .9,end = .9),
               lwd=1,position = position_nudge(x = .25))+
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat1b,
             mapping = aes(x = measure     ,y = rel,fill = "Neural signature"),position = position_nudge(x = .25))+
  
  
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  coord_flip()+
  
  theme_bw()+
  scale_y_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "top",
        legend.title.position = "top")+
  guides(fill = guide_legend(override.aes = list(alpha=c(1,1),size=c(1,5))  ))
plot1 

legend = get_legend(plot1) %>% as_ggplot()
plot1 = plot1 + theme(legend.position = "none")


##########################

dat2a = region_reliabilities %>% dplyr::filter(wave == "followup")
dat2b = ML_reliabilities %>% dplyr::filter(type == "followup")


plot2 = ggplot(dat2a,aes(x = con,y = rel ))+
#  geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "Split-half reliability  (ICC)")+
  ggtitle(label = "Follow-up wave 2")+
  geom_violinhalf(scale = "width",alpha=.75,aes(fill = "Individual regions"))+
  
  geom_segment(inherit.aes = FALSE,data = dat2b,mapping = aes(y=rel.LCI,yend=rel.UCI,x = measure,xend = measure), 
               color=viridisLite::turbo(n = 1,begin = .9,end = .9),
               lwd=1,position = position_nudge(x = .25))+
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat2b,
             mapping = aes(x = measure     ,y = rel,fill = "Neural signature"),position = position_nudge(x = .25))+
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  coord_flip()+
  
  theme_bw()+
  scale_y_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "none",
        legend.title.position = "top")+
  guides(fill = guide_legend(override.aes = list(alpha=c(1,1),size=c(1,5))  ))
plot2


#############################################


dat3a = region_reliabilities %>% dplyr::filter(wave == "long")
dat3b = ML_reliabilities %>% dplyr::filter(type == "long")


plot3 = ggplot(dat3a,aes(x = con,y = rel ))+
  #geom_hline(yintercept = 0,linetype="dashed")+
  ylab(label = "2-year longitudinal stability  (ICC)")+
  ggtitle(label = "Across waves")+
  geom_violinhalf(scale = "width",alpha=.75,aes(fill = "Individual regions"))+
  
  geom_segment(inherit.aes = FALSE,data = dat3b,mapping = aes(y=rel.LCI,yend=rel.UCI,x = measure,xend = measure), 
               color=viridisLite::turbo(n = 1,begin = .9,end = .9),
               lwd=1,position = position_nudge(x = .25))+
  geom_point(shape=21,size=2,inherit.aes = FALSE,data = dat3b,
             mapping = aes(x = measure     ,y = rel,fill = "Neural signature"),position = position_nudge(x = .25))+
  scale_fill_manual(values = viridis::turbo(n = 20)[c(4,16)])+
  
  coord_flip()+
  
  theme_bw()+
  scale_y_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  theme(axis.title.y = element_blank(),
        legend.key.size = unit(13,"points"),legend.title = element_blank(),
        legend.position = "none",
        legend.title.position = "top")+
  guides(fill = guide_legend(override.aes = list(alpha=c(1,1),size=c(1,5))  ))
plot3

########


plot_all = ggarrange(legend,plot1,plot2,plot3,align = "v",nrow=4,heights = c(.2,1,1,1),labels = c("","A","B","C"))
plot_all
ggsave(plot = plot_all,filename = paste(base_dir,"Final/reliability_plots2.jpeg",sep = ""),   dpi = 500,width = 5,height = 6)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/reliability_plots2.tiff",sep = ""),   dpi = 500,width = 5,height = 6)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/reliability_plots2.pdf",sep = ""),   dpi = 500,width = 5,height = 6)
ggsave(plot = plot_all,filename = paste(base_dir,"Final/reliability_plots2.bmp",sep = ""),   dpi = 500,width = 5,height = 6)
