
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MetBrewer)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

region_reliabilities = fread( file = paste(base_dir,"Final/region_reliabilities.csv",sep=""),data.table = F)
ML_reliabilities = fread( file = paste(base_dir,"Final/ML_reliability.csv",sep=""),data.table = F)

region_reliabilities$con = as.factor(region_reliabilities$con)
levels(region_reliabilities$con) = c("0-back","2-back","2-back > 0-back")

ML_reliabilities$measure = as.factor(ML_reliabilities$measure)
levels(ML_reliabilities$measure) = c("0-back","2-back","2-back > 0-back")

temp_cols = c(viridis::plasma(n = 1,begin = .2),
              viridis::plasma(n = 1,begin = .8),
              viridis::plasma(n = 1,begin = .5))

ML_reliabilities$y_loc = ML_reliabilities$measure %>% as.factor() %>% as.numeric()


########################
p1 = ggplot(region_reliabilities %>% dplyr::filter(wave=="baseline"),aes(x = rel,fill=as.factor(con)))+
  scale_x_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  xlab("Split-half reliability - baseline wave")+
 # ggtitle(label = "Individual regions")+
  
  #ggtitle(label = "",subtitle = "Split-half")
  
   scale_fill_manual(values=temp_cols)+
   scale_color_manual(values=temp_cols)+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*1,name = "Regions"))+
  # scale_color_viridis_d(option = "C",begin = .2,end = .8)+
  # scale_fill_viridis_d(option = "C",begin = .2,end = .8)+
  geom_density(alpha=.5)+
  theme_bw()+
  
  theme(plot.margin = unit(c(0,1,1,1), 'lines'))+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
  theme(plot.title = element_text(size=12),legend.title.position = "top")+
  labs(fill = "Condition")+
  guides(colour = guide_legend( nrow=1,override.aes = list(alpha = 1)),fill = guide_legend(nrow=1,override.aes = list(alpha = 1)))
  #geom_histogram(binwidth = .01,position = "identity",alpha=.5)
p1
legend = get_legend(p1)
legend = as_ggplot(legend)
p1 = p1 +theme(legend.position = "none")
p1


p2 = ggplot(ML_reliabilities %>% dplyr::filter(type == "baseline"),aes(x = rel,y = y_loc,fill = measure,color = measure))+
  scale_x_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  xlab("Reliability")+
 # ggtitle(label = "Neural signature predictions")+
 scale_color_manual(values=temp_cols)+
  scale_fill_manual(values=temp_cols)+
  geom_segment(aes(y=y_loc,yend=y_loc,x = rel.LCI,xend = rel.UCI),lwd=2)+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*1,name = "NSEN"),minor_breaks = c(1,2,3),breaks = c(1,2,3),limits = c(0,4))+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
  
  geom_point(shape=21,color="black",size=3)+
  theme_bw()+
  labs(fill = "Condition",color = "Condition")+
  theme(plot.title = element_text(size=12))+
  theme(plot.margin = unit(c(0,1,0,1), 'lines'))+
  
  theme(axis.title.y.left = element_blank(),legend.title.position = "top",axis.title.x = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank())+theme(legend.position = "none")

p2

plot1 = ggarrange(p2,p1,nrow = 2,common.legend = FALSE,align = "v",heights = c(.33,1))
plot1

###################################

p3 = ggplot(region_reliabilities %>% dplyr::filter(wave=="followup"),aes(x = rel,fill=as.factor(con)))+
  scale_x_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  xlab("Split-half reliability - follow-up year 2")+
 # ggtitle(label = "Individual regions")+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*1,name = "Regions"))+
  
  #ggtitle(label = "",subtitle = "Split-half")
 scale_color_manual(values=temp_cols)+
  scale_fill_manual(values=temp_cols)+
  geom_density(alpha=.5)+
  theme_bw()+
  theme(plot.margin = unit(c(0,1,1,1), 'lines'))+
  
  theme(plot.title = element_text(size=12),legend.title.position = "top")+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
  
  labs(fill = "Condition")+theme(legend.position = "none")
#geom_histogram(binwidth = .01,position = "identity",alpha=.5)
p3
p4 = ggplot(ML_reliabilities %>% dplyr::filter(type == "followup"),aes(x = rel,y = y_loc,fill = measure,color = measure))+
  scale_x_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  xlab("Reliability")+
 # ggtitle(label = "Neural signature predictions")+
 scale_color_manual(values=temp_cols)+
  scale_fill_manual(values=temp_cols)+
  geom_segment(aes(y=y_loc,yend=y_loc,x = rel.LCI,xend = rel.UCI),lwd=2)+
  geom_point(shape=21,color="black",size=3)+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*1,name = "NSEN"),minor_breaks = c(1,2,3),breaks = c(1,2,3),limits = c(0,4))+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
  
  theme_bw()+
  labs(fill = "Condition",color = "Condition")+
  theme(plot.title = element_text(size=12))+
  theme(plot.margin = unit(c(0,1,0,1), 'lines'))+
  
  theme(axis.title.y.left = element_blank(),legend.title.position = "top",axis.title.x = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank())+theme(legend.position = "none")

p4

plot2 = ggarrange(p4,p3,nrow = 2,common.legend = FALSE,align = "v",heights = c(.33,1))
plot2

##############################


p5 = ggplot(region_reliabilities %>% dplyr::filter(wave=="long"),aes(x = rel,fill=as.factor(con)))+
  scale_x_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  xlab("2-year longitudinal stability")+
 # ggtitle(label = "Individual regions")+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*1,name = "Regions"))+
  
  #ggtitle(label = "",subtitle = "Split-half")
 scale_color_manual(values=temp_cols)+
  scale_fill_manual(values=temp_cols)+
  geom_density(alpha=.5)+
  theme_bw()+
  theme(plot.margin = unit(c(0,1,1,1), 'lines'))+
  
  theme(plot.title = element_text(size=12),legend.title.position = "top")+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
  
  labs(fill = "Condition")+theme(legend.position = "none")
#geom_histogram(binwidth = .01,position = "identity",alpha=.5)
p5


p6 = ggplot(ML_reliabilities %>% dplyr::filter(type == "long"),aes(x = rel,y = y_loc,fill = measure,color = measure))+
  scale_x_continuous(limits = c(0,.7),breaks = seq(0,.7,.1))+
  xlab("Reliability")+
 # ggtitle(label = "Neural signature predictions")+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*1,name = "NSEN"),minor_breaks = c(1,2,3),breaks = c(1,2,3),limits = c(0,4))+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
    
 scale_color_manual(values=temp_cols)+
  scale_fill_manual(values=temp_cols)+
  geom_segment(aes(y=y_loc,yend=y_loc,x = rel.LCI,xend = rel.UCI),lwd=2)+
  geom_point(shape=21,color="black",size=3)+
  theme_bw()+
  theme(plot.margin = unit(c(0,1,0,1), 'lines'))+
  
  labs(fill = "Condition",color = "Condition")+
  theme(plot.title = element_text(size=12))+
  
  theme(axis.title.y.left =  element_blank(),legend.title.position = "top",axis.title.x = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank())+theme(legend.position = "none")

p6

plot3 = ggarrange(p6,p5,nrow = 2,common.legend = FALSE,align = "v",heights = c(.33,1))
plot3

###################


all_plots = ggarrange(legend,plot1,plot2,plot3,labels = c("","A","B","C"),common.legend = FALSE,
                      nrow=4,heights =c(.3,1,1,1))
all_plots
ggsave(plot = all_plots,filename = paste(base_dir,"Final/reliability.jpeg",sep = ""),dpi = 500,width = 3.5,height = 7)
ggsave(plot = all_plots,filename = paste(base_dir,"Final/reliability.pdf",sep = ""),dpi = 500,width = 4,height = 8)
ggsave(plot = all_plots,filename = paste(base_dir,"Final/reliability.png",sep = ""),dpi = 500,width = 4,height = 8)
ggsave(plot = all_plots,filename = paste(base_dir,"Final/reliability.bmp",sep = ""),dpi = 500,width = 4,height = 8)
ggsave(plot = all_plots,filename = paste(base_dir,"Final/reliability.tiff",sep = ""),dpi = 500,width = 4,height = 8)

