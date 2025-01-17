library(data.table)
library(dplyr)
library(lme4)
library(DescTools)
library(pROC)
library(epiR)
library(ggplot2)
library(tidyr)
library(MetBrewer)
library(ggpubr)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

boot_out = fread(paste(base_dir,"Final/all.boots.bin.csv",sep=""),data.table = F)
# boot_out2 = fread(paste(base_dir,"Final/all.boots4.csv",sep=""),data.table = F)
# 
# boot_out = rbind(boot_out,boot_out2)

boot_out = na.omit(boot_out)
# 
# table(boot_out$n)
# boot_out_20 = boot_out %>% dplyr::filter(n == 20)
# hist(boot_out_20$auc,breaks=100)
# 
# 
# boot_out_100 = boot_out %>% dplyr::filter(n == 100)
# hist(boot_out_100$auc,breaks=100)
# 
# boot_out %>% dplyr::filter(n == 20) %>% dplyr::select("dip_2") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 50) %>% dplyr::select("dip_2") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 100) %>% dplyr::select("dip_0") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 250) %>% dplyr::select("auc") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 500 ) %>% dplyr::select("auc") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 1000) %>% dplyr::select("auc") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 2000) %>% dplyr::select("auc") %>% as.matrix() %>% hist(breaks=100)
# boot_out %>% dplyr::filter(n == 4000) %>% dplyr::select("dip_2") %>% as.matrix() %>% hist(breaks=100)


auc = boot_out %>% 
  group_by(n) %>% 
 summarise(L_CI = quantile(auc,probs = 0.025),
           median = quantile(auc,probs = 0.5),
           U_CI = quantile(auc,probs = 0.975)) %>% 
  as.data.frame()

rel = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(rel,probs = 0.025),
            median = quantile(rel,probs = 0.5),
            U_CI = quantile(rel,probs = 0.975)) %>% 
  as.data.frame()

cog = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(cog,probs = 0.025),
            median = quantile(cog,probs = 0.5),
            U_CI = quantile(cog,probs = 0.975)) %>% 
  as.data.frame()

listsorting = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(listsorting,probs = 0.025),
            median = quantile(listsorting,probs = 0.5),
            U_CI = quantile(listsorting,probs = 0.975)) %>% 
  as.data.frame()

attention = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(attention,probs = 0.025),
            median = quantile(attention,probs = 0.5),
            U_CI = quantile(attention,probs = 0.975)) %>% 
  as.data.frame()


ple = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(ple,probs = 0.025),
            median = quantile(ple,probs = 0.5),
            U_CI = quantile(ple,probs = 0.975)) %>% 
  as.data.frame()


dip_2 = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(dip_2,probs = 0.025),
            median = quantile(dip_2,probs = 0.5),
            U_CI = quantile(dip_2,probs = 0.975)) %>% 
  as.data.frame()

dip_0 = boot_out %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(dip_0,probs = 0.025),
            median = quantile(dip_0,probs = 0.5),
            U_CI = quantile(dip_0,probs = 0.975)) %>% 
  as.data.frame()

dip_2$condition = "2 - back"
dip_0$condition = "0 - back"
dip = rbind(dip_0,dip_2)

cog$var = "Total Composite"
listsorting$var = "List Sorting"
attention$var = "Attention problems"
ple$var = "Psychotic-like experiences"

cog2 = rbind(cog,listsorting,attention,ple)
psych2 = rbind(attention,ple)

cog2$var = as.factor(cog2$var)

########################


p1=ggplot(data = auc,aes(x = log(n),y=median,fill="X",color="X"))+
  ylab(label = "AUC")+
  xlab(label = "Training Sample Size")+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  scale_y_continuous(limits = c(0.5,.9))+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25       )+
    # scale_fill_manual(values =met.brewer(name = "Austria",n = 6,type = "discrete",direction = -1,override.order = T)[3])+
    # scale_color_manual(values =met.brewer(name = "Austria",n = 6,type = "discrete",direction = -1,override.order = T)[3])+
  scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = 1)+
  scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = 1)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank())+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T),color=guide_legend(reverse = T))


p2=ggplot(data = rel,aes(x = log(n),y=median,fill="X",color="X"))+
  ylab(label = "Split-half reliability (ICC)")+
  xlab(label = "Training Sample Size")+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
 scale_y_continuous(limits = c(0,0.5))+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25       )+
  # scale_fill_manual(values =met.brewer(name = "Austria",n = 6,type = "discrete",direction = -1,override.order = T)[3])+
  # scale_color_manual(values =met.brewer(name = "Austria",n = 6,type = "discrete",direction = -1,override.order = T)[3])+
  scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = 1)+
  scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = 1)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank())+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T),color=guide_legend(reverse = T))

# p2=ggplot(data = rel,aes(x = log(n),y=median,fill="X",color="X"))+
#   scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
#   xlab(label = "Training Sample Size")+
#  # scale_y_continuous(limits = c(0,.5))+
#   
#   ylab(label = "Split-half reliability (ICC)")+
#   geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25)+
#               
#               # color = met.brewer(name = "Austria",n = 1,type = "discrete",direction = -1,override.order = T),
#               # fill = met.brewer(name = "Austria",n = 1,type = "discrete",direction = -1,override.order = T)
#   scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = 1)+
#   scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = 1)+
#   
#   geom_point(shape=21,alpha=1,size=2,show.legend = F)+
#   geom_line(size=1,show.legend = F)+
#   theme_bw()+
#   theme(legend.position = "top",
#         panel.grid.minor = element_blank())+
#   guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T),color=guide_legend(reverse = T))


p3=ggplot(data = dip,aes(x = log(n),y=median,color=condition,fill = condition))+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  xlab(label = "Training Sample Size")+
  #scale_y_continuous(limits = c(0,.5))+
  scale_fill_manual(values =met.brewer(name = "Austria",n = 2,type = "discrete"))+
  scale_color_manual(values =met.brewer(name = "Austria",n = 2,type = "discrete"))+
  ylab(label = "Hartigans' dip test")+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25
  )+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  theme_bw()+
  theme(legend.position = "top",legend.title = element_blank())+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T),color=guide_legend(reverse = T))

legend1 = get_legend(p3)
legend1 = as_ggplot(legend1)
p3 = p3 + theme(legend.position = "none")


p4=ggplot(data = cog2,aes(x = log(n),y=median,fill = var,color=var))+
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #scale_y_continuous(limits = c(-.15,.35))+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  xlab(label = "Training Sample Size")+
  ylab(label = "Standardized regression estimate")+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  scale_fill_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  scale_color_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  
  theme_bw()+
  theme(legend.position = "top",legend.title = element_blank())+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T,nrow = 2),color=guide_legend(reverse = T,nrow = 2))

legend2 = get_legend(p4)
legend2 = as_ggplot(legend2)
p4 = p4 + theme(legend.position = "none")

all = ggarrange(p1,p2,legend2,legend1,p4,p3,nrow = 3,ncol = 2,labels = c("A","B","","","C","D"),heights = c(1,.2,1),align = "v")

ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot.jpeg",sep = ""),dpi = 500,width = 8,height =7 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot.pdf",sep = ""),dpi = 500,width = 8,height =7 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot.tiff",sep = ""),dpi = 500,width = 8,height =7 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot.bmp",sep = ""),dpi = 500,width = 8,height =7 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot.png",sep = ""),dpi = 500,width = 8,height =7 )

##############################################################




boot_cog = fread(paste(base_dir,"Final/all.boots.cog.2.csv",sep=""),data.table = F)
boot_cog = na.omit(boot_cog)


# temp = boot_cog %>% dplyr::filter(n == 2560) %>% dplyr::select(c("cog","args")) 
# temp$args[which(temp$cog == min(temp$cog))]

#(1:10000) [!c(1:10000)  %in%  boot_cog$args ]

rel.cog = boot_cog %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(rel,probs = 0.025),
            median = quantile(rel,probs = 0.5),
            U_CI = quantile(rel,probs = 0.975)) %>% 
  as.data.frame()

cog.cog = boot_cog %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(cog,probs = 0.025),
            median = quantile(cog,probs = 0.5),
            U_CI = quantile(cog,probs = 0.975)) %>% 
  as.data.frame()

listsorting.cog = boot_cog %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(listsorting,probs = 0.025),
            median = quantile(listsorting,probs = 0.5),
            U_CI = quantile(listsorting,probs = 0.975)) %>% 
  as.data.frame()

attention.cog = boot_cog %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(attention,probs = 0.025),
            median = quantile(attention,probs = 0.5),
            U_CI = quantile(attention,probs = 0.975)) %>% 
  as.data.frame()


ple.cog = boot_cog %>% 
  group_by(n) %>% 
  summarise(L_CI = quantile(ple,probs = 0.025),
            median = quantile(ple,probs = 0.5),
            U_CI = quantile(ple,probs = 0.975)) %>% 
  as.data.frame()


cog.cog$var = "Total Composite"
listsorting.cog$var = "List Sorting"
attention.cog$var = "Attention problems"
ple.cog$var = "Psychotic-like experiences"

cog2.cog = rbind(cog.cog,listsorting.cog,attention.cog,ple.cog)
cog2.cog$var = as.factor(cog2.cog$var)

########################


p2.cog=ggplot(data = rel.cog,aes(x = log(n),y=median))+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  xlab(label = "Training Sample Size")+
  scale_y_continuous(limits = c(0,.5))+
  
  ylab(label = "Split-half reliability (ICC)")+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25,
              color = met.brewer(name = "Austria",n = 1,type = "discrete",direction = -1,override.order = T),
              fill = met.brewer(name = "Austria",n = 1,type = "discrete",direction = -1,override.order = T)
  )+
  geom_point(shape=21,alpha=1,size=2,show.legend = F,fill = met.brewer(name = "Austria",n = 1,type = "discrete",direction = -1,override.order = T))+
  geom_line(size=1,show.legend = F,color = met.brewer(name = "Austria",n = 1,type = "discrete",direction = -1,override.order = T))+
  theme_bw()+
  theme(legend.position = "top")+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T),color=guide_legend(reverse = T))

p2.cog


p4.cog=ggplot(data = cog2.cog,aes(x = log(n),y=median,fill = var,color=var))+
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #scale_y_continuous(limits = c(-.15,.35))+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  xlab(label = "Training Sample Size")+
  ylab(label = "Standardized regression estimate")+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  scale_fill_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  scale_color_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  
  theme_bw()+
  theme(legend.position = "top",legend.title = element_blank())+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T,nrow = 2),color=guide_legend(reverse = T,nrow = 2))
p4.cog
p4


###################################################################




cog2$model = "Neural Signature"
cog2.cog$model = "ML Model"

cog.all = rbind(cog2,cog2.cog)

cog.all$var = factor(cog.all$var,levels = levels(cog.all$var)[c(4,2,1,3)])


cog.plot=ggplot(data = cog.all,aes(x = log(n),y=median,fill = model,color=model))+
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #scale_y_continuous(limits = c(-.15,.35))+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  xlab(label = "Training Sample Size")+
  ylab(label = "Standardized regression estimate")+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  # scale_fill_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  # scale_color_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = -1)+
  scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = -1)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title = element_blank(),    
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white"))+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T,nrow = 1),color=guide_legend(reverse = T,nrow = 1))+
  facet_wrap(~var,nrow = 2,ncol=2 )
cog.plot


legend1 = get_legend(cog.plot)
legend1 = as_ggplot(legend1)
cog.plot = cog.plot + theme(legend.position = "none")


all = ggarrange(p1,p2,legend2,legend1,p4,p3,nrow = 3,ncol = 2,labels = c("A","B","","","C","D"),heights = c(1,.2,1),align = "v")

all1 = ggarrange(p1,p2,labels = c("A","B"),ncol=2)

all = ggarrange(all1,legend1,cog.plot, labels = c("","","C"),nrow = 3,heights = c(1,.2,2.1))

ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot2.jpeg",sep = ""),dpi = 500,width = 8,height =9 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot2.pdf",sep = ""),dpi = 500,width = 8,height =9 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot2.png",sep = ""),dpi = 500,width = 8,height =9 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot2.bmp",sep = ""),dpi = 500,width = 8,height =9 )
ggsave(plot = all,filename = paste(base_dir,"Final/bootstrap_plot2.tiff",sep = ""),dpi = 500,width = 8,height =9 )



######################

cog.all$tot_n =cog.all$n + sapply(cog.all$median, FUN = function(X){pwr::pwr.r.test(r = X,sig.level = .05,power = .8 )$n })
cog.all$L_CI_n =cog.all$n + sapply(cog.all$L_CI, FUN = function(X){pwr::pwr.r.test(r = X,sig.level = .05,power = .8 )$n })
cog.all$U_CI_n =cog.all$n + sapply(cog.all$U_CI, FUN = function(X){pwr::pwr.r.test(r = X,sig.level = .05,power = .8 )$n })

cog.plot2=ggplot(data = cog.all,aes(x = log(n),
                                    y=log(tot_n),
                                    fill = model,color=model))+
  
 # geom_hline(yintercept = 0,linetype="dashed",color="black")+
 # scale_y_continuous(limits = c(0,10000))+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  scale_y_continuous(limits = c(2,13), 
                     labels = exp,
                     breaks = log(2^(0:20)*10))+
  
  xlab(label = "Training Sample Size")+
  ylab(label = "Total Sample Size needed for 80% power\n(training + test sets)")+
 # geom_ribbon(aes(ymin=U_CI_n, ymax=L_CI_n),alpha=.1,size=.25)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  geom_line(size=1,show.legend = F)+
  # scale_fill_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  # scale_color_manual(values =met.brewer(name = "Egypt",n = 4,type = "discrete",direction = -1,override.order = T))+
  scale_color_viridis_d(option = "H",begin = .25,end = .85,direction = -1)+
  scale_fill_viridis_d(option =  "H",begin = .25,end = .85,direction = -1)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title = element_blank(),    
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white"))+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = T,nrow = 1),color=guide_legend(reverse = T,nrow = 1))+
  facet_wrap(~var,nrow = 2,ncol=2 )

cog.plot2

cog.all %>% group_by(var,model) %>% summarise(min_total = min(tot_n))
