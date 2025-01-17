# brainmap activation


library(data.table)
library(dplyr)
library(lme4)
library(DescTools)
library(lmerTest)



base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")


behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)
incl =  fread(paste(base_dir,"includedparticipants.csv",sep=""),data.table = F)

dat = merge(incl,behav.dat,all.x = T)

######################################


fmri_standardize=function(dat){
  
  dat = dat %>% dplyr::select(dplyr::all_of(c("src_subject_id","eventname","site_id_l","con")),ends_with("combat"))
  dat = dat[order(dat$eventname,dat$src_subject_id,dat$con),]
  
  dat[,-c(1:4)] = dat[,-c(1:4)] %>% apply(2,scale,scale=T,center=T)
  
  return(dat)
}



task_con = "nback_2b_v_0b"
task = "nback"

task_all_1 = fread(paste(base_dir,"combat/",task_con,"_all_1_combat.csv",sep=""),data.table = F) 
task_all_2 = fread(paste(base_dir,"combat/",task_con,"_all_2_combat.csv",sep=""),data.table = F) 

task_run1_1 = fread(paste(base_dir,"combat/",task_con,"_run1_1_combat.csv",sep=""),data.table = F)
task_run1_2 = fread(paste(base_dir,"combat/",task_con,"_run1_2_combat.csv",sep=""),data.table = F)

task_run2_1 = fread(paste(base_dir,"combat/",task_con,"_run2_1_combat.csv",sep=""),data.table = F)
task_run2_2 = fread(paste(base_dir,"combat/",task_con,"_run2_2_combat.csv",sep=""),data.table = F)


task_all = rbind(task_all_1,task_all_2)
task_run1 = rbind(task_run1_1,task_run1_2)
task_run2 = rbind(task_run2_1,task_run2_2)



behav.dat = dat %>% dplyr::select(all_of(c("src_subject_id","rel_family_id","site_id_l","eventname",
                                                 "motion","Prisma_fit","Prisma", "split",
                                                 "DISCOVERY","Achieva","rel_relationship_1",
                                                 "rel_relationship_2","rel_relationship_3" )))

task_run1_yr1 = task_run1 %>% dplyr::filter(eventname =="baseline_year_1_arm_1")
task_run1_yr2 = task_run1 %>% dplyr::filter(eventname =="2_year_follow_up_y_arm_1")

task_run2_yr1 = task_run2 %>% dplyr::filter(eventname =="baseline_year_1_arm_1")
task_run2_yr2 = task_run2 %>% dplyr::filter(eventname =="2_year_follow_up_y_arm_1")

task_all_yr1 = task_all %>% dplyr::filter(eventname =="baseline_year_1_arm_1")
task_all_yr2 = task_all %>% dplyr::filter(eventname =="2_year_follow_up_y_arm_1")


task_all.temp = merge(task_all,dat %>% dplyr::select(c("src_subject_id","eventname","split")))


task_all_split1 = task_all.temp %>% dplyr::filter(split ==1 )
task_all_split2 = task_all.temp %>% dplyr::filter(split ==2 )


yr1.keep = intersect(task_run1_yr1$src_subject_id,task_run2_yr1$src_subject_id)
task_run1_yr1 = task_run1_yr1 %>% dplyr::filter(src_subject_id %in% yr1.keep)
task_run2_yr1 = task_run2_yr1 %>% dplyr::filter(src_subject_id %in% yr1.keep)

yr2.keep = intersect(task_run1_yr2$src_subject_id,task_run2_yr2$src_subject_id)
task_run1_yr2 = task_run1_yr2 %>% dplyr::filter(src_subject_id %in% yr2.keep)
task_run2_yr2 = task_run2_yr2 %>% dplyr::filter(src_subject_id %in% yr2.keep)

all.keep = intersect(task_all_yr1$src_subject_id,task_all_yr2$src_subject_id)
task_all_yr1 = task_all_yr1 %>% dplyr::filter(src_subject_id %in% all.keep)
task_all_yr2 = task_all_yr2 %>% dplyr::filter(src_subject_id %in% all.keep)

#################################################################################


brain.map = function(task,return.beta=FALSE){
  
  dat2 = merge(behav.dat,task)
  
  regions = grep(pattern = "combat",x = colnames(dat2)) 
  out.map = data.frame(region = colnames(dat2)[regions],t = NA)
  
  if(return.beta == TRUE){out.map = data.frame(region = colnames(dat2)[regions],
                                               t = NA,
                                               beta=NA,
                                               SE=NA) }
  
  
  for(i in 1:length(regions)){
    
    dat2$ROI = dat2[,regions[i]]
    
    m1 = lmer(ROI ~ con + 
                rel_relationship_1+rel_relationship_2+rel_relationship_3+
                Prisma_fit+Prisma+DISCOVERY+Achieva+motion+split+
                #(1|site_id_l) +
                (1|rel_family_id) + (1|src_subject_id)
              ,data =dat2  )
    out.map$t[i] = summary(m1)$coefficients[2,4]
    
    if(return.beta == TRUE){  
      out.map$beta[i] = summary(m1)$coefficients[2,1] 
      out.map$SE[i] = summary(m1)$coefficients[2,2] 
      
      }
    
  }
  
  return(out.map)
  
}

################################
task_all_full.map = brain.map(task_all,return.beta = TRUE)

task_all_yr1.map = brain.map(task_all_yr1)
task_all_yr2.map = brain.map(task_all_yr2)

task_run1_yr1.map = brain.map(task_run1_yr1)
task_run2_yr1.map = brain.map(task_run2_yr1)

task_run1_yr2.map = brain.map(task_run1_yr2)
task_run2_yr2.map = brain.map(task_run2_yr2)

task_task_all_split1.map = brain.map(task_all_split1)
task_task_all_split2.map = brain.map(task_all_split2)


##################################
colnames(task_run1_yr1.map)[2]="t_1"
colnames(task_run2_yr1.map)[2]="t_2"
task_yr1.map = merge(task_run1_yr1.map,task_run2_yr1.map)


colnames(task_run1_yr2.map)[2]="t_1"
colnames(task_run2_yr2.map)[2]="t_2"
task_yr2.map = merge(task_run1_yr2.map,task_run2_yr2.map)


colnames(task_all_yr1.map)[2]="t_1"
colnames(task_all_yr2.map)[2]="t_2"
task_all.map = merge(task_all_yr1.map,task_all_yr2.map)

colnames(task_task_all_split1.map)[2]="t_1"
colnames(task_task_all_split2.map)[2]="t_2"
task_split.map = merge(task_task_all_split1.map,task_task_all_split2.map)


a1 = cor.test(task_all.map$t_1,task_all.map$t_2) #  0.9857655
a2 = cor.test(task_yr1.map$t_1,task_yr1.map$t_2) # 0.949578
a3 = cor.test(task_yr2.map$t_1,task_yr2.map$t_2) #  0.9732828
a4 = cor.test(task_split.map$t_1,task_split.map$t_2) #  0.9732828

############################################

write.csv(x = task_all_full.map,file = paste(base_dir,"fulltaskmap.csv",sep=""),quote = F,row.names = F)

write.csv(x = task_all.map,file = paste(base_dir,"stabilitymap.csv",sep=""),quote = F,row.names = F)
write.csv(x = task_yr1.map,file = paste(base_dir,"reliability_1_map.csv",sep=""),quote = F,row.names = F)
write.csv(x = task_yr2.map,file = paste(base_dir,"reliability_2_map.csv",sep=""),quote = F,row.names = F)

write.csv(x = task_split.map,file = paste(base_dir,"task_split_map.csv",sep=""),quote = F,row.names = F)
#####################################################

## permutations must be run first
perm.out = fread(paste(base_dir,"Final/all.permuted.associationmaps.csv",sep=""),data.table = F)

library(fitdistrplus)

accel_beta = function(dat,target){
  fit<-fitdist(data = dat^2,distr  = "beta")
  pval <- 1-pbeta(target^2,shape1 = fit$estimate[1], shape2 = fit$estimate[2]) 
  return(pval)
}


p1 = accel_beta(dat = perm.out$all_r ,target = a1$estimate)
p2 = accel_beta(dat = perm.out$yr1_r ,target = a2$estimate)
p3 = accel_beta(dat = perm.out$yr2_r ,target = a3$estimate)
p4 = accel_beta(dat = perm.out$split_r ,target = a4$estimate)



############################################################################

library(ggseg)
library(ggsegDesterieux)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggpubr)

atlas = behav.dat = fread(paste(base_dir,"Atlas_regions.csv",sep=""),data.table = F)
colnames(atlas)[2] = "region"

dat.plot = merge(atlas,task_all_full.map )
colnames(dat.plot)[1] = "region_orig"


J<-colorRampPalette(colors = c(rgb_gsea()[2], "#FFFFFF", rgb_gsea(reverse = T)[1]),
                    space = "rgb",interpolate = "linear")

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
  labs(fill =   "2-back > 0-back: t-statistic" )
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
  labs(fill =   "2-back > 0-back: t-statistic" )
task_effects2

task_effects_all   = ggarrange(task_effects,task_effects2,ncol = 2,common.legend = T,widths = c(6,1))


##################################

task_comparison1 = ggplot(data = task_yr1.map,aes(x = t_1,y=t_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  
  ggtitle(label = "Run 1 vs. Run 2: Baseline")+
  xlab(label = "Run 1 - t-statistic")+
  ylab(label = "Run 2 - t-statistic")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.95, ",p[robust] ,"= 1.1x",10^{-8},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
task_comparison1

task_comparison2 = ggplot(data = task_yr2.map,aes(x = t_1,y=t_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  
  ggtitle(label = "Run 1 vs. Run 2: Follow-up Year 2")+
  xlab(label = "Run 1 - t-statistic")+
  ylab(label = "Run 2 - t-statistic")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.97, ",p[robust] ,"= 4.3x",10^{-7},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
task_comparison2


task_comparison_all = ggplot(data = task_all.map,aes(x = t_1,y=t_2))+
  geom_hline(yintercept = 0,color="darkgrey")+
  geom_vline(xintercept = 0,color="darkgrey")+
  geom_abline(slope = 1,intercept = 0,color="darkgrey",linetype="dashed")+
  geom_point(shape=21,fill="lightblue")+
  geom_smooth(method = 'lm',formula = 'y~x',se=F,color="black")+
  scale_y_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  scale_x_continuous(limits = c(-60,60),breaks = seq(-60,60,20))+
  
  ggtitle(label = "Baseline vs. Follow-up Year 2")+
  xlab(label = "Baseline - t-statistic")+
  ylab(label = "Follow-up Year 2 - t-statistic")+
  #annotate("text",x = -50,y = 55,label = "r = 0.99",size=4)+
  #annotate("text",x = -50,y = 50,label = expression(paste("p=2.4x",10^{-7},sep="")),size=4)+
  labs(caption = expression(paste("r = 0.99, ",p[robust] ,"= 6.4x",10^{-10},sep="")))+
  
  theme_bw()+
  theme(aspect.ratio=1,plot.caption = element_text(size = 12)) 
task_comparison_all


all_scatter = ggarrange(task_comparison1,task_comparison2,task_comparison_all,ncol = 3,
                        labels =c("B","C","D") )
all_scatter


all=ggarrange(task_effects_all,all_scatter,nrow = 2,heights = c(1,1.3),labels = c("A",""))
all

ggsave(plot = all,filename = paste(base_dir,"Final/taskmap.jpeg",sep = ""),dpi = 500,width = 10,height = 7)
ggsave(plot = all,filename = paste(base_dir,"Final/taskmap.pdf",sep = ""),dpi = 500,width = 10,height = 7)
ggsave(plot = all,filename = paste(base_dir,"Final/taskmap.tiff",sep = ""),dpi = 500,width = 10,height = 7)
ggsave(plot = all,filename = paste(base_dir,"Final/taskmap.bmp",sep = ""),dpi = 500,width = 10,height = 7)
ggsave(plot = all,filename = paste(base_dir,"Final/taskmap.png",sep = ""),dpi = 500,width = 10,height = 7)




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

ggsave(plot = task_comparison_splits,filename = paste(base_dir,"Final/tasksplitscomparison.jpeg",sep = ""),dpi = 500,width = 4,height = 5)
ggsave(plot = task_comparison_splits,filename = paste(base_dir,"Final/tasksplitscomparison.pdf",sep = ""),dpi = 500,width = 4,height = 5)
ggsave(plot = task_comparison_splits,filename = paste(base_dir,"Final/tasksplitscomparison.png",sep = ""),dpi = 500,width = 4,height = 5)
ggsave(plot = task_comparison_splits,filename = paste(base_dir,"Final/tasksplitscomparison.tiff",sep = ""),dpi = 500,width = 4,height = 5)
ggsave(plot = task_comparison_splits,filename = paste(base_dir,"Final/tasksplitscomparison.bmp",sep = ""),dpi = 500,width = 4,height = 5)

