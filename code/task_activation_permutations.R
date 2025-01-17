#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]


library(data.table)
library(stringr)
library(dplyr)
library(lme4)

library(permute)

# brainmap activation permutations


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")


behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)
incl =  fread(paste(base_dir,"includedparticipants.csv",sep=""),data.table = F)

dat = merge(incl,behav.dat,all.x = T)

task_all.map = fread(paste(base_dir,"stabilitymap.csv",sep=""),data.table = F)
task_yr1.map = fread(paste(base_dir,"reliability_1_map.csv",sep=""),data.table = F)
task_yr2.map = fread(paste(base_dir,"reliability_2_map.csv",sep=""),data.table = F)

task_split.map = fread(paste(base_dir,"task_split_map.csv",sep=""),data.table = F)

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


brain.map = function(task){
  
  dat2 = merge(behav.dat,task)
  
  regions = grep(pattern = "combat",x = colnames(dat2)) 
  out.map = data.frame(region = colnames(dat2)[regions],t = NA)
  for(i in 1:length(regions)){
    
    dat2$ROI = dat2[,regions[i]]
    
    m1 = lmer(ROI ~ con + 
                rel_relationship_1+rel_relationship_2+rel_relationship_3+
                Prisma_fit+Prisma+DISCOVERY+Achieva+motion+split+
                #(1|site_id_l) +
                (1|rel_family_id) + (1|src_subject_id)
              ,data =dat2  )
    out.map$t[i] = summary(m1)$coefficients[2,3]
    
    
  }
  
  return(out.map)
  
}

################################
# permute

set.seed(args)

task_run1_yr1$con = task_run1_yr1$con[permute::shuffle(n = dim(task_run1_yr1)[1],control = how(blocks = task_run1_yr1$src_subject_id))]
task_run1_yr2$con = task_run1_yr2$con[permute::shuffle(n = dim(task_run1_yr2)[1],control = how(blocks = task_run1_yr2$src_subject_id))]
task_all_yr1$con = task_all_yr1$con[permute::shuffle(n = dim(task_all_yr1)[1],control = how(blocks = task_all_yr1$src_subject_id))]


task_all_split1$ID2 = paste(task_all_split1$src_subject_id,task_all_split1$eventname,sep = "_")
task_all_split1$con = task_all_split1$con[permute::shuffle(n = dim(task_all_split1)[1],control = how(blocks = task_all_split1$ID2))]


################################

task_all_yr1.map = brain.map(task_all_yr1)

task_run1_yr1.map = brain.map(task_run1_yr1)

task_run1_yr2.map = brain.map(task_run1_yr2)

task_task_all_split1.map = brain.map(task_all_split1)

##################################
colnames(task_run1_yr1.map)[2]="t_1.perm"
colnames(task_run1_yr2.map)[2]="t_1.perm"
colnames(task_all_yr1.map)[2]="t_1.perm"
colnames(task_task_all_split1.map)[2]="t_1.perm"

##########################################


task_all.map = merge(task_all.map,task_all_yr1.map)
task_yr1.map = merge(task_yr1.map,task_run1_yr1.map)
task_yr2.map = merge(task_yr2.map,task_run1_yr2.map)
task_split.map = merge(task_split.map,task_task_all_split1.map)




a1=cor.test(task_all.map$t_1.perm,task_all.map$t_2) #  0.9857655
a2=cor.test(task_yr1.map$t_1.perm,task_yr1.map$t_2) # 0.949578
a3=cor.test(task_yr2.map$t_1.perm,task_yr2.map$t_2) #  0.9732828
a4=cor.test(task_split.map$t_1.perm,task_split.map$t_2) #  0.9732828

############################################

out = data.frame(perm = args,
                 all_r = a1$estimate,
                 all_t = a1$statistic,
                 yr1_r = a2$estimate,
                 yr1_t = a2$statistic,
                 yr2_r = a3$estimate,
                 yr2_t = a3$statistic,
                 split_r = a4$estimate,
                 split_t = a4$statistic)


write.table(x = out,file = paste(base_dir,"perm_out/permuted.brain.maps.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")
if(args[1] == 1){
  write.table(x = t(colnames(out)),file = paste(base_dir,"perm_out/permuted.brain.maps.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  
}

