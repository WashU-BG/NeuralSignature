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

task_all = rbind(task_all_1,task_all_2)



behav.dat = dat %>% dplyr::select(all_of(c("src_subject_id","rel_family_id","site_id_l","eventname",
                                           "motion","Prisma_fit","Prisma", "split",
                                           "DISCOVERY","Achieva","rel_relationship_1",
                                           "rel_relationship_2","rel_relationship_3" )))

task_all.temp = merge(task_all,dat %>% dplyr::select(c("src_subject_id","eventname","split")))
task_all= task_all.temp

task_all_split1 = task_all.temp %>% dplyr::filter(split ==1 )
task_all_split2 = task_all.temp %>% dplyr::filter(split ==2 )


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

set.seed(args+7365383)

task_all$ID2 = paste(task_all$src_subject_id,task_all$eventname,sep = "_")
task_all$con = task_all$con[permute::shuffle(n = dim(task_all)[1],control = how(blocks = task_all$ID2))]


task_all_split1$ID2 = paste(task_all_split1$src_subject_id,task_all_split1$eventname,sep = "_")
task_all_split1$con = task_all_split1$con[permute::shuffle(n = dim(task_all_split1)[1],control = how(blocks = task_all_split1$ID2))]


task_all_split2$ID2 = paste(task_all_split2$src_subject_id,task_all_split2$eventname,sep = "_")
task_all_split2$con = task_all_split2$con[permute::shuffle(n = dim(task_all_split2)[1],control = how(blocks = task_all_split2$ID2))]

################################

task_all.map = brain.map(task_all)
task_task_all_split1.map = brain.map(task_all_split1)
task_task_all_split2.map = brain.map(task_all_split2)

##################################

a1 = t(task_all.map) %>% as.data.frame()
colnames(a1) = a1[1,]
a1 = a1[-1,]

a2 = t(task_task_all_split1.map) %>% as.data.frame()
colnames(a2) = a2[1,]
a2 = a2[-1,]

a3 = t(task_task_all_split2.map) %>% as.data.frame()
colnames(a3) = a3[1,]
a3 = a3[-1,]

############################################


write.table(x = a1,file = paste(base_dir,"perm_out/permuted.all.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")

write.table(x = a1,file = paste(base_dir,"perm_out/permuted.split1.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")

write.table(x = a1,file = paste(base_dir,"perm_out/permuted.split2.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")

if(args[1] == 1){
  write.table(x = t(colnames(a1)),file = paste(base_dir,"perm_out/permuted.all.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  write.table(x = t(colnames(a2)),file = paste(base_dir,"perm_out/permuted.split1.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
    write.table(x = t(colnames(a3)),file = paste(base_dir,"perm_out/permuted.split2.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  }

