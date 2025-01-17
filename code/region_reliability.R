
library(data.table)
library(rptR)
library(lme4)
library(dplyr)

###############

##  removed_related
no_relatives = function(input.dat){
  
  fam_nums = table(input.dat$rel_family_id) %>% as.data.frame()
  fam_nums$rel_family_id = names(table(input.dat$rel_family_id))
  
  
  for(i in 1: dim(fam_nums)[1]){
    
    temp = input.dat[which(input.dat$rel_family_id == fam_nums$rel_family_id[i]),]
    
    if(dim(table(temp$src_subject_id))[1]> 1){
      
      input.dat = input.dat[-which(input.dat$rel_family_id == fam_nums$rel_family_id[i]),]
      
      ids = table(temp$src_subject_id)
      id.keep = names(ids[unname(which(ids == max(ids)))])
      
      id.keep = id.keep[sample(x = c(1:length(id.keep)),size = 1,replace = F)]
      temp = temp[which(temp$src_subject_id == id.keep),]
      
      input.dat = rbind(input.dat,temp)
      
      
    }
    
    
  }
  
  return(input.dat)
  
}


## two visits only

onlytwo = function(input.dat){
  num_visits = table(input.dat$src_subject_id) %>% as.data.frame()
  num_visits$Var1 = as.character(num_visits$Var1)
  colnames(num_visits) = c("src_subject_id","num_visits")
  input.dat = merge(input.dat,num_visits)
  input.dat = input.dat %>% dplyr::filter(num_visits == 2)
  return(input.dat)
}

##############

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

########################################################
behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","sex2","interview_age",
                                    "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                    "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                    "tfmri_nback_all_meanmotion")) %>% unique()

###############################################

get.task.data = function(task_con,run){
task_1 = fread(paste(base_dir,"combat/",task_con,"_",run,"_1_combat.csv",sep=""),data.table = F) 
task_2 = fread(paste(base_dir,"combat/",task_con,"_",run,"_2_combat.csv",sep=""),data.table = F)
task_1$split=1
task_2$split=2
task_all = rbind(task_1,task_2)
return(task_all)
}


task_all = get.task.data(task_con = "nback_2b_v_0b",run = "all")
task_run1 = get.task.data(task_con = "nback_2b_v_0b",run = "run1")
task_run2 = get.task.data(task_con = "nback_2b_v_0b",run = "run2")

diff_all = get.task.data(task_con = "nback_2bv0b",run = "all")
diff_run1 = get.task.data(task_con = "nback_2bv0b",run = "run1")
diff_run2 = get.task.data(task_con = "nback_2bv0b",run = "run2")

diff_all$con = 3
diff_run1$con = 3
diff_run2$con = 3


################################################

regions = colnames(task_all)[grep(colnames(task_all),pattern = "combat")]



region_reliability = function(t1,t2,type,con,wave){
  merged = rbind(t1,t2)
  merged = onlytwo(merged)
  merged = no_relatives(merged)
  
  
  merged = merge(merged,dat) %>% na.omit()
  
  for(region in 1:length(regions)){
    
    merged$region =  merged[,c(which(colnames(merged) == regions[region]))] 
    
    rel =   rpt(region ~  split +
                  Prisma_fit+Prisma+DISCOVERY+Achieva+
                  tfmri_nback_all_meanmotion+
                  (1|src_subject_id)  +
                  (1|site_id_l) ,#+
                data = merged,adjusted = T,
                grname ="src_subject_id",datatype = "Gaussian",npermut = 0,nboot = 0)
    
    
    out = data.frame(type = type,
                     con=con,
                     wave = wave,
                     region = regions[region],
                     rel = rel$R$src_subject_id,
                     p = rel$P$LRT_P)
    
    if(region == 1){out.final.temp = out}else{out.final.temp = rbind(out.final.temp,out)}
    
    
  }
  
  return(out.final.temp)
}




split_0_baseline = region_reliability(t1 = task_run1 %>% dplyr::filter(con == 0, eventname == "baseline_year_1_arm_1"),
                                      t2 = task_run2 %>% dplyr::filter(con == 0, eventname == "baseline_year_1_arm_1"),
                                      type = "split_half",con = 0,wave = "baseline")
split_2_baseline = region_reliability(t1 = task_run1 %>% dplyr::filter(con == 1, eventname == "baseline_year_1_arm_1"),
                                      t2 = task_run2 %>% dplyr::filter(con == 1, eventname == "baseline_year_1_arm_1"),
                                      type = "split_half",con = 2,wave = "baseline")


split_0_followup = region_reliability(t1 = task_run1 %>% dplyr::filter(con == 0, eventname == "2_year_follow_up_y_arm_1"),
                                      t2 = task_run2 %>% dplyr::filter(con == 0, eventname == "2_year_follow_up_y_arm_1"),
                                      type = "split_half",con = 0,wave = "followup")
split_2_followup = region_reliability(t1 = task_run1 %>% dplyr::filter(con == 1, eventname == "2_year_follow_up_y_arm_1"),
                                      t2 = task_run2 %>% dplyr::filter(con == 1, eventname == "2_year_follow_up_y_arm_1"),
                                      type = "split_half",con = 2,wave = "followup")


split_diff_baseline = region_reliability(t1 = diff_run1 %>% dplyr::filter( eventname == "baseline_year_1_arm_1"),
                                      t2 = diff_run2 %>% dplyr::filter(eventname == "baseline_year_1_arm_1"),
                                      type = "split_half",con = 3,wave = "baseline")
split_diff_followup = region_reliability(t1 = diff_run1 %>% dplyr::filter( eventname == "2_year_follow_up_y_arm_1"),
                                         t2 = diff_run2 %>% dplyr::filter(eventname == "2_year_follow_up_y_arm_1"),
                                         type = "split_half",con = 3,wave = "followup")


long_0 = region_reliability(t1 = task_all %>% dplyr::filter(con == 0, eventname == "baseline_year_1_arm_1"),
                           t2 = task_all %>% dplyr::filter(con == 0, eventname == "2_year_follow_up_y_arm_1"),
                           type = "long",con = 0,wave = "long")

long_2 = region_reliability(t1 = task_all %>% dplyr::filter(con == 1, eventname == "baseline_year_1_arm_1"),
                            t2 = task_all %>% dplyr::filter(con == 1, eventname == "2_year_follow_up_y_arm_1"),
                            type = "long",con = 2,wave = "long")

long_diff = region_reliability(t1 = diff_all %>% dplyr::filter( eventname == "baseline_year_1_arm_1"),
                               t2 = diff_all %>% dplyr::filter( eventname == "2_year_follow_up_y_arm_1"),
                               type = "long",con = 3,wave = "long")


all_reliabilities = rbind(split_0_baseline,split_2_baseline,split_0_followup,split_2_followup,split_diff_baseline,split_diff_followup,long_0,long_2,long_diff)

write.table(x = all_reliabilities,file = paste(base_dir,"Final/region_reliabilities.csv",sep=""),quote = F,row.names = F,col.names = T,sep = ",")



