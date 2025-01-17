#pre-process MRI data
library(longCombat)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)


qc =fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging/mri_y_qc_incl.csv")
mri.info = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging/mri_y_adm_info.csv")
motion = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging/mri_y_qc_motion.csv")
site.info = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/abcd-general/abcd_y_lt.csv")

demo = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/abcd-general/abcd_p_demo.csv")
demo = demo %>% 
  dplyr::filter(eventname == "baseline_year_1_arm_1") %>% 
  dplyr::select(c("src_subject_id","demo_sex_v2"))
demo$demo_sex_v2[demo$demo_sex_v2 == 3] = 1

site.info = site.info %>% dplyr::filter(eventname == "baseline_year_1_arm_1" | eventname == "2_year_follow_up_y_arm_1")
site.info = site.info %>% dplyr::filter(site_id_l != "site22" )
  
QC.info = merge(qc,site.info)
QC.info = merge(QC.info,demo,by = "src_subject_id")
##################################
split1 = c("site20","site17", "site16", "site19" ,"site01", "site08", "site15", "site18", "site21", "site03")

#split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
splits = data.frame(site_id_l =unique(QC.info$site_id_l),split=2 )
splits$split[match(split1,splits$site_id_l)] = 1
QC.info = merge(QC.info,splits,by = "site_id_l")
#################################

abcd_combat = function(task,con1,con2,run,split_num){
library(dplyr)
library(tidyr)

d1 = fread(paste(basedir,task,"_",con1,"_",run,".csv",sep=""),data.table = F)
d2 = fread(paste(basedir,task,"_",con2,"_",run,".csv",sep=""),data.table = F)

d1$con = 0
d2$con = 1

dat = rbind(d1,d2)

dat = merge(dat,QC.info)

dat = dat %>% dplyr::filter(split ==split_num )

#filter on QC
dat = dat %>% dplyr::filter(imgincl_t1w_include == 1)
dat = dat[dat[,which(colnames(dat) == paste("imgincl_",task,"_include",sep=""))] == 1,]

dat2 = dat %>% dplyr::select(all_of(c("src_subject_id","eventname","site_id_l","con","interview_age","demo_sex_v2")),
                             matches("left|right|brain")) %>% 
  na.omit()

  colnames(dat2) = str_replace_all(string = colnames(dat2),pattern = "-",replacement = "_")
  colnames(dat2) = str_replace_all(string = colnames(dat2),pattern = " ",replacement = "_")
  
  
  dat2$Age1 = poly(dat2$interview_age,2)[,1]
  dat2$Age2 = poly(dat2$interview_age,2)[,2]
  
  baseline = dat2 %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
  followup = dat2 %>% dplyr::filter(eventname == "2_year_follow_up_y_arm_1")
  
  id.count = table(baseline$src_subject_id)
  to.remove = names(id.count[id.count != 2])
  to.remove2 = lapply(to.remove, function(x) which(baseline$src_subject_id %in% x)) %>% unlist()
  if(!is.null(to.remove2)){baseline = baseline[-to.remove2,]}
  baseline = baseline[order(baseline$src_subject_id),]
  
  id.count = table(followup$src_subject_id)
  to.remove = names(id.count[id.count != 2])
  to.remove2 = lapply(to.remove, function(x) which(followup$src_subject_id %in% x)) %>% unlist()
  if(!is.null(to.remove2)){followup = followup[-to.remove2,]}
    followup = followup[order(followup$src_subject_id),]

  dat2 = rbind(baseline,followup)


  detach(package:tidyr,unload = T)
  detach(package:dplyr,unload = T)
  
  dat2.combat = longCombat(data = dat2,
                             idvar = "src_subject_id",
                             timevar = "eventname",
                             batchvar = "site_id_l",
                             features = grep(x = colnames(dat2),pattern = "left|right|brain"),
                             formula = "con + demo_sex_v2 + Age1 + Age2",
                             ranef = "(1|src_subject_id)"
  )
  
  library(dplyr)
  library(tidyr)
  dat3 = dat2.combat$data_combat
  dat3$con = dat2$con
  
  write.csv(x = dat3,file = paste("C:/Users/dbara/Documents/ABCD/NerualSig/combat/",
                                  task,"_",con2,"_v_",con1,"_",run,"_",split_num,"_","combat.csv",
                                  sep=""),
            quote = F,row.names = F)
  
  
}


abcd_combat_single = function(task,con1,run,split_num){
  library(dplyr)
  library(tidyr)
  
  d1 = fread(paste(basedir,task,"_",con1,"_",run,".csv",sep=""),data.table = F)

  dat=d1
  
  dat = merge(dat,QC.info)
  dat = dat %>% dplyr::filter(split ==split_num )
  
  #filter on QC
  dat = dat %>% dplyr::filter(imgincl_t1w_include == 1)
  dat = dat[dat[,which(colnames(dat) == paste("imgincl_",task,"_include",sep=""))] == 1,]
  
  dat2 = dat %>% dplyr::select(all_of(c("src_subject_id","eventname","site_id_l","interview_age","demo_sex_v2")),
                               matches("left|right|brain")) %>% 
    na.omit()
  
  colnames(dat2) = str_replace_all(string = colnames(dat2),pattern = "-",replacement = "_")
  colnames(dat2) = str_replace_all(string = colnames(dat2),pattern = " ",replacement = "_")
  
  
  dat2$Age1 = poly(dat2$interview_age,2)[,1]
  dat2$Age2 = poly(dat2$interview_age,2)[,2]
  
  detach(package:tidyr,unload = T)
  detach(package:dplyr,unload = T)
  
  dat2.combat = longCombat(data = dat2,
                           idvar = "src_subject_id",
                           timevar = "eventname",
                           batchvar = "site_id_l",
                           features = grep(x = colnames(dat2),pattern = "left|right|brain"),
                           formula = "demo_sex_v2 + Age1 + Age2",
                           ranef = "(1|src_subject_id)"
  )
  
  library(dplyr)
  library(tidyr)
  dat3 = dat2.combat$data_combat
  dat3$con = dat2$con1
  
  write.csv(x = dat3,file = paste("C:/Users/dbara/Documents/ABCD/NerualSig/combat/",
                                  task,"_",con1,"_",run,"_",split_num,"_","combat.csv",
                                  sep=""),
            quote = F,row.names = F)
  
  
}

basedir = "C:/Users/dbara/Documents/ABCD/NerualSig/raw/"


for(split_num in 1:2){

abcd_combat(task = "nback",con1 = "0b",con2 = "2b",run = "all",split_num = split_num)
abcd_combat_single(task = "nback",con1 = "2bv0b",run = "all",split_num=split_num)

abcd_combat(task = "nback",con1 = "plc",con2 = "emo",run = "all",split_num=split_num)
abcd_combat_single(task = "nback",con1 = "fvplc",run = "all",split_num=split_num)

abcd_combat(task = "nback",con1 = "psfvntf",con2 = "ngfvntf",run = "all",split_num=split_num)

##

abcd_combat(task = "mid",con1 = "alvn",con2 = "arvn",run = "all",split_num=split_num)

abcd_combat(task = "mid",con1 = "aslvn",con2 = "allvn",run = "all",split_num=split_num)
abcd_combat_single(task = "mid",con1 = "alvsl",run = "all",split_num=split_num)

abcd_combat(task = "mid",con1 = "asrvn",con2 = "alrvn",run = "all",split_num=split_num)
abcd_combat_single(task = "mid",con1 = "alvsr",run = "all",split_num=split_num)
  
abcd_combat(task = "mid",con1 = "lpvnf",con2 = "rpvnf",run = "all",split_num=split_num)


##

abcd_combat(task = "sst",con1 = "isvcg",con2 = "csvcg",run = "all",split_num=split_num)
abcd_combat_single(task = "sst",con1 = "csvis",run = "all",split_num=split_num)
  
abcd_combat(task = "sst",con1 = "asvcg",con2 = "csvcg",run = "all",split_num=split_num)

abcd_combat(task = "sst",con1 = "asvcg",con2 = "isvcg",run = "all",split_num=split_num)

##############


  
  
abcd_combat(task = "nback",con1 = "0b",con2 = "2b",run = "run1",split_num=split_num)
abcd_combat_single(task = "nback",con1 = "2bv0b",run = "run1",split_num=split_num)

abcd_combat(task = "nback",con1 = "plc",con2 = "emo",run = "run1",split_num=split_num)
abcd_combat_single(task = "nback",con1 = "fvplc",run = "run1",split_num=split_num)

abcd_combat(task = "nback",con1 = "psfvntf",con2 = "ngfvntf",run = "run1",split_num=split_num)

##

abcd_combat(task = "mid",con1 = "alvn",con2 = "arvn",run = "run1",split_num=split_num)

abcd_combat(task = "mid",con1 = "aslvn",con2 = "allvn",run = "run1",split_num=split_num)
abcd_combat_single(task = "mid",con1 = "alvsl",run = "run1",split_num=split_num)

abcd_combat(task = "mid",con1 = "asrvn",con2 = "alrvn",run = "run1",split_num=split_num)
abcd_combat_single(task = "mid",con1 = "alvsr",run = "run1",split_num=split_num)

abcd_combat(task = "mid",con1 = "lpvnf",con2 = "rpvnf",run = "run1",split_num=split_num)


##

abcd_combat(task = "sst",con1 = "isvcg",con2 = "csvcg",run = "run1",split_num=split_num)
abcd_combat_single(task = "sst",con1 = "csvis",run = "run1",split_num=split_num)

abcd_combat(task = "sst",con1 = "asvcg",con2 = "csvcg",run = "run1",split_num=split_num)

abcd_combat(task = "sst",con1 = "asvcg",con2 = "isvcg",run = "run1",split_num=split_num)

###################

abcd_combat(task = "nback",con1 = "0b",con2 = "2b",run = "run2",split_num=split_num)
abcd_combat_single(task = "nback",con1 = "2bv0b",run = "run2",split_num=split_num)

abcd_combat(task = "nback",con1 = "plc",con2 = "emo",run = "run2",split_num=split_num)
abcd_combat_single(task = "nback",con1 = "fvplc",run = "run2",split_num=split_num)

abcd_combat(task = "nback",con1 = "psfvntf",con2 = "ngfvntf",run = "run2",split_num=split_num)

##

abcd_combat(task = "mid",con1 = "alvn",con2 = "arvn",run = "run2",split_num=split_num)

abcd_combat(task = "mid",con1 = "aslvn",con2 = "allvn",run = "run2",split_num=split_num)
abcd_combat_single(task = "mid",con1 = "alvsl",run = "run2",split_num=split_num)

abcd_combat(task = "mid",con1 = "asrvn",con2 = "alrvn",run = "run2",split_num=split_num)
abcd_combat_single(task = "mid",con1 = "alvsr",run = "run2",split_num=split_num)

abcd_combat(task = "mid",con1 = "lpvnf",con2 = "rpvnf",run = "run2",split_num=split_num)


##

abcd_combat(task = "sst",con1 = "isvcg",con2 = "csvcg",run = "run2",split_num=split_num)
abcd_combat_single(task = "sst",con1 = "csvis",run = "run2",split_num=split_num)

abcd_combat(task = "sst",con1 = "asvcg",con2 = "csvcg",run = "run2",split_num=split_num)

abcd_combat(task = "sst",con1 = "asvcg",con2 = "isvcg",run = "run2",split_num=split_num)
}
