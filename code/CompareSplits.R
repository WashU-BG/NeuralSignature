

library(data.table)
library(dplyr)
library(lmtest)
library(sandwich)
library(DescTools)



base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")


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


task_all = fread(paste(base_dir,"raw/nback_2bv0b_all.csv",sep=""),data.table = F) 
task_all = task_all %>% dplyr::select(c("src_subject_id","eventname")) %>% unique()

task_all  =merge(task_all,QC.info)

task_all = task_all %>% dplyr::filter(imgincl_t1w_include == 1,imgincl_nback_include == 1)
task_all = task_all %>% dplyr::select(c("src_subject_id","eventname","site_id_l")) %>% unique()



behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

behav.dat$motion = behav.dat[,grep(x = colnames(behav.dat),pattern = paste("nback_all_meanmotion",sep=""))]


dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","sex2","interview_age",
                                    "pds","ed",
                                    "demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h","demo_race_o","demo_race_a",
                                    "income_1","income_2","income_3","income_4","income_5",
                                    "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                    "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                    "tfmri_nb_all_beh_c2b_mrt","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt",
                                    "motion")) %>% unique() %>%  na.omit()


df = dat %>% dplyr::select(c("Prisma_fit","Prisma","DISCOVERY","Achieva"))
dat$scanner =factor(names(df)[max.col(df[])])

df = dat %>% dplyr::select(c("income_1","income_2","income_3","income_4"))
dat$income =factor(names(df)[max.col(df[])])

df = dat %>% dplyr::select(c("demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h"))
dat$race =factor(names(df)[max.col(df[])])

dat2 = merge(task_all,dat)

split1 = c("site20","site17", "site16", "site19" ,"site01", "site08", "site15", "site18", "site21", "site03")

#split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
splits = data.frame(site_id_l =unique(dat2$site_id_l),split=2 )
splits$split[match(split1,splits$site_id_l)] = 1
dat2 = merge(dat2,splits,by = "site_id_l")

num.visits = table(dat2$src_subject_id) %>% as.data.frame()
colnames(num.visits) = c("src_subject_id","num.visits")
dat2 = merge(dat2,num.visits)

famnum = table(dat2$rel_family_id) %>% as.data.frame()
colnames(famnum) = c("rel_family_id","famsize")
dat2 = merge(dat2,famnum)

write.csv(x = dat2,file = paste(base_dir,"includedparticipants.csv"),quote = F,row.names = F)


############################################



var.c = c("interview_age", "ed","pds","motion","famsize",
          "tfmri_nb_all_beh_c2b_mrt","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt")
var.d = c("sex2","num.visits",
          "income_1","income_2","income_3","income_4","income_5",
          "demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h","demo_race_o","demo_race_a")


############################################

for(i in 1:length(var.c)){
  dat2$y.var = dat2[,which(colnames(dat2) == var.c[i])]
  
  m1 = lmer(y.var ~ split +(1|src_subject_id),data = dat2)
 
  
  out=data.frame(Y = var.c[i],
                 N.all =  length(unique(dat2$src_subject_id)),
                 mean.1 = dat2 %>% dplyr::filter(split == 1) %>% dplyr::select(c(var.c[i])) %>% unlist() %>% mean(),
                 sd.1 = dat2 %>% dplyr::filter(split == 1) %>% dplyr::select(c(var.c[i])) %>% unlist() %>% sd(),
                 
                 mean.2 = dat2 %>% dplyr::filter(split ==2) %>% dplyr::select(c(var.c[i])) %>% unlist() %>% mean(),
                 sd.2 = dat2 %>% dplyr::filter(split == 2) %>% dplyr::select(c(var.c[i])) %>% unlist() %>% sd(),
                 

                 
                 T.val = summary(m1)$coefficients[2,4],
                 P = summary(m1)$coefficients[2,5]
  )
  
  if(i ==1){out.c = out}else{out.c = rbind(out.c,out)}
  
}




for(i in 1:length(var.d)){
  dat2$y.var = dat2[,which(colnames(dat2) == var.d[i])]
  
  temp = dat2 %>% dplyr::select(c("src_subject_id","split",y.var)) %>% unique()
  
  a1 = chisq.test(x = temp$split,y = temp$y.var)
  
  group.comp = temp %>% dplyr::group_by(split,y.var) %>% reframe(n = table(y.var))
  
  out=data.frame(Y = var.d[i],
                 N.all =  length(unique(temp$src_subject_id)),
                 
                 
                 N.1 = temp %>% dplyr::filter(split == 1) %>% dplyr::select("src_subject_id") %>% unlist() %>% length(),
                 N.2 = temp %>% dplyr::filter(split == 2) %>% dplyr::select("src_subject_id") %>% unlist() %>% length(),

                 y.levels = paste(names(table(temp$y.var)),collapse = ","),
                 
                 N.1.1 = group.comp$n[1],
                 N.1.2 = group.comp$n[2],
                 N.2.1 = group.comp$n[3],
                 N.2.2 = group.comp$n[4],
                 
                 chisq = a1$statistic[[1]],
                 P = a1$p.value
  )
  
  
  out$Perc.1 = round(out$N.1.2/out$N.1*100,2)
  out$Perc.2 = round(out$N.2.2/out$N.2*100.2)
  
  
  if(i ==1){out.d = out}else{out.d = rbind(out.d,out)}
  
}



write.csv(x = out.c,file = "C:/Users/dbara/Documents/ABCD/NerualSig/comp_slits_c.csv",row.names = F)
write.csv(x = out.d,file = "C:/Users/dbara/Documents/ABCD/NerualSig/comp_slits_d.csv",row.names = F)
