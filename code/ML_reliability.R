library(data.table)
library(stringr)
library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(MASS)
library(rptR)
library(lme4)
library(dplyr)
library(parallel)

############################


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

#############################

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

predictions = fread(paste(base_dir,"ML_classification_out_test_updated.csv",sep=""),data.table = F)

######################
back_0 = predictions %>% dplyr::filter(con == 0) %>% dplyr::select(-c("con"))
back_2 = predictions %>% dplyr::filter(con == 1) %>% dplyr::select(-c("con"))

colnames(back_0)[6] = c("back_0")
colnames(back_2)[6] = c("back_2")

predictions2 = merge(back_0,back_2)
predictions2$diff = predictions2$back_2 - predictions2$back_0

#######################

split_half_baseline = predictions2 %>% dplyr::filter(eventname  == "baseline_year_1_arm_1", run != 3)
split_half_followup = predictions2 %>% dplyr::filter(eventname  == "2_year_follow_up_y_arm_1", run != 3)
stability = predictions2 %>% dplyr::filter(run == 3)

keepn.obs = function(dat,keep){
v.keep = table(dat$src_subject_id) %>% as.data.frame() %>% dplyr::filter(Freq == keep) %>% dplyr::select("Var1") 
v.keep = v.keep$Var1 %>% as.character()
dat.new = dat[!is.na(match(x = dat$src_subject_id,table = v.keep)),]
return(dat.new)
}

stability2 = keepn.obs(stability,keep = 2)
split_half_baseline2 =  keepn.obs(split_half_baseline,keep = 2)
split_half_followup2 =  keepn.obs(split_half_followup,keep = 2)

############################

dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","sex2","interview_age",
                                    "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                    "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                    "tfmri_nback_all_meanmotion")) %>% unique()


stability2 = merge(stability2,dat,all.x = T) %>% na.omit()
split_half_baseline2 = merge(split_half_baseline2,dat,all.x = T) %>% na.omit()
split_half_followup2 = merge(split_half_followup2,dat,all.x = T)  %>% na.omit()


stability2 = no_relatives(stability2)
split_half_baseline2 = no_relatives(split_half_baseline2)
split_half_followup2 = no_relatives(split_half_followup2)

stability2_b = stability2 %>% 
  dplyr::select(c("src_subject_id","eventname","interview_age")) %>% 
  tidyr::pivot_wider(id_cols = src_subject_id,names_from =  eventname,values_from =interview_age )

stability2_b$diff = stability2_b$`2_year_follow_up_y_arm_1` - stability2_b$baseline_year_1_arm_1

mean(stability2_b$diff)/12
sd(stability2_b$diff)

################################
measure = c("back_0","back_2","diff")
for(i in 1:length(measure)){

stability2$measure = stability2[,which(colnames(stability2) == measure[i])]
split_half_baseline2$measure = split_half_baseline2[,which(colnames(split_half_baseline2) == measure[i])]
split_half_followup2$measure = split_half_followup2[,which(colnames(split_half_followup2) == measure[i])]

rel1 = rpt(measure ~  split +
             Prisma_fit+Prisma+DISCOVERY+Achieva+
             tfmri_nback_all_meanmotion+
             (1|src_subject_id)  +
             (1|site_id_l) ,#+
           data = split_half_baseline2,
           adjusted = T,
           grname ="src_subject_id",datatype = "Gaussian",npermut = 0,nboot = 10000,parallel = TRUE,ncores = 15)



rel2 = rpt(measure ~  split +
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              tfmri_nback_all_meanmotion+
              (1|src_subject_id)  +
              (1|site_id_l) ,#+
            data = split_half_followup2,adjusted = T,
            grname ="src_subject_id",datatype = "Gaussian",npermut = 0,nboot = 10000,parallel = TRUE,ncores = 15)



rel3 = rpt(measure ~  split +
            Prisma_fit+Prisma+DISCOVERY+Achieva+
            tfmri_nback_all_meanmotion+
            (1|src_subject_id)  +
            (1|site_id_l) ,#+
          data = stability2,adjusted = T,
          grname ="src_subject_id",datatype = "Gaussian",npermut = 0,nboot = 10000,parallel = TRUE,ncores = 15)


out = data.frame(measure = measure[i],
                 type = c("baseline","followup","long"),
                 p = c(rel1$P$LRT_P,rel3$P$LRT_P,rel3$P$LRT_P),
                 rel = c(rel1$R$src_subject_id,rel2$R$src_subject_id,rel3$R$src_subject_id),
                 rel.LCI = c(unname(rel1$CI_emp[1]),unname(rel2$CI_emp[1]),unname(rel3$CI_emp[1]))%>% unlist(),
                 rel.UCI = c(unname(rel1$CI_emp[2]),unname(rel2$CI_emp[2]),unname(rel3$CI_emp[2]))%>% unlist()
                 
                 )

if(i == 1){out.final = out}else{out.final = rbind(out.final,out)}
}
write.table(x = out.final,file = paste(base_dir,"Final/ML_reliability.csv",sep=""),quote = F,row.names = F,col.names = T,sep = ",")

