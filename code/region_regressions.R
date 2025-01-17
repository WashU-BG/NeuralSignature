#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]



library(data.table)
library(rptR)
library(lme4)
library(dplyr)
library(lmerTest)
library(sandwich)
library(lmtest)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")



###############
inormal <- function(x){qnorm((rank(x, na.last = "keep") - 0.5) / sum(!is.na(x)))}
winsorize = function(x,q=3){
  
  mean_x = mean(x,na.rm = T)
  sd_x = sd(x,na.rm = T)
  top_q = mean_x + q*sd_x
  bottom_q = mean_x - q*sd_x
  
  x[x>top_q] = top_q
  x[x<bottom_q] = bottom_q
  
  return(x)
  
}
##############


get.task.data = function(task_con,run){
  task_1 = fread(paste(base_dir,"combat/",task_con,"_",run,"_1_combat.csv",sep=""),data.table = F) 
  task_2 = fread(paste(base_dir,"combat/",task_con,"_",run,"_2_combat.csv",sep=""),data.table = F)
  task_1$split=1
  task_2$split=2
  task_all = rbind(task_1,task_2)
  return(task_all)
}


task_all = get.task.data(task_con = "nback_2b_v_0b",run = "all")
diff_all = get.task.data(task_con = "nback_2bv0b",run = "all")


diff_all$con = 3

task_all = rbind(task_all,diff_all)



##############


behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

behav.dat$diff_rate = behav.dat$tfmri_nb_all_beh_c2b_rate - behav.dat$tfmri_nb_all_beh_c0b_rate
behav.dat$diff_mrt = behav.dat$tfmri_nb_all_beh_c2b_mrt - behav.dat$tfmri_nb_all_beh_c0b_mrt


behav.dat$motion = behav.dat$tfmri_nback_all_meanmotion

cons = c(0,1,3)

regions = colnames(task_all)[grep(colnames(task_all),pattern = "combat")]


for(i in 1: length(cons) ){
  
  ML_long = task_all %>% dplyr::filter(con == cons[i])
  ML_long = merge(ML_long,behav.dat)
  
 # ML_long$region =  ML_long[,c(which(colnames(ML_long) == regions[args]))] 
  
  
  nback_perf = c("tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
                 "diff_rate","diff_mrt",
                 "tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c2b_rate")
  
  analysis.framework=expand.grid(y.var = colnames(ML_long)[c(grep(colnames(ML_long),pattern = "^cbcl.+_r$"),
                                                           grep(colnames(ML_long),pattern = "pps_y_ss_number"),
                                                           #grep(colnames(ML_long),pattern = "^bis"),
                                                           #grep(colnames(ML_long),pattern = "^upps"),
                                                           #grep(colnames(ML_long),pattern = "any"),
                                                           
                                                           #grep(colnames(ML_long),pattern = "pea_ravlt_sd_trial_i_tc"),
                                                           #grep(colnames(ML_long),pattern = "pea_ravlt_ld_trial_vii_tc"),
                                                          # grep(colnames(ML_long),pattern = "pea_wiscv_tss"),
                                                           #grep(colnames(ML_long),pattern = "lmt_scr_perc_correct"),
                                                           grep(colnames(ML_long),pattern = "_agecorrected$")#,
                                                         # grep(colnames(ML_long),pattern = paste(nback_perf,sep = "",collapse = "|")) 
                                                          )],
                                 
                                 x.var = colnames(ML_long)[which(colnames(ML_long) == regions[args])])

  for(m in 1:dim(analysis.framework)[1]){
    
    print(paste(i,m))
    
    ML_long$x.var =  ML_long[,c(which(colnames(ML_long) == analysis.framework$x.var[m]))] 
    ML_long$y.var =  ML_long[,c(which(colnames(ML_long) == analysis.framework$y.var[m]))] 
    
    
    all.vars = c("src_subject_id","eventname","site_id_l","rel_family_id","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
                 "x.var","y.var","motion","rel_relationship_1", "rel_relationship_2","rel_relationship_3",
                 "sex2","pds","interview_age","split",
                 "Prisma_fit","Prisma","DISCOVERY","Achieva"
    )
    
    ML_long2 = ML_long %>% dplyr::select(all_of(all.vars))%>% unique() %>% na.omit()
      
      
    var.names=c( "x.var","motion","rel_relationship_1","rel_relationship_2","rel_relationship_3",
                 "sex2","pds","split","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
                 "Prisma_fit","Prisma","DISCOVERY","Achieva")
    
    var.names2=c("pds","motion","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt")
    
    
    
    ML_long2 = ML_long2 %>% 
      mutate_at(.vars = var.names2,.funs = winsorize) %>% 
      mutate_at(.vars = var.names2,.funs = function(X){
        if(abs(psych::skew(X)) > 1){inormal(X)}else{X}
      }) %>% 
      mutate_at(.vars = var.names,.funs = scale,center=T,scale=T)
    
    if(length(table(ML_long2$y.var)) > 2){
      
      ML_long2 = ML_long2 %>% 
        mutate_at(.vars = "y.var",.funs = winsorize) %>% 
        mutate_at(.vars = "y.var",.funs = function(X){
          if(abs(psych::skew(X)) > 1){inormal(X)}else{X}
        }) %>% 
        mutate_at(.vars = "y.var",.funs = scale,center=T,scale=T)
      
    }
    
    
    ML_long2$Age1 = poly(ML_long2$interview_age,2)[,1] %>% scale(center = T,scale = T)
    ML_long2$Age2 = poly(ML_long2$interview_age,2)[,2] %>% scale(center = T,scale = T)
    
    if(length(table(ML_long2$y.var)) > 2){
    m1 = lmer(y.var ~ x.var + tfmri_nb_all_beh_c0b_mrt  + tfmri_nb_all_beh_c2b_mrt +
                                 Age1+Age2+
                                 rel_relationship_1+rel_relationship_2+rel_relationship_3+
                                 pds+sex2 +
                # ed+income_1+income_2+income_3+income_4+
                # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
                                 Prisma_fit+Prisma+DISCOVERY+Achieva+
                                  motion+
                                split+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
                               ,data =ML_long2  )
    m1.out = summary(m1)$coefficients[2,] %>% t() %>% as.data.frame()
    
    }
    if(length(table(ML_long2$y.var))  ==  2){
      m1= glm(y.var ~ x.var + 
                  Age1+Age2+
                  rel_relationship_1+rel_relationship_2+rel_relationship_3+
                  pds+sex2 +split+
                # ed+income_1+income_2+income_3+income_4+
                # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
                  Prisma_fit+Prisma+DISCOVERY+Achieva+motion
                ,data =ML_long2,family = "binomial"  )
    m1b = coeftest(m1,vcov = vcovCL,cluster = ~ site_id_l + rel_family_id + src_subject_id)
    m1.out = m1b[2,] %>% t() %>% as.data.frame()
    
      }
    
    
    if(any(colnames(m1.out) == "df")){
          m1.out = m1.out[,-which(colnames(m1.out) == "df")]
    }
    
    colnames(m1.out) = c("Estimate","SE","t_z","p")
    
    out      = data.frame(contrast = cons[i],
                          region =analysis.framework$x.var[m],
                          y = analysis.framework$y.var[m],
                          N = length(unique(ML_long2$src_subject_id)),
                          N.obs = length((ML_long2$src_subject_id))
                          )
    
    out = cbind(out,m1.out)
    
    if(m == 1 & i == 1){reg.long = out}else{reg.long = rbind(reg.long,out)}
    
  }
  
  
}


write.table(x = reg.long,file = paste(base_dir,"regression_out/region_regressions.conperf.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")

if(args[1] == 1){
  write.table(x = t(colnames(reg.long)),file = paste(base_dir,"regression_out/region_regressions.conperf.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  
}