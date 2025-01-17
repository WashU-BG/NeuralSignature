#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]


library(data.table)
library(lme4)
library(dplyr)





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


##############

classification.pred = fread(paste(base_dir,"ML_classification_out_test.csv",sep=""),data.table=F)
twoback = classification.pred %>% dplyr::filter(run == 3, con == 1) %>% dplyr::select(-c("con"))
zeroback = classification.pred %>% dplyr::filter(run == 3, con == 0)%>% dplyr::select(-c("con"))

colnames(twoback)[6] = "twoback"
colnames(zeroback)[6] = "zeroback"

nsen = merge(zeroback,twoback)
nsen$diff = nsen$twoback - nsen$zeroback

# plot(nsen$twoback,nsen$zeroback)
# hist(nsen$diff)

#regression.pred = regression.pred %>% dplyr::filter(run == 3)
#regression.pred = fread(paste(base_dir,"ML_regression_out_test.csv",sep=""),data.table=F)

#colnames(classification.pred)[7] = "classs.pred"
#colnames(regression.pred)[7] = "regress.pred"


#ML_all = merge(classification.pred,regression.pred)

model.setup = expand.grid(models = c("diff"))

#######################

taskcon = "nback"

task = "nback"

behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

behav.dat$diff_rate = behav.dat$tfmri_nb_all_beh_c2b_rate - behav.dat$tfmri_nb_all_beh_c0b_rate
behav.dat$diff_mrt = behav.dat$tfmri_nb_all_beh_c2b_mrt - behav.dat$tfmri_nb_all_beh_c0b_mrt


ML_indiff_regression = fread(paste(base_dir,"ML_indiff_regression_withsig.csv",sep=""),data.table = F)

nback_perf = c(
  "tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c2b_rate"
)

analysis.framework=expand.grid(#x.var =  c("twoback","zeroback","diff"),
  
  x.var =  NA,
  y.var = colnames(behav.dat)[c(grep(colnames(behav.dat),pattern = "^cbcl.+_r$"),
                                grep(colnames(behav.dat),pattern = "pps_y_ss_number"),
                                #grep(colnames(behav.dat),pattern = "^bis"),
                                #grep(colnames(behav.dat),pattern = "^upps"),
                                #grep(colnames(behav.dat),pattern = "any"),
                                
                                #grep(colnames(behav.dat),pattern = "pea_ravlt_sd_trial_i_tc"),
                                #grep(colnames(behav.dat),pattern = "pea_ravlt_ld_trial_vii_tc"),
                                # grep(colnames(behav.dat),pattern = "pea_wiscv_tss"),
                                #grep(colnames(behav.dat),pattern = "lmt_scr_perc_correct"),
                                grep(colnames(behav.dat),pattern = "_agecorrected$")
                                ,
                                grep(colnames(behav.dat),pattern = paste(nback_perf,sep = "",collapse = "|"))
  )])

analysis.framework$x.var = paste(analysis.framework$y.var,"_ML_base",sep="")


behav.dat = merge(behav.dat,ML_indiff_regression,all=TRUE)


#behav.dat$motion = behav.dat[,grep(x = colnames(behav.dat),pattern = paste(task,".+motion",sep=""))]
# ML_in_long = ML_in %>% dplyr::filter(con == 1, run == 3)
ML_in_long = merge(nsen,behav.dat,all.x = T)


analysis.framework$y.var = as.character(analysis.framework$y.var)
analysis.framework$x.var = as.character(analysis.framework$x.var)

# ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == model.setup$models[args]))] 
# ML_in_long$cov.var =  ML_in_long[,c(which(colnames(ML_in_long) == model.setup$cov.var[args]))] 
# 
# analysis.framework = analysis.framework[-which(analysis.framework$y.var == model.setup$cov.var[args]),] %>% as.data.frame()
# colnames(analysis.framework)[1]="y.var"

out = matrix(data = NA,ncol = dim(analysis.framework)[1],nrow = 50) %>% as.data.frame()
colnames(out) = analysis.framework$y.var

for(m in 1:dim(analysis.framework)[1]){
  print(paste(m))
  
  ML_in_long$y.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework$y.var[m]))] 
  ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework$x.var[m]))] 
  
  
  
  all.vars = c("src_subject_id","eventname","site_id_l","rel_family_id",
               "x.var","y.var","rel_relationship_1", "rel_relationship_2","rel_relationship_3",
               "sex2","pds","interview_age","split","diff",
               
               "ed","income_1","income_2","income_3","income_4","demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
               
               "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
               "Prisma_fit","Prisma","DISCOVERY","Achieva",
               colnames(ML_in_long)[grep(colnames(ML_in_long),
                                         pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
               
  )
  
  ML_long2 = ML_in_long %>% dplyr::select(all_of(all.vars))%>% unique() %>% na.omit()
  
  
  var.names=c( "x.var","rel_relationship_1","rel_relationship_2","rel_relationship_3",
               "sex2","pds","split","diff",
               "ed","income_1","income_2","income_3","income_4","demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
               "Prisma_fit","Prisma","DISCOVERY","Achieva",
               colnames(ML_in_long)[grep(colnames(ML_in_long),
                                         pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
  )
  
  var.names2=c("pds","ed","diff","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
               colnames(ML_in_long)[grep(colnames(ML_in_long),pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
  )
  
  
  ML_long2 = ML_long2 %>% 
    mutate_at(.vars = var.names2,.funs = winsorize) %>% 
    
    mutate_at(.vars = var.names2,.funs = function(X){
      if(abs(psych::skew(X)) > 1){log(X+2)}else{X}
    }) %>% 
    
    
    mutate_at(.vars = var.names,.funs = scale,center=T,scale=T)
  
  if(length(table(ML_long2$y.var)) > 2){
    
    ML_long2 = ML_long2 %>% 
      mutate_at(.vars = "y.var",.funs = winsorize) %>% 
      mutate_at(.vars = "y.var",.funs = function(X){
        if(abs(psych::skew(X)) > 1){log(X+2)}else{X}
      }) %>% 
      mutate_at(.vars = "y.var",.funs = scale,center=T,scale=T)
    
  }
  
  
  ML_long2$Age1 = poly(ML_long2$interview_age,2)[,1] %>% scale(center = T,scale = T)
  ML_long2$Age2 = poly(ML_long2$interview_age,2)[,2] %>% scale(center = T,scale = T)
  
 
  ############################
  m3 = lmer(y.var ~ x.var + 
              diff+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2 ,REML = FALSE )
 
 # model_diff(m3)
  
  
  model_diff = function(model){
    dat = model@frame
    
    mod = lmer(y.var ~ x.var + 
                 diff+
                 #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
                 Age1+Age2+
                 tfmri_nback_all_meanmotion+
                 rel_relationship_1+rel_relationship_2+rel_relationship_3+
                 pds+sex2 +split+
                 #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
                 # ed+income_1+income_2+income_3+income_4+
                 # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
                 Prisma_fit+Prisma+DISCOVERY+Achieva+
                 # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
                 (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id),REML = FALSE
               ,data =dat  )
    
    mod0 = update(object = mod,~. - x.var - diff,evaluate = TRUE)
    mod1 = update(object = mod,~. - diff,evaluate = TRUE)
    mod2 = update(object = mod,~. - x.var,evaluate = TRUE)
    aic_diff = (AIC(mod1) - AIC(mod0)) -  (AIC(mod2) - AIC(mod0))
    
    return(aic_diff)
  }
  print("start bootstrap")
  # model_diff(model = m3)
 
  
 
    boots = bootMer(x =m3,FUN = model_diff,nsim = 50,type = "parametric",use.u = FALSE )
    
  

    out[,m] = boots$t
    
  #################  

  
  
}# end analysis framework loop





write.table(x = out,file = paste(base_dir,"aic_out/boots.allcon.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")



if(args[1] == 1){
  write.table(x = t(colnames(out)),file = paste(base_dir,"aic_out/boots.allcon.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
}


