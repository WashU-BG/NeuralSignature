

library(data.table)
library(rptR)
library(lme4)
library(dplyr)
library(lmerTest)
library(sandwich)
library(lmtest)
library(car)
library(foreach)
library(doParallel)


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

plot(nsen$twoback,nsen$zeroback)
hist(nsen$diff)

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
ML_indiff_regression = ML_indiff_regression %>% dplyr::select(-"split")

nback_perf = c(
  "tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c2b_rate"
)


y.vars = colnames(behav.dat)[c(grep(colnames(behav.dat),pattern = "^cbcl.+_r$"),
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
)]



analysis.framework=expand.grid(
  y.var = y.vars,
  y.var2 = y.vars
)

analysis.framework = analysis.framework %>% dplyr::filter(y.var != y.var2)
analysis.framework$x.var = paste(analysis.framework$y.var2,"_ML_base",sep="")


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

library(doSNOW)
library(foreach)

cl <- makeCluster(20)
registerDoSNOW(cl)

iterations = dim(analysis.framework)[1]

pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
# dim(analysis.framework)[1]
reg.long = foreach(m = 1:iterations,.combine = 'rbind',.packages = c('lme4','lmerTest','psych','dplyr'),.options.snow = opts) %dopar%
{

#for(m in 1:dim(analysis.framework)[1])
 # {
  #print(paste(m))
  
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
               "Prisma_fit","Prisma","DISCOVERY","Achieva", "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate",
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
  
  # ML_long2$motion1 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,1] %>% scale(center = T,scale = T)
  # ML_long2$motion2 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,2] %>% scale(center = T,scale = T)
  # ML_long2$motion3 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,3] %>% scale(center = T,scale = T)
  # ML_long2$motion4 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,4] %>% scale(center = T,scale = T)
  
  ####################################
  
  m1 = lmer(y.var ~ x.var + 
              # diff+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              #motion1 + motion2 + motion3 + motion4+ 
              tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2  )
  
  m2 = lmer(y.var ~ x.var + 
              # diff+
              tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              #motion1 + motion2 + motion3 + motion4+ 
               tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2  )
  
  
  m1.out = summary(m1)$coefficients[2,] %>% t() %>% as.data.frame()
  m1.ci = confint(m1,parm = "x.var")
  m1.out = cbind(m1.out,t(m1.ci[1,]))
  
  colnames(m1.out) = c("Estimate","SE","df","t","p","L_CI","U_CI")
  m1.out$mod = "base"
  
  m2.out = summary(m2)$coefficients[c(2),] %>% t() %>% as.data.frame()
  
  m2.ci = tryCatch(confint(m2,parm = "x.var"),error = function(e){return(t(c(-1,-1)))})
  
  m2.out = cbind(m2.out,t(m2.ci[1,]))
  
  colnames(m2.out) = c("Estimate","SE","df","t","p","L_CI","U_CI")
  m2.out$mod = c("con.perf")
  
  ####################################
  
  out      = data.frame(#con = taskcon,
    x = analysis.framework$x.var[m],
    y = analysis.framework$y.var[m],
    N = length(unique(ML_long2$src_subject_id)),
    N.obs = length((ML_long2$src_subject_id))
  )
  
  out1 = cbind(out,m1.out)
  out2 = cbind(out,m2.out)

  out = rbind(out1,out2)
  return(out)
  #if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
  # if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
  
  
  
}# end analysis framework loop

close(pb)
stopCluster(cl)
registerDoSEQ()


write.table(x = reg.long,file = paste(base_dir,"ML_regressions_compare_MLmodels_toeachother_fixed.csv",sep=""),quote = F,row.names = F,col.names = T,sep = ",")

