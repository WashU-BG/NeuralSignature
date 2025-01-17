

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


#ML_indiff_regression = fread(paste(base_dir,"ML_indiff_regression.csv",sep=""),data.table = F)
ML_indiff_regression2 = fread(paste(base_dir,"ML_indiff_regression_withsig.csv",sep=""),data.table = F)

#colnames(ML_indiff_regression2)[-c(1:4)] = paste (colnames(ML_indiff_regression2)[-c(1:4)],"_2",sep="")

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

analysis.framework$x.var = paste(analysis.framework$y.var,"_ML",sep="")


#behav.dat = merge(behav.dat,ML_indiff_regression,all=TRUE)
behav.dat = merge(behav.dat,ML_indiff_regression2,all=TRUE)


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


for(m in 1:dim(analysis.framework)[1]){
  print(paste(m))
  
  ML_in_long$y.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework$y.var[m]))] 
  ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == paste(analysis.framework$x.var[m],"_base",sep="") ))] 
  ML_in_long$x.var2 =  ML_in_long[,c(which(colnames(ML_in_long) == paste(analysis.framework$x.var[m],"_sig",sep="") ))] 
  
  
  
  all.vars = c("src_subject_id","eventname","site_id_l","rel_family_id",
               "x.var","x.var2","y.var","rel_relationship_1", "rel_relationship_2","rel_relationship_3",
               "sex2","pds","interview_age","split","diff",
               
               "ed","income_1","income_2","income_3","income_4","demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
               
               "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
               "Prisma_fit","Prisma","DISCOVERY","Achieva",
               colnames(ML_in_long)[grep(colnames(ML_in_long),
                                         pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
               
  )
  
  ML_long2 = ML_in_long %>% dplyr::select(all_of(all.vars))%>% unique() %>% na.omit()
  
  
  var.names=c( "x.var","x.var2","rel_relationship_1","rel_relationship_2","rel_relationship_3",
               "sex2","pds","split","diff",
               "ed","income_1","income_2","income_3","income_4","demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
               "Prisma_fit","Prisma","DISCOVERY","Achieva",
               colnames(ML_in_long)[grep(colnames(ML_in_long),
                                         pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
  )
  
  var.names2=c( "x.var","x.var2","pds","ed","diff","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
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
  
  ML_long2$motion1 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,1] %>% scale(center = T,scale = T)
  ML_long2$motion2 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,2] %>% scale(center = T,scale = T)
  ML_long2$motion3 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,3] %>% scale(center = T,scale = T)
  ML_long2$motion4 = poly(ML_long2$tfmri_nback_all_meanmotion,4)[,4] %>% scale(center = T,scale = T)
  
  
  m0 = lmer(y.var ~ 
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              motion1 + motion2 + motion3 + motion4+#tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2  )
  
    m1 = lmer(y.var ~ diff + 
                #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
                Age1+Age2+
                motion1 + motion2 + motion3 + motion4+# tfmri_nback_all_meanmotion+
                rel_relationship_1+rel_relationship_2+rel_relationship_3+
                pds+sex2 +split+
                #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
                # ed+income_1+income_2+income_3+income_4+
                # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
                Prisma_fit+Prisma+DISCOVERY+Achieva+
                # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
                (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
              ,data =ML_long2  )
    m1.ci = confint(m1,parm = "diff")
    m1.out = summary(m1)$coefficients[2,] %>% t() %>% as.data.frame()
    m1.out = cbind(m1.out,t(m1.ci[1,]))
    colnames(m1.out) = c("Estimate","SE","df","t","p","L_CI","U_CI")

  m1.out$var = "neuralsig"
###################################
  m2 = lmer(y.var ~ x.var + 
             # diff+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              motion1 + motion2 + motion3 + motion4+# tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2  )
  m2.ci = confint(m2,parm = c("x.var"))
  m2.out = summary(m2)$coefficients[c(2),] %>% t() %>% as.data.frame()
  m2.out = cbind(m2.out,t(m2.ci[1,]))
  
  colnames(m2.out) = c("Estimate","SE","df","t","p","L_CI","U_CI")
  m2.out$var = c("ML")
  
  
  ############################
  m3 = lmer(y.var ~ x.var + 
              diff+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              motion1 + motion2 + motion3 + motion4+#tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2  )
  m3.ci = confint(m3,parm = c("x.var","diff"))
  m3.out = summary(m3)$coefficients[c(2,3),] %>% as.data.frame()
  m3.out = cbind(m3.out,m3.ci)
  
  colnames(m3.out) = c("Estimate","SE","df","t","p","L_CI","U_CI")
  m3.out$var = c("ML.con","neuralsig.con")
  #
#######################
  m4 = lmer(y.var ~ x.var2 + 
              # diff+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
              Age1+Age2+
              motion1 + motion2 + motion3 + motion4+#tfmri_nback_all_meanmotion+
              rel_relationship_1+rel_relationship_2+rel_relationship_3+
              pds+sex2 +split+
              #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
              # ed+income_1+income_2+income_3+income_4+
              # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
              (1|site_id_l) + (1|rel_family_id) +(1|src_subject_id)
            ,data =ML_long2  )
  m4.ci = confint(m4,parm = c("x.var2"))
  m4.out = summary(m4)$coefficients[c(2),] %>% t() %>% as.data.frame()
  m4.out = cbind(m4.out,t(m4.ci[1,]))
  
  colnames(m4.out) = c("Estimate","SE","df","t","p","L_CI","U_CI")
  m4.out$var = c("ML2")
  
  
 
################
  
  
  
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
 #  print("start bootstrap")
 # # model_diff(model = m3)
 #  n.cores=10
 #  cl = makeCluster(n.cores)
 #  registerDoParallel(cl)
 # 
 #  boots = foreach(i = c(1:n.cores),.packages = c("lme4","dplyr"),.combine = rbind) %dopar% {
 #    
 #    out = bootMer(x =m3,FUN = model_diff,nsim = 100,type = "parametric",use.u = FALSE )
 # 
 #        return(out$t)
 #    
 #  }
 # 
 #  stopCluster(cl)
 #  registerDoSEQ()
  
  # 
  # findp = function(p,dat){
  #   interval = quantile(dat,probs = c(0 + p/2,1 - p/2))
  #   if(interval[1] < 0 & interval[2] < 0 | interval[1] > 0 & interval[2] > 0){return(p)}else{return(1)}
  # }
  # 
  # 
  # aic_p = optimise(findp,interval = c(0,1),dat =boots[,1])$minimum 
  # 
  
#################  
 a =  anova(m0,m1,m2,m3,m4)
 
 m1.out$AIC = a$AIC[2]
 m2.out$AIC = a$AIC[3]
 m3.out$AIC = a$AIC[4]
 m4.out$AIC = a$AIC[4]
 
 a2 = anova(m3,m1)
 
  out      = data.frame(#con = taskcon,
    x = analysis.framework$x.var[m],
    y = analysis.framework$y.var[m],
    N = length(unique(ML_long2$src_subject_id)),
    N.obs = length((ML_long2$src_subject_id)),
   # m3.diff = diff.test$`Pr(>Chisq)`[2],
    AIC.base = a$AIC[1]#,
    #aic_diff_p = aic_p
  )
  
  out1 = cbind(out,m1.out)
  out2 = cbind(out,m2.out)
  out3 = cbind(out,m3.out)
  out4 = cbind(out,m4.out)
  
  out = rbind(out1,out2,out3,out4)
  
  if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
  # if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
  
  
  
}# end analysis framework loop





write.table(x = reg.long,file = paste(base_dir,"ML_regressions_compare_ML_conmotion4.csv",sep=""),quote = F,row.names = F,col.names = T,sep = ",")
