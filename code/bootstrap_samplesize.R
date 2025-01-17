#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]

print(args)

library(data.table)
library(stringr)
library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(MASS)
library(pROC)
library(rptR)
library(lme4)
library(lmerTest)
library(diptest)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

#################

fmri_standardize=function(dat){
  
  dat = dat %>% dplyr::select(dplyr::all_of(c("src_subject_id","eventname","site_id_l","con")),ends_with("combat"))
  dat = dat[order(dat$eventname,dat$src_subject_id,dat$con),]
  
  dat.a = dat %>% dplyr::filter(con == 1)
  dat.b = dat %>% dplyr::filter(con == 0)
  
  # test removing this
  # fmri_mean = (dat.a[,-c(1:4)] + dat.b[,-c(1:4)])/2
  # 
  # dat.a[,-c(1:4)] = dat.a[, -c(1:4)] - fmri_mean
  # dat.b[,-c(1:4)] = dat.b[, -c(1:4)] - fmri_mean
  
  dat.new = rbind(dat.a,dat.b)
  dat.new = dat.new[order(dat.new$eventname,dat.new$src_subject_id,dat.new$con),]
  
  dat.new[,-c(1:4)] = dat.new[,-c(1:4)] %>% apply(2,scale,scale=T,center=T)
  
  return(dat.new)
}


grouped_cv_folds = function(reps,inner_fold,siteID){
  for (i in 1:reps){
    t_folds <- groupKFold(siteID, k = inner_fold) 
    
    names(t_folds) = paste(names(t_folds),".Rep",sep = "",
                           str_pad(string = as.character(i),
                                   width = str_length(as.character(reps)),
                                   side = "left",
                                   pad = "0")
    )
    
    if(i ==1){
      folds=t_folds
    }else{folds = c(folds,t_folds)}
    
  }
  return(folds)
}


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

##################

analysis.framework = data.frame(train = c(1,2),test = c(2,1))

#analysis.framework2 = expand.grid(reps = c(1:5000),nsize = c(20,50cd ..,100,250))
#analysis.framework2 = expand.grid(reps = c(1:5000),nsize = c(500,1000,2000,4000))

#analysis.framework2 = expand.grid(reps = c(1:1),nsize = c(20,50,100,250,500,1000,2000,4000))

analysis.framework2 = expand.grid(reps = c(1:2000),nsize = c(2^(1:9)*10))
#analysis.framework2 = expand.grid(reps = c(1:1000),nsize = c(80,160))


analysis.framework2$nsize = analysis.framework2$nsize/10 %>% round()

ML.model.out.final=list()

i = 1
  
  #split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
  task_con = "nback_2b_v_0b"
  task = "nback"
  
  task_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F) 
  
  test_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F) 
  test_run1 = fread(paste(base_dir,"combat/",task_con,"_run1_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
  test_run2 = fread(paste(base_dir,"combat/",task_con,"_run2_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
  
  
  test_all = test_all %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
  test_run1 = test_run1 %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
  test_run2 = test_run2 %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
  
  
  test_all = fmri_standardize(dat = test_all)
  test_run1 = fmri_standardize(dat = test_run1)
  test_run2 = fmri_standardize(dat = test_run2)
  
  #############
  
  behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)
  
  dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","site_id_l")) %>% unique()
  task_all = merge (task_all,dat)
  
ids = task_all %>% 
  dplyr::filter(eventname == "baseline_year_1_arm_1") %>% 
  dplyr::select(c("src_subject_id","site_id_l","rel_family_id","eventname")) %>% 
  unique()

ids = no_relatives(ids)
  
sites = ids$site_id_l %>% unique()

set.seed(args)

for(site in 1:10){
  temp = ids %>% dplyr::filter(site_id_l == sites[site])
  
  keep = temp[sample(c(1:dim(temp)[1]),size = analysis.framework2$nsize[args],replace = TRUE),]

  if(site == 1){keep.final = keep}else{keep.final = rbind(keep.final,keep)}  
  
}

train.dat = merge(keep.final,task_all)

  ####################################################
train.dat = fmri_standardize(dat = train.dat)

set.seed(args)

#  train.dat = task_all
  train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)
  
  cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
  train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","rel_family_id","eventname","split")))]
  train.dat$con = as.factor(train.dat$con)
  
  ML.model.out=list()
  train.acc.out = list()
  
  set.seed(args)
  
  print(paste(i,"ENET"))
  Elnet_model_function = function(){train(
    con ~ . , data = train.dat,
    trControl = cv_5_5,
    family="binomial",
    method = "glmnet",
    metric="Accuracy",
    tuneLength = 20
  )}
 
  Elnet_model = tryCatch(expr = Elnet_model_function(),warning = Elnet_model_function())
  
  print("Finished CV, running final model")
  
  bestone= Elnet_model$results[oneSE(Elnet_model$results,num = 5,metric = "Accuracy",maximize = T),]
  
  
  t2=glmnet::glmnet(y = train.dat$con, x = train.dat[,-1],family="binomial",
                    alpha = bestone$alpha,lambda = bestone$lambda)
  
  
  
 # if(sum(glmnet::coef.glmnet(t2) ) == 0  | sd(glmnet::coef.glmnet(t2) ) == 0 ){
 #   print("sum of model coefs is 0")
 #   j = 1
 #   while(sum(glmnet::coef.glmnet(t2) ) == 0 | sd(glmnet::coef.glmnet(t2) ) == 0){
 #     print(j)
 #    t2=glmnet::glmnet(y = train.dat$con, x = train.dat[,-1],family="binomial",
 #                     alpha = Elnet_model$results$alpha[1],lambda = Elnet_model$results$lambda[j])
 #    j=j+1
 #   }
 #   
 # }
 #  
    
  ML.model.out$Elnet = t2  
  train.acc.out$Elnet = bestone
  
  #########################################################
  
  
  test_all$run = 3
  test_run1$run = 1
  test_run2$run = 2
  
  test_all = rbind(test_all,test_run1,test_run2)
  
  
  test.out.all = data.frame(src_subject_id = test_all$src_subject_id,
                            eventname = test_all$eventname,
                            site_id_l = test_all$site_id_l,
                            con = test_all$con,
                            run =  test_all$run,
                            split = analysis.framework$test[i],
                            ENET = NA
  )
  
  ###
  
  test.all.brain =  test_all[, grep(x = colnames(test_all),pattern = "combat")]
  
  ###
  
  
  a = predict(ML.model.out$Elnet,test.all.brain %>% as.matrix(),type="response")
  test.out.all$ENET = a[,1]
  
  #################################
 
 test_auc =  test.out.all %>% 
    dplyr::filter(run == 3) %>% dplyr::select(c("con","ENET"))
 auc_out = pROC::roc(test_auc$con,test_auc$ENET)
 auc_out = auc_out$auc %>% as.numeric()
  
  ##########

 back_0 = test.out.all %>% dplyr::filter(con == 0,run !=3) %>% dplyr::select(-c("con"))
 back_2 = test.out.all %>% dplyr::filter(con == 1,run !=3) %>% dplyr::select(-c("con"))
 
 colnames(back_0)[6] = c("back_0")
 colnames(back_2)[6] = c("back_2")
 

 
 predictions2 = merge(back_0,back_2)
 predictions2$diff = predictions2$back_2 - predictions2$back_0
 
 dip_0 = diptest::dip(x = back_0$back_0)
 dip_2 = diptest::dip(x = back_2$back_2)
 dip_diff = diptest::dip(x = predictions2$diff)
 
 
 keepn.obs = function(dat,keep){
   v.keep = table(dat$src_subject_id) %>% as.data.frame() %>% dplyr::filter(Freq == keep) %>% dplyr::select("Var1") 
   v.keep = v.keep$Var1 %>% as.character()
   dat.new = dat[!is.na(match(x = dat$src_subject_id,table = v.keep)),]
   return(dat.new)
 }
 
 predictions2 =  keepn.obs(predictions2,keep = 2)
 
 dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","sex2","interview_age",
                                     "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                     "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                     "tfmri_nback_all_meanmotion")) %>% unique()
 
 
 predictions2 = merge(predictions2,dat,all.x = T) %>% na.omit()
 predictions2 = no_relatives(predictions2)

 
 rel1 = rpt(diff ~  
              Prisma_fit+Prisma+DISCOVERY+Achieva+
              tfmri_nback_all_meanmotion+
              (1|src_subject_id)  +
              (1|site_id_l) ,#+
            data = predictions2,adjusted = T,
            grname ="src_subject_id",datatype = "Gaussian",npermut = 0,nboot = 0)

rel_out = rel1$R$src_subject_id

#################################################################


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


classification.pred = test.out.all
twoback = classification.pred %>% dplyr::filter(run == 3, con == 1) %>% dplyr::select(-c("con"))
zeroback = classification.pred %>% dplyr::filter(run == 3, con == 0)%>% dplyr::select(-c("con"))

colnames(twoback)[6] = "twoback"
colnames(zeroback)[6] = "zeroback"

nsen = merge(zeroback,twoback)
nsen$diff = nsen$twoback - nsen$zeroback


behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)



#behav.dat$motion = behav.dat[,grep(x = colnames(behav.dat),pattern = paste(task,".+motion",sep=""))]
# ML_in_long = ML_in %>% dplyr::filter(con == 1, run == 3)
ML_in_long = merge(nsen,behav.dat,all.x = T)



analysis.framework3=expand.grid(#x.var =  c("twoback","zeroback","diff"),
  x.var =  c("diff"),
  y.var = c("nihtbx_totalcomp_agecorrected","nihtbx_list_agecorrected","cbcl_scr_syn_attention_r","pps_y_ss_number"
  ))

analysis.framework3$y.var = as.character(analysis.framework3$y.var)
analysis.framework3$x.var = as.character(analysis.framework3$x.var)

# ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == model.setup$models[args]))] 
# ML_in_long$cov.var =  ML_in_long[,c(which(colnames(ML_in_long) == model.setup$cov.var[args]))] 
# 
# analysis.framework3 = analysis.framework3[-which(analysis.framework3$y.var == model.setup$cov.var[args]),] %>% as.data.frame()
# colnames(analysis.framework3)[1]="y.var"

if( length(unique(nsen$diff)) > 1 ){
for(m in 1:dim(analysis.framework3)[1]){
  print(paste(m))
  
  ML_in_long$y.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework3$y.var[m]))] 
  ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework3$x.var[m]))] 
  
  
  
  all.vars = c("src_subject_id","eventname","site_id_l","rel_family_id",
               "x.var","y.var","rel_relationship_1", "rel_relationship_2","rel_relationship_3",
               "sex2","pds","interview_age",
               "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
               "Prisma_fit","Prisma","DISCOVERY","Achieva",
               colnames(ML_in_long)[grep(colnames(ML_in_long),
                                         pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
               
  )
  
  ML_long2 = ML_in_long %>% dplyr::select(all_of(all.vars))%>% unique() %>% na.omit()
  
  
  var.names=c( "x.var","rel_relationship_1","rel_relationship_2","rel_relationship_3",
               "sex2","pds",
               "Prisma_fit","Prisma","DISCOVERY","Achieva",
               colnames(ML_in_long)[grep(colnames(ML_in_long),
                                         pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
  )
  
  var.names2=c("pds","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
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
  
  if(length(table(ML_long2$y.var)) > 2){
    m1 = lmer(y.var ~ x.var + 
                Age1+Age2+tfmri_nback_all_meanmotion+
                rel_relationship_1+rel_relationship_2+rel_relationship_3+
                pds+sex2 +
                #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
                #ed+income_1+income_2+income_3+income_4+
                #demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
                Prisma_fit+Prisma+DISCOVERY+
                # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
                (1|site_id_l) + (1|rel_family_id) 
              ,data =ML_long2  )
    m1.out = summary(m1)$coefficients[2,] %>% t() %>% as.data.frame()
    

  }
  # if(length(table(ML_long2$y.var))  ==  2){
  #   m1= glm(y.var ~ x.var + 
  #             Age1+Age2+
  #             rel_relationship_1+rel_relationship_2+rel_relationship_3+
  #             pds+sex2 +split+
  #             #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
  #             #ed+income_1+income_2+income_3+income_4+
  #             #demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
  #             Prisma_fit+Prisma+DISCOVERY+Achieva#+
  #           # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans
  #           ,data =ML_long2,family = "binomial"  )
  #   m1b = coeftest(m1,vcov = vcovCL,cluster = ~ site_id_l + rel_family_id + src_subject_id)
  # 
  #   m1.out = m1b[2,] %>% t() %>% as.data.frame()
  # 
  # }
  
  
  if(any(colnames(m1.out) == "df")){
    m1.out = m1.out[,-which(colnames(m1.out) == "df")]
  }
  
  colnames(m1.out) = c("Estimate","SE","t_z","p")
  #colnames(m1.out) = c("Estimate","SE","t_z","p")
  
  out      = data.frame(#con = taskcon,
    x = analysis.framework3$x.var[m],
    y = analysis.framework3$y.var[m],
    N = length(unique(ML_long2$src_subject_id)),
    N.obs = length((ML_long2$src_subject_id))
  )
  
  out = cbind(out,m1.out)
  
  if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
  # if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
  
  
  
}# end analysis framework loop

}

out.all1 = data.frame(args = args, 
                     rep = analysis.framework2$reps[args], 
                     n = analysis.framework2$nsize[args]*10,
                     auc = auc_out,
                     rel = rel_out,
                     dip_0 = dip_0,
                     dip_2 = dip_2,
                     dip_diff = dip_diff)


if( length(unique(nsen$diff)) > 1 ){
  
  out.all2 = data.frame(cog = reg.long$Estimate[1],
                       listsorting = reg.long$Estimate[2],
                       attention = reg.long$Estimate[3],
                       ple = reg.long$Estimate[4])
  
}else{
  out.all2 = data.frame(cog = NA,
                        listsorting = NA,
                        attention = NA,
                        ple = NA)
}
  
out.all = cbind(out.all1,out.all2)

write.table(x = out.all,file = paste(base_dir,"boot_out/boots.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")



if(args[1] == 1){
  write.table(x = t(colnames(out.all)),file = paste(base_dir,"boot_out/boots.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
}



