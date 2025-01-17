#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]

library(tidyr)
library(data.table)
library(stringr)
library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(MASS)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

#################

fmri_standardize=function(dat){
  
  # dat = dat  %>% dplyr::select(dplyr::all_of(c("src_subject_id","eventname","site_id_l")))
  dat[,-c(1:3)] = dat[,-c(1:3)]  %>% apply(2,scale,scale=T,center=T)
  
  return(dat)
}

winsorize = function(x,q=3){
  
  mean_x = mean(x,na.rm = T)
  sd_x = sd(x,na.rm = T)
  top_q = mean_x + q*sd_x
  bottom_q = mean_x - q*sd_x
  
  x[x>top_q] = top_q
  x[x<bottom_q] = bottom_q
  
  return(x)
  
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


################

behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

analysis.framework=expand.grid(
  train = c(1,2),
  y.var = colnames(behav.dat)[c(grep(colnames(behav.dat),pattern = "^cbcl.+_r$"),
                                grep(colnames(behav.dat),pattern = "pps_y_ss_number"),
                                grep(colnames(behav.dat),pattern = "_agecorrected$"),
                                grep(colnames(behav.dat),pattern = "^tfmri_nb_all_beh_c.+b_rate")
  )])

#######################

  
  task_con = "nback_2b_v_0b"
  task_con2 = "nback_2bv0b"
  
  task_all.orig = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[args],"_combat.csv",sep=""),data.table = F)
  
  
  task_all1 = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[args],"_combat.csv",sep=""),data.table = F)
  task_all2 = fread(paste(base_dir,"combat/",task_con2,"_all_",analysis.framework$train[args],"_combat.csv",sep=""),data.table = F)
  task_all1 = task_all1  %>% pivot_wider(names_from = con,values_from = ends_with("combat"))
  task_all = merge(task_all1,task_all2)
  

  task_all = fmri_standardize(dat = task_all)

    
    
    behav.dat$y.var =  behav.dat[,c(which(colnames(behav.dat) == analysis.framework$y.var[args]))] 
    
    dat = behav.dat  %>% dplyr::select("src_subject_id","eventname","y.var") %>% na.omit()
    
    
    ####################################################
    
    
    
    all = dat  %>%
      mutate_at(.vars = "y.var",.funs = winsorize)  %>%
      mutate_at(.vars = "y.var",.funs = function(X){
        if(abs(psych::skew(X)) > 1){log(X+2)}else{X}
      })  %>%
      mutate_at(.vars = "y.var",.funs = scale,center=T,scale=T)
    
    dat = all  %>% dplyr::select("src_subject_id","eventname","y.var")
    
    task_all = merge(task_all,dat)  %>% na.omit()
    
    
    ####################################################
    train.dat = task_all
    train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)
    
    cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
    train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","eventname","split")))]
    

    Elnet_model = train(
      y.var ~ . , data = train.dat,
      trControl = cv_5_5,
      #family="gaussian",
      method = "glmnet",
      metric="RMSE",
      tuneLength = 10
    )

    
    bestone= Elnet_model$results[oneSE(Elnet_model$results,num = 5,metric = "RMSE",maximize = F),]
    
    t2=glmnet::glmnet(y = train.dat$y.var, x = train.dat %>% dplyr::select(-c("y.var")),
                      alpha = bestone$alpha,lambda = bestone$lambda)
    
    ML.model.out=list()
    ML.model.out$mod = t2
    names(ML.model.out)[1] = paste(analysis.framework$y.var[args],"_",analysis.framework$train[args],sep="")
    
    #########################################################


saveRDS(ML.model.out,file = paste(base_dir,"enet_mods/",analysis.framework$y.var[args],"_",analysis.framework$train[args],".RData",sep=""))
