#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]

# ML classification permutation

library(data.table)
library(stringr)
library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(MASS)
library(permute)

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


##################

analysis.framework = data.frame(train = c(1,2),test = c(2,1))

ML.model.out.final=list()

i =1
  
  #split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
  task_con = "nback_2b_v_0b"
  task = "nback"
  
  task_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F) 
  
  task_all = fmri_standardize(dat = task_all)
  
  
  set.seed(args+9347)
  
  task_all$ID2 = paste(task_all$src_subject_id,task_all$eventname,sep = "_")
  task_all$con = task_all$con[permute::shuffle(n = dim(task_all)[1],control = how(blocks = task_all$ID2))]
  
  
  ####################################################
  train.dat = task_all
  train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)
  
  cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
  train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","eventname","split","ID2")))]
  train.dat$con = as.factor(train.dat$con)
  
  ML.model.out=list()
  train.acc.out = list()
  
  tic()
  print(paste(i,"ENET"))
  Elnet_model = train(
    con ~ . , data = train.dat,
    trControl = cv_5_5,
    family="binomial",
    method = "glmnet",
    metric="Accuracy",
    tuneLength = 20
  )
  toc()
  bestone= Elnet_model$results[oneSE(Elnet_model$results,num = 5,metric = "Accuracy",maximize = T),]
  
  
  t2=glmnet::glmnet(y = train.dat$con, x = train.dat[,-1],family="binomial",
                    alpha = bestone$alpha,lambda = bestone$lambda)
  ML.model.out$Elnet = t2  
  train.acc.out$Elnet = bestone
  
  weight.out = data.frame(regions = rownames(t2$beta),
                          enet_weights = t2$beta[,1] ,
                          split = analysis.framework$train[i]
  )
  
  pred.out.train =  predict(ML.model.out$Elnet, train.dat[,-1] %>% as.matrix(),type="response")
  
  pred.out.train = pred.out.train[,1]
  
  weight.out$haufe_weight = apply( train.dat[,-1],2,function(X){cov(X, (pred.out.train > .5)*1 )})
  #############################################
  
  out_weights = weight.out[,c(1,2)] %>% t() %>% as.data.frame()
  out_haufe = weight.out[,c(1,4)] %>% t() %>% as.data.frame()
  
  ###############################################
  
  
  
  write.table(x = out_weights[2,],file = paste(base_dir,"perm_out/weights.perm.",args[1],".csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  
  write.table(x = out_haufe[2,],file = paste(base_dir,"perm_out/haufe.perm.",args[1],".csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")

  
  if(args[1] == 1){
    write.table(x = t(colnames(out_weights[2,])),file = paste(base_dir,"perm_out/weights.perm.0.csv",sep=""),
                quote = F,row.names = F,col.names = F,sep = ",")
    write.table(x = t(colnames(out_haufe[2,])),file = paste(base_dir,"perm_out/haufe.perm.0.csv",sep=""),
                quote = F,row.names = F,col.names = F,sep = ",")

  }
  
  
  
  
  