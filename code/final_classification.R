
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

for(i in c(1:2)){

#split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
task_con = "nback_2b_v_0b"
task = "nback"

task_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F) 

test_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F) 
test_run1 = fread(paste(base_dir,"combat/",task_con,"_run1_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
test_run2 = fread(paste(base_dir,"combat/",task_con,"_run2_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)


task_all = fmri_standardize(dat = task_all)

test_all = fmri_standardize(dat = test_all)
test_run1 = fmri_standardize(dat = test_run1)
test_run2 = fmri_standardize(dat = test_run2)


####################################################
train.dat = task_all
train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)

cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","eventname","split")))]
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


train.out.all = data.frame(src_subject_id = task_all$src_subject_id,
                          eventname = task_all$eventname,
                          site_id_l = task_all$site_id_l,
                          con = task_all$con,
                          run =  3,
                          split = analysis.framework$train[i],
                          ENET = pred.out.train
)

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

ML.model.out.final[[i]] = ML.model.out$Elnet



if(i == 1){
  test.out.all.final = test.out.all
  train.out.all.final = train.out.all
  weight.out.final = weight.out
  param.final = bestone
  
}else{
  
  test.out.all.final = rbind(test.out.all.final,test.out.all)
  train.out.all.final = rbind(train.out.all.final,train.out.all)
  weight.out.final = rbind(weight.out.final,weight.out)
  param.final = rbind(param.final,bestone)
  
    }

}

write.csv(x = test.out.all.final,file = paste(base_dir,"ML_classification_out_test_updated.csv",sep=""),quote = F,row.names = F)
write.csv(x = train.out.all.final,file = paste(base_dir,"ML_classification_out_train_updated.csv",sep=""),quote = F,row.names = F)
write.csv(x = weight.out.final,file = paste(base_dir,"ML_classification_out_weights_updated.csv",sep=""),quote = F,row.names = F)
write.csv(x = param.final,file = paste(base_dir,"ML_classification_out_params_updated.csv",sep=""),quote = F,row.names = F)

saveRDS(ML.model.out.final, file=paste(base_dir,"ML_models.classification.final_updated.RData",sep=""))
