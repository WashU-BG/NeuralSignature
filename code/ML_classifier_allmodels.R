
library(data.table)
library(stringr)
library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(MASS)
library(randomForest)
library(gbm)
library(kernlab)
library(tictoc)

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

i = 2

ML.model.out.final=list()


  
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
  

  #####
  library(doParallel)
  library(parallel)

  
  ####################################################
  train.dat = task_all
  train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)
  
  cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
  train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","eventname","split")))]
  train.dat$con = as.factor(train.dat$con)
  
  ML.model.out=list()
  train.acc.out = list()
  
#################
  cl <-  makeCluster(20)
  registerDoParallel(cl) 
  ############
  
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
  bestone= Elnet_model$results[oneSE(Elnet_model$results,num = 10,metric = "Accuracy",maximize = T),]
 print(bestone)
  
  t2=glmnet::glmnet(y = train.dat$con, x = train.dat[,-1],family="binomial",
                    alpha = bestone$alpha,lambda = bestone$lambda)
  ML.model.out$Elnet = t2  
  train.acc.out$Elnet = bestone
  
  #################################
  # PLS-LDA

  print(paste("PLS-LDA"))
  tic()
  PLS_model = train(
    con ~ . , data = train.dat,
    trControl = cv_5_5,
    method = "pls",
    family="binomial",
    metric="Accuracy",
    #tuneLength = 5
    tuneGrid = expand.grid(ncomp = seq(1,30,1))
  )
toc()
  bestone = PLS_model$results[oneSE(PLS_model$results,num = 10,metric = "Accuracy",maximize = T),]
  print(bestone)
  PLS_final = caret::plsda(x = train.dat[,-1], y =  train.dat[,1], method = "oscorespls",
                           ncomp = bestone$ncomp,
                           probMethod = "softmax",
                           type='prob')
  ML.model.out$PLS_LDA = PLS_final
  train.acc.out$PLS_LDA = bestone
  
  ###############################
  ## RF
  print(paste("RF"))
  
  RF2 <- list(label = "RF2",
              library = c("randomForest"),
              type = "Classification",
              parameters = data.frame(parameter = c('mtry','ntree'),
                                      class = c( 'numeric','numeric'),
                                      label = c('mtry', 'ntree')),
              grid = function(x, y, len = NULL) {
                grid <- expand.grid(ncomp = seq(1, min(ncol(x) - 1, len), by = 1),
                                    mtry = 2:len)
                
              },
              loop = NULL,
              fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                mod <- randomForest::randomForest(x, y,try = param$mtry, ntree = param$ntree)
                mod
              },
              predict = function(modelFit, newdata, submodels = NULL) {       
                predict(modelFit, newdata)
              },
              prob = NULL,
              varImp = NULL,
              predictors = function(x, ...) rownames(x$projection),
              levels = function(x) x$obsLevels,
              sort = function(x) x[order(x[,1]),])
  
tic()
  RF_mod = train(
    con ~ . , 
    data = train.dat,
    method = RF2,
    family="binomial",
    
    trControl = cv_5_5,
    tuneGrid = expand.grid(mtry = c(7,13,56),
                           ntree = c(100,250,500,1000))
  )
toc()
  
  bestone= RF_mod$results[oneSE(RF_mod$results,num = 10,metric = "Accuracy",maximize = T),]
  print(bestone)
  RF_mod_final = randomForest::randomForest(x = train.dat[,-1],y = train.dat[,1],
                                            mtry = bestone$mtry, ntree = bestone$ntree)
  
  ML.model.out$RF = RF_mod_final
  train.acc.out$RF = bestone
  
  
  
  ####################################
  # SVM
  print(paste("SVM"))
  

  tic()
  svm_mod = train(
    con ~ . , 
    data = train.dat,
    method = 'svmLinear2',
    trControl = cv_5_5,
    #tuneLength = 5
    tuneGrid = expand.grid( cost = c(.001,.25,1,5,10) )
  )
toc()
  bestone= svm_mod$results[oneSE(svm_mod$results,num = 10,metric = "Accuracy",maximize = T),]
  print(bestone)
  
  t1=e1071::svm(  con ~ . , data = train.dat,cost=bestone$cost,kernel='linear',
                  probability=T,type = 'C-classification')
  ML.model.out$SVM = t1
  train.acc.out$SVM = bestone
  
  
##########################################
  
  # gbm
  print(paste("GBM"))
 

  tic()
  gbm_mod = train(
    con ~ . , 
    data = train.dat,
    method = 'gbm',
    trControl = cv_5_5,

    tuneGrid = expand.grid( n.trees = c(50,100,250,500),
                            interaction.depth = c(1,3,6,10),
                            shrinkage = c(0.001,.01,0.1,1),
                            n.minobsinnode = c(.01,1,10))
  )
  toc()
  
  # tic()
  # gbm_mod2 = train(
  #   con ~ . , 
  #   data = train.dat,
  #   method = 'gbm',
  #   trControl = cv_5_5,
  #   
  #   tuneGrid = expand.grid( n.trees = c(100,500,1000),
  #                           interaction.depth = c(6,10),
  #                           shrinkage = c(0.1,1),
  #                           n.minobsinnode = c(.01,10))
  # )
  # toc()
  # bestone2= gbm_mod2$results[oneSE(gbm_mod2$results,num = 10,metric = "Accuracy",maximize = T),]
  # bestone2
  bestone= gbm_mod$results[oneSE(gbm_mod$results,num = 10,metric = "Accuracy",maximize = T),]
  print(bestone)
  
  train.dat.temp = train.dat
  train.dat.temp$con = as.numeric(train.dat.temp$con)
  train.dat.temp$con = train.dat.temp$con - 1
  
  t1 = gbm::gbm(con  ~ . , data = train.dat.temp,shrinkage = bestone$shrinkage,interaction.depth = bestone$interaction.depth,
                n.minobsinnode = bestone$n.minobsinnode,distribution = "bernoulli")
  
  ML.model.out$GBM = t1
  train.acc.out$GBM = bestone
  
  ########################################
  # add - LASSO-PCA
  print("LASSO-PCA")
  
  
  PCA_lasso <- list(label = "PCA-lasso",
                    library = c("glmnet"),
                    type = "Classification",
                    parameters = data.frame(parameter = c('lambda'),
                                            class = c("numeric"),
                                            label = c('lambda')),
                    grid = function(x, y, len = NULL) {
                      grid <- expand.grid(ncomp = seq(1, min(ncol(x) - 1, len), by = 1))
                      
                    },
                    loop = NULL,
                    fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                      # pre <- plsda(x, y, ncomp = param$ncomp)
                      pre <- stats::princomp(x)
                      scores <- as.matrix(x)  %*% pre$loadings
                      scores = as.data.frame(scores)
                      #colnames(scores) = sub(pattern = " ",replacement = "_",x = colnames(scores))
                      # scores$y = y
                      
                      mod=glmnet::glmnet(y = y, x = scores,family = "binomial",
                                         alpha = 1,lambda = param$lambda)
                      
                      # mod = stats::glm(y ~ .,data=scores,family = "binomial")
                      mod$projection <- pre$loadings
                      mod
                    },
                    predict = function(modelFit, newdata, submodels = NULL) {       
                      
                      scores <- as.matrix(newdata)  %*% modelFit$projection
                      #return ((predict(object = modelFit, newx = scores)>0.5)*1)
                      return ((predict(object = modelFit, newx = scores,type="class")))
                      
                      
                    },
                    prob = NULL,
                    varImp = NULL,
                    predictors = function(x, ...) rownames(x$projection),
                    levels = function(x) x$obsLevels,
                    sort = function(x) x[order(x[,1]),])
  
  # get lambda values
  pre <- princomp(train.dat[,-1],scores = TRUE)
  scores <- pre$scores
  t1  =cv.glmnet(x = scores,y = train.dat[,1],family="binomial",nfolds = 5)
  
  tic()
  
  pca_lasso_mod = train(
    con ~ . , 
    data = train.dat,
    family="binomial",
    method = PCA_lasso,
    trControl = cv_5_5,
    tuneGrid = expand.grid(lambda =  t1$lambda )) 
  
  toc()
  
  bestone= pca_lasso_mod$results[oneSE(pca_lasso_mod$results,num = 10,metric = "Accuracy",maximize = T),]
  print(bestone)
  
  Lasso_pca_final = glmnet::glmnet(y = train.dat[,1], x = scores,family = "binomial",
                                   alpha = 1,lambda = bestone$lambda)
  
  Lasso_pca_final$projection = pre$loadings
  
  #ML.model.out$PLS_SVM_final_pls = PLS_SVM_final_pls
  ML.model.out$Lasso_pca_final = Lasso_pca_final
  train.acc.out$Lasso_pca_final = bestone
  
  ####################################
  # SVM
  print(paste("SVM rbf"))
  

  tic()
  svm_rbf = train(
    con ~ . , 
    data = train.dat,
    method = 'svmRadial',
    trControl = cv_5_5,
    #tuneLength = 5
    tuneGrid = expand.grid( C = c(.001,.25,1,10,100),
                            sigma = c(.0001,.001,.01,1,10) )
    # tuneGrid = expand.grid( C = c(1,10),
    #                         sigma = c(.0001,1) )
  )
toc()
   # svm_rbf$results
  bestone= svm_rbf$results[oneSE(svm_rbf$results,num = 5,metric = "Accuracy",maximize = T),]
  bestone.orig = bestone

 performance = svm_rbf$results
  stop.crit = 1
  while (stop.crit <= 10) {
    stop.crit = stop.crit + 1
   t1 = kernlab::ksvm(con ~. ,data= train.dat, 
                     kernel= "rbfdot",
                     C = bestone$C,
                     kpar = list(sigma = bestone$sigma),
                     prob.model = TRUE,
                     type = 'C-svc')
   tryCatch( {a = predict(t1,train.dat[,-1] %>% as.matrix(),type="probabilities")}, error=function(e) {
     # message('An Error Occurred')
     # print(e)
   })
   
   if (exists("a") == TRUE){stop.crit = 11}
   if (exists("a") != TRUE){
     matching = apply(performance,1,function(X){X == bestone}) %>% apply(2,sum) 
     performance = performance[-which(matching == 6),]
     bestone= performance[oneSE(performance,num = 5,metric = "Accuracy",maximize = T),]
   }
 }
  print(bestone)
  ML.model.out$SVM_rbf = t1
  train.acc.out$SVM_rbf = bestone
  
  
  
  
  #######################
  stopCluster(cl)
  registerDoSEQ()
 
  ##########################################

saveRDS(ML.model.out, file=paste(base_dir,"ML_models.2.classification.all.RData",sep=""))
saveRDS(train.acc.out, file=paste(base_dir,"ML_models.2.classification.trainacc.RData",sep=""))
