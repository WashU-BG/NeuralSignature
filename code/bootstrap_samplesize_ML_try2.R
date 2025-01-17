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
library(rptR)
# library(foreach)
# library(doParallel)
library(lmerTest)


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")

#################

fmri_standardize=function(dat){
  
  # dat = dat  %>% dplyr::select(dplyr::all_of(c("src_subject_id","eventname","site_id_l","rel_family_id")))
  
  change = which(colnames(dat) %in% c("src_subject_id","eventname","site_id_l","rel_family_id"))
  
  dat[,-change] = dat[,-change]  %>% apply(2,scale,scale=T,center=T)
  
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


################

behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

analysis.framework2=data.frame(
  y.var = colnames(behav.dat)[c(grep(colnames(behav.dat),pattern = "^cbcl.+_r$"),
                                grep(colnames(behav.dat),pattern = "pps_y_ss_number"),
                                grep(colnames(behav.dat),pattern = "_agecorrected$"),
                                grep(colnames(behav.dat),pattern = "^tfmri_nb_all_beh_c.+b_rate")
  )])


analysis.framework3 = expand.grid(reps = c(1:2000),nsize = c(2^(1:9)))
#analysis.framework3$nsize = analysis.framework3$nsize/10 %>% round()

#######################


# all.out.final = foreach(args=1:dim(analysis.framework2)[1],
#                         .combine = merge,
#                         .packages=c('glmnet','caret','tidyr','dplyr','data.table','stringr')) %do% {

analysis.framework = data.frame(train = c(1,2),test = c(2,1))
i=1

  print(i)
  
  #split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
  task_con = "nback_2b_v_0b"
  task_con2 = "nback_2bv0b"
  
  task_all.orig = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F)
  
  
  task_all1 = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F)
  task_all2 = fread(paste(base_dir,"combat/",task_con2,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F)
  task_all1 = task_all1  %>% pivot_wider(names_from = con,values_from = ends_with("combat"))
  task_all = merge(task_all1,task_all2)
  
  # task_all = fread(paste(base_dir,"combat/",task_con2,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F) 
  test_all.orig = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
  
  test_all1 = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
  test_all2 = fread(paste(base_dir,"combat/",task_con2,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
  test_all1 = test_all1  %>% pivot_wider(names_from = con,values_from = ends_with("combat"))
  test_all = merge(test_all1,test_all2)
  # test_all = fread(paste(base_dir,"combat/",task_con2,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F) 
  
  task_all = task_all %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
  test_all = test_all %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
  

  test_all = fmri_standardize(dat = test_all)
  

    test_all1 = fread(paste(base_dir,"combat/",task_con,"_run1_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
    test_all2 = fread(paste(base_dir,"combat/",task_con2,"_run1_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
    test_all1 = test_all1  %>% pivot_wider(names_from = con,values_from = ends_with("combat"))
    test1 = merge(test_all1,test_all2)
    test1 = test1 %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
    test1 = fmri_standardize(dat = test1)

    test_all1 = fread(paste(base_dir,"combat/",task_con,"_run2_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
    test_all2 = fread(paste(base_dir,"combat/",task_con2,"_run2_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F)
    test_all1 = test_all1  %>% pivot_wider(names_from = con,values_from = ends_with("combat"))
    test2 = merge(test_all1,test_all2)
    test2 = test2 %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
    test2 = fmri_standardize(dat = test2)
  
  
  ##############################################
  # 
  # sites = unique(task_all.orig$site_id_l)
  # tic()
  # for(site in 1:length(sites)){
  #   print(paste("site",site))
  #   train.dat = task_all.orig %>% dplyr::filter(site_id_l != sites[site])
  #   test.dat = task_all.orig %>% dplyr::filter(site_id_l == sites[site])
  #   
  #   train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)
  #   
  #   cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
  #   train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","eventname","split")))]
  #   train.dat$con = as.factor(train.dat$con)
  #   
  #   
  #   
  #   Elnet_model = train(
  #     con ~ . , data = train.dat,
  #     trControl = cv_5_5,
  #     family="binomial",
  #     method = "glmnet",
  #     metric="Accuracy",
  #     tuneLength = 20
  #   )
  #   
  #   bestone= Elnet_model$results[oneSE(Elnet_model$results,num = 5,metric = "Accuracy",maximize = T),]
  #   
  #   
  #   t2=glmnet::glmnet(y = train.dat$con, x = train.dat[,-1],family="binomial",
  #                     alpha = bestone$alpha,lambda = bestone$lambda)
  #   
  #   pred.out.train =  predict(t2, test.dat %>% dplyr::select(ends_with("combat")) %>% as.matrix(),type="response")
  #   
  #   
  #   out.temp = data.frame(src_subject_id = test.dat$src_subject_id,
  #                         eventname = test.dat$eventname,
  #                         site_id_l = test.dat$site_id_l,
  #                         con = test.dat$con,
  #                         run =  3,
  #                         ENET = pred.out.train[,1]
  #   )
  #   
  #   if(site == 1){train.sig =out.temp }else{train.sig = rbind(train.sig,out.temp)}
  #   
  # }
  # 
  # b = toc()
  # print(unname((b$toc - b$tic)/60))
  # 
  # train.sig = train.sig %>% pivot_wider(names_from = con,values_from = ENET,names_prefix = "ENET")
  # train.sig$diff = train.sig$ENET1 - train.sig$ENET0
  # train.sig$diff  = train.sig$diff %>% scale(center = T,scale = T) %>% c()
  # task_all1 = merge(task_all,train.sig %>% dplyr::select(c("src_subject_id","eventname","diff")))
  # 
  # 
  # 
  # test.out.all.final = fread(paste(base_dir,"ML_classification_out_test_updated.csv",sep=""),data.table = F)
  # test.out = test.out.all.final %>% dplyr::filter(split == analysis.framework$test[i], run == 3)
  # test.out = test.out %>% pivot_wider(names_from = con,values_from = ENET,names_prefix = "ENET")
  # test.out$diff = test.out$ENET1 - test.out$ENET0
  # test.out$diff  = test.out$diff %>% scale(center = T,scale = T) %>% c()
  # 
  # test_all = merge(test_all,test.out %>% dplyr::select(c("src_subject_id","eventname","diff")))
  # 
  #all1 = rbind(task_all1,test_all)
  
  args2=33
    #print(args)
    
    behav.dat$y.var =  behav.dat[,c(which(colnames(behav.dat) == analysis.framework2$y.var[args2]))] 
    
    dat = behav.dat  %>% dplyr::select("src_subject_id","eventname","rel_family_id","y.var") %>% na.omit()
    
    
    ####################################################
    
    
    
    all = dat
    
    all = all  %>%
      mutate_at(.vars = "y.var",.funs = winsorize)  %>%
      mutate_at(.vars = "y.var",.funs = function(X){
        if(abs(psych::skew(X)) > 1){log(X+2)}else{X}
      })  
    
    dat = all  %>% dplyr::select("src_subject_id","eventname","rel_family_id","y.var")
    
    task_all = merge(task_all,dat)  %>% na.omit()
    
    #task_all = no_relatives(task_all)
    
    ids = task_all %>% 
      dplyr::filter(eventname == "baseline_year_1_arm_1") %>% 
      dplyr::select(c("src_subject_id","site_id_l","rel_family_id","eventname")) %>% 
      unique()
    
    ids = no_relatives(ids)
    
    sites = ids$site_id_l %>% unique()
    
    # args=8
    
    set.seed(args)
    
    for(site in 1:10){
      temp = ids %>% dplyr::filter(site_id_l == sites[site])
      
      keep = temp[sample(c(1:dim(temp)[1]),size = analysis.framework3$nsize[args],replace = TRUE),]
      
      if(site == 1){keep.final = keep}else{keep.final = rbind(keep.final,keep)}  
      
    }
    ################
   ####### keep.final = ids#######
    #################
    train.dat = merge(keep.final,task_all)
    
    #  train.dat = task_all
    
    train.dat = fmri_standardize(dat = train.dat)
    
    ####################################################
    set.seed(args)
    train.dat_folds = grouped_cv_folds(reps = 10,inner_fold = 5,siteID = train.dat$site_id_l)
    
    cv_5_5 = trainControl(method = "repeatedcv",index = train.dat_folds)
    train.dat = train.dat[,is.na(match(colnames(train.dat),c("src_subject_id","site_id_l","rel_family_id","eventname","split")))]
    
    ML.model.out=list()
    train.acc.out = list()
  
    
    #############
    # library(foreach)
    # library(doParallel)
    # cl <- makeCluster(15)
    # registerDoParallel(cl)
    #######################
    
    print(paste("ENET 1 "))
    Elnet_model = train(
      y.var ~ . , data = train.dat,
      trControl = cv_5_5,
      #family="gaussian",
      method = "glmnet",
      metric="RMSE",
      tuneLength = 10
    )
    #################
    # stopCluster(cl)
    # registerDoSEQ()
    ##############
    
  #  bestone= Elnet_model$results[oneSE(Elnet_model$results,num = 5,metric = "RMSE",maximize = F),]
    
    # t2=glmnet::glmnet(y = train.dat$y.var, x = train.dat %>% dplyr::select(-c("y.var")),
    #                   alpha = bestone$alpha,lambda = bestone$lambda)
    
    t2=glmnet::glmnet(y = train.dat$y.var, x = train.dat %>% dplyr::select(-c("y.var")),
                      alpha = Elnet_model$bestTune$alpha,lambda = Elnet_model$bestTune$lambda)
    
    print(sum(abs(t2$beta)))
    
    mod_results = Elnet_model$results
    bestone = mod_results[which(mod_results$RMSE == min(mod_results$RMSE)),]
    print(bestone)
    
    while(sum(abs(t2$beta)) == 0){

      mod_results = mod_results[-c(which(rownames(mod_results) == rownames(bestone))),]

     bestone = mod_results[which(mod_results$RMSE == min(mod_results$RMSE)),]
     print(bestone)
     
      t2=glmnet::glmnet(y = train.dat$y.var, x = train.dat %>% dplyr::select(-c("y.var")),
                        alpha = bestone$alpha,lambda = bestone$lambda)

    }
    
    
    # if(sum(t2$beta) == 0){
    #   write.table(x = c("ERROR"),file = paste(base_dir,"boot_out/boots.error.cog.",args[1],".csv",sep=""),
    #               quote = F,row.names = F,col.names = F,sep = ",")
    #   stop()
    # }
    
    
    # tic()
    # print(paste("ENET 2 "))
    # 
    # Elnet_model2 = train(
    #   y.var ~ . , data = train.dat %>% dplyr::select(-c("diff")),
    #   trControl = cv_5_5,
    #   #family="gaussian",
    #   method = "glmnet",
    #   metric="RMSE",
    #   tuneLength = 10
    # )
    # b = toc()
    # print(unname((b$toc - b$tic)/60))
    # 
    # bestone2= Elnet_model2$results[oneSE(Elnet_model2$results,num = 5,metric = "RMSE",maximize = F),]
    # t3=glmnet::glmnet(y = train.dat$y.var, x = train.dat %>% dplyr::select(-c("y.var","diff")),
    #                   alpha = bestone2$alpha,lambda = bestone2$lambda)
    # 
    # 
    #########################################################
    
    print("Regressions")
    test.dat.out.all = data.frame(src_subject_id = test_all$src_subject_id,
                                  eventname = test_all$eventname,
                                  site_id_l = test_all$site_id_l,
                                #  split = analysis.framework$test[i],
                                  ENET = NA
    )
    
    ###
    
    test.all.brain =  test_all[, grep(x = colnames(test_all),pattern = "combat")]
    a = predict(t2,test.all.brain  %>% as.matrix())
    test.dat.out.all$ENET = a[,1]

    
    ###############################################
    
    
    
    behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)
    
    ML_indiff_regression = test.dat.out.all
    
    nback_perf = c(
      "tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c2b_rate"
    )
    
    
    # y.vars = colnames(behav.dat)[c(grep(colnames(behav.dat),pattern = "^cbcl.+_r$"),
    #                                grep(colnames(behav.dat),pattern = "pps_y_ss_number"),
    #                                #grep(colnames(behav.dat),pattern = "^bis"),
    #                                #grep(colnames(behav.dat),pattern = "^upps"),
    #                                #grep(colnames(behav.dat),pattern = "any"),
    #                                
    #                                #grep(colnames(behav.dat),pattern = "pea_ravlt_sd_trial_i_tc"),
    #                                #grep(colnames(behav.dat),pattern = "pea_ravlt_ld_trial_vii_tc"),
    #                                # grep(colnames(behav.dat),pattern = "pea_wiscv_tss"),
    #                                #grep(colnames(behav.dat),pattern = "lmt_scr_perc_correct"),
    #                                grep(colnames(behav.dat),pattern = "_agecorrected$")
    #                                ,
    #                                grep(colnames(behav.dat),pattern = paste(nback_perf,sep = "",collapse = "|"))
    # )]
    
    
    y.vars = c("tfmri_nb_all_beh_c2b_rate","nihtbx_totalcomp_agecorrected",
               "nihtbx_list_agecorrected",
               "cbcl_scr_syn_attention_r","pps_y_ss_number")
    analysis.framework=data.frame(y.var = y.vars)
    
    ML_in_long = merge(behav.dat,ML_indiff_regression,all=TRUE)
    
    
    #behav.dat$motion = behav.dat[,grep(x = colnames(behav.dat),pattern = paste(task,".+motion",sep=""))]
    # ML_in_long = ML_in %>% dplyr::filter(con == 1, run == 3)
   # ML_in_long = merge(nsen,behav.dat,all.x = T)
    
    
    analysis.framework$y.var = as.character(analysis.framework$y.var)
    #analysis.framework$x.var = as.character(analysis.framework$x.var)
    
    # ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == model.setup$models[args]))] 
    # ML_in_long$cov.var =  ML_in_long[,c(which(colnames(ML_in_long) == model.setup$cov.var[args]))] 
    # 
    # analysis.framework = analysis.framework[-which(analysis.framework$y.var == model.setup$cov.var[args]),] %>% as.data.frame()
    # colnames(analysis.framework)[1]="y.var"
    
    # library(doSNOW)
    # library(foreach)
    # 
    # cl <- makeCluster(15)
    # registerDoSNOW(cl)
    # 
    # iterations = dim(analysis.framework)[1]
    # 
    # pb <- txtProgressBar(max = iterations, style = 3)
    # progress <- function(n) setTxtProgressBar(pb, n)
    # opts <- list(progress = progress)
    # # dim(analysis.framework)[1]
    # reg.long = foreach(m = 1:iterations,.combine = 'rbind',.packages = c('lme4','lmerTest','psych','dplyr'),.options.snow = opts) %dopar%
    #   
      
      for(m in 1:dim(analysis.framework)[1]){
        print(paste(m))
        
        ML_in_long$y.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework$y.var[m]))] 
       # ML_in_long$x.var =  ML_in_long[,c(which(colnames(ML_in_long) == analysis.framework$x.var[m]))] 
        
        
        
        all.vars = c("src_subject_id","eventname","site_id_l","rel_family_id",
                     "ENET","y.var","rel_relationship_1", "rel_relationship_2","rel_relationship_3",
                     "sex2","pds","interview_age",#"split",
                     
                     #"ed","income_1","income_2","income_3","income_4","demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
                     
                     "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
                     "Prisma_fit","Prisma","DISCOVERY","Achieva",
                     colnames(ML_in_long)[grep(colnames(ML_in_long),
                                               pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$")]
                     
        )
        
        ML_long2 = ML_in_long %>% dplyr::select(all_of(all.vars))%>% unique() %>% na.omit()
        
        cor(ML_long2$ENET,ML_long2$y.var)
        
        
        var.names=c( "ENET","rel_relationship_1","rel_relationship_2","rel_relationship_3",
                     "sex2","pds",
                     #"ed","income_1","income_2","income_3","income_4","demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
                     "Prisma_fit","Prisma","DISCOVERY","Achieva", "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate",
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
        
  
        ####################################
        
        m1 = lmer(y.var ~ ENET + 
                    # diff+
                    #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+
                    Age1+Age2+
                    #motion1 + motion2 + motion3 + motion4+ 
                    tfmri_nback_all_meanmotion+
                    rel_relationship_1+rel_relationship_2+rel_relationship_3+
                    pds+sex2 +
                    #tfmri_nb_all_beh_c2b_rate+tfmri_nb_all_beh_c0b_rate+tfmri_nb_all_beh_c2b_mrt+tfmri_nb_all_beh_c0b_mrt+
                    # ed+income_1+income_2+income_3+income_4+
                    # demo_race_w+demo_race_b+demo_race_na+demo_race_pi+demo_race_h+
                    Prisma_fit+Prisma+
                    # tfmri_nback_all_meanmotion+tfmri_nback_all_meanrot+tfmri_nback_all_meantrans+tfmri_nback_all_maxmotion+tfmri_nback_all_maxrot+tfmri_nback_all_maxtrans+
                    (1|site_id_l) + (1|rel_family_id) #+(1|src_subject_id)
                  ,data =ML_long2  )
        
       
        
        m1.out = summary(m1)$coefficients[2,] %>% t() %>% as.data.frame()
        
        
        ####################################
        
        out      = data.frame(#con = taskcon,
          y = analysis.framework$y.var[m],
          N = length(unique(ML_long2$src_subject_id)),
          N.obs = length((ML_long2$src_subject_id))
        )
        
        out1 = cbind(out,m1.out)


        if(m == 1 ){reg.long = out1}else{reg.long = rbind(reg.long,out1)}
        #if(m == 1 ){reg.long = out}else{reg.long = rbind(reg.long,out)}
        
      }
        
    reg.long
    
    ##########################
    print("Reliability")
    keepn.obs = function(dat,keep){
      v.keep = table(dat$src_subject_id) %>% as.data.frame() %>% dplyr::filter(Freq == keep) %>% dplyr::select("Var1") 
      v.keep = v.keep$Var1 %>% as.character()
      dat.new = dat[!is.na(match(x = dat$src_subject_id,table = v.keep)),]
      return(dat.new)
    }
    
    test1$run = 1
    test2$run = 2
    
    test.out.all = rbind(test1,test2)
    
    predictions2 =  keepn.obs(test.out.all,keep = 2)
    
    a = predict(object = t2,predictions2 %>% dplyr::select(dplyr::contains("combat")) %>% as.matrix())
    predictions2$ENET = a[,1]
      
    dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","sex2","interview_age",
                                        "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                        "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                        "tfmri_nback_all_meanmotion")) %>% unique()
    
    
    predictions2 = merge(predictions2,dat,all.x = T) %>% na.omit() %>% dplyr::select(dplyr::all_of(c("src_subject_id","site_id_l","eventname","rel_family_id","sex2","interview_age",
                                                                                                     "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                                                                                     "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                                                                                     "tfmri_nback_all_meanmotion","ENET")))
    predictions2 = no_relatives(predictions2)
    
    
    
    rel1 = rpt(ENET  ~  
                 Prisma_fit+Prisma+DISCOVERY+Achieva+
                 tfmri_nback_all_meanmotion+
                 (1|src_subject_id)  +
                 (1|site_id_l) ,
               data = predictions2,adjusted = T,
               grname ="src_subject_id",datatype = "Gaussian",npermut = 0,nboot = 0)
    
    rel_out = rel1$R$src_subject_id
    #rel_out
    
    
    
    
    
    
    ##########################
    
    
    
    out.all1 = data.frame(args = args, 
                          rep = analysis.framework3$reps[args], 
                          n = analysis.framework3$nsize[args]*10,
                          rel=rel_out)
    
    
    out.all2 = data.frame(nback_rate = reg.long$Estimate[1],
                          cog = reg.long$Estimate[2],
                          listsorting = reg.long$Estimate[3],
                          attention = reg.long$Estimate[4],
                          ple = reg.long$Estimate[5])
    
    
    out.all= cbind(out.all1,out.all2)
    
    out.all
    # bestone$Rsquared %>% sqrt()
    # out.all
    write.table(x = out.all,file = paste(base_dir,"boot_out/boots.cog.",args[1],".csv",sep=""),
                quote = F,row.names = F,col.names = F,sep = ",")
    
    
    
    if(args[1] == 1){
      write.table(x = t(colnames(out.all)),file = paste(base_dir,"boot_out/boots.cog.0.csv",sep=""),
                  quote = F,row.names = F,col.names = F,sep = ",")
    }
    
    
 
