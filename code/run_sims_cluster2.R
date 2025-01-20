#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]


# with correlation btw means and associations

library(data.table)
library(dplyr)
library(faux) 
library(diptest)
library(pROC)
library(stringr)



base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/sims/")


##################

sim_neural_sig = function(train_n,
                          test_n,
                          var.num,  
                          effect.sim,
                          mean.sd ,
                          within.obs.correl, 
                          total.var ,
                          rel = 1,
                          yrel = 1,
                          total_n = 10000){
  
  ##################################
  add.noise = function(x,rel){
    x2 =(x+stats::rnorm(length(x), 0, sqrt((1-rel)/rel)))
    return(x2)
  }
  # sim data
  #means = rep(main.effect.condition,var.num)
  
 # params= MASS::mvrnorm(n =var.num,mu = c(0,0),Sigma = matrix(c(mean.sd^2,effect.sim,
 #                                                          effect.sim,1),byrow = TRUE,nrow = 2) )
  
 means = rnorm(n =var.num,mean = 0,sd = mean.sd )
 
 
 
# means = params[,1]

  var.num2 = var.num*2
  
  mean.vec = matrix(c(rep(0,var.num),means),nrow = 2,byrow = T) %>% c()
  
  cor.vec = rep(0,(var.num2^2-var.num2)/2)
  
  index = 1
  nextindex = var.num2-1
  while(nextindex > 0){
    index = c(index,max(index)+nextindex) 
    nextindex = nextindex - 1
  }
  
  index = index[(seq_len(length(index)) %% 2 ) == 1]
  
  cor.vec[index] =  rep(within.obs.correl,var.num)
  
  dat <- faux::rnorm_multi(
    n = total_n, 
    vars = var.num2, 
    r =  cor.vec, 
    mu = mean.vec, 
    sd = rep(1,var.num2), 
    varnames = letters[c(1:var.num2)]
  ) 
  
  ## make difference scores
  
  make_diff = function(dat,var.num2){
    
    odd_col =  seq_len(var.num2) %% 2 
    odd_col = c(1:var.num2)[odd_col == 1]
    
    diff.mat = matrix(data = NA,nrow =total_n,var.num2/2 ) %>% as.data.frame()
    
    for(i  in 1:length(odd_col)){
      diff.mat[,i] =  dat[,odd_col[i]+1] - dat[,odd_col[i]]
    }
    
    diff.mat = apply(diff.mat,2,scale,center=TRUE,scale=TRUE) %>% as.data.frame()
    
    return(diff.mat)
  }
  
  diff.mat = make_diff(dat,var.num2)
  
  
  #betas = params[,2]
  effect.sim
  means
  
  betas = rnorm(n = var.num,
                sd = sqrt(1 - (effect.sim^2 )),
                mean = scale(means,center = T,scale = T) * effect.sim) 
  
 
  
 # if(sum(betas/abs(betas)) < 0){betas = betas * -1}
  
  
  normalizer =total.var/sum(betas^2)
  betas2 = betas^2*normalizer
  betas2 = sqrt(betas2)
  betas = betas2 * (betas/abs(betas))
  
  
  ymean = apply(t(t(diff.mat) * betas),1,sum)
  ysd  = sqrt(1 - sum(betas^2))
  y <- stats::rnorm(total_n, ymean , ysd) %>% scale(center = TRUE,scale = TRUE)
  summary(lm(y ~., data = diff.mat))
  
  diff.mat$y = y
  diff.mat.orig = diff.mat
  
  dat_t2 = apply(dat,MARGIN = 2,add.noise,rel=rel) %>% as.data.frame()
  dat = apply(dat,MARGIN = 2,add.noise,rel=rel) %>% as.data.frame()
  
  
  diff.mat = make_diff(dat,var.num2)
  diff.mat2 = make_diff(dat_t2,var.num2)
  
  diff.mat$y = add.noise(diff.mat.orig$y,yrel)
  diff.mat2$y = add.noise(diff.mat.orig$y,yrel)
  
  
  diff.mat$ID = c(1:total_n)
  diff.mat2$ID = c(1:total_n)
  
  
  ###########################################
  
  make_long = function(tempdat,var.num){
    var.num2 = var.num * 2
    odd_col =  seq_len(var.num2) %% 2 
    odd_col = c(1:var.num2)[odd_col == 1]
    
    con0 = tempdat[,odd_col] %>% as.data.frame()
    con1 = tempdat[,-odd_col] %>% as.data.frame()
    
    colnames(con0) = paste("Var",c(1:var.num),sep="")
    colnames(con1) = paste("Var",c(1:var.num),sep="")
    
    con0$con = 0
    con1$con = 1
    
    con0$ID = c(1:total_n)
    con1$ID = c(1:total_n)
    
    tempdat_out = rbind(con0,con1)
    return(tempdat_out)
  }
  dat2 = make_long(dat,var.num)
  dat2_t2 = make_long(dat_t2,var.num)
  
  #############################
  # classifier model
  
  IDs = unique(dat2$ID)
  g1 = sample(IDs,size = train_n,replace = FALSE)
  
  test_pool = IDs[!IDs %in% g1]
  g2 = sample(test_pool,size = test_n,replace = FALSE)
  
  train = dat2[dat2$ID  %in%  g1 ,]
  
  test = dat2[dat2$ID  %in%  g2 ,]
  test2 = dat2_t2[dat2_t2$ID  %in%  g2 ,]
  
  standardize = function(tempdat){
    tempdat[,grep("Var",colnames(tempdat))] = tempdat[,grep("Var",colnames(tempdat))] %>% 
      apply(2,function(X){
        X = scale(X,center = TRUE,scale=TRUE)
        attributes(X) = NULL
        return(X)
        
      })
    return(tempdat)
  }
  
  train = standardize(train)
  test = standardize(test)
  test2 = standardize(test2)
  
  log_mod = glm(con ~., data = train[,-grep("ID",colnames(train))])
  
  test$pred = predict(log_mod,test,type="response")
  test2$pred = predict(log_mod,test2,type="response")
  
  return_diff_sig = function(test_temp){
    
    test0 = test_temp %>% dplyr::filter(con == 0) %>% dplyr::select(c("ID","pred"))
    test1 = test_temp %>% dplyr::filter(con == 1) %>% dplyr::select(c("ID","pred"))
    
    colnames(test0)[2] = "pred0"
    colnames(test1)[2] = "pred1"
    
    testdiff = merge(test1,test0)
    testdiff$diff = testdiff$pred1 - testdiff$pred0
    return(testdiff)
  }
  
  testdiff = return_diff_sig(test)
  testdiff2 = return_diff_sig(test2)
  
  ###################################################
  # Regression model
  
  testdiff = merge(testdiff,diff.mat)
  testdiff2 = merge(testdiff2,diff.mat2)
  
  traindiff =  diff.mat[diff.mat$ID  %in%  g1 ,]
  
  lm_mod = lm(y ~., data = traindiff[,-grep("ID",colnames(traindiff))])
  testdiff$y_pred = predict(lm_mod ,testdiff%>% dplyr::select(starts_with("V")))
  testdiff2$y_pred = predict(lm_mod ,testdiff2%>% dplyr::select(starts_with("V")))
  
  mtest = lm(y ~. ,testdiff[,-c(1:4)]) %>% summary()
  
  
  #################################
  # outcomes
  
  diff.rel = cor(testdiff$diff,testdiff2$diff)
  y_pred.rel = cor(testdiff$y_pred,testdiff2$y_pred)
  
  vs = sapply(c(1:var.num),simplify = TRUE,function(X){
    
    cor(testdiff[,grep("^V",colnames(testdiff))[X]],
        testdiff2[,grep("^V",colnames(testdiff2))[X]]
    ) })
  V_rel = median(vs)
  
  
  
  cor.out = cor(testdiff) 
  cor.out = cor.out[grep(pattern = "y$",rownames(cor.out)),] %>% t() %>% as.data.frame()
  
  cor.out1 = cor.out[,grep(pattern = "^V",colnames(cor.out))]   %>% as.matrix() 
  cor.out2 = cor.out[,grep(pattern = "diff|pred",colnames(cor.out))]  %>% as.data.frame()
  
  colnames(cor.out2) = str_replace(string = colnames(cor.out2),pattern = "_",replacement = ".")
  
  cor.out1 = abs(cor.out1)
  
  out = data.frame(total_n = total_n,train_n = train_n,test_n = test_n,
                   yrel=yrel,rel=rel,
                   var.num = var.num,
                   effect.sim = effect.sim,
                   mean.sd = mean.sd,
                   #main.effect.condition = main.effect.condition,
                   within.obs.correl = within.obs.correl,
                   total.var = total.var,
                   diff.rel = diff.rel,
                   y.pred.rel = y_pred.rel,
                   V.rel = V_rel,
                   obs.effect.sim =  cor(means,betas),
                   auc = as.numeric(pROC::roc(test$con,test$pred,quiet=TRUE)$auc),
                   test.r2 = mtest$r.squared,
                   test.adjr2 = mtest$adj.r.squared,
                   min.V = min(cor.out1),
                   max.V = max(cor.out1),
                   med.V  = median(cor.out1)
  ) 
  out = cbind(out,cor.out2)
  
  
  # else{
  #   out = data.frame(total_n = total_n,
  #                    train_n = train_n,
  #                    test_n = test_n,
  #                    yrel=yrel,rel=rel,
  #                    var.num = var.num,
  #                    main.effect.condition = main.effect.condition,
  #                    within.obs.correl = within.obs.correl,
  #                    total.var = total.var,
  #                    diff.rel = NA,
  #                    y.pred.rel = NA,
  #                    V.rel = NA,
  #                    auc = NA,
  #                    test.r2 = NA,
  #                    test.adjr2 = NA,
  #                    min.V = NA,
  #                    max.V = NA,
  #                    med.V  = NA,
  #                    diff    = NA,
  #                    pred  = NA, 
  #                    y.pred = NA  )   }
  
  return(out)
}

#########################################################

sim_settings = read.csv(file = paste(base_dir,"sim_in/sim_in.",args,".csv",sep=""))


t1 = sim_neural_sig(train_n = sim_settings$train_n[1],
                    test_n = sim_settings$test_n[1],
                    var.num = sim_settings$var.num[1],
                   # main.effect.condition = sim_settings$main.effect.condition[1],
                   effect.sim = sim_settings$effect.sim[1],
                   mean.sd = sim_settings$mean.sd[1],
                    within.obs.correl = sim_settings$within.obs.correl[1],
                    total.var = sim_settings$total.var[1],
                    rel = 1,
                    yrel =1,
                    total_n = sim_settings$total_n[1])
t1$rep[1] = 1
t1$arg[1] = args

out.all = matrix(NA,nrow = dim(sim_settings)[1],ncol = dim(t1)[2]) %>% as.data.frame()
colnames(out.all) = colnames(t1)


for(i in 1: dim(sim_settings)[1]){
  print(i)
  out.all[i,] = sim_neural_sig(train_n = sim_settings$train_n[i],
                               test_n = sim_settings$test_n[i],
                               var.num = sim_settings$var.num[i],
                              # main.effect.condition = sim_settings$main.effect.condition[i],
                               effect.sim = sim_settings$effect.sim[i],
                               mean.sd = sim_settings$mean.sd[i],
                               within.obs.correl = sim_settings$within.obs.correl[i],
                               total.var = sim_settings$total.var[i],
                               rel = 1,
                               yrel =1,
                               total_n = sim_settings$total_n[i])
  
  #out.all$rep[i] = sim_settings$reps[i]
  out.all$arg[i] = args
  
}


write.table(x = out.all,file = paste(base_dir,"sim_out/sim.out.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")



if(args[1] == 1){
  write.table(x = t(colnames(out.all)),file = paste(base_dir,"sim_out/sim.out.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  
}

# 
# # 
# # 
# # ###
# train_n = sim_settings$train_n[i]
# test_n = sim_settings$test_n[i]
# var.num = sim_settings$var.num[i]
# main.effect.condition = sim_settings$main.effect.condition[i]
# within.obs.correl = sim_settings$within.obs.correl[i]
# total.var = sim_settings$total.var[i]
# rel = 1
# yrel =1
# total_n = sim_settings$total_n[i]
# # ###
