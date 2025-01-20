#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
args=as.numeric(args)
args=args[1]

###############

library(data.table)
library(dplyr)
library(scales)
library(Matrix)
library(caret)
library(pls)
library(stringr)


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/sims/")


##################
# # 
# var.num  = 100
# shape =.05
# rate=1.5
# rate2 = 25
# train_n = 100
# test_n = 5000
# effect.sim = .9
# mean.sd = .2
# total.var =.2
# total_n = 10000





sim_neural_sig = function(var.num,
                          shape,
                          rate,
                         # rate2,
                          train_n,
                          test_n,
                          effect.sim,
                          mean.sd ,
                          total.var ,
                          total_n = 10000){
  
  ##################################
 # sim.cor = function(n.var,shape,rate,rate2){
  
     n.var = var.num*2
     eigenvalues.diag = rgamma(n = n.var,shape = shape,rate = rate)

     while(min(eigenvalues.diag) <0 ){rgamma(n = n.var,shape = shape,rate = rate)}
 
     eigenvalues = diag(n.var)
    diag(eigenvalues) = eigenvalues.diag
    
    # eigenvectors = rexp(n = n.var^2,rate = rate2)
    # flip = sample(1:length(eigenvectors),size = length(eigenvectors)/2,replace = F)
    # eigenvectors[flip] = eigenvectors[flip] * -1

    eigenvectors = rnorm(n = n.var^2,mean = 0,sd = 1)
    
    
    eigenvectors = matrix(data = eigenvectors,
                          nrow = n.var,ncol = n.var)
    
    #eigenvectors = qr(eigenvectors)$qr
    
    cov.mat = eigenvectors %*% eigenvalues %*% t(eigenvectors)

    new.seed = round(digits = 0,runif(n = 1,min = 1,max = 1000000))
    while(is.complex(eigen(cov.mat,only.values = TRUE)$values[1])){
    set.seed(new.seed)
      cat(new.seed)
      new.seed = new.seed + 1
      cat("complex! ")
      n.var = var.num*2
      
      eigenvalues.diag = rgamma(n = n.var,shape = shape,rate = rate)
      
      while(min(eigenvalues.diag) <0 ){rgamma(n = n.var,shape = shape,rate = rate)}
      
      eigenvalues = diag(n.var)
      diag(eigenvalues) = eigenvalues.diag
      
      # eigenvectors = rexp(n = n.var^2,rate = rate2)
      # flip = sample(1:length(eigenvectors),size = length(eigenvectors)/2,replace = F)
      # eigenvectors[flip] = eigenvectors[flip] * -1
      
      eigenvectors = rnorm(n = n.var^2,mean = 0,sd = 1)
      
      
      eigenvectors = matrix(data = eigenvectors,
                            nrow = n.var,ncol = n.var)
      
      #eigenvectors = qr(eigenvectors)$qr
      
      cov.mat = eigenvectors %*% eigenvalues %*% t(eigenvectors)
      

    }
    
    # while(min(eigen(cov.mat)$values) < 0){
    #   cat("neg-eigen!")
    #   n.var = var.num*2
    #   eigenvalues.diag = rgamma(n = n.var,shape = shape,rate = rate)
    #   
    #   while(min(eigenvalues.diag) <0 ){rgamma(n = n.var,shape = shape,rate = rate)}
    #   
    #   eigenvalues = diag(n.var)
    #   diag(eigenvalues) = eigenvalues.diag
    #   
    #   # eigenvectors = rexp(n = n.var^2,rate = rate2)
    #   # flip = sample(1:length(eigenvectors),size = length(eigenvectors)/2,replace = F)
    #   # eigenvectors[flip] = eigenvectors[flip] * -1
    #   
    #   eigenvectors = rnorm(n = n.var^2,mean = 0,sd = 1)
    #   
    #   
    #   eigenvectors = matrix(data = eigenvectors,
    #                         nrow = n.var,ncol = n.var)
    #   
    #   #eigenvectors = qr(eigenvectors)$qr
    #   
    #   cov.mat = eigenvectors %*% eigenvalues %*% t(eigenvectors)
    # }
    # 
    # cor.mat = cov2cor(cov.mat)
    # cor.mat = as.matrix(Matrix::nearPD(cor.mat)$mat)
  
    cov.mat = as.matrix(Matrix::nearPD(cov.mat)$mat)
  # corrplot::corrplot(  as.matrix(cov2cor(cov.mat)),method = 'color',tl.pos = 'n',order = 'hclust')
  
      
      #   return(cor.mat)
  # }
  
  #cor.mat =  sim.cor(n.var = var.num*2,shape = shape,rate = rate,rate2 = rate2)
  # 
  # corrplot::corrplot( eigen2,method = 'color',tl.pos = 'n',order = 'hclust')
  # 
  # cov.mat = make.cov(n.var=n.var,shape=shape,rate=rate,rate2=rate2)
  # corrplot::corrplot(  as.matrix(cor.mat),method = 'color',tl.pos = 'n',order = 'hclust')
  
    #dat <- MASS::mvrnorm(n = total_n,mu = rep(0,var.num*2),Sigma = cor.mat)
    dat <- MASS::mvrnorm(n = total_n,mu = rep(0,var.num*2),Sigma = cov.mat)
   
     dat = apply(dat,2,function(X){ (X - mean(X))/sd(X) })
  
     
     # cor2 = cor(dat)
  # 
  # clust = matrix(0,ncol = 2,nrow = var.num)
  # 
  # 
  # for(i in 1: (var.num - 1 )){
  #   clust[i,1] = colnames(cor2)[1]
  #  # clust[i,3] = max(cor2[1,-1])
  #   keep1 = which(cor2[1,-1] == max(cor2[1,-1]))
  #   keep =  colnames(cor2)[which(colnames(cor2) ==  names(which(cor2[1,-1] == max(cor2[1,-1]))))]
  #   clust[i,2] = keep
  #   cor2 = cor2[-unname(keep1+1),-unname(keep1+1)]
  #   cor2 = cor2[-1,-1]
  # }
  # clust[var.num,] = colnames(cor2)
  # clust = matrix(t(clust),ncol = 1,byrow = FALSE)
  # clust =  match(clust,colnames(dat))
  
  
  clust = hclust(dist(t(dat)))
  
  # corrplot::corrplot( cor(dat[,clust]),method = 'color',tl.pos = 'n',order = 'hclust')
 # corrplot::corrplot( cor(dat),method = 'color',tl.pos = 'n',order = 'hclust')
  
  
  odd_col = (seq_len(var.num*2) %% 2 ) == 1
  
  dat1 = dat[,clust$order[odd_col]] %>% scale(center=TRUE,scale=TRUE)
  dat2 = dat[,clust$order[!odd_col]]  %>% scale(center=TRUE,scale=TRUE)
  # dat1 = dat[,clust[odd_col]] %>% scale(center=TRUE,scale=TRUE)
  # dat2 = dat[,clust[!odd_col]]  %>% scale(center=TRUE,scale=TRUE)
  
  colnames(dat2) = colnames(dat1)
  
  dat2.orig = dat2
  
  make_diff = function(dat1,dat2){
    
    diff.mat = matrix(data = NA,nrow =total_n,ncol(dat1 )) %>% as.data.frame()
    
    for(i  in 1:ncol(dat1)){
      diff.mat[,i] = dat2[,i] - dat1[,i] 
    }
    
    diff.mat = apply(diff.mat,2,scale,center=TRUE,scale=TRUE) %>% as.data.frame()
    return(diff.mat)
    
  }
  
  diff.mat = make_diff(dat1,dat2)
  
  #corrplot::corrplot( cor(diff.mat),method = 'color',order = 'hclust',tl.pos = 'n')
  
  
  betas = rnorm(n = var.num,
                sd = 1,
                mean = 0)
  
  normalizer =total.var/sum(betas^2)
  betas2 = betas^2*normalizer
  betas2 = sqrt(betas2)
  betas = betas2 * (betas/abs(betas))
  
  pca = princomp(diff.mat)$scores
  
  ymean = apply(t(t(pca) * betas),1,sum)
  ysd  = sqrt(1 - sum(betas^2))
  y <- stats::rnorm(total_n, ymean , ysd) %>% scale(center = TRUE,scale = TRUE)
  #obs.betas = summary(lm(y ~., data = diff.mat))$coefficients[-1,1] %>% scale(center = TRUE,scale = TRUE)
  
  obs.betas = cor(y,diff.mat) %>% c() %>% scale(center = TRUE,scale = TRUE) 
  
  means = rnorm(n = var.num,
                mean = 0,
                sd = 1) 
  
  for(i in 1:length(effect.sim)){
    #i=1
    desired_correlations <- matrix(c(
      1, effect.sim[i],
      effect.sim[i], 1 ), 2, 2 )
    x =cbind(obs.betas,means)
    x.chol <- x %*% solve(chol(var(x))) %*% chol(desired_correlations)
    new.means = x.chol[,2]
    
    new.means =(new.means - mean(new.means))/sd(new.means)
    new.means = new.means * mean.sd
    
    
    dat2 =t(t(dat2.orig) + new.means)
    
    diff.mat = make_diff(dat1,dat2)
    diff.mat$y = y
    
    diff.mat$ID = c(1:total_n)
    
    
    ###########################################
    
    make_long = function(dat1,dat2){
      dat1 = as.data.frame(dat1)
      dat2 = as.data.frame(dat2)
      
      dat1$con = 0
      dat2$con = 1
      
      dat1$ID = c(1:total_n)
      dat2$ID = c(1:total_n)
      
      tempdat_out = rbind(dat1,dat2)
      return(tempdat_out)
    }
    long.dat = make_long(dat1,dat2)
    
    #############################
    # classifier model
    
    IDs = unique(long.dat$ID)
    g1 = sample(IDs,size = train_n,replace = FALSE)
    
    test_pool = IDs[!IDs %in% g1]
    g2 = sample(test_pool,size = test_n,replace = FALSE)
    
    train = long.dat[long.dat$ID  %in%  g1 ,]
    
    test = long.dat[long.dat$ID  %in%  g2 ,]
    
    
    standardize = function(tempdat){
      tempdat[,grep("combat",colnames(tempdat))] = tempdat[,grep("combat",colnames(tempdat))] %>% 
        apply(2,function(X){
          X = scale(X,center = TRUE,scale=TRUE)
          attributes(X) = NULL
          return(X)
          
        })
      return(tempdat)
    }
    
    train = standardize(train)
    test = standardize(test)
    
    #################################################
    fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 5)
    
    
    PLS_model = train(
      as.factor(con) ~ . , data = train[,-which(colnames(train) == 'ID')],
      trControl = fitControl,
      method = "pls",
      family="binomial",
      metric="Accuracy",
      #tuneLength = 5
      tuneGrid = expand.grid(ncomp = seq(1,15,1))
    )
    
    
    PLS_final = caret::plsda(x = train[,-c(which(colnames(train) == "ID"|colnames(train) == "con"))], y =  as.factor(train[,which(colnames(train) == 'con')]), 
                             method = "oscorespls",
                             ncomp = PLS_model$bestTune$ncomp,
                             probMethod = "softmax",
                             type='prob')
    
    pred = predict(object = PLS_final,
                   newdata = test[,-c(which(colnames(test) == "ID"|colnames(test) == "con"))],
                   type="prob",
                   ncomp = PLS_model$bestTune$ncomp)
    test$pred = pred[,2,]
    
    
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
    
    ###################################################
    # Regression model
    
    testdiff = merge(testdiff,diff.mat)
    
    traindiff =  diff.mat[diff.mat$ID  %in%  g1 ,]
    
    PLS_model_reg = train(
      y ~ . , data = traindiff[,-which(colnames(train) == 'ID')],
      trControl = fitControl,
      method = "pls",
      metric="RMSE",
      tuneGrid = expand.grid(ncomp = seq(1,15,1))
    )
    
    
    
    PLS_final_reg = pls::plsr(y ~ . , data = traindiff[,-which(colnames(train) == 'ID')],ncomp = PLS_model_reg$bestTune$ncomp)
    
    
    
    testdiff$y_pred = predict(object = PLS_final_reg,
                              newdata = testdiff%>% dplyr::select(starts_with("V")),
                              ncomp = PLS_model_reg$bestTune$ncomp)
    
    
    mtest = lm(testdiff$y ~. ,testdiff%>% dplyr::select(starts_with("V"))) %>% summary()
    
    
    #################################
    # outcomes
    
    cor.out = cor(testdiff) 
    cor.out = cor.out[grep(pattern = "y$",rownames(cor.out)),] %>% t() %>% as.data.frame()
    
    cor.out1 = cor.out[,grep(pattern = "^V",colnames(cor.out))]   %>% as.matrix() 
    cor.out2 = cor.out[,grep(pattern = "diff|pred",colnames(cor.out))]  %>% as.data.frame()
    
    colnames(cor.out2) = str_replace(string = colnames(cor.out2),pattern = "_",replacement = ".")
    
    cor.out1 = abs(cor.out1)
    
    out = data.frame(total_n = total_n,
                     train_n = train_n,
                     test_n = test_n,
                     var.num = var.num,
                     effect.sim = effect.sim[i],
                     mean.sd = mean.sd,
                     shape = shape,
                     rate = rate,
                    # rate2 = rate2,
                     total.var = total.var,
                     obs.effect.sim =  cor(new.means,obs.betas),
                     auc = as.numeric(pROC::roc(test$con,test$pred,quiet=TRUE)$auc),
                     test.r2 = mtest$r.squared,
                     test.adjr2 = mtest$adj.r.squared,
                     min.V = min(cor.out1),
                     max.V = max(cor.out1),
                     med.V  = median(cor.out1)
    ) 
    out = cbind(out,cor.out2)
    
    if(i == 1){out.all = out}else{out.all = rbind(out.all,out)}
  }
  
  return(out.all)

  }




#########################################################

# set.seed(121)
# 
# sim_neural_sig(var.num = 150,shape = 0.05,rate = 1.5,test_n = 5000,total_n = 10000,
#                train_n = 40,
#                effect.sim = .8,
#                total.var = .01,
#                mean.sd = .15)



sim_settings = read.csv(file = paste(base_dir,"sim_in/sim_in.complex.",args,".csv",sep=""))


for(i in 1: dim(sim_settings)[1]){
  print(i)
  print(sim_settings$seed[i])
  set.seed(sim_settings$seed[i])
  
  out = sim_neural_sig(train_n = sim_settings$train_n[i],
                              # mean.sd = sim_settings$mean.sd[i],
                                mean.sd = 0.1,
                       
                               total.var = sim_settings$total.var[i],
                               test_n = 5000,
                               var.num = 150,
                               effect.sim = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,.99),
                               shape = 0.05,
                               rate = 1.5,
                               #rate2 = 25,
                               total_n = 10000)
  
  out$arg = args
  
  if(i == 1){out.all = out}else{out.all = rbind(out.all,out)}
  
}


write.table(x = out.all,file = paste(base_dir,"sim_out/sim.out.complex.",args[1],".csv",sep=""),
            quote = F,row.names = F,col.names = F,sep = ",")



if(args[1] == 1){
  write.table(x = t(colnames(out.all)),file = paste(base_dir,"sim_out/sim.out.complex.0.csv",sep=""),
              quote = F,row.names = F,col.names = F,sep = ",")
  
}


