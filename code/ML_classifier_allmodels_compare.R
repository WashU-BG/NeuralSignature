
library(data.table)
library(dplyr)
library(lme4)
library(DescTools)
library(pROC)
library(epiR)
library(ggplot2)
library(tidyr)
library(ggpubr)

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


predict.out = function(dat,mods){
  
  new.dat = dat[,-c(1:4)] %>% as.matrix()
  
 test1 =  predict(mods$Elnet,new.dat,type="response")
 dat$Elnet = test1[,1]
 
 test2 =  predict(mods$PLS_LDA,new.dat,type="prob") %>% as.data.frame()
 dat$PLS = test2[,2]
 
 test3 =  predict(mods$RF,new.dat,type="prob") %>% as.data.frame()
 dat$RF = test2[,2]
 
 test4 =  predict(mods$GBM,new.dat %>%  as.data.frame(),type="response") %>% as.data.frame()
 dat$GBM = test4[,1]
 
 test5 =  predict(mods$SVM,new.dat %>%  as.data.frame(),probability = T) %>% as.data.frame()
 dat$SVM_lin=attr(test5$.,which = "probabilities")[,2]
 
 test6 =  predict(mods$SVM_rbf,new.dat %>%  as.data.frame(),type="prob") %>% as.data.frame()
 dat$SVM_rbf = test6[,2]
 
 
 scores = new.dat %*% mods$Lasso_pca_final$projection
 test7 =  predict(mods$Lasso_pca_final,scores,type="response") %>% as.data.frame()
 dat$Lasso_PCA = test7[,1]
 
 return(dat)
  
}


analysis.framework = data.frame(train = c(1,2),test = c(2,1))
################

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

for(i in c(1:2)){
  
  #split1 = c("site06","site20","site04","site16","site19","site05","site01","site13","site09","site21")
  task_con = "nback_2b_v_0b"
  task = "nback"
  
  task_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$train[i],"_combat.csv",sep=""),data.table = F) 
  
  test_all = fread(paste(base_dir,"combat/",task_con,"_all_",analysis.framework$test[i],"_combat.csv",sep=""),data.table = F) 
 
  
  task_all = fmri_standardize(dat = task_all)
  
  test_all = fmri_standardize(dat = test_all)

  
  train.mods=readRDS(paste(base_dir,"ML_models.",analysis.framework$train[i],".classification.all.RData",sep=""))
  
  #test.mods=readRDS(paste(base_dir,"ML_models.",analysis.framework$test[i],".classification.all.RData",sep=""))
  
  train.pred = predict.out(dat =task_all,mods =  train.mods)
  test.pred = predict.out(dat =test_all,mods =  train.mods)
  
  
  train.pred$split = analysis.framework$train[i]
  test.pred$split = analysis.framework$test[i]
  
  if(i == 1){
    train.all = train.pred
    test.all = test.pred
  }else{
    train.all = rbind(train.all,train.pred)
    test.all = rbind(test.all,test.pred)
      }
}
 
#######################################################
 
  get_auc=function(con,pred){
    test = pROC::roc(con,pred,)
    test.ci = pROC::ci(test,of = "auc")
    
    return(c(test$auc %>% as.numeric(),test.ci[1],test.ci[3] ))
  }
  

auc_test_performance = test.all %>%
  dplyr::select(c("src_subject_id","eventname","con","split","Elnet","PLS","Lasso_PCA","SVM_lin","SVM_rbf","RF","GBM")) %>% 
  pivot_longer(names_to = "model",values_to = "pred",cols = c("Elnet","PLS","Lasso_PCA","SVM_lin","SVM_rbf","RF","GBM")) %>% 
  group_by(split,model) %>%
  summarise(auc = get_auc(con,pred)[1],L.CI = get_auc(con,pred)[2],U.CI = get_auc(con,pred)[3])


v_lines = auc_test_performance %>% dplyr::filter(model == "Elnet")


auc_test_performance$model = as.factor(auc_test_performance$model)
auc_test_performance$model = factor(auc_test_performance$model,levels = levels(auc_test_performance$model)[c(5,2,7,6,4,3,1)])

levels(auc_test_performance$model)=c("Random Forest","GBM","SVM - RBF","SVM - Linear","PLS - LDA", "PCA - Lasso","Elastic Net")
 

auc_performance = ggplot(data = auc_test_performance,aes(y = model,x =auc))+
  
   scale_x_continuous(name = "Area Under the ROC Curve")+
   scale_y_discrete(name = "ML Algorithm")+
   
   geom_rect(inherit.aes = F,data = v_lines,aes(xmin =L.CI,xmax = U.CI ,ymin = 0,ymax = 8),fill = "blue",alpha=.25)+
   geom_vline(data = v_lines,aes(xintercept = L.CI),linetype = "dashed")+
   geom_vline(data = v_lines,aes(xintercept = U.CI),linetype = "dashed")+
   
  geom_linerange( aes(y=model, xmin=L.CI, xmax=U.CI),
                  stat = "identity",linewidth=1,color="black",
                  show.legend = F)+
  
  geom_point(shape=16,size=3,color="black")+
   theme_bw()+
   facet_wrap(~split,labeller = labeller(split = c("1" = "Split = 1","2" = "Split = 2")))

##########################################################  

cor.plot = function(cor.mat){

  ind = expand.grid(row=c(1:dim(cor.mat)[1]),col=c(1:dim(cor.mat)[1])) %>% as.matrix()
  
  cor.mat2 = base::data.frame( col = dimnames(cor.mat)[[2]][ind[,2]] ,
                               row = dimnames(cor.mat)[[1]][ind[,1]] ,
                               val = cor.mat[ ind ] ) %>% as.data.frame()
  var.names = colnames(cor.mat)
  
  cor.mat2$col = factor(cor.mat2$col,levels = rev(var.names))
  cor.mat2$row = factor(cor.mat2$row,levels = (var.names))
  
  segs = dim(cor.mat)[1] + .5
  segment1 = data.frame(x = seq(0.5,segs,1),xend = seq(0.5,segs,1), y = .5, yend = segs)
  segment2 = data.frame(y = seq(0.5,segs,1),yend = seq(0.5,segs,1), x = .5, xend = segs)
  
  segment3=rbind(segment1,segment2)
  
  out =  ggplot2::ggplot(cor.mat2,ggplot2::aes(x = .data$row, y = .data$col, fill = .data$val,label = round(.data$val,2)))+
    ggplot2::geom_raster()+
    ggplot2::geom_segment(data = segment3,inherit.aes = F,ggplot2::aes(x = .data$x,xend = .data$xend, y = .data$y, yend = .data$yend),color="black")+
    ggplot2::scale_fill_gradient2(limits = c(-1,1), 
                                  guide = guide_colorbar( frame.colour = "black",
                                                                            barwidth = 10,
                                                                            barheight =1.5,
                                                                            draw.ulim = T,
                                                                            title.position = "top",
                                                                            ticks.colour = "black",
                                                                            frame.linewidth = .5,
                                                                            label.theme = element_text(size=10),
                                                                            ticks.linewidth = 1),
                                  high = "#C42503FF",
                                  low = "#455BCDFF",
                                  mid = "white",
                                  midpoint = 0)+
    ggplot2::geom_text(color="white")+
    ggplot2::theme_classic()+
    ggplot2::coord_equal()+
    ggplot2::scale_x_discrete(position = "bottom") +
    ggplot2::labs(fill = "Correlation (r)")+
    ggplot2::theme(legend.position = "top",
                   axis.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "black"),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


return(out)
  
  
}



###################################



train.dat = train.all[,grep(x = colnames(train.all),pattern = "combat")]

haufe = data.frame(region = colnames(train.all)[grep(x = colnames(train.all),pattern = "combat")])

haufe$Elnet = apply(train.dat,2,function(X){cov(X,train.all$Elnet)})
haufe$PLS = apply(train.dat,2,function(X){cov(X,train.all$PLS)})
haufe$RF = apply(train.dat,2,function(X){cov(X,train.all$RF)})
haufe$GBM = apply(train.dat,2,function(X){cov(X,train.all$GBM)})
haufe$SVM_lin = apply(train.dat,2,function(X){cov(X,train.all$SVM_lin)})
haufe$SVM_rbf = apply(train.dat,2,function(X){cov(X,train.all$SVM_rbf)})
haufe$Lasso_PCA = apply(train.dat,2,function(X){cov(X,train.all$Lasso_PCA)})

cor.mat = cor(haufe[,-1])
cor.mat = cor.mat[c(1,7,2,5,6,4,3),c(1,7,2,5,6,4,3)]

colnames(cor.mat) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
row.names(cor.mat) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))

haufe.plot = cor.plot(cor.mat)
########################################################

test.wide =   test.all %>%
  dplyr::select(c("src_subject_id","eventname","con","split","Elnet","PLS","Lasso_PCA","SVM_lin","SVM_rbf","RF","GBM")) %>% 
  pivot_longer(names_to = "model",values_to = "pred",cols = c("Elnet","PLS","Lasso_PCA","SVM_lin","SVM_rbf","RF","GBM")) %>% 
  pivot_wider(names_from = con,values_from = pred,names_prefix = "con_")


test.wide$diff = test.wide$con_1 - test.wide$con_0

zero_back = test.wide %>% dplyr::select(c("src_subject_id","eventname","split","model","con_0")) %>% 
  pivot_wider(names_from = model,values_from = con_0)

two_back = test.wide %>% dplyr::select(c("src_subject_id","eventname","split","model","con_1")) %>% 
  pivot_wider(names_from = model,values_from = con_1)

diff_score = test.wide %>% dplyr::select(c("src_subject_id","eventname","split","model","diff")) %>% 
  pivot_wider(names_from = model,values_from = diff)

zero_cor = cor(zero_back[,-c(1:3)])
two_cor = cor(two_back[,-c(1:3)])
diff_cor = cor(diff_score[,-c(1:3)])

zero_cor = zero_cor[c(1,3,2,4,5,7,6),c(1,3,2,4,5,7,6)]
colnames(zero_cor) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
row.names(zero_cor) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
zero_plot = cor.plot(zero_cor)
zero_plot = zero_plot + ggtitle("0-back")


two_cor = two_cor[c(1,3,2,4,5,7,6),c(1,3,2,4,5,7,6)]
colnames(two_cor) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
row.names(two_cor) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
two_plot = cor.plot(two_cor)
two_plot = two_plot + ggtitle("2-back")


diff_cor = diff_cor[c(1,3,2,4,5,7,6),c(1,3,2,4,5,7,6)]
colnames(diff_cor) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
row.names(diff_cor) = rev(c("RF","GBM","SVM - RBF","SVM - Lin","PLS - LDA", "PCA - Lasso","E Net"))
diff_plot = cor.plot(diff_cor)
diff_plot = diff_plot + ggtitle("2-back > 0-back")

all.plot = ggarrange(zero_plot,two_plot,diff_plot,ncol = 3,common.legend = TRUE)
all.plot


final.plot1 = ggarrange(auc_performance,haufe.plot,ncol = 2,labels = c("A","B"))
final.plot= ggarrange(final.plot1,all.plot,nrow = 2,labels = c("","C"),heights = c(.9,1))

final.plot

ggsave(plot =final.plot,filename ="C:/Users/dbara/Documents/ABCD/NerualSig/comparemodels.jpeg",
       dpi=500,width = 10,height = 9 )
