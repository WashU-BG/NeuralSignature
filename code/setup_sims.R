library(dplyr)

base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/sims/sim_in/")
# 
# analysis.framework = expand.grid(
#   train_n = unique(2^c(1:8) * 10),
#   var.num = unique(seq(2,10,2)),
#   main.effect.condition =unique(c(1,.5,.25,.1,.05)),
#   total_n = 11000,
#   within.obs.correl = unique(seq(0,.9,.1)),
#   total.var = unique(c(0.005,0.01,0.025,0.05,0.075,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6)),
#   test_n = 5000,
#   total_n = 11000,
#   reps=unique(c(1:1000))
# ) 


# 
# analysis.framework = expand.grid(
#   train_n = unique(2^c(1:8) * 10),
#   #var.num = unique(seq(2,10,1)),
#   var.num = 5,
#   total_n = 8000,
#   test_n = 5000,
#   reps = 5)
# gc()
# analysis.framework$main.effect.condition = runif(n = dim(analysis.framework)[1],min = 0,max = 1)
# gc()
# analysis.framework$total.var = runif(n = dim(analysis.framework)[1],min = 0.0005,max = .6)
# gc()
# analysis.framework$within.obs.correl = runif(n = dim(analysis.framework)[1],min = 0,max = .9)
# gc()


n.sim = 10000
analysis.framework = data.frame(  
                                  total_n = 8000,
                                  test_n = 5000,
                                  effect.sim = runif(n = n.sim,min = -.999,max = .999),
                                  mean.sd = runif(n = n.sim,min = 0.0005,max = 1),
                                  total.var = runif(n = n.sim,min = 0.0005,max = .6),
                                  within.obs.correl = runif(n = n.sim,min = 0,max = .9)
                                  )
gc()
analysis.framework2 = expand.grid( train_n = unique(2^c(1:8) * 10),
                                   var.num = unique(seq(4,10,2)))

analysis.framework3 = merge(analysis.framework,analysis.framework2)
gc()
analysis.framework3$node = c(1:200)
gc()



for(i in 1: 200){
  temp = analysis.framework3 %>% dplyr::filter(node == i)
  gc()
  write.csv(x = temp,file = paste(base_dir,"sim_in.",i,".csv",sep=""),row.names = FALSE)
}
##########################################

# sim_neural_sig(var.num = 150,shape = 0.05,rate = 1.5,rate2 = 25,test_n = 5000,total_n = 10000,
#                train_n = 40,
#                effect.sim = c(0,.1,.3,.6,.9,.99),
#                total.var = .01,
#                mean.sd = .1)
# n.sim = 70000
# analysis.framework = data.frame(  
#   mean.sd = runif(n = n.sim,min = 0.01,max = 0.1),
#   total.var = runif(n = n.sim,min = 0.0005,max = .2))
# analysis.framework2 = expand.grid( train_n = unique(2^c(1:8) * 10))
# analysis.framework3 = merge(analysis.framework,analysis.framework2)
# analysis.framework3$seed = c(1:nrow(analysis.framework3))
# 
# analysis.framework3$node = c(1:200)
# nrow(analysis.framework3)
# 
# for(i in 1: 200){
#   temp = analysis.framework3 %>% dplyr::filter(node == i)
#   gc()
#   write.csv(x = temp,file = paste(base_dir,"sim_in/sim_in.complex.",i,".csv",sep=""),row.names = FALSE)
# }

#######################

 n.sim = 10000
# analysis.framework = data.frame(  
#   mean.sd = runif(n = n.sim,min = 0.01,max = 0.1),
#   total.var = runif(n = n.sim,min = 0.0005,max = .2))
analysis.framework2 = expand.grid( train_n = unique(2^c(1:8) * 10),
                                   total.var = c(0.1,0.05,0.01,0.005))

analysis.framework = data.frame(analysis = c(1:n.sim))
  
  
analysis.framework3 = merge(analysis.framework,analysis.framework2)
analysis.framework3$seed = c(1:nrow(analysis.framework3))

analysis.framework3$node = c(1:200)
nrow(analysis.framework3)

for(i in 1: 200){
  temp = analysis.framework3 %>% dplyr::filter(node == i)
  gc()
  write.csv(x = temp,file = paste(base_dir,"sim_in/sim_in.complex.",i,".csv",sep=""),row.names = FALSE)
}
################################



# 
# 
# sim.num = 1000
# for(i in 1:sim.num){
#   print(i)
#   if(i == 1){
#     temp =  sim_neural_sig(train_n = 80,test_n = 10000,total_n = 50000,
#                            var.num = 10,
#                            effect.sim = 0.9,
#                            mean.sd = 1,
#                            within.obs.correl = .9,
#                            total.var = .1,
#                            rel = 1,
#                            yrel = 1) 
#     sim.res = matrix(NA,nrow = sim.num,ncol = ncol(temp)) %>% as.data.frame()
#     colnames(sim.res) = colnames(temp)
#   }
#   
#   sim.res[i,] = sim_neural_sig(train_n = 80,test_n = 10000,total_n = 50000,
#                                var.num = 10,
#                                effect.sim = 0.9,
#                                mean.sd = 1,
#                                within.obs.correl = .9,
#                                total.var = .1,
#                                rel = 1,
#                                yrel = 1) 
#   
# }
# 
# 
# 
# sim.res$comparison = sim.res$diff - sim.res$y.pred
# hist(sim.res$comparison)
# 
# 
# library(foreach)
# library(doParallel)
# 
# 
# cl <- makeCluster(15)
# registerDoParallel(cl)
# 
# sim_out = foreach(i=1:1000,.combine = rbind,.packages = c('dplyr','faux','stringr','pROC'),.export = 'sim_neural_sig' ) %dopar% {
#   
#   sim_neural_sig(train_n = 40,test_n = 10000,total_n = 50000,
#                  var.num = 4,
#                  effect.sim = .9,
#                  mean.sd = 1,
#                  within.obs.correl = .6,
#                  total.var = .05,
#                  rel = 1,
#                  yrel = 1) 
# 
#   
#   }
# 
# 
# stopCluster(cl)
# registerDoSEQ()
# 
# sim_out$comparison = sim_out$diff^2 - sim_out$y.pred^2
# hist(sim_out$comparison)
# 
# hist(sim_out$auc)
# hist(sim_out$obs.effect.sim)
