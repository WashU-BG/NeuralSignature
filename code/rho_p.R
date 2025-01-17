
base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")



region_regressions = fread( file = paste(base_dir,"Final/all_region_regressions.csv",sep=""),data.table = F)
region_regressions =  region_regressions %>% dplyr::filter(contrast == 3)



names = fread( file = paste(base_dir,"Final/VariableNames.csv",sep=""),data.table = F)

names$y2 = as.factor(names$Expanded)
names$y2 = factor(names$y2,levels = rev(names$Expanded))

region_regressions = merge(region_regressions,names)



ML_regressions = fread( file = paste(base_dir,"ML_regressions_log3.csv",sep=""),data.table = F)
ML_regressions = ML_regressions[-grep("mrt|diff",x = ML_regressions$y),]
ML_regressions = merge(ML_regressions,names)


perm.out = fread(paste(base_dir,"Final/maps.all.permuted.csv",sep=""),data.table = F)

task_all_full.map = fread(paste(base_dir,"fulltaskmap.csv",sep=""),data.table = F)

variables = unique(region_regressions$y)

out = data.frame(rep = c(1:10000),cor=NA)

for(i in 1: dim(out)[1]){
  print(i)
  
  null.cor = data.frame(y = variables,map.cor=NA)
  
  for (ii in 1:length(variables)){
    
    temp =region_regressions %>% dplyr::filter(y == variables[ii]) 
    
    t1 = perm.out[i,] %>% t() %>% as.data.frame()
    t1$region =rownames(t1)
    colnames(t1)[1] = "t"
    temp3 = merge(temp,t1)
    null.cor$map.cor[ii] = cor(temp3$Estimate,temp3$t)
  }
  
  temp4 = merge(ML_regressions,null.cor)
  
  out$cor[i] = cor(temp4$Estimate,temp4$map.cor,method = "spearman")
  
}


 sum(out$cor^2 > 0.979^2)
