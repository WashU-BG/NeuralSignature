
library(ggpointdensity)
library(ggplot2)
library(data.table)
library(dplyr)
library(gtools)


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/sims/")

sim_in = fread(paste(base_dir,"all.sim.out4.csv",sep=""),header = TRUE)


hist(sim_in$auc)

c(sim_in$diff>sim_in$max.V) %>% sum() / 72000000 * 100

sim_in2 = sim_in[auc > 0.6]
c(sim_in2$diff>sim_in2$max.V) %>% sum() / nrow(sim_in2) * 100

#sim_in$main.effect.condition.gr = gtools::quantcut(sim_in$main.effect.condition ,q = 20)
#sim_in$within.obs.correl.gr = gtools::quantcut(sim_in$within.obs.correl ,q = 20)
#sim_in$total.var.gr = gtools::quantcut(sim_in$total.var ,q = 20)
# 
# #hist(sim_in$auc,breaks = 100)
# 
# sim_medians = sim_in %>% 
#   
#   group_by(train_n,
#            var.num,
#            main.effect.condition.gr,
#            within.obs.correl.gr,
#            total.var.gr) %>% 
#   summarise(auc.med = median(auc),
#             test.r2.med = median(test.r2),
#             test.adjr2.med = median(test.adjr2),
#             diff.med = median(diff),
#             y.pred.med = median(y.pred))
# 
# rm(sim_in)
# gc()
# 
# sim_medians$perf.diff = sim_medians$diff.med - sim_medians$y.pred.med
# 
# #sim_medians = sim_medians %>% dplyr::filter(main.effect.condition > 0,total.var > 0 )
# 
# 
# sim_medians$var.exp = gtools::quantcut(sim_medians$test.r2.med ,q = 6)
# sim_medians$auc.range = gtools::quantcut( sim_medians$auc.med,q = 6)
# 
# 
# sim.count = sim_medians %>% 
#   group_by(var.exp,auc.range,train_n) %>% 
#   summarise(n = length(train_n))
# 
# ####################
# 
# 
# nsen_plot = ggplot(sim_medians,aes(x =diff.med,y = y.pred.med))+
#   xlab(label = "Signature prediction (r)")+
#   ylab(label = "Linear model prediction (r)")+
#   # ggtitle(label = "",subtitle = "2-back probability")+
#   # scale_color_viridis_c(option = "B")+
#   ggpointdensity::geom_pointdensity(aes( color = after_stat(log(density)) ),
#                                     size=1,adjust=.005,method="default") + 
#   #geom_smooth(method = 'lm',se = F,formula = 'y~x',color="black",lwd = 1.5,linetype="dashed")+
#   
#   geom_abline(slope = 1,intercept = 0,lwd = 1,linetype="dashed",color="black")+
#  # geom_vline(xintercept = 0,lwd = 1,linetype="dashed",color="black")+
#   #geom_hline(yintercept = 0,lwd = 1,linetype="dashed",color="black")+
#   
#   theme_bw()+
#   theme(aspect.ratio = 1,legend.position = "right",legend.title.position = "top")+
#   labs(color =   "Point density" )+
#   scale_color_gradientn(colours = viridisLite::magma(n = 512),
#                         breaks = log(2^c(seq(0,12,2))),
#                         labels = (2^c(seq(0,12,2))),
#                         guide = guide_colorbar( frame.colour = "black",
#                                                 # barwidth = 12,
#                                                 # barheight =1.5,
#                                                 draw.ulim = T,
#                                                 title.position = "top",
#                                                 ticks.colour = "black"))
# 
# nsen_plot
# ggsave(plot = nsen_plot,filename = paste(base_dir,"sim_plot.jpeg"),dpi = 500,width = 6,height = 5)
# 
# #######################################
# 
# sim_in_sub = sim_in[sample(x = c(1:nrow(sim_in)),size = 200000,replace = F),]
# 
# 
# 
# 
# nsen_plot = ggplot(sim_in_sub,aes(x =diff,y = y.pred))+
#   xlab(label = "Signature prediction (r)")+
#   ylab(label = "Linear model prediction (r)")+
#   # ggtitle(label = "",subtitle = "2-back probability")+
#   # scale_color_viridis_c(option = "B")+
#   ggpointdensity::geom_pointdensity(aes( color = after_stat(log(density)) ),
#                                     size=1,adjust=.005,method="default") + 
#   #geom_smooth(method = 'lm',se = F,formula = 'y~x',color="black",lwd = 1.5,linetype="dashed")+
#   
#   geom_abline(slope = 1,intercept = 0,lwd = 1,linetype="dashed",color="black")+
#   # geom_vline(xintercept = 0,lwd = 1,linetype="dashed",color="black")+
#   #geom_hline(yintercept = 0,lwd = 1,linetype="dashed",color="black")+
#   
#   theme_bw()+
#   theme(aspect.ratio = 1,legend.position = "right",legend.title.position = "top")+
#   labs(color =   "Point density" )+
#   scale_color_gradientn(colours = viridisLite::magma(n = 512),
#                         breaks = log(2^c(seq(0,24,2))),
#                         labels = (2^c(seq(0,24,2))),
#                         guide = guide_colorbar( frame.colour = "black",
#                                                 # barwidth = 12,
#                                                 # barheight =1.5,
#                                                 draw.ulim = T,
#                                                 title.position = "top",
#                                                 ticks.colour = "black"))
# 
# nsen_plot
# 
# ggsave(plot = nsen_plot,filename = paste(base_dir,"sim_plot_big.jpeg"),dpi = 500,width = 6,height = 5)
# 
# # 
# # all.min = min(c(min(sim_in$diff), min(sim_in$y.pred)))
# # all.max = max(c(max(sim_in$diff), max(sim_in$y.pred)))
# 


diff.vals = seq(-1,1,length.out = 50)
y.pred.vals = seq(-1,1,length.out = 50)

dense = expand.grid(diff = diff.vals,y.pred = y.pred.vals,n = NA)

sim_in2 = sim_in[,c("diff","y.pred"),with = FALSE]

counter = 1

for(i in 1:length(y.pred.vals)){
  
  for(j in 1:length(diff.vals)){

  # temp =
   
   dense$n[counter] = nrow( sim_in2[y.pred >= y.pred.vals[i] & y.pred < y.pred.vals[i+1] &
                                      diff >= diff.vals[j] & diff < diff.vals[j+1]] )
   counter = counter+1
   print(counter/nrow(dense)*100)
  }
}

dense$n2 = dense$n
dense$n2[dense$n2 > 0] = log(dense$n2[dense$n2 > 0])

#dense$n3 = dense$n / 72000000*100
dense$n3 = dense$n / 720000*100
dense$n3[dense$n2 > 0] = log(dense$n3[dense$n2 > 0])


nsen_plot = ggplot(data = dense, aes(x = diff, y = y.pred , fill = n2)) + 
  geom_raster()+
  #coord_equal()+
  xlab(label = "Signature prediction (r)")+
  ylab(label = "Linear model prediction (r)")+
  scale_y_continuous(limits = c(-.8,.8),breaks = seq(-.8,.8,.2))+
  scale_x_continuous(limits = c(-.8,.8),breaks = seq(-.8,.8,.2))+
  
  scale_fill_gradientn(colours = c("#FFFFFF",viridisLite::magma(n = 511)),
                        breaks = log(rev(.2^c(seq(0,10,1))) * 72000000),
                        labels = rev(.2^c(seq(0,10,1))*100),
                      # breaks = rev(log((.2^c(seq(0,10,2))) * 100)),
                      # labels = rev((.2^c(seq(0,10,2))) * 100),
                        guide = guide_colorbar( frame.colour = "black",
                                                # barwidth = 12,
                                                 barheight =12,
                                                draw.ulim = T,
                                                title.position = "top",
                                                ticks.colour = "black"))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_abline(slope = 1,intercept = 0,linetype = 'dashed')+
  theme_classic()+
  theme(aspect.ratio = 1,legend.position = "right",legend.title.position = "top")+
  labs(fill =   "Percent of simulations" )
nsen_plot
ggsave(plot = nsen_plot,filename = paste(base_dir,"sim_plot.jpeg"),dpi = 500,width = 6,height = 5)



dense2 = dense[dense$diff == dense$y.pred,]
dense2$n4 = dense2$n / 72000000*100
sum(dense2$n4[dense2$diff >= 0])

dense3 = dense %>% dplyr::select(c("diff","y.pred","n"))
dense3$n = dense3$n / 72000000*100

dense4 = dense3 %>% tidyr::pivot_wider(id_cols = "diff",names_from = "y.pred",values_from = "n") %>% as.data.frame()
row.names(dense4) = dense4$diff
dense4 = dense4[,-1]
dense4 = as.matrix(dense4)

sum(
diag(dense4) %>% sum(),
dense4[row(dense4) == (col(dense4) - 1)] %>% sum(),
dense4[row(dense4) == (col(dense4) + 1)] %>% sum()
)

# 
# ########################################################
# 
# 
# dense = MASS::kde2d(x =sim_in$diff,y = sim_in$y.pred,n = 100)
# 
# dense_z = as.data.frame(dense$z)
# colnames(dense_z) = dense$x
# dense_z$y = dense$y
# 
# dense2 = dense_z %>% tidyr::pivot_longer(cols = !y ,names_to = "x",values_to = "density")
# dense2$x = as.numeric(dense2$x)
# 
# dense2$density2 = log(dense2$density)
# dense2$density2[dense2$density2 <0] = 0
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# sim_in_sub = sim_in[sample(x = c(1:nrow(sim_in)),size = 10000,replace = F),]
# 
# ggplot(sim_in_sub, aes(x =diff,y = y.pred))+
#   geom_density_2d_filled()


  # stat_density_2d(
  #   geom = "raster",
  #   aes(fill = after_stat((density))),
  #   contour = FALSE
  # ) + 
  # scale_fill_gradientn(colours = viridisLite::magma(n = 512),
  #                     #  breaks = log(2^c(seq(0,24,2))),
  #                      # labels = (2^c(seq(0,24,2))),
  #                       guide = guide_colorbar( frame.colour = "black",
  #                                               # barwidth = 12,
  #                                               # barheight =1.5,
  #                                               draw.ulim = T,
  #                                               title.position = "top",
  #                                               ticks.colour = "black"))
# 
# #######################
# nsen_plot2 = nsen_plot  + 
#   facet_wrap( total.var ~  main.effect.condition,ncol = 6)
# 
# ggsave(plot = nsen_plot2,filename = paste(base_dir,"sim_plot.jpeg"),dpi = 300,width = 20,height = 20)
# 
# 
# 
# nsen_plot3 = nsen_plot  + 
#   facet_wrap( var.exp ~  auc.range,ncol = 4)
# 
# ggsave(plot = nsen_plot3,filename = paste(base_dir,"sim_plot2.jpeg"),dpi = 300,width = 10,height = 10)
# 
# 
# nsen_plot4 = nsen_plot  + 
#   facet_wrap( train_n ~  auc.range,ncol = 4)
# 
# ggsave(plot = nsen_plot4,filename = paste(base_dir,"sim_plot3.jpeg"),dpi = 300,width = 20,height = 20)
# 
# 

# 
# 
# ggplot(sim_medians,aes(x =perf.diff,y = log(train_n) ))+
#   xlab(label = "Difference in performance")+
#   ylab(label = "Signature AUC")+
#   scale_y_continuous(labels =  unique(sim_medians$train_n),
#                      breaks =log(unique(sim_medians$train_n)) )+
#   # ggtitle(label = "",subtitle = "2-back probability")+
#   # scale_color_viridis_c(option = "B")+
#   ggpointdensity::geom_pointdensity(aes( color = after_stat(log(density)) ),
#                                     size=1,adjust=.005,method="default") + 
#   #geom_smooth(method = 'lm',se = F,formula = 'y~x',color="black",lwd = 1.5,linetype="dashed")+
#   
#  # geom_abline(slope = 1,intercept = 0,lwd = 1,linetype="dashed",color="black")+
#   geom_vline(xintercept = 0,lwd = 1,linetype="dashed",color="black")+
#  # geom_hline(yintercept = .5,lwd = 1,linetype="dashed",color="black")+
#   
#   theme_bw()+
#   theme(aspect.ratio = 1,legend.position = "right",legend.title.position = "top",
#         panel.background = element_rect(fill = 'lightgrey'),
#         panel.grid.major = element_line(color = 'black'),
#         panel.grid.minor = element_line(color = 'black'))+
#   labs(color =   "Point density" )+
#   scale_color_gradientn(colours = viridisLite::magma(n = 512),
#                         breaks = log(2^c(seq(0,14,2))),
#                         labels = (2^c(seq(0,14,2))),
#                         guide = guide_colorbar( frame.colour = "black",
#                                                 # barwidth = 12,
#                                                 # barheight =1.5,
#                                                 draw.ulim = T,
#                                                 title.position = "top",
#                                                 ticks.colour = "black"))+ 
#   facet_wrap(  ~  auc.range)
# 
# 
# 
# 
# ##########################################
# 
# var.exp.labs = levels(sim_medians$var.exp) %>% as.character()
# var.exp.labs2 = var.exp.labs
# var.exp.labs = paste("Population R^2 = ",var.exp.labs,sep="")
# names(var.exp.labs) = var.exp.labs2
# 
#  nsen_all= ggplot(data = sim_medians,aes(y =perf.diff,x = as.factor(train_n ),fill = var.exp,color= var.exp))+
#   xlab(label = "Training sample size")+
#   ylab(label = "Signature prediction - Linear model prediction\n positive = signature performs better")+
#   geom_boxplot(outliers = FALSE,alpha=.5) +
#   scale_fill_viridis_d(option = "H")+
#   scale_color_viridis_d(option = "H")+
#   
#   geom_hline(yintercept  = 0,linetype = "dashed")+
#   theme_bw()+
#   theme(legend.position = "top")+
#   labs(fill='Classifier AUC range',color='Classifier AUC range')+
#   facet_wrap(~auc.range, strip.position = "top",ncol = 2,labeller = labeller(var.exp = var.exp.labs)) +
#   theme(strip.background = element_blank(), strip.placement = "outside")
# nsen_all
# ggsave(plot = nsen_all,filename = paste(base_dir,"sim_plot_all.jpeg"),dpi = 500,width = 8,height = 10)
# 

######################################

sim_in = sim_in %>% dplyr::filter(test.r2 < 0.3)


sim_in$perf.diff = sim_in$diff^2 - sim_in$y.pred^2
sim_in$var.exp = gtools::quantcut(sim_in$test.r2 ,q = 9)
sim_in$auc.range = gtools::quantcut( sim_in$auc,q = 4)
sim_in$obs.effect.sim.range = gtools::quantcut( sim_in$obs.effect.sim,q = 4)


auc.range = levels(sim_in$auc.range) %>% as.character()
auc.range2 = auc.range
auc.range.labs = paste("Classifier AUC = ",auc.range,sep="")
names(auc.range.labs) = auc.range

nsen_all= ggplot(data = sim_in,aes(y =perf.diff,x = as.factor(train_n ),fill = var.exp,color= var.exp))+
  xlab(label = "Training sample size")+
  ylab(label = "Signature prediction - Linear model prediction\n positive = signature performs better")+
  geom_boxplot(outliers = FALSE,alpha=.5) +
  scale_fill_viridis_d(option = "C")+
  scale_color_viridis_d(option = "C")+
  
  geom_hline(yintercept  = 0,linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "top")+
  guides(color=guide_legend(nrow=3, byrow=TRUE),fill=guide_legend(nrow=3, byrow=TRUE)) +
  labs(fill=expression(paste('Population ',R^2)),color=expression(paste('Population ',R^2)))+
  facet_wrap(~obs.effect.sim.range + auc.range, strip.position = "top",ncol = 3) +
  
 # facet_wrap(~auc.range, strip.position = "top",ncol = 3,labeller = labeller(auc.range = auc.range.labs)) +
  theme(strip.background = element_blank(), strip.placement = "outside")
nsen_all
ggsave(plot = nsen_all,filename = paste(base_dir,"sim_plot_all2.jpeg"),dpi = 500,width = 10,height = 10)



cor(sim_in$test.adjr2,sim_in$diff^2)
cor(sim_in$test.adjr2,sim_in$y.pred^2)


#########################


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/sims/")

sim_in2 = fread(paste(base_dir,"all.sim.out.complex.CSV",sep=""),header = TRUE)
sim_in2$perf.diff = sim_in2$diff - sim_in2$y.pred

#train_n 
#effect.sim
#auc

sim_in2$var.exp = gtools::quantcut(sim_in2$total.var ,q = 9)


sim_in2$auc.range = gtools::quantcut( sim_in2$auc,q = 6)


auc.range = levels(sim_in2$auc.range) %>% as.character()
auc.range2 = auc.range
auc.range.labs = paste("Classifier AUC = ",auc.range,sep="")
names(auc.range.labs) = auc.range


sim_in2$effect.sim2 = as.factor(sim_in2$effect.sim)
effect.sim.range = levels(sim_in2$effect.sim2) %>% as.character()
effect.sim.range2 = effect.sim.range
effect.sim.range.labs = paste("Map correlation = ",effect.sim.range,sep="")
names(effect.sim.range.labs) = effect.sim.range




nsen_all2= ggplot(data = sim_in2,aes(y =perf.diff,x = as.factor(train_n ),fill = var.exp,color= var.exp))+
  xlab(label = "Training sample size")+
  ylab(label = "Signature prediction - Linear model prediction\n positive = signature performs better")+
  geom_boxplot(outliers = FALSE,alpha=.5) +
  scale_fill_viridis_d(option = "C")+
  scale_color_viridis_d(option = "C")+
  
  geom_hline(yintercept  = 0,linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "top")+
  guides(color=guide_legend(nrow=3, byrow=TRUE),fill=guide_legend(nrow=3, byrow=TRUE)) +
  labs(fill=expression(paste('Population ',R^2)),color=expression(paste('Population ',R^2)))+
  facet_wrap(~obs.effect.sim.range + auc.range, strip.position = "top",ncol = 3) +
  
  facet_wrap(~effect.sim2 + auc.range , strip.position = "top",ncol = 6,
             labeller = labeller(auc.range = auc.range.labs,effect.sim2 =effect.sim.range.labs )) +
  theme(strip.background = element_blank(), strip.placement = "outside")
#nsen_all2
ggsave(plot = nsen_all2,filename = paste(base_dir,"sim_plot_all.complex.jpeg"),dpi = 500,width = 15,height = 15)


#########################


base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/sims/")

sim_in2 = fread(paste(base_dir,"all.sim.out.complex2.CSV",sep=""),header = TRUE)

auc = sim_in2 %>% 
  group_by(train_n ,effect.sim,total.var) %>% 
  summarise(L_CI = quantile(auc,probs = 0.025),
            median = quantile(auc,probs = 0.5),
            U_CI = quantile(auc,probs = 0.975)) %>% 
  as.data.frame()

sim3a = sim_in2 %>% 
  group_by(train_n ,effect.sim,total.var) %>% 
  summarise(L_CI = quantile(diff,probs = 0.025),
            median = quantile(diff,probs = 0.5),
            U_CI = quantile(diff,probs = 0.975)) %>% 
  as.data.frame()

sim3b = sim_in2 %>% 
  group_by(train_n ,effect.sim,total.var) %>% 
  summarise(L_CI = quantile(y.pred,probs = 0.025),
            median = quantile(y.pred,probs = 0.5),
            U_CI = quantile(y.pred,probs = 0.975)) %>% 
  as.data.frame()

sim3a$var = "Signature"
sim3b$var = "ML model"

sim3 = rbind(sim3a,sim3b)
sim3 = as.data.frame(sim3)

sim3$var = as.factor(sim3$var)
sim3$var = factor(sim3$var,levels =levels(sim3$var)[c(2,1)] )


sim3$total.var2 = as.factor(sim3$total.var)
sim3$effect.sim2 = as.factor(sim3$effect.sim)

to_plot = sim3%>% dplyr::filter(total.var == 0.01 | total.var == 0.1,
                                effect.sim == 0|effect.sim == .99| effect.sim == 0.2|effect.sim == 0.4|effect.sim == 0.6|effect.sim == .8|effect.sim == 0.9      )


total.var.range = levels(factor(to_plot$total.var))
total.var.labs = paste("Population R^2 = ",total.var.range,sep="")
names(total.var.labs) = total.var.range

var.range =  levels(factor(to_plot$var))
var.labs = paste("Model = ",var.range,sep="")
names(var.labs) = var.range


p1 = ggplot(data = to_plot,aes(x = log(train_n ),y=median,fill=effect.sim2,color=effect.sim2))+
  scale_color_viridis_d(option = "C",begin = 0,end = 1,direction = 1)+
  scale_fill_viridis_d(option =  "C",begin = 0,end = 1,direction = 1)+
  geom_hline(yintercept = 0,color="black",linetype="dashed")+
  ylab(label = "Correlation (r)")+
  xlab(label = "Training Sample Size")+
  scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
  geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25       )+

  geom_line(size=.5,show.legend = F)+
  geom_point(shape=21,alpha=1,size=2,show.legend = F)+
  labs(fill="Map alignment (r)",color="Map alignment (r)")+
  
  theme_bw()+
  theme(legend.position = "top",
        panel.grid.minor = element_blank())+
  theme(strip.background = element_blank(), strip.placement = "outside")+
  guides(fill=guide_legend(override.aes = c(alpha=1),reverse = F),color=guide_legend(reverse = F))+
  facet_wrap(~total.var + var,ncol = 2,
             labeller = labeller(total.var = total.var.labs,var =var.labs ))


p1
ggsave(plot = p1,filename = paste(base_dir,"sim_plot_all.complex2.jpeg"),dpi = 500,width =6,height = 7)


# 
# ggplot(data = sim3 %>% dplyr::filter(var == "y.pred",total.var == 0.01,
#                                      effect.sim == 0|effect.sim == .99| effect.sim == 0.2|effect.sim == 0.4|effect.sim == 0.6|effect.sim == .8|effect.sim == 0.9      ),
#        aes(x = log(train_n ),
#                                                            y=median,fill=effect.sim2,color=effect.sim2))+
#   scale_color_viridis_d(option = "C",begin = 0,end = 1,direction = 1)+
#   scale_fill_viridis_d(option =  "C",begin = 0,end = 1,direction = 1)+
#    scale_y_continuous(limits = c(-0.15,0.4))+
#   
#   ylab(label = "Correlation (r)")+
#   xlab(label = "Training Sample Size")+
#   scale_x_continuous(labels = exp,breaks = log(2^(1:9)*10))+
#   geom_ribbon(aes(ymin=L_CI, ymax=U_CI),alpha=.1,size=.25       )+
#   geom_line(size=1,show.legend = F)+
#   geom_point(shape=21,alpha=1,size=2,show.legend = F)+
#   theme_bw()+
#   theme(legend.position = "top",
#         panel.grid.minor = element_blank())+
#   guides(fill=guide_legend(override.aes = c(alpha=1),reverse = F),color=guide_legend(reverse = F))
