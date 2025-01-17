library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(ggpubr)

# Function to make measures unreliable
add.noise = function(x,rel){
  x2 = base::scale(x = (x+stats::rnorm(length(x), 0, sqrt((1-rel)/rel))),
                   center = TRUE, scale = TRUE)
  return(x2)
}
#########################

N=1000000 # total N (for reliability and effect size calculations)
N_plot=20 # N for plotting
r1 = .9 # correlation between perfectly reliable variables
mean1 = 0 # variable means
mean2 = 2
rel1 = .5 # reliability of the two variables
rel2 = .5

#########################
#Simulate some data
set.seed(93)
dat = MASS::mvrnorm(n=N, mu=c(0,0), Sigma=matrix(data = c(1,r1,r1,1),nrow=2,byrow = T), empirical=TRUE) %>% as.data.frame()
dat$ID = c(1:N)

# if the data were perfectly reliable, we would have the exact same observation at each visit
dat1 = dat
dat2 = dat

# make less reliable and adjust variable mean
dat1$V1 = add.noise(dat1$V1,rel = rel1) + mean1
dat1$V2 = add.noise(dat1$V2,rel = rel2) + mean2

dat2$V1 = add.noise(dat2$V1,rel = rel1) + mean1
dat2$V2 = add.noise(dat2$V2,rel = rel2) + mean2

# compute difference score
dat1$diff = dat1$V2 - dat1$V1
dat2$diff = dat2$V2 - dat2$V1

# reliability. yields equivalent results too ICC(3,1)
cor(dat1$V1,dat2$V1)
cor(dat1$V2,dat2$V2)

cor(dat1$diff,dat2$diff)

cor(dat1$V1,dat1$V2)
cor(dat2$V1,dat2$V2)


########################################################
# Reshape for plotting

keep_plot = sample(c(1:N),size = N_plot,replace = F)

dat1p = dat1[keep_plot,]
dat2p = dat2[keep_plot,]

dat1a = dat1p %>% dplyr::select(c("ID","V1","V2")) %>% pivot_longer(cols = starts_with("V"),names_to = "Condition",values_to = "V")
dat1a$visit = "Visit 1"

dat2a = dat2p %>% dplyr::select(c("ID","V1","V2")) %>% pivot_longer(cols = starts_with("V"),names_to = "Condition",values_to = "V")
dat2a$visit = "Visit 2"


dat3a = rbind(dat1a,dat2a)
dat3a$Condition = as.factor(dat3a$Condition)
levels(dat3a$Condition) = c("Control","Manipulation")

##########

dat1b = dat1p %>% dplyr::select(c("ID","diff")) 
dat1b$Visit = "Visit 1"

dat2b = dat2p %>% dplyr::select(c("ID","diff"))
dat2b$Visit = "Visit 2"

dat3b = rbind(dat1b,dat2b)

dat1p$Visit = "Visit 1"
dat2p$Visit = "Visit 2"

dat3p  = rbind(dat1p,dat2p)

colnames(dat1b)[2] = "Visit1"
colnames(dat2b)[2] = "Visit2"

dat3c  = cbind(dat1b[,-3],dat2b[,-3])
dat3c = dat3c[,-3]



colnames(dat1p)[c(1,2)] = c("Visit1","Visit1")
colnames(dat2p)[c(1,2)] = c("Visit2","Visit2")

dat4a = cbind(dat1p[,c(1,3)],dat2p[,c(1,3)])
dat4a$Condition = "Control"
dat4b = cbind(dat1p[,c(2,3)],dat2p[,c(2,3)])
dat4b$Condition = "Manipulation"
dat4c = rbind(dat4a,dat4b)
dat4c = dat4c[,-2]

#ICC(3,1)
# cor(dat1$V1,dat2$V1)
# rptR::rpt(V ~ 1 + (1|ID),grname = "ID",datatype = "Gaussian",data = dat3a %>% dplyr::filter(Condition == "Control"),nboot = 0,npermut = 0)
# cor(dat1$V2,dat2$V2)
# rptR::rpt(V ~ 1 + (1|ID),grname = "ID",datatype = "Gaussian",data = dat3a %>% dplyr::filter(Condition == "Manipulation"),nboot = 0,npermut = 0)
# cor(dat1$diff,dat2$diff)
# rptR::rpt(diff ~ 1 + (1|ID),grname = "ID",datatype = "Gaussian",data = dat3b ,nboot = 0,npermut = 0)



# plot
###############################################
plot1 = ggplot(data = dat3a,aes(x = Condition,y =V,fill =  Condition,color = Condition))+
  scale_color_viridis_d(option = "H",begin = .2,end = .8)+
  scale_fill_viridis_d(option = "H",begin = .2,end = .8)+
  geom_boxplot(color="darkgrey",alpha=.25,show.legend = F)+
  geom_line(inherit.aes = F,aes(x = Condition,y =V, group=ID),size=.75)+
  geom_point(shape=21,position = position_dodge(width = 1),size=2,show.legend = F,color = "grey")+
  facet_wrap(~visit,ncol = 2)+
  geom_hline(yintercept = 0,linetype="dashed")+
  
  ggtitle(label = "Replicable and large\nwithin-subject effects",subtitle = paste("d = ",round(mean(c(mean(dat1$V2 - dat1$V1), mean(dat2$V2 - dat2$V1))),2),sep=""))+
  ylab(label = "Outcome variable")+
  theme_classic()
plot1

plot2 = ggplot(data = dat3b,aes(x = Visit,y =diff,fill =  Visit,color = Visit))+
  scale_color_viridis_d(option = "H",begin = .3,end = .7)+
  scale_fill_viridis_d(option = "H",begin = .3,end = .7)+
  geom_boxplot(color="darkgrey",alpha=.25,show.legend = F)+
  scale_y_continuous(limits = c(-5,5))+
 # geom_line(inherit.aes = F,aes(x = Visit,y =diff, group=ID),size=.75)+
  geom_point(shape=21,position = position_dodge(width = 1),size=2,show.legend = F,color = "grey")+
geom_hline(yintercept = 0,linetype="dashed")+
    ggtitle(label = "Mean difference score is\nreplicably greater than 0"
            ,subtitle = "")+
  ylab(label = "Difference score (Manipulation - Control)")+
  theme_classic()
plot2

dat3c$V1Ranked = rev(rank(dat3c$Visit1,))
dat3c$V2Ranked = rev(rank(dat3c$Visit2))
dat3c$freq = 1
dat3c$rankchange = dat3c$V1Ranked - dat3c$V2Ranked
dat3c_b = dat3c %>% dplyr::select(c("ID","rankchange"))
dat3b = merge(dat3b,dat3c_b)

plot3 = ggplot(data = dat3b,aes(x = Visit,y =diff,fill =  Visit,color = Visit))+
 # scale_color_viridis_d(option = "H",begin = .3,end = .7)+
  scale_fill_viridis_d(option = "H",begin = .3,end = .7)+
  geom_boxplot(color="darkgrey",alpha=.25,show.legend = F)+
  scale_y_continuous(limits = c(-5,5))+
  geom_line(inherit.aes = F,aes(x = Visit,y =diff, group=ID),size=.75)+
  geom_point(shape=21,position = position_dodge(width = 1),size=2,show.legend = F,color = "grey")+
  geom_hline(yintercept = 0,linetype="dashed")+
  ggtitle(label = "Individual's level of activation is\nnot necessarily consistent"
          ,subtitle = paste("ICC = ",round(cor(dat1$diff,dat2$diff),2),sep="")
  )+
  ylab(label = "Difference score (Manipulation - Control)")+
  theme_classic()
plot3

########################################

plot_all = ggarrange(plot1,plot2,plot3,ncol = 3,nrow=1,labels = c("A","B","C"))
plot_all



base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/Final/")

ggsave(plot = plot_all,filename = paste(base_dir,"difference_score_example.jpeg"),dpi = 500,width = 10,height = 5)
