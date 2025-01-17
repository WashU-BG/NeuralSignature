# find a balanced split of the ABCD data


library(data.table)
library(dplyr)
library(lmtest)
library(sandwich)
library(DescTools)



base_dir = c("C:/Users/dbara/Documents/ABCD/NerualSig/")


qc =fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging/mri_y_qc_incl.csv")
mri.info = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging/mri_y_adm_info.csv")
motion = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging/mri_y_qc_motion.csv")
site.info = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/abcd-general/abcd_y_lt.csv")

demo = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/abcd-general/abcd_p_demo.csv")
demo = demo %>% 
  dplyr::filter(eventname == "baseline_year_1_arm_1") %>% 
  dplyr::select(c("src_subject_id","demo_sex_v2"))
demo$demo_sex_v2[demo$demo_sex_v2 == 3] = 1

site.info = site.info %>% dplyr::filter(eventname == "baseline_year_1_arm_1" | eventname == "2_year_follow_up_y_arm_1")
site.info = site.info %>% dplyr::filter(site_id_l != "site22" )

QC.info = merge(qc,site.info)
QC.info = merge(QC.info,demo,by = "src_subject_id")
##################################


task_all = fread(paste(base_dir,"raw/nback_2bv0b_all.csv",sep=""),data.table = F) 
task_all = task_all %>% dplyr::select(c("src_subject_id","eventname")) %>% unique()

task_all  =merge(task_all,QC.info)

task_all = task_all %>% dplyr::filter(imgincl_t1w_include == 1,imgincl_nback_include == 1)
task_all = task_all %>% dplyr::select(c("src_subject_id","eventname","site_id_l")) %>% unique()



behav.dat = fread(paste(base_dir,"behavioral_data.csv",sep=""),data.table = F)

behav.dat$motion = behav.dat[,grep(x = colnames(behav.dat),pattern = paste("nback_all_meanmotion",sep=""))]


dat = behav.dat %>% dplyr::select(c("src_subject_id","eventname","rel_family_id","sex2","interview_age",
                                    "pds","ed",
                                    "demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h",
                                    "income_1","income_2","income_3","income_4",
                                    "rel_relationship_1","rel_relationship_2","rel_relationship_3",
                                    "Prisma_fit","Prisma","DISCOVERY","Achieva" ,
                                    "tfmri_nb_all_beh_c2b_mrt","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt",
                                    "motion")) %>% unique() %>%  na.omit()
dat$tfmri_nb_all_beh_c2b_mrt = log(dat$tfmri_nb_all_beh_c2b_mrt)
dat$tfmri_nb_all_beh_c0b_mrt = log(dat$tfmri_nb_all_beh_c0b_mrt)

df = dat %>% dplyr::select(c("Prisma_fit","Prisma","DISCOVERY","Achieva"))
dat$scanner =factor(names(df)[max.col(df[])])

df = dat %>% dplyr::select(c("income_1","income_2","income_3","income_4"))
dat$income =factor(names(df)[max.col(df[])])

df = dat %>% dplyr::select(c("demo_race_w","demo_race_b","demo_race_na","demo_race_pi","demo_race_h"))
dat$race =factor(names(df)[max.col(df[])])

dat2 = merge(task_all,dat)

famnum = table(dat2$rel_family_id) %>% as.data.frame()
colnames(famnum) = c("rel_family_id","famsize")
dat2 = merge(dat2,famnum)

nvists = table(dat2$src_subject_id) %>% as.data.frame()
colnames(nvists) = c("src_subject_id","nvists")
dat2 = merge(dat2,nvists)


site.order = DescTools::CombSet(x = unique(dat2$site_id_l),m = 10,repl = FALSE,ord = FALSE)
site.order = site.order %>% t() %>% as.data.frame()
###############
library(foreach)
library(doSNOW)

registerDoSNOW(cl <- makeCluster(20))

pb <- txtProgressBar(max = dim(site.order)[2], style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

#for(split.num in 1: dim(site.order)[2]){ 
perf.out = foreach( split.num = 1:dim(site.order)[2], .combine=rbind,
         .packages=c('lmtest','sandwich','dplyr'), .options.snow = opts) %dopar%{
 # print(split.num)
splits = data.frame(site_id_l =unique(dat2$site_id_l),split=0 )

splits$split[match(site.order[c(1:10),split.num],splits$site_id_l)] = 1

temp = merge(dat2,splits)

#lm.vars = c("interview_age","pds","ed","motion","famsize")
lm.vars = c("motion"
            ,"interview_age","ed","famsize","pds","tfmri_nb_all_beh_c2b_mrt","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt"
            )

for(i in 1:length(lm.vars)){
  temp$temp = temp[,which(colnames(temp) == lm.vars[i])]
  
  m1 = lm(temp ~ split,data=temp)
  m1a = coeftest(x = m1, vcov = vcovCL, cluster = ~ src_subject_id )
  p.out =  m1a[2,4]
  if(i == 1){ps = p.out}else{ps = c(ps,p.out)}
}


lm.vars = c("eventname")

for(i in 1:length(lm.vars)){
  temp$temp = temp[,which(colnames(temp) == lm.vars[i])]
  
  m1 = chisq.test(temp$split,temp$temp)
  p.out =  m1$p.value
  ps = c(ps,p.out)
}



temp2 = temp %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
lm.vars = c("nvists","sex2","income_1","income_2","income_3","income_4","Prisma_fit","Prisma","DISCOVERY","Achieva",
            "demo_race_w","demo_race_b","demo_race_h","demo_race_na","demo_race_pi")

for(i in 1:length(lm.vars)){
  temp2$temp = temp2[,which(colnames(temp2) == lm.vars[i])]
  
  m1 = chisq.test(temp2$split,temp2$temp)
  p.out =  m1$p.value
  ps = c(ps,p.out)
}


perf = cbind(split.num,t(ps)) %>% as.data.frame()
return(perf)
#if(split.num == 1){perf.out = perf}else{perf.out = rbind(perf.out,perf)}

#print(perf.out[which(perf.out$p.min == max(perf.out$p.min)),])

}
stopCluster(cl)
foreach::registerDoSEQ()
# max(perf.out$p.min)
# which(perf.out$p.min == max(perf.out$p.min))
 
colnames(perf.out)[-1] = c("motion"
                           ,"interview_age","ed","famsize","pds","tfmri_nb_all_beh_c2b_mrt",
                           "tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0b_mrt",
                           "eventname",
                           "nvists","sex2","income_1","income_2","income_3","income_4","Prisma_fit","Prisma","DISCOVERY","Achieva",
                           "demo_race_w","demo_race_b","demo_race_h","demo_race_na","demo_race_pi"
                           )

write.csv(x = perf.out,file = "C:/Users/dbara/Documents/ABCD/NerualSig/perf.out_new.csv",quote = F,row.names = F)
write.csv(x = site.order,file = "C:/Users/dbara/Documents/ABCD/NerualSig/site.order_new.csv",quote = F,row.names = F)

perf.out2 = perf.out %>% dplyr::filter( eventname > 0.05,
                                                motion > 0.05,
                                                interview_age > 0.05,
                                                pds>0.05,
                                                ed > 0.05,
                                                sex2 > 0.05,
                                                nvists > 0.05,
                                                famsize > 0.05,
                                                demo_race_w > 0.05,
                                                demo_race_b > 0.05,
                                        tfmri_nb_all_beh_c2b_mrt > 0.05,
                                        tfmri_nb_all_beh_c2b_rate > 0.05, 
                                        tfmri_nb_all_beh_c0b_rate > 0.05,
                                        tfmri_nb_all_beh_c0b_mrt > 0.05,
                                        income_1 > 0.05,income_2>0.05,income_3>0.05,income_4>0.05
                                        
                                                )
# split 110485
match(site.order[c(1:10),perf.out2$split.num[1]],splits$site_id_l)
site.order[c(1:10),perf.out2$split.num[1]]


split.num=1
splits = data.frame(site_id_l =unique(behav.dat$site_id_l),split=0 )
splits$split[match(site.order[c(1:10),perf.out2$split.num[split.num]],splits$site_id_l)] = 1

temp = merge(dat2,splits)


temp %>% dplyr::filter(split == 1) %>% dplyr::select("src_subject_id") %>% 
  unique() %>% 
  dim()
temp %>% dplyr::filter(split == 0) %>% dplyr::select("src_subject_id") %>% 
  unique() %>% 
  dim()


site.list = c("site20","site17", "site16", "site19" ,"site01", "site08", "site15", "site18", "site21", "site03")


s1 = temp %>% dplyr::filter(split == 0)
s2 = temp %>% dplyr::filter(split == 1)

length(unique(s1$site_id_l))
length(unique(s2$site_id_l))

length(unique(s1$src_subject_id))
length(unique(s2$src_subject_id))

length((s1$src_subject_id))
length((s2$src_subject_id))
