# clean & merge ABCD data v 5.0
# David Baranger - 7/7/2023

library(data.table)
library(dplyr)
library(foreach)
library(stringr)

setwd("C:/Users/dbara/Documents/ABCD/ABCD_5_1/core")


winsorize = function(x,q=3){
  
  mean_x = mean(x,na.rm = T)
  sd_x = sd(x,na.rm = T)
  top_q = mean_x + q*sd_x
  bottom_q = mean_x - q*sd_x
  
  x[x>top_q] = top_q
  x[x<bottom_q] = bottom_q
  
  return(x)
  
}

any_non_zero = function(dat){
  val_out = apply(dat,
                  MARGIN = 1,function(X){
                    (sum(X,na.rm = T)>0)*1
                  })
  return(val_out)
}



#########################################

file.name = c("./physical-health/ph_p_dhx.csv")
file_in = fread(input =file.name,data.table = F,header = T,na.strings = "",#sep="\t",
                stringsAsFactors = F)
file_in = file_in %>% dplyr::filter(eventname == "baseline_year_1_arm_1")

file_in[file_in == 999] = NA
file_in[file_in == 777] = NA

file_in[file_in == ""] = NA

baseline_data = file_in

########################################

file.name = c("./abcd-general/abcd_p_demo.csv")
file_in = fread(input =file.name,data.table = F,header = T,na.strings = "",#sep="\t",
                stringsAsFactors = F)
file_in[file_in == 999] = NA
file_in[file_in == 777] = NA

file_in[file_in == ""] = NA


long.dat = file_in[,c(1,2,grep(x = colnames(file_in),pattern = "_ed_|_income_"))]
file_in = file_in %>% dplyr::filter(eventname == "baseline_year_1_arm_1")

baseline_data = merge(baseline_data,file_in)
long_data = long.dat

######################

file.name = "./mental-health/mh_p_fhx.csv"
file_in = fread(input =file.name,data.table = F,header = T,na.strings = "",#sep="\t",
                stringsAsFactors = F)
file_in[file_in == 999] = NA
file_in[file_in == 777] = NA

file_in[file_in == ""] = NA



prob.names = colnames(file_in)[grep(x = colnames(file_in),pattern = "famhx_ss_fath|famhx_ss_moth|famhx_ss_fulsib")] %>% as.data.frame()
prob.names = str_split(string = prob.names$.,pattern =  "_",simplify = T) %>% as.data.frame()
prob.names = unique(prob.names$V5)

probs = file_in[,grep(x = colnames(file_in),pattern = "famhx_ss_fath|famhx_ss_moth|famhx_ss_fulsib")]

prob.out = matrix(data = NA,ncol = length(prob.names),nrow = dim(probs)[1]) %>% as.data.frame()

colnames(prob.out) = paste("famhx_ss_firstdeg_",prob.names,"_p",sep="")

for(i in 1:length(prob.names)){
  
  any_hist = probs[,grep(colnames(probs),pattern = prob.names[i])]
  
  prob.out[,i] = apply(any_hist,1,function(X){
    
    (sum(na.omit(X)) > 1)*1
    
  })
  
}

prob.out$src_subject_id = file_in$src_subject_id

baseline_data = merge(baseline_data,prob.out,all=T)


##############################

file.name = "./abcd-general/abcd_y_lt.csv"
file_in = fread(input =file.name,data.table = F,header = T,na.strings = "",#sep="\t",
                stringsAsFactors = F)
file_in[file_in == 999] = NA
file_in[file_in == 777] = NA

file_in[file_in == ""] = NA

file_in$site_id_l[file_in$site_id_l == "site22"] = "site21"

long.dat = file_in[,c(1,2,3,grep(x = colnames(file_in),pattern = "age|type"))]

file_in = file_in %>% dplyr::filter(eventname == "baseline_year_1_arm_1")
file_in = file_in[,c(1:5)]

long_data = merge(long_data,long.dat,all=T)
baseline_data = merge(file_in,baseline_data,all=T)


##################

file.name = "./genetics/gen_y_pihat.csv"
file_in = fread(input =file.name,data.table = F,header = T,na.strings = "",#sep="\t",
                stringsAsFactors = F)
file_in[file_in == 999] = NA
file_in[file_in == 777] = NA

file_in[file_in == ""] = NA

baseline_data = merge(file_in,baseline_data,all=T)


################################################

merge_in_long=function(path){
  
  file.name = path
  file_in = fread(input =file.name,data.table = F,header = T,na.strings = "",#sep="\t",
                  stringsAsFactors = F)
  file_in[file_in == 999] = NA
  file_in[file_in == 777] = NA
  
  #file_in[file_in == ""] = NA
  
  long_data = merge(long_data,file_in,all=T)
  return(long_data)
}

long_data = merge_in_long(path = "./physical-health/ph_p_pds.csv")
long_data = merge_in_long(path = "./physical-health/ph_y_pds.csv")


long_data = merge_in_long(path = "./mental-health/mh_p_cbcl.csv")
long_data = merge_in_long(path = "./neurocognition/nc_y_nihtb.csv")
long_data = merge_in_long(path = "./neurocognition/nc_y_ravlt.csv")
long_data = merge_in_long(path = "./mental-health/mh_y_pps.csv")

long_data = merge_in_long(path = "./mental-health/mh_y_upps.csv")
long_data = merge_in_long(path = "./mental-health/mh_y_bisbas.csv")
long_data = merge_in_long(path = "./mental-health/mh_y_poa.csv")
long_data = merge_in_long(path = "./neurocognition/nc_y_cct.csv")
long_data = merge_in_long(path = "./neurocognition/nc_y_lmt.csv")
long_data = merge_in_long(path = "./neurocognition/nc_y_wisc.csv")


long_data = merge_in_long(path = "./imaging/mri_y_adm_info.csv")
long_data = merge_in_long(path = "./imaging/mri_y_qc_incl.csv")
long_data = merge_in_long(path = "./imaging/mri_y_qc_motion.csv")
long_data = merge_in_long(path = "./imaging/mri_y_tfmr_nback_beh.csv")

############################################################################


famhx = baseline_data[,grep(colnames(baseline_data),pattern = "famhx_ss_firstdeg")]
baseline_data$famhx_ss_firstdeg_alc_dg_p = any_non_zero(famhx[,c(1,2)])
baseline_data$famhx_ss_firstdeg_mh_p = any_non_zero(famhx[,-c(1,2)])


baseline_data$sex2 = baseline_data$demo_sex_v2
#baseline_data$sex2[baseline_data$demo_sex_v2 == "F"] = 2
baseline_data$sex2[baseline_data$sex2 == 3] = NA

baseline_data$demo_race_w = baseline_data$demo_race_a_p___10
baseline_data$demo_race_b = baseline_data$demo_race_a_p___11
baseline_data$demo_race_na = any_non_zero(cbind(baseline_data$demo_race_a_p___12, baseline_data$demo_race_a_p___13))
baseline_data$demo_race_pi = any_non_zero(cbind(baseline_data$demo_race_a_p___14 , baseline_data$demo_race_a_p___15 , 
                                                baseline_data$demo_race_a_p___16 , baseline_data$demo_race_a_p___17) )
baseline_data$demo_race_a = any_non_zero(cbind(baseline_data$demo_race_a_p___18 , baseline_data$demo_race_a_p___19 , 
                                               baseline_data$demo_race_a_p___20 , baseline_data$demo_race_a_p___21,
                                               baseline_data$demo_race_a_p___22 , baseline_data$demo_race_a_p___23,
                                               baseline_data$demo_race_a_p___24) )
baseline_data$demo_ethn_v2[baseline_data$demo_ethn_v2 == 777] = NA
baseline_data$demo_race_h = baseline_data$demo_ethn_v2
baseline_data$demo_race_h[baseline_data$demo_race_h == 2] = 0

baseline_data$demo_race_o = baseline_data$demo_race_a_p___25



baseline_data$dgs_before = any_non_zero(cbind(
  baseline_data$devhx_8_coc_crack,
  
  baseline_data$devhx_8_her_morph,
  baseline_data$devhx_8_oxycont))

baseline_data$dgs_after = 
  any_non_zero(cbind(
    baseline_data$devhx_9_coc_crack,
    
    baseline_data$devhx_9_her_morph,
    baseline_data$devhx_9_oxycont))

baseline_data$any_mj = any_non_zero(cbind(baseline_data$devhx_8_marijuana, 
                                          baseline_data$devhx_9_marijuana))



baseline_data$devhx_10[baseline_data$devhx_10 == -1] = NA

baseline_data$birth_weight_oz[is.na(baseline_data$birth_weight_oz)] = 0
baseline_data$total_birth_weight = (baseline_data$birth_weight_lbs * 16) + baseline_data$birth_weight_oz
baseline_data$total_birth_weight = winsorize(baseline_data$total_birth_weight)
baseline_data$mat_age_birth = winsorize(baseline_data$devhx_3_p)
baseline_data$mat_age_knew_preg = winsorize(log(baseline_data$devhx_7_p+1))
# 
# baseline_data$try_dg = any_non_zero(cbind(baseline_data$tlfb_alc_sip, baseline_data$tlfb_tob_puff,
#                                           baseline_data$tlfb_alc_use  ))



baseline_data$mj_pre_post = baseline_data$devhx_9_marijuana
baseline_data$mj_pre      = baseline_data$devhx_8_marijuana
baseline_data$mj_pre[baseline_data$mj_pre_post == 1] = 0



#dat = na.omit(dat)

###################################################
# Long covariates


baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 777] = NA
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 13] = 12
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 14] = 12
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 22] = 14
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 23] = 14
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 17] = 14
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 15] = 14
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 16] = 14
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 18] = 16
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 19] = 18
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2 == 21] = 20
baseline_data$demo_prnt_ed_v2[baseline_data$demo_prnt_ed_v2  <  9] = 8


baseline_data$demo_comb_income_v2[baseline_data$demo_comb_income_v2 == 777] = NA
baseline_data$demo_comb_income_v2[baseline_data$demo_comb_income_v2 <=  6] = 1
baseline_data$demo_comb_income_v2[baseline_data$demo_comb_income_v2 == 7] = 2
baseline_data$demo_comb_income_v2[baseline_data$demo_comb_income_v2 == 8] = 3
baseline_data$demo_comb_income_v2[baseline_data$demo_comb_income_v2 == 9] = 4
baseline_data$demo_comb_income_v2[baseline_data$demo_comb_income_v2 == 10] = 5



long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 777] = NA
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 13] = 12
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 14] = 12
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 22] = 14
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 23] = 14
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 17] = 14
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 15] = 14
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 16] = 14
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 18] = 16
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 19] = 18
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l == 21] = 20
long_data$demo_prnt_ed_v2_2yr_l[long_data$demo_prnt_ed_v2_2yr_l  <  9] = 8

long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 777] = NA
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 13] = 12
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 14] = 12
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 22] = 14
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 23] = 14
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 17] = 14
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 15] = 14
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 16] = 14
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 18] = 16
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 19] = 18
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l == 21] = 20
long_data$demo_prnt_ed_v2_l[long_data$demo_prnt_ed_v2_l  <  9] = 8



long_data$demo_comb_income_v2_l[long_data$demo_comb_income_v2_l == 777] = NA
long_data$demo_comb_income_v2_l[long_data$demo_comb_income_v2_l <=  6] = 1
long_data$demo_comb_income_v2_l[long_data$demo_comb_income_v2_l == 7] = 2
long_data$demo_comb_income_v2_l[long_data$demo_comb_income_v2_l == 8] = 3
long_data$demo_comb_income_v2_l[long_data$demo_comb_income_v2_l == 9] = 4
long_data$demo_comb_income_v2_l[long_data$demo_comb_income_v2_l == 10] = 5




var_keep = c("src_subject_id","rel_family_id",#,
             "site_id_l",
             "rel_group_id",
             "rel_ingroup_order","rel_relationship",
             "devhx_9_marijuana","devhx_8_marijuana",#"mj_total_base",
             "famhx_ss_firstdeg_alc_dg_p","famhx_ss_firstdeg_mh_p","sex2","demo_race_w","demo_race_b","demo_race_na",
             "demo_race_pi","demo_race_h","demo_race_o","demo_race_a","dgs_before","dgs_after","devhx_10","devhx_6_p","total_birth_weight",
             "famhx_ss_firstdeg_alc_p","famhx_ss_firstdeg_dg_p", "famhx_ss_firstdeg_dprs_p","famhx_ss_firstdeg_ma_p", 
             "famhx_ss_firstdeg_vs_p", "famhx_ss_firstdeg_trb_p","famhx_ss_firstdeg_nrv_p" ,
             "mat_age_birth","mat_age_knew_preg","devhx_8_alcohol","devhx_9_alcohol","devhx_8_tobacco","devhx_9_tobacco")


dat = baseline_data %>% dplyr::select(all_of(var_keep))





long_data = long_data %>% dplyr::filter(  eventname == "baseline_year_1_arm_1" | 
                                            eventname == "2_year_follow_up_y_arm_1" )

long_temp = long_data %>% dplyr::select(#c(2:6),
  "src_subject_id"  ,    "eventname"  ,
  "interview_age",
  "demo_prnt_ed_v2_2yr_l","demo_prnt_ed_v2_l",
  "demo_comb_income_v2_l",
  #                                               "tlfb_cal_scr_num_events",
  "pds_p_ss_male_category","pds_p_ss_female_category",
  "pds_y_ss_male_category","pds_y_ss_female_category")


base_temp = baseline_data %>% dplyr::select(c(1),"demo_prnt_ed_v2","demo_comb_income_v2")


all_dat = merge(long_temp,base_temp,all=T)

all_dat$income = coalesce(all_dat$demo_comb_income_v2,all_dat$demo_comb_income_v2_l)
all_dat$ed = coalesce(all_dat$demo_prnt_ed_v2,all_dat$demo_prnt_ed_v2_l,all_dat$demo_prnt_ed_v2_2yr_l)
all_dat$pds = coalesce(all_dat$pds_p_ss_male_category,all_dat$pds_p_ss_female_category,
                       all_dat$pds_y_ss_male_category,all_dat$pds_y_ss_female_category)

#all_dat = all_dat[apply(all_dat[,c( (dim(all_dat)[2]-2): dim(all_dat)[2])],1,function(X){sum(is.na(X)) < 3}),]



all_dat = all_dat[order(all_dat$interview_age,decreasing = F),]

all_dat = all_dat %>% dplyr::select(c("src_subject_id","eventname","interview_age","income","ed","pds"))

all_dat = all_dat[!is.na(all_dat$interview_age),]


IDs = unique(all_dat$src_subject_id)

out = matrix(NA,nrow = dim(all_dat)[1],ncol = dim(all_dat)[2]) %>% as.data.frame()


#########################################

r = 1
for(i in 1:length(IDs)){
  #if(r > dim(out)[1]){r = 1}
  temp = all_dat %>% dplyr::filter(src_subject_id == IDs[i])
  
  if(dim(temp)[1] > 1){
    temp = zoo::na.locf.default(object = temp,na.rm = F) #locf
    temp = temp[c(dim(temp)[1]:1),]
    temp = zoo::na.locf.default(object = temp,na.rm = F) #locb
    temp = temp[c(dim(temp)[1]:1),]
  }
  
  
  for(z in 1: dim(temp)[1]){
    out[r,] = temp[z,]
    r=r+1
    #if(r == dim(out)[1]){stop()}
  }
}

colnames(out) = colnames(all_dat)

out[,c(3:6)] = apply(out[,c(3:6)] ,MARGIN = 2,as.numeric)

###################################################################
sui = fread("C:/Users/dbara/Documents/ABCD/NerualSig/SUI_counts_240208.csv",header = T,data.table = F)

##################################################################

nback_perf = c("tfmri_nb_all_beh_c0b_mrt","tfmri_nb_all_beh_c2b_mrt",
               "tfmri_nb_all_beh_2b_nt","tfmri_nb_all_beh_c2b_nt",
               "tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c2b_rate")





long2 = long_data %>% dplyr::select(c("src_subject_id","eventname","visit_type"),
                                    
                                    "mri_info_softwareversion",
                                    grep(colnames(long_data),pattern = "^tfmri_nback.+all.+motion|^tfmri_nback.+all.+rot|^tfmri_nback.+all.+trans$"),
                                    grep(colnames(long_data),pattern = "^cbcl.+_r$"),
                                    grep(colnames(long_data),pattern = "pps_y_ss_number"),
                                    grep(colnames(long_data),pattern = "^bis.+ss"),
                                    grep(colnames(long_data),pattern = "^upps.+ss"),
                                    grep(colnames(long_data),pattern = "cash_choice_task"),
                                    grep(colnames(long_data),pattern = "pea_ravlt_sd_trial_i_tc"),
                                    grep(colnames(long_data),pattern = "pea_ravlt_ld_trial_vii_tc"),
                                    grep(colnames(long_data),pattern = "pea_wiscv_tss"),
                                    grep(colnames(long_data),pattern = "lmt_scr_perc_correct"),
                                    grep(colnames(long_data),pattern = "mri_info_manufacturersmn"),
                                    grep(colnames(long_data),pattern = "^nihtbx.+_agecorrected"),
                                    grep(colnames(long_data),pattern = paste(nback_perf,sep = "",collapse = "|"))
                                    
)


long2 = long2[,-c(grep(colnames(long2),pattern = "_nt$"),
                  grep(colnames(long2),pattern = "_nm"),
                  grep(colnames(long2),pattern = "basm"),
                  grep(colnames(long2),pattern = "bism"))]




duplicates = long2 %>% t() %>% duplicated()
# rsfmri data have duplicates - it's the full square matrix. 

long2 = long2[,!duplicates]
dim(long2)[2]

#long2 = merge(long2,QC.info,all=T)
long2 = merge(out,long2,all=T)

cbcl_long = merge(dat,long2,by = "src_subject_id",all=T)

cbcl_long = cbcl_long %>% dplyr::filter(  eventname == "baseline_year_1_arm_1" | 
                                            # eventname == "1_year_follow_up_y_arm_1" #| 
                                            eventname == "2_year_follow_up_y_arm_1" 
)




########################################################
cbcl_long = merge(cbcl_long,sui,all=T)

#cbcl_long = cbcl_long %>% dplyr::filter(sui ==0)

########################################################


cbcl_long$demo_race_h[is.na(cbcl_long$demo_race_h)] = min(cbcl_long$demo_race_h,na.rm = T)


cbcl_long$rel_family_id[is.na(cbcl_long$rel_family_id)]=0

cbcl_long$income_1 = (cbcl_long$income ==1)*1
cbcl_long$income_2 = (cbcl_long$income ==2)*1
cbcl_long$income_3 = (cbcl_long$income ==3)*1
cbcl_long$income_4 = (cbcl_long$income ==4)*1
cbcl_long$income_5 = (cbcl_long$income ==5)*1


################

cbcl_long$Prisma_fit   <- ifelse(cbcl_long$mri_info_manufacturersmn  == 'Prisma_fit', 1, 0)
cbcl_long$Prisma <- ifelse(cbcl_long$mri_info_manufacturersmn  == 'Prisma', 1, 0)
cbcl_long$DISCOVERY <- ifelse(cbcl_long$mri_info_manufacturersmn  == 'DISCOVERY MR750', 1, 0)
cbcl_long$Achieva  <- ifelse(cbcl_long$mri_info_manufacturersmn  == 'Achieva dStream', 1, 0)

#table(cbcl_long$visit_type)

################################

cbcl_long$rel_relationship_1 = ifelse(test = cbcl_long$rel_relationship == 1,yes = 1,no = 0)
cbcl_long$rel_relationship_2 = ifelse(test = cbcl_long$rel_relationship == 2,yes = 1,no = 0)
cbcl_long$rel_relationship_3 = ifelse(test = cbcl_long$rel_relationship == 3,yes = 1,no = 0)


#####################################################

write.csv(x = cbcl_long,file = "C:/Users/dbara/Documents/ABCD/NerualSig/behavioral_data.csv",quote = F,row.names = F)
