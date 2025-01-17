# clean & merge ABCD taskfMRI data
# David Baranger - 1/7/2023

library(data.table)
library(dplyr)
library(openxlsx)
library(foreach)
library(stringr)

base_dir = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/imaging"
# 
# 
# datadictionary3 = fread(file = "C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/data.dictionary.txt",
#                       header = T,data.table = F,fill=TRUE,blank.lines.skip = TRUE)
datadictionary=openxlsx::read.xlsx(xlsxFile ="C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/data.dictionary.xlsx" )
####

readin_fmri=function(path,base,run,task,con,sub=FALSE){

  file_in = fread(input =paste(base_dir,"/",path,".csv",sep=""),data.table = F,header = T,
                  na.strings = "",#sep="\t",
                  stringsAsFactors = F)
  
  colnames(file_in)[-c(1,2)] = datadictionary$expanded[match(colnames(file_in)[-c(1,2)],datadictionary$var)]
  
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "Mean beta weight for MID ",replacement = "Beta weight for MID all ")
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "Mean beta weight for nBack ",replacement = "Beta weight for nBack all ")
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "Mean beta weight for SST ",replacement = "Beta weight for SST all ")
  
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "Run",replacement = "run")
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "run 1",replacement = "run1")
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "run 2",replacement = "run2")
  
  
   colnames(file_in) = str_replace(string = colnames(file_in),pattern = "left hemisphere cortical Destrieux ROI ",
                                                            replacement = "cortical Destrieux ROI left hemisphere ")
  colnames(file_in) = str_replace(string = colnames(file_in),pattern = "right hemisphere cortical Destrieux ROI ",
                                  replacement = "cortical Destrieux ROI right hemisphere ")
  
  file_in =  file_in %>% dplyr::select(c("src_subject_id","eventname"),contains(run))
  
  if(run == " all "){
    file_in =  file_in %>% dplyr::select(-contains(" run "))
  }
  
  file_in =  file_in %>% dplyr::select(c("src_subject_id","eventname"),contains("Beta weight"))
  
  if(dim(file_in)[2] == 2){
    file_in = fread(input =paste(base_dir,"/",path,".csv",sep=""),data.table = F,header = T,
                    na.strings = "",#sep="\t",
                    stringsAsFactors = F)
    
    colnames(file_in)[-c(1,2)] = datadictionary$expanded[match(colnames(file_in)[-c(1,2)],datadictionary$var)]
    file_in =  file_in %>% dplyr::select(-contains(" run "))
    file_in =  file_in %>% dplyr::select(c("src_subject_id","eventname"),contains("Beta weight"))
  }
  
  colnames(file_in)[-c(1,2)] = substring(text =   colnames(file_in)[-c(1,2)],
                              first = str_locate(  colnames(file_in)[-c(1,2)],pattern = "ROI")[,2]+2,
                              last = str_length(  colnames(file_in)[-c(1,2)]))

  if(sub == TRUE){
    file_in = file_in[,-grep("ventricle",colnames(file_in))]
    file_in = file_in[,-grep("inf-lat-vent",colnames(file_in))]
    file_in = file_in[,-grep("white",colnames(file_in))]
    file_in = file_in[,-grep("csf",colnames(file_in))]
    }
  
  
  file_in$task = task
  file_in$con = con
  
  return(file_in)
}

####

combine_brain=function(task,con,run){
  
subcortex = readin_fmri(base = base_dir,path = paste("mri_y_tfmr_",task,"_",con,"_aseg",sep=""),run =run,task = task,con = con,sub=TRUE)
cortex = readin_fmri(base = base_dir,path = paste("mri_y_tfmr_",task,"_",con,"_dst",sep=""),run = run,task = task,con = con)

if(dim(subcortex)[2] != 23){stop(paste("wrong subcortex dimensions",task,con,run))}
if(dim(cortex)[2] != 152){stop(paste("wrong cortex dimensions",task,con,run))}

all = merge(subcortex,cortex)
write.csv(x = all,file = paste("C:/Users/dbara/Documents/ABCD/NerualSig/raw/",task,"_",con,"_",str_trim(run),".csv",sep=""),row.names = F)
}

####

runs = c(" all ","run1","run2")

for(i in 1:length(runs)){

combine_brain(task = "mid",con = "allvn",run = runs[i])
combine_brain(task = "mid",con = "alrvn",run = runs[i])
combine_brain(task = "mid",con = "alvn",run = runs[i])
combine_brain(task = "mid",con = "arvn",run = runs[i])
combine_brain(task = "mid",con = "aslvn",run = runs[i])
combine_brain(task = "mid",con = "asrvn",run = runs[i])
combine_brain(task = "mid",con = "alvsl",run = runs[i])
combine_brain(task = "mid",con = "alvsr",run = runs[i])
combine_brain(task = "mid",con = "lpvnf",run = runs[i])
combine_brain(task = "mid",con = "rpvnf",run = runs[i])

combine_brain(task = "nback",con = "0b",run = runs[i])
combine_brain(task = "nback",con = "2b",run = runs[i])
combine_brain(task = "nback",con = "2bv0b",run = runs[i])
combine_brain(task = "nback",con = "emo",run = runs[i])
combine_brain(task = "nback",con = "plc",run = runs[i])
combine_brain(task = "nback",con = "emovntf",run = runs[i])
combine_brain(task = "nback",con = "fvplc",run = runs[i])
combine_brain(task = "nback",con = "ngfvntf",run = runs[i])
combine_brain(task = "nback",con = "psfvntf",run = runs[i])

combine_brain(task = "sst",con = "asvcg",run = runs[i])
combine_brain(task = "sst",con = "cgvfx",run = runs[i])
combine_brain(task = "sst",con = "csvcg",run = runs[i])
combine_brain(task = "sst",con = "csvis",run = runs[i])
combine_brain(task = "sst",con = "igvcg",run = runs[i])
combine_brain(task = "sst",con = "igvis",run = runs[i])
combine_brain(task = "sst",con = "isvcg",run = runs[i])

}

