# make abcd data dictionary

library(data.table)
library(dplyr)
library(stringr)

setwd("C:/Users/dbara/Documents/ABCD/ABCD_5_1/core/")

files = list.files(all.files = T,full.names = T,recursive = T)

for(i in 1: length(files)){
print(i)
  file.temp = fread(file = files[i],nrows = 1,header = F,data.table = F)
  file.temp = t(file.temp) %>% as.data.frame()
  
  file.temp$path = files[i]
  if(i ==1){
write.table(x = file.temp,file = "data.dictionary.csv",sep = ",",append = F,quote = F,row.names = F,col.names = F)
  }else{write.table(x = file.temp,file = "data.dictionary.csv",sep = ",",append = T,quote = F,row.names = F,col.names = F)}
}

datadictionary = read.csv(file = "data.dictionary.csv",header = F)
colnames(datadictionary) = c("var_name","file")

datadictionary2 = read.csv(file = "data.dictionary.2.csv",header = T) # from https://data-dict.abcdstudy.org/?
colnames(datadictionary2)[3] = "expanded"
datadictionary2 = datadictionary2[,c(1:3)]

datadictionary = merge(datadictionary,datadictionary2,by = "var_name",all=T)

datadictionary = unique(datadictionary)

# write.table(x = datadictionary,file = "data.dictionary.txt",sep = "\t",append = F,quote = F,row.names = F,col.names = T)
write.xlsx(x = datadictionary,file = "data.dictionary.xlsx" )

#######################################################################################

#old
datadictionary$expanded=NA

oldfilenames = list.files(path = "C:/Users/dbara/Documents/Backups/FinalHPBackup/Documents/BRAINlab/ABCD/Package_1195434/abcd-4.0-data-dictionaries")
#oldfilenames = str_replace(string = oldfilenames,pattern = ".csv",replacement = ".txt")


for(i in 1: length(oldfilenames)){
  print(i)
  temp.header =  fread(file = paste("C:/Users/dbara/Documents/Backups/FinalHPBackup/Documents/BRAINlab/ABCD/Package_1195434/abcd-4.0-data-dictionaries/",oldfilenames[i],sep=""),header = T,data.table = F)
  # 
  # if(dim(temp.header)[1] == 0){
  #   oldfilenames[i] = str_replace(string = oldfilenames[i],pattern = ".csv",replacement = ".txt")
  #   oldfilenames[i] = str_replace(string = oldfilenames[i],pattern = "02",replacement = "0201")
  #   
  #   temp.header =  fread(file = paste("C:/Users/David/Documents/BRAINlab/ABCD/Package_1195434/",oldfilenames[i],sep=""),nrows = 2,header = F,data.table = F)
  #   temp.header = t(temp.header) %>% as.data.frame()
  #   colnames(temp.header)=c("ElementName","ElementDescription")
  # 
  #   
  #   }
  
  
  datadictionary$expanded[match(temp.header$ElementName,datadictionary$var) %>% na.omit()]  <-  temp.header$ElementDescription[!is.na(match(temp.header$ElementName,datadictionary$var))]

  
  }

oldfilenames = list.files(path = "C:/Users/David/Documents/BRAINlab/ABCD/Package_1195434/orig4.0")


for(i in 1: length(oldfilenames)){
  print(i)

    temp.header =  fread(file = paste("C:/Users/David/Documents/BRAINlab/ABCD/Package_1195434/orig4.0/",oldfilenames[i],sep=""),nrows = 2,header = F,data.table = F)
    temp.header = t(temp.header) %>% as.data.frame()
    colnames(temp.header)=c("ElementName","ElementDescription")

  
  
  datadictionary$expanded[match(temp.header$ElementName,datadictionary$var) %>% na.omit()]  <-  temp.header$ElementDescription[!is.na(match(temp.header$ElementName,datadictionary$var))]
  
  
}




library(openxlsx )
write.xlsx(x = datadictionary, file ="data.dictionary.xlsx" , sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
