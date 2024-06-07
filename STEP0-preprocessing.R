rm(list = ls())

# options(repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/") # using the fastest mirror
# install.packages("pacman") if not installed
if (!requireNamespace('pacman', quietly = TRUE)) {
      install.packages('pacman')
}
# using pacman for loading packages
pacman::p_load(tidyverse, here, psych, CTT, car, lavaan, semPlot, semTools) # use here for directory management

# setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analyses/3_2_1_Standardized_Data")
data_dir <- here::here("3_2_1_Standardized_Data")

#############################
######CHN MLOC DATA##########
######pre processing#########
#############################

MLOC_data <- read.csv(here::here(data_dir,"MLOC0804.csv"))

#colnames(MLOC_data)
#take the original variables names, "Q139" - mails
MLOC_data_names <- c("Q1.ID", "Q2.name" , "Q139","MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4" )

#take the MLOC data only
MLOC <- MLOC_data[,MLOC_data_names]

#record MLOC data with the scores number
MLOCrecode <- function(v){
  v[which(v=="很不同意")] <- -3
  v[which(v=="不同意")] <- -2
  v[which(v=="不太同意")] <- -1
  v[which(v=="基本同意")] <- 1
  v[which(v=="同意")] <- 2
  v[which(v=="很同意")] <- 3
  return(v)
}

MLOC_recode <- t(apply(MLOC, 1, MLOCrecode))

#function to detect if there is any omission or all responses have the same value in MLOC data
cleaning_OmissionSames_MLOC <- function(m){ #input with a dataset matrixs
  MLOCnames <- c("MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")
  mMLOC <- m[,MLOCnames] #take the only MLOC response values with the original MLOC variable names
  num_MLOC <- apply(mMLOC, 2, as.numeric) 
  cleaning01 <-function(v){   
    if (length(v!=length(MLOCnames))) res <- 0  #if the length were not equal to the length of the MLOC variable, marked with 0, which means that at least there is an omission somewhere
    if (anyNA(v)==T) res <- 0 #detect cases with NA, with the "numeric"
    else {fretable <- table(v) #detect cases with the same answers using the frequency table, if the length of the table is equal to 1, it means that there was only one choice that was repeated, also marked with 0, the left cases are marked with 1
    frenvm <- length(fretable)
    if (frenvm<=1) res <- 0
    else res <- 1}
    return(res)
  }
  stayornot <- apply(num_MLOC, 1, cleaning01) #apply the cleaning mark function above to the MLOC data matrix
  staymarked <- cbind(m,stayornot) #combine the marked 0 or 1 vector with the input matrix
  cleaned <- staymarked[which(stayornot==1),1:(length(m[1,]))] #select the valid data with the data marked with a 1
  return(cleaned)
}

#apply the function above to the MLOC data 
cleaned_MLOC <- cleaning_OmissionSames_MLOC(MLOC_recode) #178 participants here


#function to locate duplicated data, in this case duplicated ID number
find_dup_loc <- function(v){
  dup <- v[which(duplicated(v))] #due to the original "duplicated" function, only the second repeated value will be returned
  find_loc <- function(valor){ #function here to located the two times of duplicated value
    valor_loc <- which(v==valor) 
    return(valor_loc)
  }
  res <- mapply(find_loc,dup) #use mapply to find to the location of all the duplicate values that have been detected.
  return(res)
}

(find_dup_loc(cleaned_MLOC[,1])) #return with a matrix, with columns names of ID number, and each columns with two locations of the duplicated ID number 

cleaned_MLOC[unlist((find_dup_loc(cleaned_MLOC[,1]))),] #review all responses with the replicated ID numbers

#delete participants with invalid ID, and repeat responses
#6313 - two different valid participants, only the same ID was signed
fin_MLOC <- cleaned_MLOC[-c(1,74,83,95,161,176),]
#length(fin_MLOC[,1])

#recode 6313, 将hmz1969 被试编号改为 2313
which(fin_MLOC[,"Q1.ID"]=="6313")
fin_MLOC[which(fin_MLOC[,"Q1.ID"]=="6313")[2],"Q1.ID"] <- "2313"




#read MLOC retest data 
qdata <- read.csv(here::here("Q0804.csv"))
qdata_checked <- qdata[which(qdata[,"check.1"]=="不合理"& qdata[,"check.2"]=="6"),] #select valid data with two checks correct

#colnames(qdata_checked)
names_ori_used <- c("Q1.ID","Q139","times","Q3.gender","Q4.age","Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.","Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5","Q140_1","Q140_2","Q140_3","Q140_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

qdata_selected <- qdata_checked[,names_ori_used] #selected data with variable's names above

find_dup_loc(qdata_selected[,"Q1.ID"]) #which in this case, not only duplicated, but also tripled
q_dup_ID <- unique(find_dup_loc(qdata_selected[,"Q1.ID"])) #caused the output of tripled data were repeated

#identify and filter each repeat case
#6313 as above, two different valid participants, only the same ID had been signed, keep the same as the MLOC data
#编号重复，不是同一人 loc： 113 183, 与MLOC data保持一致
qdata_selected[183,]
qdata_selected[183,"Q1.ID"] <- 2313


#loc:38,116,133, with ID number equal to NA, and the loc 38, there was no ID number either email, will be removed directly
#被试编号为空，且loc38，无被试编号与邮箱-将被删除
qdata_selected[q_dup_ID[[7]][1],]
qdata_selected[38,]

#according to the mail, reidentify the participants' ID number
#根据邮箱信息查表，得被试编号
qdata_selected[q_dup_ID[[7]][2],"Q1.ID"] <- 605
qdata_selected[q_dup_ID[[7]][3],"Q1.ID"] <- 667

qdata_selected_IDnoNA <- qdata_selected[-38,]

#re-locate the repeat ID, the location changed with the modification above
q_dup_ID_final <- unique(find_dup_loc(qdata_selected_IDnoNA[,"Q1.ID"]))

#loc:144 147 148, one participant had responded three times #同一被试填写三次
qdata_selected_IDnoNA[q_dup_ID_final[[7]],]

#function to check responses of each participant, search with ID number, input: ID number & data matrix
showresponse <- function(v,m){
  res <- m[v,]
  return(res)
}

#apply the showresponse function to repeat ID, recheck their answers one by one
q_needcheckresponse <- lapply(q_dup_ID_final, showresponse,m=qdata_selected_IDnoNA)

# Here the original response data was viewed according to each repeat ID number as [[1:15]], and the choice was made to delete the later responses according to the order in which they were filled in, as [1] or [2].
#1:15组 这里根据每一个重复的被试编号进行了源数据的查看，根据填写时间的顺序，选择删除填写较后的作答
q_needcheckresponse[[15]] #1:15

delete_loc <- c(q_dup_ID_final[[1]][2],q_dup_ID_final[[2]][2],q_dup_ID_final[[3]][2],q_dup_ID_final[[4]][2],q_dup_ID_final[[5]][2],q_dup_ID_final[[6]][2],q_dup_ID_final[[7]][c(2,3)],q_dup_ID_final[[8]][2],q_dup_ID_final[[9]][2],q_dup_ID_final[[10]][1],q_dup_ID_final[[11]][2],q_dup_ID_final[[12]][1],q_dup_ID_final[[13]][2],q_dup_ID_final[[14]][1],q_dup_ID_final[[15]][1])

qdata_nodup <- qdata_selected_IDnoNA[-delete_loc,]

#select the MLOC data in the filtered participants. NOTE: four items were missing of the very first version of MLOC, with variables' names: "Q140_1","Q140_2","Q140_3","Q140_4", for this error, we had recorded MLOC again
qdata_all_MLOC <- qdata_nodup[,c("Q140_1","Q140_2","Q140_3","Q140_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")]

#recode the selected MLOC data into scores
ori_MLOC_recode <- t(apply(qdata_all_MLOC, 1, MLOCrecode))

#using length to identify the missing NA number of participants, meanwhile changing the category into numeric of MLOC data
forMLOCsublength <- length(which(is.na(
  apply(ori_MLOC_recode, 2, as.numeric)[,1]
)))

length(ori_MLOC_recode[,1])

qdata_missed_MLOC <- qdata_nodup[1:forMLOCsublength,] #select participants with missing part of MLOC

matchedMLOC_ID <- (match(qdata_missed_MLOC[,"Q1.ID"],fin_MLOC[,"Q1.ID"])) #match participants in this two times of collections of data, replace the related data with their own new data

length(which(is.na(matchedMLOC_ID))) #there were 22 participants failed to match, in other words, there were 22 participants did not complete the retest MLOC questionnarie. 22人MLOC数据未回收到

#function to mark no matched participants' MLOC data as 0, matched take with its own MLOC responses
findMLOCdata <- function(n){
  MLOC_loc <- match(n,fin_MLOC[,"Q1.ID"])
  if (is.na(MLOC_loc))  MLOC_res <- rep(0,times=24) 
  else
  MLOC_res <- fin_MLOC[MLOC_loc,c("MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4" )] #using the original correct variable names of MLOC
  return(MLOC_res)
}

MLOCdata_IDmatched_qdata_missed <- t(mapply(findMLOCdata,qdata_missed_MLOC[,"Q1.ID"]))

dim(MLOCdata_IDmatched_qdata_missed)

qdata_uni_final <- cbind(qdata_nodup[1:forMLOCsublength,c("Q1.ID","Q139","times","Q3.gender","Q4.age","Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.","Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5")],MLOCdata_IDmatched_qdata_missed)

write.csv(qdata_uni_final,"CHN_retest_Q0804_MLOC.csv",row.names = F)







#############################
###########CHN DATA##########
########## CLEANING #########
#############################

data_DZL <- read.csv(here::here("CHN_DZL.csv"))
data_LJG <- read.csv(here::here("CHN_LJG.csv"))
data_HCP_S <- read.csv(here::here("CHN_HCP_Student.csv"))
data_HCP_A <- read.csv(here::here("CHN_HCP_1_2.csv"))
re_data_HCP_A <- read.csv(here::here("CHN_retest_Q0804_MLOC.csv"))


#class(data_Gese_1)
#rownames(data_Gese_1)
#colnames(re_data_HCP_A)
#names_HCP_A <- colnames(data_HCP_A)
#write.table(names_HCP_A,file = "names_HCP_A.txt",eol = ",",row.names = F)

##### all original variables' names
##### 
###names_ori_used <- c("check.1.","check.2.","Q3.gender","Q4.age.","Q5.ethnic.groups.","Q6.educational.atta.","Q7.SES.","Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad","FAD.Q1.","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")


names_ori_used <- c("check.1","check.2","Q1.ID","Q3.gender","Q4.age","Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.","Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

names_ori_used_DZL <- c("check.1","check.2","Q2.name","Q3.gender","Q4.age","Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.","Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

names_retest_used <- c("Q1.ID","Q3.gender","Q4.age","Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.","Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

names_used <- c("datasetNO","check1","check2","ID","gender","age","ethnic","edu","SES","objSES","subSES","abroad","abroad_c","abroad_t","FD1","SD2","UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10","UP11","FW12","FD13","SD14","UP15","FW16","FD17","SD18","UP19","UP20","FW21","SD22","FW23","SD24","UP25","FW26","UP27",
                "BFI1","BFI2","BFI3","BFI4","BFI5","BFI6","BFI7","BFI8","BFI9","BFI10","BFI11","BFI12","BFI13","BFI14","BFI15","BFI16","BFI17","BFI18","BFI19","BFI20","BFI21","BFI22","BFI23","BFI24","BFI25","BFI26","BFI27","BFI28","BFI29","BFI30","BFI31","BFI32","BFI33","BFI34","BFI35","BFI36","BFI37","BFI38","BFI39","BFI40","BFI41","BFI42","BFI43","BFI44","BFI45","BFI46","BFI47","BFI48","BFI49","BFI50","BFI51","BFI52","BFI53","BFI54","BFI55","BFI56","BFI57","BFI58","BFI59","BFI60",
                "MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8","MLOC9","MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16","MLOC17","MLOC18","MLOC19","MLOC20","MLOC21","MLOC22","MLOC23","MLOC24")


re_names_used <- c("datasetNO","ID","gender","age","ethnic","edu","SES","objSES","subSES","abroad","abroad_c","abroad_t","FD1","SD2","UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10","UP11","FW12","FD13","SD14","UP15","FW16","FD17","SD18","UP19","UP20","FW21","SD22","FW23","SD24","UP25","FW26","UP27",
                   "BFI1","BFI2","BFI3","BFI4","BFI5","BFI6","BFI7","BFI8","BFI9","BFI10","BFI11","BFI12","BFI13","BFI14","BFI15","BFI16","BFI17","BFI18","BFI19","BFI20","BFI21","BFI22","BFI23","BFI24","BFI25","BFI26","BFI27","BFI28","BFI29","BFI30","BFI31","BFI32","BFI33","BFI34","BFI35","BFI36","BFI37","BFI38","BFI39","BFI40","BFI41","BFI42","BFI43","BFI44","BFI45","BFI46","BFI47","BFI48","BFI49","BFI50","BFI51","BFI52","BFI53","BFI54","BFI55","BFI56","BFI57","BFI58","BFI59","BFI60",
                   "MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8","MLOC9","MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16","MLOC17","MLOC18","MLOC19","MLOC20","MLOC21","MLOC22","MLOC23","MLOC24")

FADnames <- c("FD1","SD2","UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10","UP11","FW12","FD13","SD14","UP15","FW16","FD17","SD18","UP19","UP20","FW21","SD22","FW23","SD24","UP25","FW26","UP27")

BFInames <- c("BFI1","BFI2","BFI3","BFI4","BFI5","BFI6","BFI7","BFI8","BFI9","BFI10","BFI11","BFI12","BFI13","BFI14","BFI15","BFI16","BFI17","BFI18","BFI19","BFI20","BFI21","BFI22","BFI23","BFI24","BFI25","BFI26","BFI27","BFI28","BFI29","BFI30","BFI31","BFI32","BFI33","BFI34","BFI35","BFI36","BFI37","BFI38","BFI39","BFI40","BFI41","BFI42","BFI43","BFI44","BFI45","BFI46","BFI47","BFI48","BFI49","BFI50","BFI51","BFI52","BFI53","BFI54","BFI55","BFI56","BFI57","BFI58","BFI59","BFI60")

MLOCnames <- c("MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8","MLOC9","MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16","MLOC17","MLOC18","MLOC19","MLOC20","MLOC21","MLOC22","MLOC23","MLOC24")
#####

#add the dataset No. for each point of collections
#HCP-dataset 3, with 3.1 (students) & 3.2
data_HCP_S_selected <- data_HCP_S[,names_ori_used]
dataset_NO <- rep(3.1,times=length(data_HCP_S_selected[,1]))
data3.1 <- cbind(dataset_NO,data_HCP_S_selected)
colnames(data3.1) <- names_used

data_HCP_A_selected <- data_HCP_A[,names_ori_used]
dataset_NO <- rep(3.2,times=length(data_HCP_A_selected[,1]))
data3.2 <- cbind(dataset_NO,data_HCP_A_selected)
colnames(data3.2) <- names_used

#DZL-dataset 1
data_DZL_selected <- data_DZL[,names_ori_used_DZL]
dataset_NO <- rep(1,times=length(data_DZL_selected[,1]))
data1 <- cbind(dataset_NO,data_DZL_selected)
colnames(data1) <- names_used

#LJG-dataset 2
data_LJG_selected <- data_LJG[,names_ori_used]
dataset_NO <- rep(2,times=length(data_LJG_selected[,1]))
data2 <- cbind(dataset_NO,data_LJG_selected)
colnames(data2) <- names_used


CHN_alldata_ori <- rbind(data1,data2,data3.1,data3.2)

#valid all CHN data with 2 correct checks
CHN_data_val_checks <- CHN_alldata_ori[which(CHN_alldata_ori[,"check1"]=="不合理" & CHN_alldata_ori[,"check2"]==5),]

dim(CHN_alldata_ori)
#table(CHN_data_val_checks$gender)

#table(CHN_data_val_checks$age)
#age data collected by year of birth, check the data with less than 4 digits 
years <- as.numeric(substring(str_remove_all(CHN_data_val_checks$age,"[^0-9]"),1,4))
years[which(nchar(years)!=4)]
CHN_data_val_checks$age[which(nchar(years)!=4)]
years[which(years==908)] <- 1990
years[which(years==69)] <- 1969
years[which(years==97)] <- 1997

years[which(years<1900)] <- NA
years[which(years>2023)]
years[which(years==9103)] <- 1991
years[which(years==9806)] <- 1998

CHN_data_val_checks$age <- 2023-years #calculate age data


#the descriptive data from columns 1-13, CHN_data_val_checks[,1:13]
CHN_descriptions <- apply(CHN_data_val_checks[,1:13],2,table)


#FAD-data
#original FAD-data form "Points(Number) + related Chinese indication" like "5 非常同意" , here we take the numeric data directly, using the "st_remove_all" function
FADdata <- CHN_data_val_checks[,FADnames]
dim(FADdata)
FAD_Num <- matrix(as.numeric( 
  apply(FADdata, 2, str_remove_all,pattern="[^0-9]")),ncol = 27)
CHN_data_val_checks[,FADnames] <- FAD_Num #replace 

#BFI-data, also record with points
BFIdata <- CHN_data_val_checks[,BFInames]
dim(BFIdata)
BFIrecode <- function(v){
  v[which(v=="非常不同意")] <- 1
  v[which(v=="不太同意")] <- 2
  v[which(v=="态度中立")] <- 3
  v[which(v=="比较同意")] <- 4
  v[which(v=="非常同意")] <- 5
  return(v)
}
BFI_Num <- matrix(as.numeric(apply(BFIdata,2,BFIrecode)),ncol = 60)
CHN_data_val_checks[,BFInames] <- BFI_Num


###MLOC####
#error MLOC-data with 7-points

MLOCdata <- CHN_data_val_checks[,MLOCnames]
MLOCrecode <- function(v){
  v[which(v=="很不同意")] <- -3
  v[which(v=="不同意")] <- -2
  v[which(v=="不太同意")] <- -1
  v[which(v=="态度中立")] <- 0
  v[which(v=="同意")] <- 1
  v[which(v=="比较同意")] <- 2
  v[which(v=="很同意")] <- 3
  return(v)
}

MLOC_7_Num <- matrix(as.numeric(apply(MLOCdata, 2, MLOCrecode)),ncol = 24)
CHN_data_val_checks[,MLOCnames] <- MLOC_7_Num




#function to detect if there is any omission or all responses have the same value in different scales

cleaning_OmissionSames <- function(m,nombre){ #input: data matrix, scale variable names
  m_selected <- m[,nombre] #select data with specific scale name
  long <- length(nombre) #concrete real or theoretical length of the scale, is used for comparison with practical data in the next step
  cleaning01 <-function(v){   #input: responses vector of each trial 
    if (length(v!=long)) res <- 0 #detect cases with omissions
    if (anyNA(v)==T) res <- 0 #detect cases with NA
    else {fretable <- table(v) #detect cases with same responses, using the frequency table, if there were only one or less option was selected,the length of the frequency table would be equal to or less than 1, otherwise, consider as a valid trial
    frenvm <- length(fretable)
    if (frenvm<=1) res <- 0
    else res <- 1}
    return(res)
  }
  stayornot <- apply(m_selected, 1, cleaning01) #apply the cleaning01 function to the data matrix
  staymarked <- cbind(m,stayornot)
  cleaned <- staymarked[which(stayornot==1),1:(length(m[1,]))] # return only the clean data with marked 1
  return(cleaned)
}

CHN_FAD_cleaned <- cleaning_OmissionSames(CHN_data_val_checks,FADnames)
CHN_FAD_BFI_cleaned <- cleaning_OmissionSames(CHN_FAD_cleaned,BFInames)
CHN_FAD_BFI_MLOC_cleaned <- cleaning_OmissionSames(CHN_FAD_BFI_cleaned,MLOCnames)

dim(CHN_FAD_BFI_MLOC_cleaned)

#check the repeat ID again in the whole CHN dataset
dup_locs_CHN <- unique(find_dup_loc(CHN_FAD_BFI_MLOC_cleaned[,"ID"]))

lapply(dup_locs_CHN, showresponse,m=CHN_FAD_BFI_MLOC_cleaned)


# Select the responses to be deleted based on the duplicate ID number, or the participants' name, mainly in chronological order.
#根据重复的被试编号，或者被试填写的姓名进行对比，主要是时间顺序上的比对，选择需要删除的作答
delete_loc_CHN <- c(dup_locs_CHN[[6]][2],dup_locs_CHN[[7]][1],dup_locs_CHN[[9]][2],dup_locs_CHN[[12]][2],dup_locs_CHN[[14]][c(1,2)],dup_locs_CHN[[15]][2],dup_locs_CHN[[16]][2],dup_locs_CHN[[18]][2],dup_locs_CHN[[19]][2],dup_locs_CHN[[24]][2], dup_locs_CHN[[26]][1],dup_locs_CHN[[27]][1],dup_locs_CHN[[28]][2],dup_locs_CHN[[29]][2])


#showresponse(dup_locs_CHN[[29]],CHN_FAD_BFI_MLOC_cleaned)

#[[6]]删除序列为2的作答，同为名为Jolie的被试作答-判断标准：作答时间
#[[7]],删除序列为1的作答，同一被试赵川，判断标准：作答时间，及第几次作答
#[[9]]删除序列为2的作答，同为名为朱琳的被试作答-判断标准：作答时间
#[[12]]删除序列为2的作答，同为ID为202528424的被试作答-判断标准：作答时间
#[[14]]删除序列为1，2的作答，同为ID为694的被试作答-判断标准：作答时间
##[[15]]删除序列为2的作答，同为ID为6116的被试作答-判断标准：作答时间
##[[16]]删除序列为2的作答，同为ID为6211的被试作答-判断标准：作答时间
#[[17]]不删除，有两位被试被发放了同一被试编号6217，留相同邮箱retest，但未参加重测，故保留两人数据
#[[18]]删除序列为2的作答，同为ID为6175的被试作答-判断标准：作答时间
#[[19]]删除序列为2的作答，同为ID为6314的被试作答-判断标准：作答时间
#[[20]]被试编号6331-重复被试编号，后者参与重测，保留被试编号为6331，前者编号重新赋值为2331
CHN_FAD_BFI_MLOC_cleaned[dup_locs_CHN[[20]][1],"ID"] <- 2331

#[[21]]被试编号6313-重复被试编号，保持与MLOC数据集中一致，男生编号重新赋值为2313
CHN_FAD_BFI_MLOC_cleaned[dup_locs_CHN[[21]][2],"ID"] <- 2313

#[[24]]删除序列为2的作答，同为ID为653的被试作答-判断标准：作答时间


#被试编号672-重复被试编号，前者参与重测，保留被试编号为672，后者编号重新赋值为2672
CHN_FAD_BFI_MLOC_cleaned[dup_locs_CHN[[25]][2],"ID"] <- 2672

#[[26]]删除序列为1的作答，同为ID为631的被试作答-判断标准：作答时间
##[[27]]删除序列为1的作答，同为ID为620的被试作答-判断标准：作答时间
##[[28]]删除序列为2的作答，同为ID为628的被试作答-判断标准：作答时间
##[[29]]删除序列为2的作答，同为ID为609的被试作答，IP相同，前者参见重测-判断标准：作答时间

CHN_cleaned_nodup <- CHN_FAD_BFI_MLOC_cleaned[-delete_loc_CHN,]
dim(CHN_cleaned_nodup)

#function to calculate the FAD scores
FAD_Plus_scorescalculate <- function(m){ #input with data matrix
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- as.data.frame(m[,fadnames]) #using the FAD variable names to select the FAD data
  FADScores <- function(v){ #input with vector of responses of each trial
    FDNames <- c("FD1","FD5","FD9","FD13", "FD17")
    SDNames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
    UPNames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
    FWNames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")
    FD <- sum(v[FDNames])/length(FDNames)
    SD <- sum(v[SDNames])/length(SDNames)
    UP <- sum(v[UPNames])/length(UPNames)
    FW <- sum(v[FWNames])/length(FWNames)
    Scores <- cbind(FD,SD,UP,FW)
    return(Scores)
  }
  fad4scores <- t(apply(mfad,1,FADScores))
  colnames(fad4scores) <- c("FD","SD","UP","FW")
  allwithscores <- cbind(m,fad4scores)
  return(allwithscores) #return with the input original dataset and the four factors' scores
}



CHN_cleaned_scores <- FAD_Plus_scorescalculate(CHN_cleaned_nodup)






######################
###CHN retest data####
######################

re_data_HCP_A_selected <- re_data_HCP_A[,names_retest_used]

#retest dataset NO equals to 9
re_dataset_NO <- rep(9,times=length(re_data_HCP_A_selected[,1]))
re_data <- cbind(re_dataset_NO,re_data_HCP_A_selected)
colnames(re_data) <- re_names_used



re_FADdata <- re_data[,FADnames]
dim(re_FADdata)
re_FAD_Num <- matrix(as.numeric( #same treatment of FAD data, only take the part of number in responses
  apply(re_FADdata, 2, str_remove_all,pattern="[^0-9]")),ncol = 27)
re_data[,FADnames] <- re_FAD_Num


re_BFIdata <- re_data[,BFInames]
dim(re_BFIdata) 
re_BFI_Num <- matrix(as.numeric(apply(re_BFIdata,2,BFIrecode)),ncol = 60) #record BFI data as points
re_data[,BFInames] <- re_BFI_Num


re_dup_locs <- unique(find_dup_loc(re_data[,"ID"])) #check if there were any duplicated cases in retest dataset-none
lapply(re_dup_locs, showresponse,m=re_data_cleaned)


redata_scores <- FAD_Plus_scorescalculate(re_data) #calculate the retest FAD data



#############################
####### ENG JPN FRN #########
######## CLEANING ###########
#############################


######## ENG data ###########

#where rearrranged the English version datasets from the original datasets from OSF


ENG1.1 <- read.csv(here::here("ENG_Brian_1A.csv"))
ENG1.2 <- read.csv(here::here("ENG_Brian_1B.csv"))

ENG1.3 <- read.csv(here::here("ENG_Brian_1C.csv"))
age <- ENG1.3[,"Age"]
gender <- ENG1.3[,"Gender"]
ENG1.3 <- cbind(age,gender,ENG1.3[,FADnames])

ENG1.4 <- read.csv(here::here("ENG_Brian_2.csv"))
describe(ENG1.4) #which FW4 needs recode
table(ENG1.4[,"FW4"])

recode13578_ENG1.4 <- function(v){ #function made for recoding FW4 in Brian2 dataset
  v[which(v==3)] <- 2
  v[which(v==5)] <- 3
  v[which(v==7)] <- 4
  v[which(v==8)] <- 5
  return(v)
} 
ENG1.4[,"FW4"] <- recode13578_ENG1.4(ENG1.4[,"FW4"])

table(ENG1.4$age)
ENG1.4$age <- ENG1.4$age+17 #recode "age" with original data with 17 difference


ENG2 <- read.csv(here::here("ENG_Lysanne.csv"))


ENG3.1 <- read.csv(here::here("ENG_Nadelhoffer_1.csv"))
#table(ENG3.1[,"Check"]), base on SPSS info, where should select "Disagree" == 2
ENG3.1 <- ENG3.1[which(ENG3.1[,"Check"]==2),]


ENG3.2 <- read.csv(here::here("ENG_Nadelhoffer_2.csv"))
#table(ENG3.2[,"Check"]), base on SPSS info, where should select "Disagree" == 2
ENG3.2 <- ENG3.2[which(ENG3.2[,"Check"]==2),]

ENG3.3 <- read.csv(here::here("ENG_Nadelhoffer_3.csv"))
#table(ENG3.3[,"Check"]), base on SPSS info, the correct answer should be 2 (questions: the day before today and the day after)
ENG3.3 <- ENG3.3[which(ENG3.3[,"Check"]==2),]

ENG3.4 <- read.csv(here::here("ENG_Nadelhoffer_4.csv"))
#table(ENG3.4[,"Check"]), base on SPSS info, the correct answer should be 9 or 2 (questions: the day before today and the day after)
ENG3.4 <- ENG3.4[which(ENG3.4[,"Check"]==9),]

ENG3.5 <- read.csv(here::here("ENG_Nadelhoffer_5.csv"))
#table(ENG3.5[,"Check"]), base on SPSS info, the correct answer should be 24 selecting "Disagree"
ENG3.5 <- ENG3.5[which(ENG3.5[,"Check"]==24),]


ENG <- dplyr::bind_rows(ENG1.1,ENG1.2,ENG1.3,ENG1.4,ENG2,ENG3.1,ENG3.2,ENG3.3,ENG3.4,ENG3.5)

NO <- c(rep("ENG1.1",times=length(ENG1.1[,1])),
        rep("ENG1.2",times=length(ENG1.2[,1])),
        rep("ENG1.3",times=length(ENG1.3[,1])),
        rep("ENG1.4",times=length(ENG1.4[,1])),
        rep("ENG2",times=length(ENG2[,1])),
        rep("ENG3.1",times=length(ENG3.1[,1])),
        rep("ENG3.2",times=length(ENG3.2[,1])),
        rep("ENG3.3",times=length(ENG3.3[,1])),
        rep("ENG3.4",times=length(ENG3.4[,1])),
        rep("ENG3.5",times=length(ENG3.5[,1])))
#where Brian1A: NO=ENG1.1 UK
#Brian1B: NO=ENG1.2 UK
#Brian1C: NO=ENG1.3 US
#Brian2: NO=ENG1.4 US
#Lysanne: NO=ENG2 US
#Nadelhoffer_1: NO=ENG3.1 US
#Nadelhoffer_2: NO=ENG3.2 US
#Nadelhoffer_3: NO=ENG3.3 US
#Nadelhoffer_4: NO=ENG3.4 US
#Nadelhoffer_5: NO=ENG3.5


ENG_final <- cbind(NO,ENG)
#dim(ENG_final)
ENG_final_cleaned <- cleaning_OmissionSames(ENG_final,FADnames)
ENG_final_cleaned_scores <- FAD_Plus_scorescalculate(ENG_final_cleaned)




################################
##########  FRN data ###########
################################
#French version study 1 only with FAD-Plus data, and study 2 dataset with BFI data, but this dataset with BFI first version 45 items, we maintain the BFI data and dimensions' scores for now

names_BFI_FRN <- c( "BFI_1","BFI_2","BFI_3","BFI_4","BFI_5","BFI_6","BFI_7","BFI_8", "BFI_9" ,
                    "BFI_10","BFI_11","BFI_12", "BFI_13", "BFI_14","BFI_15" ,"BFI_16","BFI_17" ,"BFI_18", 
                    "BFI_19","BFI_20","BFI_21" ,"BFI_22","BFI_23" , "BFI_24","BFI_25", "BFI_26" , "BFI_27",
                    "BFI_28", "BFI_29","BFI_30","BFI_31" ,"BFI_32", "BFI_33" ,"BFI_34","BFI_35","BFI_36",
                    "BFI_37","BFI_38", "BFI_39","BFI_40","BFI_41" , "BFI_42" , "BFI_43", "BFI_44" ,"BFI_45",
                    "Extraversion","Agreabilite" , "Conscience", "EmotionsNegatives","Ouverture")

FRN_1.1 <- read.csv(here::here("FRN_Study1.csv"))
FRN_1.2_BFI <- read.csv(here::here("FRN_Study2_BFI.csv"))

FRN_data <- dplyr::bind_rows(FRN_1.1,FRN_1.2_BFI)
FRN_data <- FRN_data[,c("gender","age",FADnames,names_BFI_FRN)]


NO <- rep("FRN",times=length(FRN_data[,1]))
FRN <- cbind(NO,FRN_data) #removing the first "id" column 

FRN_final_cleaned <- cleaning_OmissionSames(FRN,FADnames) #which part of FRN data has factors scores

anyNA(FRN_final_cleaned[,FADnames]) #check if there were any NA in the data after cleaning

FRN_final_cleaned_scores <- FAD_Plus_scorescalculate(FRN_final_cleaned) #calculate the FAD scores



################################
########## JPN data ############
################################
#In this case, we have two translations of Japanese FAD-Plus version, the Goto version has two data sources, one with LOC data; the other version only has one source and with LOC data. 
# we will rearrange these two sources, and have two datasets following:
#JPN_1: Goto version, part of data has LOC data
#JPN_2: Watanabe version with LOC data
# setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analyses/3_2_1_Standardized_Data")

JPN_1_Goto2015 <- read.csv(here::here("JPN_1_Goto2015.csv")) #this is the data from Goto (2015) only have FAD-Plus data with Goto translation version
JPN_2_LOC2021 <- read.csv(here::here("JPN_2_LOC2021.csv")) #two datasets in different versions with LOC data

names_LOC_JPN <- c("LOC_1","LOC_2","LOC_3","LOC_4","LOC_5","LOC_6","LOC_7")
JPN_1_Goto2021_LOC <- JPN_2_LOC2021[,c("age","gender",FADnames,names_LOC_JPN)]

JPN_1 <- dplyr::bind_rows(JPN_1_Goto2015[,-1],JPN_1_Goto2021_LOC)
JPN_2 <- JPN_2_LOC2021[,c("age","gender", #where JPN2 was using the different variable names of FAD
                          "FAD_W_1","FAD_W_2","FAD_W_3", "FAD_W_4", "FAD_W_5", "FAD_W_6",
                          "FAD_W_7","FAD_W_8","FAD_W_9", "FAD_W_10", "FAD_W_11", "FAD_W_12",
                          "FAD_W_13","FAD_W_14","FAD_W_15", "FAD_W_16", "FAD_W_17", "FAD_W_18",
                          "FAD_W_19","FAD_W_20","FAD_W_21", "FAD_W_22", "FAD_W_23", "FAD_W_24",
                          "FAD_W_25","FAD_W_26","FAD_W_27",
                          names_LOC_JPN)]
colnames(JPN_2) <- c("age","gender",colnames(JPN_1)[1:27],names_LOC_JPN) #where we should rename the item varible names in the second version 

NO <- rep("JPN1",times=length(JPN_1[,1]))
JPN_1 <- cbind(NO,JPN_1)

JPN_1_final_cleaned <- cleaning_OmissionSames(JPN_1,FADnames)
JPN_1_final_cleaned_scores <- FAD_Plus_scorescalculate(JPN_1_final_cleaned)


NO <- rep("JPN2",times=length(JPN_2[,1]))
JPN_2 <- cbind(NO,JPN_2)
JPN_2_final_cleaned <- cleaning_OmissionSames(JPN_2,FADnames)
JPN_2_final_cleaned_scores <- FAD_Plus_scorescalculate(JPN_2_final_cleaned)



#####saving data #######
# setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analyses/3_2_3_Save_points")
write.csv(CHN_cleaned_scores,file = here::here("3_2_3_Save_points","CHN_230711.csv"),row.names = F)
write.csv(redata_scores,file = here::here("3_2_3_Save_points","re_CHN_230804.csv"),row.names = F)

write.csv(ENG_final_cleaned_scores,here::here("3_2_3_Save_points","ENG.csv"),row.names=FALSE,fileEncoding ="UTF-8")
write.csv(FRN_final_cleaned_scores,here::here("3_2_3_Save_points","FRN.csv"),row.names=FALSE,fileEncoding ="UTF-8")
write.csv(JPN_1_final_cleaned_scores,here::here("3_2_3_Save_points","JPN_1.csv"),row.names=FALSE,fileEncoding ="UTF-8")
write.csv(JPN_2_final_cleaned_scores,here::here("3_2_3_Save_points","JPN_2.csv"),row.names=FALSE,fileEncoding ="UTF-8")

