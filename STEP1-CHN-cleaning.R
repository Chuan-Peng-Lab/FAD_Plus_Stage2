# datacleanning of FAD-Plus-newstart
#
rm(list = ls())
# install.packages("pacman") if not installed
if (!requireNamespace('pacman', quietly = TRUE)) {
      install.packages('pacman')
}

# using pacman for loading packages
pacman::p_load(tidyverse, psych, CTT, lavaan, semPlot, semTools) 

# define the data dir within the R Proj
data_dir <- here::here("3_2_1_Standardized_Data")

# load data
data_DZL <- read.csv(here::here(data_dir,"CHN_DZL.csv"))
data_LJG <- read.csv(here::here(data_dir,"CHN_LJG.csv"))
data_HCP_S <- read.csv(here::here(data_dir,"CHN_HCP_Student.csv"))
data_HCP_A <- read.csv(here::here(data_dir,"CHN_HCP_1_2.csv"))
re_data_HCP_A <- read.csv(here::here(data_dir,"CHN_retest_Q0804_MLOC.csv"))

#####
# names_ori_used <- c("check.1.","check.2.","Q3.gender","Q4.age.",
# "Q5.ethnic.groups.","Q6.educational.atta.","Q7.SES.","Q8.objSES_1",
# "Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad",
# "FAD.Q1.","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6",
# "FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12",
# "FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17","FAD.Q18",
# "FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24",
# "FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2",
# "BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1",
# "BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5",
# "BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4",
# "BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3",
# "BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2",
# "BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1",
# "BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5",
# "BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4",
# "BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3",
# "BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2",
# "BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1",
# "BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5",
# "BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4",
# "BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3",
# "BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3",
# "MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4",
# "MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1",
# "MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1",
# "MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1",
# "MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")
#####

names_ori_used <- c("check.1","check.2","Q1.ID","Q3.gender","Q4.age",
                    "Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.",
                    "Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad",
                    "Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4",
                    "FAD.Q5","FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10",
                    "FAD.Q11","FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15",
                    "FAD.Q16","FAD.Q17","FAD.Q18","FAD.Q19","FAD.Q20",
                    "FAD.Q21","FAD.Q22","FAD.Q23","FAD.Q24","FAD.Q25",
                    "FAD.Q26","FAD.Q27","BFI.1.5._1","BFI.1.5._2",
                    "BFI.1.5._3","BFI.1.5._4","BFI.1.5._5","BFI.6.10._1",
                    "BFI.6.10._2","BFI.6.10._3","BFI.6.10._4","BFI.6.10._5",
                    "BFI.11.15._1","BFI.11.15._2","BFI.11.15._3","BFI.11.15._4",
                    "BFI.11.15._5","BFI.16.20_1","BFI.16.20_2","BFI.16.20_3",
                    "BFI.16.20_4","BFI.16.20_5","BFI.21.25_1","BFI.21.25_2",
                    "BFI.21.25_3","BFI.21.25_4","BFI.21.25_5","BFI.26.30_1",
                    "BFI.26.30_2","BFI.26.30_3","BFI.26.30_4","BFI.26.30_5",
                    "BFI.31.45_1","BFI.31.45_2","BFI.31.45_3","BFI.31.45_4",
                    "BFI.31.45_5","BFI.46.60_1","BFI.46.60_2","BFI.46.60_3",
                    "BFI.46.60_4","BFI.46.60_5","BFI.61.75_1","BFI.61.75_2",
                    "BFI.61.75_3","BFI.61.75_4","BFI.61.75_5","BFI.76.90_1",
                    "BFI.76.90_2","BFI.76.90_3","BFI.76.90_4","BFI.76.90_5",
                    "BFI.91.105_1","BFI.91.105_2","BFI.91.105_3","BFI.91.105_4",
                    "BFI.91.105_5","BFI.106.120_1","BFI.106.120_2","BFI.106.120_3",
                    "BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1","MLOC.1.4_2",
                    "MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3",
                    "MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3",
                    "MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3",
                    "MLOC.13.16_4","MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3",
                    "MLOC.17.20_4","MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

names_ori_used_DZL <- c("check.1","check.2","Q2.name","Q3.gender","Q4.age",
                        "Q5.ethnic.groups","Q6.educational.atta.","Q7.SES.",
                        "Q8.objSES_1","Q9.subSES_1","Q10.abroad","Q11.abroad",
                        "Q12.abroad","FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5",
                        "FAD.Q6","FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11",
                        "FAD.Q12","FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17",
                        "FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22","FAD.Q23",
                        "FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27","BFI.1.5._1",
                        "BFI.1.5._2","BFI.1.5._3","BFI.1.5._4","BFI.1.5._5",
                        "BFI.6.10._1","BFI.6.10._2","BFI.6.10._3", "BFI.6.10._4",
                        "BFI.6.10._5","BFI.11.15._1","BFI.11.15._2","BFI.11.15._3",
                        "BFI.11.15._4","BFI.11.15._5","BFI.16.20_1","BFI.16.20_2",
                        "BFI.16.20_3","BFI.16.20_4","BFI.16.20_5","BFI.21.25_1",
                        "BFI.21.25_2","BFI.21.25_3","BFI.21.25_4","BFI.21.25_5",
                        "BFI.26.30_1","BFI.26.30_2","BFI.26.30_3","BFI.26.30_4",
                        "BFI.26.30_5","BFI.31.45_1","BFI.31.45_2","BFI.31.45_3",
                        "BFI.31.45_4","BFI.31.45_5","BFI.46.60_1","BFI.46.60_2",
                        "BFI.46.60_3","BFI.46.60_4","BFI.46.60_5","BFI.61.75_1",
                        "BFI.61.75_2","BFI.61.75_3","BFI.61.75_4","BFI.61.75_5",
                        "BFI.76.90_1","BFI.76.90_2","BFI.76.90_3","BFI.76.90_4",
                        "BFI.76.90_5","BFI.91.105_1","BFI.91.105_2","BFI.91.105_3",
                        "BFI.91.105_4","BFI.91.105_5","BFI.106.120_1","BFI.106.120_2",
                        "BFI.106.120_3","BFI.106.120_4","BFI.106.120_5","MLOC.1.4_1",
                        "MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4","MLOC.5.8_1","MLOC.5.8_2",
                        "MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1","MLOC.9.12_2","MLOC.9.12_3",
                        "MLOC.9.12_4","MLOC.13.16_1","MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4",
                        "MLOC.17.20_1","MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4",
                        "MLOC.21.24_1","MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

names_retest_used <- c("Q1.ID","Q3.gender","Q4.age","Q5.ethnic.groups",
                       "Q6.educational.atta.","Q7.SES.","Q8.objSES_1",
                       "Q9.subSES_1","Q10.abroad","Q11.abroad","Q12.abroad",
                       "FAD.Q1","FAD.Q2.","FAD.Q3","FAD.Q4","FAD.Q5","FAD.Q6",
                       "FAD.Q7","FAD.Q8","FAD.Q9","FAD.Q10","FAD.Q11","FAD.Q12",
                       "FAD.Q13","FAD.Q14","FAD.Q15","FAD.Q16","FAD.Q17",
                       "FAD.Q18","FAD.Q19","FAD.Q20","FAD.Q21","FAD.Q22",
                       "FAD.Q23","FAD.Q24","FAD.Q25","FAD.Q26","FAD.Q27",
                       "BFI.1.5._1","BFI.1.5._2","BFI.1.5._3","BFI.1.5._4",
                       "BFI.1.5._5","BFI.6.10._1","BFI.6.10._2","BFI.6.10._3",
                       "BFI.6.10._4","BFI.6.10._5","BFI.11.15._1","BFI.11.15._2",
                       "BFI.11.15._3","BFI.11.15._4","BFI.11.15._5","BFI.16.20_1",
                       "BFI.16.20_2","BFI.16.20_3","BFI.16.20_4","BFI.16.20_5",
                       "BFI.21.25_1","BFI.21.25_2","BFI.21.25_3","BFI.21.25_4",
                       "BFI.21.25_5","BFI.26.30_1","BFI.26.30_2","BFI.26.30_3",
                       "BFI.26.30_4","BFI.26.30_5","BFI.31.45_1","BFI.31.45_2",
                       "BFI.31.45_3","BFI.31.45_4","BFI.31.45_5","BFI.46.60_1",
                       "BFI.46.60_2","BFI.46.60_3","BFI.46.60_4","BFI.46.60_5",
                       "BFI.61.75_1","BFI.61.75_2","BFI.61.75_3","BFI.61.75_4",
                       "BFI.61.75_5","BFI.76.90_1","BFI.76.90_2","BFI.76.90_3",
                       "BFI.76.90_4","BFI.76.90_5","BFI.91.105_1","BFI.91.105_2",
                       "BFI.91.105_3","BFI.91.105_4","BFI.91.105_5","BFI.106.120_1",
                       "BFI.106.120_2","BFI.106.120_3","BFI.106.120_4","BFI.106.120_5",
                       "MLOC.1.4_1","MLOC.1.4_2","MLOC.1.4_3","MLOC.1.4_4",
                       "MLOC.5.8_1","MLOC.5.8_2","MLOC.5.8_3","MLOC.5.8_4","MLOC.9.12_1",
                       "MLOC.9.12_2","MLOC.9.12_3","MLOC.9.12_4","MLOC.13.16_1",
                       "MLOC.13.16_2","MLOC.13.16_3","MLOC.13.16_4","MLOC.17.20_1",
                       "MLOC.17.20_2","MLOC.17.20_3","MLOC.17.20_4","MLOC.21.24_1",
                       "MLOC.21.24_2","MLOC.21.24_3","MLOC.21.24_4")

names_used <- c("datasetNO","check1","check2","ID","gender","age","ethnic",
                "edu","SES","objSES","subSES","abroad","abroad_c","abroad_t",
                "FD1","SD2","UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10",
                "UP11","FW12","FD13","SD14","UP15","FW16","FD17","SD18","UP19",
                "UP20","FW21","SD22","FW23","SD24","UP25","FW26","UP27",
                "BFI1","BFI2","BFI3","BFI4","BFI5","BFI6","BFI7","BFI8","BFI9","BFI10","BFI11",
                "BFI12","BFI13","BFI14","BFI15","BFI16","BFI17","BFI18","BFI19","BFI20","BFI21",
                "BFI22","BFI23","BFI24","BFI25","BFI26","BFI27","BFI28","BFI29","BFI30","BFI31",
                "BFI32","BFI33","BFI34","BFI35","BFI36","BFI37","BFI38","BFI39","BFI40","BFI41",
                "BFI42","BFI43","BFI44","BFI45","BFI46","BFI47","BFI48","BFI49","BFI50","BFI51",
                "BFI52","BFI53","BFI54","BFI55","BFI56","BFI57","BFI58","BFI59","BFI60",
                "MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8","MLOC9",
                "MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16","MLOC17",
                "MLOC18","MLOC19","MLOC20","MLOC21","MLOC22","MLOC23","MLOC24")


re_names_used <- c("datasetNO","ID","gender","age","ethnic","edu","SES","objSES","subSES",
                   "abroad","abroad_c","abroad_t","FD1","SD2","UP3","FW4","FD5","SD6","UP7",
                   "FW8","FD9","SD10","UP11","FW12","FD13","SD14","UP15","FW16","FD17","SD18",
                   "UP19","UP20","FW21","SD22","FW23","SD24","UP25","FW26","UP27",
                   "BFI1","BFI2","BFI3","BFI4","BFI5","BFI6","BFI7","BFI8","BFI9","BFI10",
                   "BFI11","BFI12","BFI13","BFI14","BFI15","BFI16","BFI17","BFI18","BFI19",
                   "BFI20","BFI21","BFI22","BFI23","BFI24","BFI25","BFI26","BFI27","BFI28",
                   "BFI29","BFI30","BFI31","BFI32","BFI33","BFI34","BFI35","BFI36","BFI37",
                   "BFI38","BFI39","BFI40","BFI41","BFI42","BFI43","BFI44","BFI45","BFI46",
                   "BFI47","BFI48","BFI49","BFI50","BFI51","BFI52","BFI53","BFI54","BFI55",
                   "BFI56","BFI57","BFI58","BFI59","BFI60",
                   "MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8","MLOC9",
                   "MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16","MLOC17",
                   "MLOC18","MLOC19","MLOC20","MLOC21","MLOC22","MLOC23","MLOC24")

FADnames <- c("FD1","SD2","UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10","UP11","FW12",
              "FD13","SD14","UP15","FW16","FD17","SD18","UP19","UP20","FW21","SD22","FW23",
              "SD24","UP25","FW26","UP27")

BFInames <- c("BFI1","BFI2","BFI3","BFI4","BFI5","BFI6","BFI7","BFI8","BFI9","BFI10","BFI11",
              "BFI12","BFI13","BFI14","BFI15","BFI16","BFI17","BFI18","BFI19","BFI20","BFI21",
              "BFI22","BFI23","BFI24","BFI25","BFI26","BFI27","BFI28","BFI29","BFI30","BFI31",
              "BFI32","BFI33","BFI34","BFI35","BFI36","BFI37","BFI38","BFI39","BFI40","BFI41",
              "BFI42","BFI43","BFI44","BFI45","BFI46","BFI47","BFI48","BFI49","BFI50","BFI51",
              "BFI52","BFI53","BFI54","BFI55","BFI56","BFI57","BFI58","BFI59","BFI60")

MLOCnames <- c("MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8","MLOC9","MLOC10",
               "MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16","MLOC17","MLOC18","MLOC19",
               "MLOC20","MLOC21","MLOC22","MLOC23","MLOC24")

data_DZL_selected <- data_DZL[,names_ori_used_DZL]
dataset_NO <- rep(1,times=length(data_DZL_selected[,1]))
data1 <- cbind(dataset_NO,data_DZL_selected)
colnames(data1) <- names_used

data_LJG_selected <- data_LJG[,names_ori_used]
dataset_NO <- rep(2,times=length(data_LJG_selected[,1]))
data2 <- cbind(dataset_NO,data_LJG_selected)
colnames(data2) <- names_used


data_HCP_S_selected <- data_HCP_S[,names_ori_used]
dataset_NO <- rep(3.1,times=length(data_HCP_S_selected[,1]))
data3.1 <- cbind(dataset_NO,data_HCP_S_selected)
colnames(data3.1) <- names_used


data_HCP_A_selected <- data_HCP_A[,names_ori_used]
dataset_NO <- rep(3.2,times=length(data_HCP_A_selected[,1]))
data3.2 <- cbind(dataset_NO,data_HCP_A_selected)
colnames(data3.2) <- names_used


CHN_alldata_ori <- rbind(data1,data2,data3.1,data3.2)


CHN_data_val_checks <- CHN_alldata_ori[which(CHN_alldata_ori[,"check1"]=="不合理" & CHN_alldata_ori[,"check2"]==5),]




#table(CHN_data_val_checks$gender)

#table(CHN_data_val_checks$age)
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

CHN_data_val_checks$age <- 2023-years



CHN_descriptions <- apply(CHN_data_val_checks[,1:13],2,table)


#FAD-data
FADdata <- CHN_data_val_checks[,FADnames]
dim(FADdata)
FAD_Num <- matrix(as.numeric(
                  apply(FADdata, 2, str_remove_all,pattern="[^0-9]")),ncol = 27)
CHN_data_val_checks[,FADnames] <- FAD_Num


#BFI-data
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
# MLOC-data-7-points

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


cleaning_OmissionSames <- function(m,nombre){
  m_selected <- m[,nombre]
  long <- length(nombre)
  cleaning01 <-function(v){   
    if (length(v!=long)) res <- 0 
    if (anyNA(v)==T) res <- 0 #detect cases with NA
    else {fretable <- table(v) #detect cases with same responses
    frenvm <- length(fretable)
    if (frenvm<=1) res <- 0
    else res <- 1}
    return(res)
  }
  stayornot <- apply(m_selected, 1, cleaning01)
  staymarked <- cbind(m,stayornot)
  cleaned <- staymarked[which(stayornot==1),1:(length(m[1,]))]
  return(cleaned)
}

CHN_FAD_cleaned <- cleaning_OmissionSames(CHN_data_val_checks,FADnames)

CHN_FAD_BFI_cleaned <- cleaning_OmissionSames(CHN_FAD_cleaned,BFInames)
CHN_FAD_BFI_MLOC_cleaned <- cleaning_OmissionSames(CHN_FAD_BFI_cleaned,MLOCnames)

find_dup_loc <- function(v){
  dup <- v[which(duplicated(v))]
  find_loc <- function(valor){
    valor_loc <- which(v==valor) 
    return(valor_loc)
  }
  res <- mapply(find_loc,dup)
  return(res)
}

dup_locs <- unique(find_dup_loc(CHN_FAD_BFI_MLOC_cleaned[,"ID"]))
showresponse <- function(v,m){
  res <- m[v,]
  return(res)
}

lapply(dup_locs, showresponse,m=CHN_FAD_BFI_MLOC_cleaned)


#根据重复的被试编号，或者被试填写的姓名进行，主要是时间顺序上的比对，选择需要删除的作答
delete_loc <- c(dup_locs[[6]][2],dup_locs[[7]][1],dup_locs[[9]][2],
                dup_locs[[12]][2],dup_locs[[14]][c(1,2)],dup_locs[[15]][2],
                dup_locs[[16]][2],dup_locs[[18]][2],dup_locs[[19]][2],
                dup_locs[[24]][2], dup_locs[[26]][1],dup_locs[[27]][1],
                dup_locs[[28]][2],dup_locs[[29]][2])


#showresponse(dup_locs[[29]],CHN_FAD_BFI_MLOC_cleaned)

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
CHN_FAD_BFI_MLOC_cleaned[dup_locs[[20]][1],"ID"] <- 2331

#[[21]]被试编号6313-重复被试编号，保持与MLOC数据集中一致，男生编号重新赋值为2313
CHN_FAD_BFI_MLOC_cleaned[dup_locs[[21]][2],"ID"] <- 2313

#[[24]]删除序列为2的作答，同为ID为653的被试作答-判断标准：作答时间


#被试编号672-重复被试编号，前者参与重测，保留被试编号为672，后者编号重新赋值为2672
CHN_FAD_BFI_MLOC_cleaned[dup_locs[[25]][2],"ID"] <- 2672

#[[26]]删除序列为1的作答，同为ID为631的被试作答-判断标准：作答时间
##[[27]]删除序列为1的作答，同为ID为620的被试作答-判断标准：作答时间
##[[28]]删除序列为2的作答，同为ID为628的被试作答-判断标准：作答时间
##[[29]]删除序列为2的作答，同为ID为609的被试作答，IP相同，前者参见重测-判断标准：作答时间

CHN_cleaned_nodup <- CHN_FAD_BFI_MLOC_cleaned[-delete_loc,]
dim(CHN_cleaned_nodup)


FAD_Plus_scorescalculate <- function(m){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- as.data.frame(m[,fadnames])
  FADScores <- function(v){
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
  return(allwithscores)
}

CHN_cleaned_scores <- FAD_Plus_scorescalculate(CHN_cleaned_nodup)

write.csv(CHN_cleaned_scores,file = here::here("3_2_3_Save_points","CHN_230711.csv"),row.names = F)

### for retest data ####
re_data_HCP_A_selected <- re_data_HCP_A[,names_retest_used]

re_dataset_NO <- rep(9,times=length(re_data_HCP_A_selected[,1]))
re_data <- cbind(re_dataset_NO,re_data_HCP_A_selected)
colnames(re_data) <- re_names_used

re_FADdata <- re_data[,FADnames]
dim(re_FADdata)
re_FAD_Num <- matrix(as.numeric(
  apply(re_FADdata, 2, str_remove_all,pattern="[^0-9]")),ncol = 27)
re_data[,FADnames] <- re_FAD_Num

re_BFIdata <- re_data[,BFInames]
dim(re_BFIdata)
re_BFI_Num <- matrix(as.numeric(apply(re_BFIdata,2,BFIrecode)),ncol = 60)
re_data[,BFInames] <- re_BFI_Num

re_dup_locs <- unique(find_dup_loc(re_data[,"ID"]))

lapply(re_dup_locs, showresponse,m=re_data_cleaned)

redata_scores <- FAD_Plus_scorescalculate(re_data)

write.csv(redata_scores,file = here::here("3_2_3_Save_points","re_CHN_230804.csv"),row.names = F)