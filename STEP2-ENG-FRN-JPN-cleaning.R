

rm(list = ls())
setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analayses/3_2_1_Standardized_Data")

library("car")
library("dplyr")
library("psych")
#Sys.setlocale(category="LC_ALL",locale="en_US.UTF-8") #run this for mac OS setting the local language solving the problems in reading Chinese dataset

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

#function for deleting omission & same responses cases
cleaning_OmissionSames <- function(m){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- m[,fadnames]
  cleaning01 <-function(v){   
    if (length(v!=27)) res <- 0 
    if (anyNA(v)==T) res <- 0 #detect cases with NA
    else {fretable <- table(v) #detect cases with same responses
    frenvm <- length(fretable)
    if (frenvm<=1) res <- 0
    else res <- 1}
    return(res)
  }
  stayornot <- apply(mfad, 1, cleaning01)
  staymarked <- cbind(m,stayornot)
  cleaned <- staymarked[which(stayornot==1),1:(length(m[1,]))]
  return(cleaned)
}

#function for calculating FAD+ Scores
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


################################
##########  ENG data ###########
################################

#where rearrranged the English version datasets from the original datasets from OSF


ENG1.1 <- read.csv("ENG_Brian_1A.csv")
ENG1.2 <- read.csv("ENG_Brian_1B.csv")

ENG1.3 <- read.csv("ENG_Brian_1C.csv")
age <- ENG1.3[,"Age"]
gender <- ENG1.3[,"Gender"]
ENG1.3 <- cbind(age,gender,ENG1.3[,fadnames])


ENG1.4 <- read.csv("ENG_Brian_2.csv")
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


ENG2 <- read.csv("ENG_Lysanne.csv")


ENG3.1 <- read.csv("ENG_Nadelhoffer_1.csv")
#table(ENG3.1[,"Check"]), base on SPSS info, where should select "Disagree" == 2
ENG3.1 <- ENG3.1[which(ENG3.1[,"Check"]==2),]


ENG3.2 <- read.csv("ENG_Nadelhoffer_2.csv")
#table(ENG3.2[,"Check"]), base on SPSS info, where should select "Disagree" == 2
ENG3.2 <- ENG3.2[which(ENG3.2[,"Check"]==2),]

ENG3.3 <- read.csv("ENG_Nadelhoffer_3.csv")
#table(ENG3.3[,"Check"]), base on SPSS info, the correct answer should be 2 (questions: the day before today and the day after)
ENG3.3 <- ENG3.3[which(ENG3.3[,"Check"]==2),]

ENG3.4 <- read.csv("ENG_Nadelhoffer_4.csv")
#table(ENG3.4[,"Check"]), base on SPSS info, the correct answer should be 9 or 2 (questions: the day before today and the day after)
ENG3.4 <- ENG3.4[which(ENG3.4[,"Check"]==9),]

ENG3.5 <- read.csv("ENG_Nadelhoffer_5.csv")
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
ENG_final_cleaned <- cleaning_OmissionSames(ENG_final)
ENG_final_cleaned_scores <- FAD_Plus_scorescalculate(ENG_final_cleaned)

setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analayses/3_2_3_Save_points")
write.csv(ENG_final_cleaned_scores,"ENG.csv",row.names=FALSE,fileEncoding ="UTF-8")



################################
##########  FRN data ###########
################################
#French version study 1 only with FAD-Plus data, and study 2 dataset with BFI data, but this dataset with BFI first version 45 items, we continue to have the BFI data and dimensions' scores for now

setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analayses/3_2_1_Standardized_Data")

names_BFI_FRN <- c( "BFI_1","BFI_2","BFI_3","BFI_4","BFI_5","BFI_6","BFI_7","BFI_8", "BFI_9" ,
                    "BFI_10","BFI_11","BFI_12", "BFI_13", "BFI_14","BFI_15" ,"BFI_16","BFI_17" ,"BFI_18", 
                    "BFI_19","BFI_20","BFI_21" ,"BFI_22","BFI_23" , "BFI_24","BFI_25", "BFI_26" , "BFI_27",
                    "BFI_28", "BFI_29","BFI_30","BFI_31" ,"BFI_32", "BFI_33" ,"BFI_34","BFI_35","BFI_36",
                    "BFI_37","BFI_38", "BFI_39","BFI_40","BFI_41" , "BFI_42" , "BFI_43", "BFI_44" ,"BFI_45",
                    "Extraversion","Agreabilite" , "Conscience", "EmotionsNegatives","Ouverture")

FRN_1.1 <- read.csv("FRN_Study1.csv")
FRN_1.2_BFI <- read.csv("FRN_Study2_BFI.csv")

FRN_data <- dplyr::bind_rows(FRN_1.1,FRN_1.2_BFI)
FRN_data <- FRN_data[,c("gender","age",fadnames,names_BFI_FRN)]


NO <- rep("FRN",times=length(FRN_data[,1]))
FRN <- cbind(NO,FRN_data) #removing the first "id" column 

FRN_final_cleaned <- cleaning_OmissionSames(FRN) #which part of FRN data has factors scores

anyNA(FRN_final_cleaned[,fadnames])

FRN_final_cleaned_scores <- FAD_Plus_scorescalculate(FRN_final_cleaned)

setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analayses/3_2_3_Save_points")

write.csv(FRN_final_cleaned_scores,"FRN.csv",row.names=FALSE,fileEncoding ="UTF-8")


################################
########## JPN data ############
################################
#In this case, we have two translations of Japanese FAD-Plus version, the Goto version has two data sources, one with LOC data; the other version only has one source and with LOC data. 
# we will rearrange these two sources, and have two datasets following:
#JPN_1: Goto version, part of data has LOC data
#JPN_2: Watanabe version with LOC data
setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analayses/3_2_1_Standardized_Data")

JPN_1_Goto2015 <- read.csv("JPN_1_Goto2015.csv") #this is the data from Goto (2015) only have FAD-Plus data with Goto translation version
JPN_2_LOC2021 <- read.csv("JPN_2_LOC2021.csv") #two datasets in different versions with LOC data

names_LOC_JPN <- c("LOC_1","LOC_2","LOC_3","LOC_4","LOC_5","LOC_6","LOC_7")
JPN_1_Goto2021_LOC <- JPN_2_LOC2021[,c("age","gender",fadnames,names_LOC_JPN)]

JPN_1 <- dplyr::bind_rows(JPN_1_Goto2015[,-1],JPN_1_Goto2021_LOC)
JPN_2 <- JPN_2_LOC2021[,c("age","gender",
                          "FAD_W_1","FAD_W_2","FAD_W_3", "FAD_W_4", "FAD_W_5", "FAD_W_6",
                          "FAD_W_7","FAD_W_8","FAD_W_9", "FAD_W_10", "FAD_W_11", "FAD_W_12",
                          "FAD_W_13","FAD_W_14","FAD_W_15", "FAD_W_16", "FAD_W_17", "FAD_W_18",
                          "FAD_W_19","FAD_W_20","FAD_W_21", "FAD_W_22", "FAD_W_23", "FAD_W_24",
                          "FAD_W_25","FAD_W_26","FAD_W_27",
                          names_LOC_JPN)]
colnames(JPN_2) <- c("age","gender",colnames(JPN_1)[1:27],names_LOC_JPN) #where we should rename the item varible names in the second version 

NO <- rep("JPN1",times=length(JPN_1[,1]))
JPN_1 <- cbind(NO,JPN_1)

JPN_1_final_cleaned <- cleaning_OmissionSames(JPN_1)
JPN_1_final_cleaned_scores <- FAD_Plus_scorescalculate(JPN_1_final_cleaned)

setwd("~/Desktop/FAD_New_Start/3_Stage2/3_2_Analayses/3_2_3_Save_points")

write.csv(JPN_1_final_cleaned_scores,"JPN_1.csv",row.names=FALSE,fileEncoding ="UTF-8")

NO <- rep("JPN2",times=length(JPN_2[,1]))
JPN_2 <- cbind(NO,JPN_2)
JPN_2_final_cleaned <- cleaning_OmissionSames(JPN_2)
JPN_2_final_cleaned_scores <- FAD_Plus_scorescalculate(JPN_2_final_cleaned)

write.csv(JPN_2_final_cleaned_scores,"JPN_2.csv",row.names=FALSE,fileEncoding ="UTF-8")

