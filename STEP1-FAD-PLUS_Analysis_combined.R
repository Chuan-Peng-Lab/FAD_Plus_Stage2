
## Setting Working Directory ##

rm(list = ls())
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
parentDir <- dirname(curWD)
setwd(file.path(parentDir, "3_2_3_Save_points"))

library(CTT)
library(dplyr)
library(psych)
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)
library(ltm)
library(mirt)
library(lordif)


# Read data files
CHN <- read.csv("CHN_230711.csv")
re_CHN <- read.csv("re_CHN_230804.csv")
ENG <- read.csv("ENG.csv")
FRN <- read.csv("FRN.csv")
JPN_1 <- read.csv("JPN_1.csv")
JPN_2 <- read.csv("JPN_2.csv")

ALLdata <- list(CHN,ENG,FRN,JPN_1,JPN_2)

#re-name the NO of datasets based on the orden in Table 1 in stage2 
CHN1 <- rbind(CHN[which(CHN[,"datasetNO"]==3.1),],
              CHN[which(CHN[,"datasetNO"]==3.2),])
CHN2 <- CHN[which(CHN[,"datasetNO"]==1),] 
CHN3 <- CHN[which(CHN[,"datasetNO"]==2),]

CHN1.2 <- CHN[which(CHN[,"datasetNO"]==3.2),] #in which will match the retest dataset
length(CHN1.2[,1])


#存在未匹配的情况，原因可能是：
#最开始筛查发放retest信息的时候，只是简单的通过两个注意力检测check，后期有可能被有na值，重复值，或者其他条件给删掉了
#其中三个MLOC的回收回复为空，决定删掉不用这4个subjects的response
which(is.na(match(re_CHN[,"ID"],CHN1.2[,"ID"])))
reCHN_cal_data <- re_CHN[-c(48,64,72,111),] #there were six participants without responses data, deleted their data 


#function用来匹配retest data
#function to match retest ID from the original ID of dataset 1.2
findretest <- function(n){ #input: retest ID
  retest_loc <- match(n,CHN1.2[,"ID"]) #match with the original dataset 1.2
  res <- CHN1.2[retest_loc,] #return the matched ID
  return(res) 
}
reCHN_ori_data <- t(mapply(findretest, reCHN_cal_data[,"ID"]))
as.data.frame(reCHN_ori_data)
length(reCHN_cal_data[,1]) #188 participants matched



########################################
########### Basic descriptive ##########
########################################


descriptionsALL <- lapply(ALLdata,describe)

#CHN
table(CHN$datasetNO)
table(CHN$gender)
table(CHN$edu)
table(CHN$age)

mean(CHN$age, na.rm = T) #23.8122
sd(CHN$age, na.rm = T) #7.835254
which(is.na(CHN$age)==T) #NA=2


table(CHN$SES)

table(CHN$ethnic)
table(CHN$objSES)
table(CHN$subSES)
table(CHN$abroad)

table(CHN1$gender) # F:223  M: 214 
mean(CHN1$age, na.rm = T) #21.91494
sd(CHN1$age, na.rm = T) #2.283911
table(CHN1$edu) #n1-1  n2-  n3-4  n4-11  n5-323  n6-96  n7-2
summary(as.numeric(CHN1$SES))  #mean 24106 median 12000
table(CHN1$abroad) #NO:398 YES:39

table(CHN2$gender) # F:376  M: 77 
mean(CHN2$age, na.rm = T) #30.9978
sd(CHN2$age, na.rm = T) #8.379884
table(CHN2$edu) #n1-  n2-2 n3-22  n4-27  n5-248  n6-129  n7-27
summary(as.numeric(CHN2$SES))  #mean 34255 median 15000
table(CHN2$abroad) #NO:322 YES:133

table(CHN3$gender)# F:229  M: 127 
mean(CHN3$age, na.rm = T) #16.94382
sd(CHN3$age, na.rm = T) #1.347796
which(is.na(CHN3$age)==T)
table(CHN3$edu) #n1-  n2- n3-347  n4-  n5-2  n6-7  n7-
summary(as.numeric(CHN3$SES))  #mean 7214 median 5000
table(CHN3$abroad) #NO:350 YES:4

table(reCHN_cal_data$gender)# F:80  M: 108 
table(reCHN_cal_data$edu)#n1-  n2- n3-1  n4-3  n5-137  n6-46  n7-1
summary(reCHN_cal_data$SES) #mean 15010 median 12000
table(reCHN_cal_data$abroad) #NO:169 YES:17
mean(2023-as.numeric(substring(str_remove_all(reCHN_cal_data$age,"[^0-9]"),1,4))) #22.10106
sd(2023-as.numeric(substring(str_remove_all(reCHN_cal_data$age,"[^0-9]"),1,4))) #2.027986

#ENG
table(ENG$NO)
sum(table(ENG$NO)) #3256
table(ENG$gender) #M:1733 F:1478
table(ENG$age)
mean(ENG$age,na.rm = T) #28.85254
sd(ENG$age,na.rm = T)#15.14765


#FRN
sum(table(FRN$NO))
table(FRN$gender) #M:579 F:325
table(FRN$age)
mean(FRN$age,na.rm = T) # 26.29097
sd(FRN$age,na.rm = T) #8.678731

#JPN
length(JPN_1[,"NO"])
length(JPN_2[,"NO"])

table(JPN_1$age)
table(JPN_1$gender)
mean(JPN_1$age,na.rm = T) # 38.14963
sd(JPN_1$age,na.rm = T) #10.62093

table(JPN_2$age)
table(JPN_2$gender)
mean(JPN_2$age,na.rm = T) # 38.15875
sd(JPN_2$age,na.rm = T) #10.61706


########################################
############  correlations #############
########################################

#cor(FADitems),cor(FADitems-factors),
#cor(FADfactors-BFIfactors),cor(FADfactors-MLOC) CRITERION VALIDITY
#
#SUPPLEMENTARY:MLOC 6-points VS 7-points


#function to calculate the all correlations between the factors and items
forcorrelationsFAD <- function(m){ #input: data matrix 
  fadnameswith4 <- c("FD","SD","UP","FW", #concrete FAD factor and items' names
                     "FD1","FD5","FD9","FD13","FD17",
                     "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                     "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                     "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  relations <- cor(m[,fadnameswith4]) #calculate the correlations using the "cor" function
  #function to determine if the correlation value greater than 0.5 or not
  correlationsto01 <- function(v){ 
    v_abs <- abs(v) #take the absolute value first
    cuts <- cut(v_abs,breaks = c(0,0.5,1),labels=c(0,1)) 
    #use "cut" function to classify the value, if the value is in the range of 0-0.5, mark it as 0, if is 0.5-1, mark it as 1. 
    return(cuts)
  }
  c01 <- apply(relations,1,correlationsto01) #apply the mark 0/1 function the correlation matrix 
  return(cbind(relations,c01)) #return the correlation matrix and with 0&1 also
}

#lappy to all the list of each language's dataset
correlationsFAD <- lapply(ALLdata,forcorrelationsFAD)


#CHN 
#correlationsFAD[[1]]
#FD1-FD9 0.621353576802785, FD5-FD13 0.51028907389053, UP11-UP19 0.539769454, UP15-UP27 0.522938755924847


#ENG
#FD1-FD5 0.61655911, FD1-FD9 0.79003534, FD1-FD13 0.51763799, FD1-FD17  0.53722948, FD5-FD9 0.57856566, FD5-FD13 0.56612362, 
#UP3-UP20 0.50228066, UP11-UP19 0.583069698, UP20-UP25 0.51960160, 
#FW4-FW21 0.583703781, FW8-FW16 0.551046172, FW16-FW23 0.630766282



#FRN
#FD5-FD9 0.53400699
#
#
for (i in 1:length(correlationsFAD)) {
  textnames <- paste("correlations",i,".txt",sep = "")
  write.table(correlationsFAD[[i]],textnames)
}








#######BFI#########
#CHN
#function to calculate BIG5 5 dimensions' scores
CHN_BFIS <- function(datos,nombre,key){ #input: data, names of BFI variables, record key
  databfi <- datos[,nombre] #concrete one BFI factor's data
  data_corrected <- matrix(t(apply(databfi,1,function(x){x*key})),ncol = length(key)) #calculate with multiplying by key
  datafinal <- na.exclude(data_corrected)
  bfiscores <- apply(datafinal, 1, function(x){sum(x)/length(key)}) #calculate a average score
  return(bfiscores) #return the factor score
}  

#calculate the scores of 外向性 (Extraversion) with items: 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
CHN_BFI_Enames <- c("BFI1","BFI6", "BFI11", "BFI16", "BFI21", "BFI26", "BFI31", "BFI36", "BFI41", "BFI46", "BFI51", "BFI56")
CHN_BFI_Ekeys <- c(1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
CHN_BFI_E <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Enames,CHN_BFI_Ekeys)


#calculate the scores of 宜人性 (Agreeableness) with items: 2, 7, 12R, 17R, 22R, 27, 32, 37R, 42R, 47R, 52, 57
CHN_BFI_Anames <- c("BFI2","BFI7", "BFI12", "BFI17", "BFI22", "BFI27", "BFI32", "BFI37", "BFI42","BFI47", "BFI52", "BFI57")
CHN_BFI_Akeys <- c(1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1)
CHN_BFI_A <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Anames,CHN_BFI_Akeys)


#calculate the scores of 尽责性 (Conscientiousness) with items: 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
CHN_BFI_Cnames <- c("BFI3","BFI8", "BFI13", "BFI18", "BFI23", "BFI28", "BFI33", "BFI38", "BFI43","BFI48", "BFI53", "BFI58")
CHN_BFI_Ckeys <- c(-1, -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
CHN_BFI_C <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Cnames,CHN_BFI_Ckeys)


#calculate the scores of 负性情绪/神经质 (Negative Emotionality) with items: 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
CHN_BFI_Nnames <- c("BFI4","BFI9", "BFI14", "BFI19", "BFI24", "BFI29", "BFI34", "BFI39", "BFI44","BFI49", "BFI54", "BFI59")
CHN_BFI_Nkeys <- c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
CHN_BFI_N <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Nnames,CHN_BFI_Nkeys)


#calculate the scores of 开放性 (Open-Mindedness) with items: 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
CHN_BFI_Onames <- c("BFI5","BFI10", "BFI15", "BFI20", "BFI25", "BFI30", "BFI35", "BFI40", "BFI45","BFI50", "BFI55", "BFI60")
CHN_BFI_Okeys <- c(-1, 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
CHN_BFI_O <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Onames,CHN_BFI_Okeys)


CHN_BFIS <- cbind(CHN_BFI_E,CHN_BFI_A,CHN_BFI_C,CHN_BFI_N,CHN_BFI_O)

#unit the CHN BFI scores, correlate with the four factors of FAD
CHN_BFI_FAD_Final <- cbind(CHN_BFIS,ALLdata[[1]][,c("FD","SD","UP","FW")])
corCHN_BFI_FAD <- cor(CHN_BFI_FAD_Final[,c("CHN_BFI_E","CHN_BFI_A","CHN_BFI_C","CHN_BFI_N","CHN_BFI_O")],CHN_BFI_FAD_Final[,c("FD","SD","UP","FW")])
write.table(corCHN_BFI_FAD,"CHN_BFI_FAD.txt") #save correlation martix with BFI 5 dimensions



#FRN
fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
FRN_BFI <- FRN[-c(which(is.na(FRN[,"BFI_1"]))),] #select data with BFI data in FRN dataset

FRN_FAD_BFI <- FRN_BFI[,c("FD","SD","UP","FW",fadnames)]
cor_FRN_BFI <- cor(FRN_BFI[,c("Extraversion","Agreabilite" , "Conscience", "EmotionsNegatives","Ouverture")],FRN_FAD_BFI) #calculate the correlation directly between the factors of BFI and FAD, factors of BFI had already calculated by original FRN dataset

write.table(cor_FRN_BFI,"FRN_BFI_FAD.txt")


#########MLOC#########
#select MLOC data in retest dataset 
re_CHN_FAD_MLOC <- reCHN_cal_data[,c("FD","SD","UP","FW",fadnames)]

CHN_MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
CHN_MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
CHN_MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")

#select MLOC data in the retest dataset, the only correct collection of MLOC was in the retest dataset, define as "MLOC_6_raw"
MLOC_names <- c(CHN_MLOC_INames,CHN_MLOC_PNames,CHN_MLOC_CNames)
MLOC_6_raw <- reCHN_cal_data[,c("ID",MLOC_names)]
MLOC_7_raw <- reCHN_ori_data[,c("ID",MLOC_names)] #7-point data in original first collection, "reCHN_ori_data" already matched with same ID as the reCHN_cal_data above
filtered_MLOC_6_IDdata <- MLOC_6_raw[-which(MLOC_6_raw[,2]==0),] #eliminate the 0s data, there was no 0 should be selected, if there is a 0, that was not matched in the previous script, "filtered_MLOC_6_IDdata" define ID + valid MLOC data
filtered_MLOC_6_alldata <- reCHN_cal_data[-which(MLOC_6_raw[,2]==0),] # "filtered_MLOC_6_alldata" define that all data depends on valid MLOC data

#prepare to calculate the correlation between the 7-point and 6-point MLOC
na.omit(match(MLOC_7_raw[,"ID"],filtered_MLOC_6_IDdata[,"ID"]))
matched_MLOC_7_IDdata <- MLOC_7_raw[-which(-MLOC_6_raw[,2]==0),] #MLOC_7 also needs to eliminate that cases with 0s in MLOC_6 that share share the same position, because two datasets have already been matched 
matched_MLOC_7_alldata <- reCHN_ori_data[-as.vector(which(MLOC_6_raw[,2]==0)),]

#calculate MLOC scores
CHN_MLOC_6_I <- apply(filtered_MLOC_6_alldata[,CHN_MLOC_INames]+3,1,sum)
CHN_MLOC_6_P <- apply(filtered_MLOC_6_alldata[,CHN_MLOC_PNames]+3,1,sum)
CHN_MLOC_6_C <- apply(filtered_MLOC_6_alldata[,CHN_MLOC_CNames]+3,1,sum) 
CHN_6_MLOCS <- cbind(CHN_MLOC_6_I,CHN_MLOC_6_P,CHN_MLOC_6_C)

#calculate the correlations between the MLOC and FAD
cor_CHN_MLOC_6_FAD <- cor(CHN_6_MLOCS,filtered_MLOC_6_alldata[,c("FD","SD","UP","FW")])

write.table(cbind(filtered_MLOC_6_alldata,CHN_6_MLOCS),"MLOC6_FAD_filtered.txt")
write.table(cor_CHN_MLOC_6_FAD,"CHN_MLOC6_FAD.txt")


####which response with 7 points using the same keys
CHN_MLOC_7_Idata <- apply(matched_MLOC_7_alldata[,CHN_MLOC_INames],2,as.numeric)
CHN_MLOC_7_I <- apply(CHN_MLOC_7_Idata+3, 1, sum)

CHN_MLOC_7_Pdata <- apply(matched_MLOC_7_alldata[,CHN_MLOC_PNames],2,as.numeric)
CHN_MLOC_7_P <- apply(CHN_MLOC_7_Pdata+3, 1, sum)

CHN_MLOC_7_Cdata <- apply(matched_MLOC_7_alldata[,CHN_MLOC_CNames],2,as.numeric)
CHN_MLOC_7_C <- apply(CHN_MLOC_7_Cdata+3, 1, sum)

CHN_7_MLOCS <- cbind(CHN_MLOC_7_I,CHN_MLOC_7_P,CHN_MLOC_7_C)

cor_re_CHN_7_MLOC_FAD <- cor(CHN_7_MLOCS,apply(matched_MLOC_7_alldata[,c("FD","SD","UP","FW")],2,as.numeric))
#cor_CHN_MLOC_6_FAD


########JPN
names_LOC_JPN <- c("LOC_1","LOC_2","LOC_3","LOC_4","LOC_5","LOC_6","LOC_7")

JPN1_LOC <- JPN_1[-c(which(is.na(JPN_1[,"LOC_1"]))),] #take the valid LOC data in JPN1 dataset

#check the structure of JPN_LOC 
psych::fa(JPN1_LOC[,names_LOC_JPN], nfactors = 2, rotate = "oblimin")$loading

#calculate the scores of LOC assuming that 1-4 as factor 1, 5-7 factor 2
only_JPN1_LOC <- JPN1_LOC[,names_LOC_JPN]
JPN1_LOC_1_Scores <- apply(only_JPN1_LOC[,c("LOC_1","LOC_2","LOC_3","LOC_4")],1,sum) #calculate LOC1 scores by sum
JPN1_LOC_2_Scores <- apply(only_JPN1_LOC[,c("LOC_5","LOC_6","LOC_7")],1,sum)#calculate LOC22 scores by sum

cor_LOC_JPN1 <- cor(cbind(JPN1_LOC_1_Scores,JPN1_LOC_2_Scores),JPN1_LOC[,c("FD","SD","UP","FW")]) #calculate correlation between the LOC and FAD in JPN1 data



JPN2_LOC_1_Scores <- apply(JPN_2[,c("LOC_1","LOC_2","LOC_3","LOC_4")],1,sum)
JPN2_LOC_2_Scores <- apply(JPN_2[,c("LOC_5","LOC_6","LOC_7")],1,sum)

cor_LOC_JPN2 <- cor(cbind(JPN2_LOC_1_Scores,JPN2_LOC_2_Scores),JPN_2[,c("FD","SD","UP","FW")])#same calculation of correlations in JPN22





########################################
############  RELIABILITY ##############
########################################

#alpha, omega, test-retest

alphasandomegas <- function(m){  #input: data matrix 
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26") #concrete FAD data in the data matrix
  alpha0 <- (CTT::reliability(m[,fadnames]))$alpha # calculate alpha with CTT package
  omega0 <- (psych::omega(m[,fadnames]))$omega.tot # calculate omega with psych package
  FDnames <- c("FD1","FD5","FD9","FD13","FD17") #concrete data in each factor
  SDnames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
  UPnames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
  FWnames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  alphafd <- (CTT::reliability(m[,FDnames]))$alpha #calculate the alpha in each factor
  alphasd <- (CTT::reliability(m[,SDnames]))$alpha
  alphaup <- (CTT::reliability(m[,UPnames]))$alpha
  alphafw <- (CTT::reliability(m[,FWnames]))$alpha
  omegafd <- (psych::omega(m[,FDnames]))$omega.tot #and the omega for each factor
  omegasd <- (psych::omega(m[,SDnames]))$omega.tot
  omegaup <- (psych::omega(m[,UPnames]))$omega.tot
  omegafw <- (psych::omega(m[,FWnames]))$omega.tot
  res <- cbind(alpha0,alphafd,alphasd,alphaup,alphafw,
               omega0,omegafd,omegasd,omegaup,omegafw)
  return(res) #return all alphas and omegas
}
alphaandomega <- lapply(ALLdata,alphasandomegas) #lappy in all data set list
reliabilities <- matrix(unlist(alphaandomega),nrow = 5,byrow = T)  #order the reliabilities into matrix and rename the columns
colnames(reliabilities) <- c("alpha","alphaFD","alphaSD","alphaUP","alphaFW",
                             "omega","omegaFD","omegaSD","omegaUP","omegaFW")
rownames(reliabilities) <- c("CHN","ENG","FRN","JPN_1","JPN_2")
write.table(reliabilities,"alphasandomegas.txt")


###test-retest reliability

fadfactornames <- c("FD","SD","UP","FW")

re_CHN_FAD_factors_scores <- reCHN_cal_data[,fadfactornames] #concrete the retest FAD data of four factors

re_CHN_ori_FAD_factors_scores <- matrix(as.numeric(reCHN_ori_data[,fadfactornames]),ncol = length(fadfactornames)) #sort for correlation calculation
colnames(re_CHN_ori_FAD_factors_scores) <- fadfactornames

cor(re_CHN_FAD_factors_scores,re_CHN_ori_FAD_factors_scores) #use "cor" for correlations
#     FD           SD          UP         FW
#FD  0.6806167  0.327128362  0.07046590 -0.2112382
#SD  0.2494703  0.671196296 -0.08362712  0.2127180
#UP  0.1880708 -0.004421683  0.65947826  0.1529998
#FW -0.2439795  0.211073618  0.13406782  0.7603420




########################################
################  CFA ##################
########################################


#for further MI testing, first of all need to test 4-factor model fits as CONSTRUCT VALIDITY, and also to compare loadings in different datasets

forcfaFAD_model4 <- function(m){ #input: data matrix
  #model define as the original 4-factor model
  model <-  'FD =~ FD1 + FD5 + FD9 + FD13 + FD17; 
  SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24;
  UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
  FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- m[,fadnames] #concrete fad data
  rescfa <- lavaan::cfa(model,mfad,estimator="ml") #CFA calculation with lavaan package
  semPlot::semPaths(rescfa,"std",rotation = 2,layout = "tree2",nCharNodes = 0,
                    sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                    mar=c(2,6,2,4), curvePivot = TRUE,
                    edge.label.cex=1.2, residuals = F) #loading path figure
  res <- standardizedSolution(rescfa) #return results of CFA 
  return(res)
}

cfaFAD_model4 <- lapply(ALLdata, forcfaFAD_model4) #also lappy to all data list

for (i in 1:length(cfaFAD_model4)) {
  textnames <- paste("cfas",i,".txt",sep = "")
  write.table(cfaFAD_model4[[i]],textnames)
}






########################################
###### Measurement Invariance ##########
########### CFA MULTIGROUP #############
########################################

#in the MI, there are following pairs data have been compared: all non-english VS ENG, CHN VS ENG, FRN VS ENG, JPN1 VS ENG, JPN2 VS ENG

#define de the four factor model
FAD_model4 <-  'FD =~ FD1 + FD5 + FD9 + FD13 + FD17
                SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24
                UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27
                FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'

#this model is a test model, we have tried to release different items one by one...
#FAD_model4_test <-  'FD =~ FD1 + FD5 + c(v1,v1)*FD9 + FD13 + c(v1,v1)*FD17
#                     SD =~ SD2 + c(v1,v1)*SD6 + SD10 + SD14 + c(v1,v1)*SD18 + SD22 + c(v1,v1)*SD24
#                     UP =~ UP3 + c(v1,v1)*UP7 +  UP11 + c(v1,v1)*UP15 + UP19 + c(v1,v1)*UP20 + c(v1,v1)*UP25 + c(v1,v1)*UP27  
#                     FW =~ FW4 + c(v1,v1)*FW8 + c(v1,v1)*FW12 + FW16 + FW21 + c(v1,v1)*FW23 + c(v1,v1)*FW26'

#model for partial MI, the same as in the stage 1
FAD_model4_test_old <-  'FD =~ FD1 + FD5 + c(v1,v1)*FD9 + c(v1,v1)*FD13 + c(v1,v1)*FD17
                     SD =~ SD2 + c(v1,v1)*SD6 + SD10 + c(v1,v1)*SD14 + c(v1,v1)*SD18 + c(v1,v1)*SD22 + c(v1,v1)*SD24
                     UP =~ UP3 + c(v1,v1)*UP7 + c(v1,v1)* UP11 + c(v1,v1)*UP15 + c(v1,v1)*UP19 + c(v1,v1)*UP20 + c(v1,v1)*UP25 + c(v1,v1)*UP27  
                     FW =~ FW4 + c(v1,v1)*FW8 + c(v1,v1)*FW12 + c(v1,v1)*FW16 + c(v1,v1)*FW21 + c(v1,v1)*FW23 + c(v1,v1)*FW26'

#function made for testing MI in our case that could use different cfa model, which we achieved partial MI
forcfaMI <- function(m1,m2,nombre,modelo){ #input: dataset1, dataset2, variables' names (for concrete data), model to fit
  res <- list()
  fadnames <- nombre
  model <-  modelo
  m1fad <- m1[,fadnames] #take the fad data in the dataset1
  m2fad <- m2[,fadnames] #take the fad data in the dataset2
  g <- c(rep("0",length(m1[,1])),rep("1",length(m2[,1]))) #"g" marks different datasets, 0 for input dataset1, 1 for input dataset 2
  datas <- cbind(rbind(m1fad,m2fad),g) #cbind two datasets and the g variable above, to prepare the cfa MI calculation
  datas$g <- factor(datas$g,levels = c("0","1")) #made the g as a factor category
  fit1 <- cfa(model,datas,group = "g") #configural invariance
  fit2 <- cfa(model,datas,group = "g", group.equal="loadings") #metric invariance
  fit3 <- cfa(model,datas,group = "g", group.equal = c("intercepts", "loadings")) #scalar invariance
  fit4 <- cfa(model,datas,group = "g", group.equal = c("intercepts", "loadings","residuals")) #strict Invariance
  res[[1]] <- lavTestLRT(fit1, fit2, fit3, fit4) # for comparing lavaan models
  res[[2]] <- rbind(summary(fit1,fit.measures=T)$fit, summary(fit2,fit.measures=T)$fit, summary(fit3,fit.measures=T)$fit, summary(fit4,fit.measures=T)$fit) #summary the each fit indexs
  #res1 <- modindices(fit1, sort = T, maximum.number = 10)
  return(res) #return with a list, first label comparsin, second label summary of indexs
} 


#non-ENG VS ENG
nonENG <- rbind(CHN[,fadnames],FRN[,fadnames],JPN_1[,fadnames],JPN_2[,fadnames]) #rbind all NO-ENG data 
nonENG_ENG_model4 <- forcfaMI(nonENG,ENG,fadnames,FAD_model4) # test the traditional MI
#nonENG_ENG_modeltest <- forcfaMI(nonENG,ENG,fadnames,FAD_model4_test)
nonENG_ENG_modeltest_old <- forcfaMI(nonENG,ENG,fadnames,FAD_model4_test_old) #test the partial MI


#UK_ENG VS US_ENG
#table(ENG[,"NO"]) #only ENG 1.1 1.2 - UK
#UK_ENG <- ENG[which(ENG[,"NO"]=="ENG1.1"|ENG[,"NO"]=="ENG1.2"),]
#length(UK_ENG$NO)
#mean(UK_ENG$age)
#sd(UK_ENG$age)
#US_ENG <- ENG[intersect(which(ENG[,"NO"]!="ENG1.1"),which(ENG[,"NO"]!="ENG1.2")),]

#UK_US_model4 <- forcfaMI(UK_ENG,US_ENG,fadnames,FAD_model4)
#UK_US_modeltest <- forcfaMI(UK_ENG,US_ENG,fadnames,FAD_model4_test)
#UK_US_modeltest_old <- forcfaMI(UK_ENG,US_ENG,fadnames,FAD_model4_test_old)


#CHN VS ENG
CHN_ENG_model4 <- forcfaMI(CHN,ENG,fadnames,FAD_model4) #MI
#CHN_ENG_modeltest <- forcfaMI(CHN,ENG,fadnames,FAD_model4_test)
CHN_ENG_modeltest_old <- forcfaMI(CHN,ENG,fadnames,FAD_model4_test_old) #partial MI


#FRN VS ENG
FRN_ENG_model4 <- forcfaMI(FRN,ENG,fadnames,FAD_model4) # MI
#FRN_ENG_modeltest <- forcfaMI(FRN,ENG,fadnames,FAD_model4_test)
FRN_ENG_modeltest_old <- forcfaMI(FRN,ENG,fadnames,FAD_model4_test_old) #partial MI

#JPN VS ENG
#JPN_FAD <- rbind(JPN_1[,fadnames],JPN_2[,fadnames])
#JPN_ENG_model4 <- forcfaMI(JPN_FAD,ENG,fadnames,FAD_model4)
#JPN_ENG_modeltest <- forcfaMI(JPN_FAD,ENG,fadnames,FAD_model4_test)

JPN1_ENG_model4 <- forcfaMI(JPN_1[,fadnames],ENG,fadnames,FAD_model4) # MI
#JPN1_ENG_modeltest <- forcfaMI(JPN_1[,fadnames],ENG,fadnames,FAD_model4_test)
JPN1_ENG_modeltest_old <- forcfaMI(JPN_1[,fadnames],ENG,fadnames,FAD_model4_test_old) #partial MI


JPN2_ENG_model4 <- forcfaMI(JPN_2[,fadnames],ENG,fadnames,FAD_model4) # MI
#JPN2_ENG_modeltest <- forcfaMI(JPN_2[,fadnames],ENG,fadnames,FAD_model4_test)
JPN2_ENG_modeltest_old <- forcfaMI(JPN_2[,fadnames],ENG,fadnames,FAD_model4_test_old) #partial MI




#SUPPLEMENTARY: test the MI between 3 CHN sites, and between the two JPN datasets
table(CHN[,"datasetNO"]) #also rename the dataset with order in Table 1 stage 2
CHN_1 <- CHN[which(CHN[,"datasetNO"]==3.1|CHN[,"datasetNO"]==3.2),]
CHN_2 <- CHN[which(CHN[,"datasetNO"]==1),]
CHN_3 <- CHN[which(CHN[,"datasetNO"]==2),]

#traditional MI in sub CHN
CHN1_2_model4 <- forcfaMI(CHN_1,CHN_2,fadnames,FAD_model4)
CHN1_3_model4 <- forcfaMI(CHN_1,CHN_3,fadnames,FAD_model4)
CHN2_3_model4 <- forcfaMI(CHN_2,CHN_3,fadnames,FAD_model4)


#CHN1_2_modeltest <- forcfaMI(CHN_1,CHN_2,fadnames,FAD_model4_test)
#CHN1_3_modeltest <- forcfaMI(CHN_1,CHN_3,fadnames,FAD_model4_test)
#CHN2_3_modeltest <- forcfaMI(CHN_2,CHN_3,fadnames,FAD_model4_test)

#partial MI in sub CHN
CHN1_2_modeltest_old <- forcfaMI(CHN_1,CHN_2,fadnames,FAD_model4_test_old)
CHN1_3_modeltest_old <- forcfaMI(CHN_1,CHN_3,fadnames,FAD_model4_test_old)
CHN2_3_modeltest_old <- forcfaMI(CHN_2,CHN_3,fadnames,FAD_model4_test_old)

#MI between JPN1 and JPN2
JPN1_2_model4 <- forcfaMI(JPN_1,JPN_2,fadnames,FAD_model4)
#JPN1_2_modeltest <- forcfaMI(JPN_1,JPN_2,fadnames,FAD_model4_test)
JPN1_2_modeltest_old <- forcfaMI(JPN_1,JPN_2,fadnames,FAD_model4_test_old)


#MI between the CHN(all) with JPN1
CHN_JPN1_model4 <- forcfaMI(CHN,JPN_1,fadnames,FAD_model4)
#CHN_JPN1_modeltest <- forcfaMI(CHN,JPN_1,fadnames,FAD_model4_test)
CHN_JPN1_modeltest_old <- forcfaMI(CHN,JPN_1,fadnames,FAD_model4_test_old)


#MI between the CHN(all) with JPN2
CHN_JPN2_model4 <- forcfaMI(CHN,JPN_2,fadnames,FAD_model4)
#CHN_JPN2_modeltest <- forcfaMI(CHN,JPN_2,fadnames,FAD_model4_test)
CHN_JPN2_modeltest_old <- forcfaMI(CHN,JPN_2,fadnames,FAD_model4_test_old)


#mean(UK_ENG$age)
#mean(CHN_1$age,na.rm = T)
#UK_CHN3_model4 <- forcfaMI(UK_ENG,CHN1,fadnames,FAD_model4)
#UK_CHN1_modeltest <- forcfaMI(UK_ENG,CHN3,fadnames,FAD_model4_test)
#UK_CHN1_modeltest_old <- forcfaMI(UK_ENG,CHN1,fadnames,FAD_model4_test_old)





####supplementary, test EFA with with half data of non-ENG, intent to obtain a NEW MODEL

#function to randomly obtain half of FAD data 
set.seed(36)
takehalf <- function(m){ #input: data matrix 
  longitud <- length(m[,1])
  sample_loc <- sample(longitud,size = (longitud/2),replace = F) #use "sample" directly take the half data
  res1 <- m[sample_loc,fadnames]
  res2 <- m[-sample_loc,fadnames]
  res <- list(res1,res2) #return a list, with the first label of one half data, the second with the other half
  return(res)
}

#take half of data in all different datasets
CHN_1_half <- takehalf(CHN_1)
CHN_2_half <- takehalf(CHN_2)
CHN_3_half <- takehalf(CHN_3)

ENG_half <- takehalf(ENG)
FRN_half <- takehalf(FRN)
JPN_1_half <- takehalf(JPN_1)
JPN_2_half <- takehalf(JPN_2)

#rbind all half data, 1 for EFA fitting
ALL_half_1 <- rbind(CHN_1_half[[1]],CHN_2_half[[1]],CHN_3_half[[1]],
                      ENG_half[[1]],FRN_half[[1]],JPN_1_half[[1]],JPN_2_half[[1]])

write.csv(ALL_half_1,"halftestnmf.csv")

#second part 2 for MI testing
nonENGhalf_2 <- rbind(CHN_1_half[[2]],CHN_2_half[[2]],CHN_3_half[[2]],
                      FRN_half[[2]],JPN_1_half[[2]],JPN_2_half[[2]])

CHN_half_2 <- rbind(CHN_1_half[[2]],CHN_2_half[[2]],CHN_3_half[[2]])

JPN_half_2 <- rbind(JPN_1_half[[2]],JPN_2_half[[2]])

#length(ALL_half_1[,1])
correlations_half <- cor(ALL_half_1)



#ALLFAD_data <- rbind(CHN[,fadnames],FRN[,fadnames],ENG[,fadnames],JPN_1[,fadnames],JPN_2[,fadnames])
#fa.parallel(ALLFAD_data,cor = "poly",n.iter = 100)


#parallel analyses
fa.parallel(ALL_half_1,cor = "poly",n.iter = 100) #7/8 factors ; 6 components
fa <- fa(correlations_half,nfactors = 4 ,n.obs = 4966, rotate="promax") #EFA testing
fa
fa$loadings

factor.plot(fa)
fa.diagram(fa) #结果会根据sample到的数据，model会改变，目前跑出的结果是在下面的new6model，但是仍然有很大问题

#one of the new model generated by EFA
new_6_model <- 'MR1 =~ FD1 + FD5 + FD9 + FD13 + FD17
                MR3 =~ UP19 + UP27 + UP11 + UP7 + UP25 + UP3 + UP15
                MR2 =~ FW4 + FW8 + FW12 + FW21  + FW26
                MR4 =~ SD2  + SD10  + SD22 + SD24
                MR5 =~ FW16 + FW23
                MR6 =~ SD6+ SD14 + SD18 + UP20

                MR1~~ MR3 + MR4 + MR6
                MR2~~ MR5
                MR4~~ MR6'

#test model with 3 factors
new_3_model <- 'MR1 =~ FD1 + FD5 + FD9 + FD13 + FD17 + SD2 + SD10  + SD22 + SD24 + SD6+ SD14 + SD18 
                MR3 =~ UP19 + UP27 + UP11 + UP7 + UP25 + UP3 + UP15 + UP20
                MR2 =~ FW4 + FW16 + FW8 + FW12 + FW21 + FW23 + FW26
                
                MR1~~ MR3'

rescfa_model3 <- cfa(new_3_model,nonENGhalf_2)
semPlot::semPaths(rescfa_model3,"std",rotation = 2,layout = "tree2",nCharNodes = 0,
                  sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                  mar=c(2,6,2,4), curvePivot = TRUE,
                  edge.label.cex=1.2, residuals = F) #loadings path figure
standardizedSolution(rescfa_model3)

#function to test cfa using the 3 factor model (new_3_model)
forcfaFAD_model3 <- function(m){ #input: data matrix
  model <-  'MR1 =~ FD1 + FD5 + FD9 + FD13 + FD17 + SD2 + SD10  + SD22 + SD24 + SD6+ SD14 + SD18 
  MR3 =~ UP19 + UP27 + UP11 + UP7 + UP25 + UP3 + UP15 + UP20
  MR2 =~ FW4 + FW16 + FW8 + FW12 + FW21 + FW23 + FW26

  MR1~~ MR3'
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- m[,fadnames] #concrete fad data
  rescfa <- lavaan::cfa(model,mfad,estimator="ml") #test cfa using the lavaan package
  semPlot::semPaths(rescfa,"std",rotation = 2,layout = "tree2",nCharNodes = 0,
                    sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                    mar=c(2,6,2,4), curvePivot = TRUE,
                    edge.label.cex=1.2, residuals = F)
  res <- standardizedSolution(rescfa) #return the cfa results
  return(res)
}

cfa(new_3_model,ALL_half_1) 

#in the very first step of testing cfa with the new 6-factor model, FRN and JPN data could not fit the 6-factor model, means that we could not continue with the multiCFA MI test, only CHN and ENG could fit
rescfa_model6 <- cfa(new_6_model,FRN_half[[2]]) #FRN_half[[2]], JPN_half_2[[2]] 法国日本的数据无法拟合这个新model，只有中国和英文的数据可以，后面也就没了做这两个国家MI的意义
standardizedSolution(rescfa_model6)




########################################
###############   IRT   ################
########################################

detach("package:psych")

#when using the IRT method to detect MI, in other words, DIF item, the input data must be unidimensional, so obtain the data for each factor first
FDnames <- c("FD1","FD5","FD9","FD13","FD17")
SDnames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
UPnames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
FWnames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")

#one of the method IRT using the generalized graded unfolding model, using the mirt package, was not used finally cause it took time
# ggumfad1 <- function(m,dimensionnames) { input:data matrix, one factor variable's names
#  n <- length(m[1,])
#  mdim<- m[,dimensionnames] 
#  res <- mirt(mdim,1,itemtype = "ggum") #apply mirt, indicate the itemtype "ggum" 
#  ressum <- summary(res)
#  coefres <- coef(res)
#  listcoefres <- unlist(coefres)
#  l <- length(listcoefres)
#  listcoefres <- listcoefres[-c(l-1,l)]
#  coefmatrix <- matrix(listcoefres,nrow = n,byrow = T)
#  resfinal <- cbind(ressum[[1]],ressum[[2]],coefmatrix)
#  return(resfinal)
# }

#little estimation of each dimension with irt using generalized graded unfolding model #TAKES TIME
#CHN_FD_ggum <- ggumfad1(CHN,FDnames)
#CHN_SD_ggum <- ggumfad1(CHN,SDnames)
#CHN_UP_ggum <- ggumfad1(CHN,UPnames)
#CHN_FW_ggum <- ggumfad1(CHN,FWnames)


#function to detect DIF using the lordif package
forLordif <- function(m1,m2,dimensionnames){ #input: dataset for comparsion 1, dataset for comparsion 2, factor variable names
  m1dim <- m1[,dimensionnames] #concrete data of input factor in dataset1
  m2dim <- m2[,dimensionnames] #concrete data of input factor in dataset2
  g <- c(rep("0",length(m1dim[,1])),rep("1",length(m2dim[,1]))) #also use the "g" variable differs data from two datasets
  datas <- rbind(m1dim,m2dim) #rbind data, prepare for the input for lordif
  reslordif <- lordif(datas, g, criterion = "Chisqr", #detect DIF with "lordif",set alpha=0.05, if set =0.01 better results detect with more items without DIF
                      pseudo.R2 = c("Nagelkerke"),
                      alpha = 0.05, minCell = 1)
}


forLordif(nonENG,ENG,FDnames) 
forLordif(nonENG,ENG,SDnames) 
forLordif(nonENG,ENG,UPnames) 
forLordif(nonENG,ENG,FWnames)

###res CHN-ENG
forLordif(CHN,ENG,FDnames) 
forLordif(CHN,ENG,SDnames) #SD 2 
forLordif(CHN,ENG,UPnames) 
forLordif(CHN,ENG,FWnames)

#forLordif(CHN_3,UK_ENG,FDnames)
#forLordif(CHN_3,UK_ENG,SDnames) #SD 2, SD6, SD22
#forLordif(CHN_3,UK_ENG,UPnames)
#forLordif(CHN_3,UK_ENG,FWnames) #FW4, FW8

forLordif(CHN_3,CHN_1,FDnames) #FD1, FD5, FD9, FD13
forLordif(CHN_3,CHN_1,SDnames)
forLordif(CHN_3,CHN_1,UPnames) #UP3, UP15, UP19, UP27
forLordif(CHN_3,CHN_1,FWnames) 

forLordif(CHN_3,CHN_2,FDnames) #FD1,FD9
forLordif(CHN_3,CHN_2,SDnames) #SD14, SD24
forLordif(CHN_3,CHN_2,UPnames) #UP7
forLordif(CHN_3,CHN_2,FWnames) 

forLordif(CHN_1,CHN_2,FDnames)  #FD1,FD9, FD13
forLordif(CHN_1,CHN_2,SDnames)  #SD2
forLordif(CHN_1,CHN_2,UPnames)  
forLordif(CHN_1,CHN_2,FWnames)  #FW12, FW21, FW26

###res FRN-ENG
forLordif(FRN,ENG,FDnames) 
forLordif(FRN,ENG,SDnames) 
forLordif(FRN,ENG,UPnames) 
forLordif(FRN,ENG,FWnames) 


#forLordif(JPN_FAD,ENG,FDnames) 
#forLordif(JPN_FAD,ENG,SDnames)
#forLordif(JPN_FAD,ENG,UPnames) 
#forLordif(JPN_FAD,ENG,FWnames)
#
###res JPN_1-ENG
forLordif(JPN_1,ENG,FDnames) 
forLordif(JPN_1,ENG,SDnames)
forLordif(JPN_1,ENG,UPnames) 
forLordif(JPN_1,ENG,FWnames)

###res JPN_2-ENG
forLordif(JPN_2,ENG,FDnames) 
forLordif(JPN_2,ENG,SDnames)
forLordif(JPN_2,ENG,UPnames) 
forLordif(JPN_2,ENG,FWnames)

forLordif(JPN_1,JPN_2,FDnames)  #FD5,FD9,FD17
forLordif(JPN_1,JPN_2,SDnames)  #SD2
forLordif(JPN_1,JPN_2,UPnames)  #UP11,UP27
forLordif(JPN_1,JPN_2,FWnames)


#forLordif(UK_ENG,US_ENG,FDnames) 
#forLordif(UK_ENG,US_ENG,SDnames)
#forLordif(UK_ENG,US_ENG,UPnames) 
#forLordif(UK_ENG,US_ENG,FWnames)




########################################
############   plot corr   #############
########################################


# Create data frames for correlation values and confidence intervals
# for ENG data (BFI and MLOC)
n_row <- 5 * 4
Eng_corr_BFI2FAD <- data.frame(
  Study = c("BFI_E", "BFI_E", "BFI_E", "BFI_E", 
            "BFI_A", "BFI_A", "BFI_A", "BFI_A", 
            "BFI_C", "BFI_C", "BFI_C", "BFI_C", 
            "BFI_N", "BFI_N", "BFI_N", "BFI_N", 
            "BFI_O", "BFI_O", "BFI_O", "BFI_O"),
  SubStudy = c("FD", "SD", "UP", "FW", 
               "FD", "SD", "UP", "FW",
               "FD", "SD", "UP", "FW", 
               "FD", "SD", "UP", "FW", 
               "FD", "SD", "UP", "FW"),
  Type = rep("ORG(ENG)", n_row),
  cor_value = rep(NaN, n_row),
  lowerCI = rep(NaN, n_row),
  upperCI = rep(NaN, n_row),
  ifout = rep(0, n_row),
  ifout_p = rep(0, n_row),
  stringsAsFactors = FALSE
)

Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_A" & Eng_corr_BFI2FAD$SubStudy == "FD", "cor_value"] <- 0.19
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_C" & Eng_corr_BFI2FAD$SubStudy == "FD", "cor_value"] <- -0.03
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_N" & Eng_corr_BFI2FAD$SubStudy == "FD", "cor_value"] <- 0.22
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_O" & Eng_corr_BFI2FAD$SubStudy == "FD", "cor_value"] <- 0.04
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_E" & Eng_corr_BFI2FAD$SubStudy == "FD", "cor_value"] <- -0.09
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_A" & Eng_corr_BFI2FAD$SubStudy == "SD", "cor_value"] <- -0.01
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_C" & Eng_corr_BFI2FAD$SubStudy == "SD", "cor_value"] <- -0.08
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_N" & Eng_corr_BFI2FAD$SubStudy == "SD", "cor_value"] <- -0.04
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_O" & Eng_corr_BFI2FAD$SubStudy == "SD", "cor_value"] <- 0.02
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_E" & Eng_corr_BFI2FAD$SubStudy == "SD", "cor_value"] <- 0.09
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_A" & Eng_corr_BFI2FAD$SubStudy == "UP", "cor_value"] <- 0.07
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_C" & Eng_corr_BFI2FAD$SubStudy == "UP", "cor_value"] <- -0.13
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_N" & Eng_corr_BFI2FAD$SubStudy == "UP", "cor_value"] <- 0.03
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_O" & Eng_corr_BFI2FAD$SubStudy == "UP", "cor_value"] <- 0.10
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_E" & Eng_corr_BFI2FAD$SubStudy == "UP", "cor_value"] <- 0.05
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_A" & Eng_corr_BFI2FAD$SubStudy == "FW", "cor_value"] <- 0.17
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_C" & Eng_corr_BFI2FAD$SubStudy == "FW", "cor_value"] <- -0.04
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_N" & Eng_corr_BFI2FAD$SubStudy == "FW", "cor_value"] <- -0.07
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_O" & Eng_corr_BFI2FAD$SubStudy == "FW", "cor_value"] <- 0.03
Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == "BFI_E" & Eng_corr_BFI2FAD$SubStudy == "FW", "cor_value"] <- 0.20

n_row <- 4*3
Eng_corr_MLOC2FAD <- data.frame(
  Study = c("MLOC_I", "MLOC_I", "MLOC_I", "MLOC_I", 
            "MLOC_P", "MLOC_P", "MLOC_P", "MLOC_P", 
            "MLOC_C", "MLOC_C", "MLOC_C", "MLOC_C"),
  SubStudy = c("FD", "SD", "UP", "FW", 
               "FD", "SD", "UP", "FW",
               "FD", "SD", "UP", "FW"),
  Type = rep("ORG(ENG)", n_row),
  cor_value = rep(NaN, n_row),
  lowerCI = rep(NaN, n_row),
  upperCI = rep(NaN, n_row),
  ifout = rep(0, n_row),
  ifout_p = rep(0, n_row),
  stringsAsFactors = FALSE
)

Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_I" & Eng_corr_MLOC2FAD$SubStudy == "FD", "cor_value"] <- -0.19
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_P" & Eng_corr_MLOC2FAD$SubStudy == "FD", "cor_value"] <- 0.27
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_C" & Eng_corr_MLOC2FAD$SubStudy == "FD", "cor_value"] <- 0.49
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_I" & Eng_corr_MLOC2FAD$SubStudy == "SD", "cor_value"] <- 0.14
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_P" & Eng_corr_MLOC2FAD$SubStudy == "SD", "cor_value"] <- 0.28
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_C" & Eng_corr_MLOC2FAD$SubStudy == "SD", "cor_value"] <- 0.16
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_I" & Eng_corr_MLOC2FAD$SubStudy == "UP", "cor_value"] <- 0.13
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_P" & Eng_corr_MLOC2FAD$SubStudy == "UP", "cor_value"] <- 0.05
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_C" & Eng_corr_MLOC2FAD$SubStudy == "UP", "cor_value"] <- 0.20
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_I" & Eng_corr_MLOC2FAD$SubStudy == "FW", "cor_value"] <- 0.35
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_P" & Eng_corr_MLOC2FAD$SubStudy == "FW", "cor_value"] <- -0.07
Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == "MLOC_C" & Eng_corr_MLOC2FAD$SubStudy == "FW", "cor_value"] <- -0.01


# Define unified variable names for FAD, BFI, and MLOC
fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

BFI_name <- c("BFI_E","BFI_A","BFI_C","BFI_N","BFI_O")
FAD_name <- c("FD", "SD", "UP", "FW")
MLOC_name <- c("MLOC_I","MLOC_P","MLOC_C")


# Function to calculate the correlation coefficient between two variables
# Input: x, y - two numeric vectors
# Output: correlation coefficient
calculate_r <- function(x, y) {
  cor(x, y, use = "complete.obs")
}

# Function to create a distribution of r values using bootstrap sampling
# Input: data - data frame, vars_xx - name of the first variable, vars_yx - name of the second variable, n_bootstrap - number of bootstrap samples
# Output: distribution of correlation coefficients
bootstrap_r_distribution <- function(data, vars_xx, vars_yx, n_bootstrap = 5000) {
  r_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    tryCatch({
      sampled_indices <- sample(1:length(data[[vars_xx]]), size = length(data[[vars_xx]]), replace = TRUE)
      sampled_newr_1 <- data[[vars_xx]][sampled_indices]
      sampled_newr_2 <- data[[vars_yx]][sampled_indices]
      r_values[i] <- calculate_r(sampled_newr_1, sampled_newr_2)
    }, error = function(e) {
      cat("Error in iteration", i, ": ", e$message, "\n")
    })
  }
  r_values
}

# Function to check if the raw correlation value is an outlier based on the bootstrap distribution
# Input: rawr - raw correlation value, r_distribution - bootstrap distribution of correlation coefficients, alpha - significance level
# Output: list indicating if the value is an outlier and its p-value
is_outlier <- function(rawr, r_distribution, alpha = 0.05) {
  lower_bound <- quantile(r_distribution, alpha/2)
  upper_bound <- quantile(r_distribution, 1 - alpha/2)
  p_value <- if (rawr < lower_bound) {
    ecdf(r_distribution)(rawr)
  } else if (rawr > upper_bound) {
    1 - ecdf(r_distribution)(rawr)
  } else {
    1
  }
  
  is_outlier_flag <- (rawr < lower_bound || rawr > upper_bound)
  
  return(list(is_outlier = as.numeric(is_outlier_flag), p_value = p_value))
}


# Function to create a data frame with correlation values and confidence intervals for BFI data
# Input: data - data frame, vars_x - vector of variable names for BFI, vars_y - vector of variable names for FAD
# Output: data frame with correlation values, confidence intervals, and outlier information
cor_with_ci_to_dataframe_BFI <- function(data, vars_x, vars_y) {
  result_df <- data.frame(
    Study = character(),
    SubStudy = character(),
    Type = character(),
    cor_value = numeric(),
    lowerCI = numeric(),
    upperCI = numeric(),
    stringsAsFactors = FALSE,
    ifout = numeric(),
    ifout_p = numeric()
  )
  
  for (var_xx in vars_x) {
    for (var_yx in vars_y) {
      
      
      # Calculate the bootstrap distribution of r values.
      r_distribution <- bootstrap_r_distribution(data, var_xx, var_yx)
      
      #cor_result <- cor.test(data[[var_xx]], data[[var_yx]])
      #cor_value <- cor_result$estimate
      #ci_low <- cor_result$conf.int[1]
      #ci_high <- cor_result$conf.int[2]
      
      # Calculate the mean, lowerCI, and upperCI from the bootstrap distribution.
      cor_value <- mean(r_distribution)
      ci_low <- quantile(r_distribution, 0.025)
      ci_high <- quantile(r_distribution, 0.975)
      lang_type <- substr(deparse(substitute(data)), 1, 3)
      
      
      rawr <- Eng_corr_BFI2FAD[Eng_corr_BFI2FAD$Study == var_xx & Eng_corr_BFI2FAD$SubStudy == var_yx, "cor_value"]
      result <- is_outlier(rawr, r_distribution)
      
      
      
      result_df <- rbind(result_df, data.frame(
        Study = var_xx,
        SubStudy = var_yx,
        Type = lang_type,
        cor_value = cor_value,
        lowerCI = ci_low,
        upperCI = ci_high,
        ifout = result$is_outlier,
        ifout_p = result$p_value
      ))
    }
  }
  
  return(result_df)
}


# Function to create a data frame with correlation values and confidence intervals for MLOC data
# Input: data - data frame, vars_x - vector of variable names for MLOC, vars_y - vector of variable names for FAD
# Output: data frame with correlation values, confidence intervals, and outlier information
cor_with_ci_to_dataframe_MLOC <- function(data, vars_x, vars_y) {
  result_df <- data.frame(
    Study = character(),
    SubStudy = character(),
    Type = character(),
    cor_value = numeric(),
    lowerCI = numeric(),
    upperCI = numeric(),
    stringsAsFactors = FALSE,
    ifout = numeric(),
    ifout_p = numeric()
  )
  
  for (var_xx in vars_x) {
    for (var_yx in vars_y) {
      
      # Calculate the bootstrap distribution of r values.
      r_distribution <- bootstrap_r_distribution(data, var_xx, var_yx)
      
      #cor_result <- cor.test(data[[var_xx]], data[[var_yx]])
      #cor_value <- cor_result$estimate
      #ci_low <- cor_result$conf.int[1]
      #ci_high <- cor_result$conf.int[2]
      
      # Calculate the mean, lowerCI, and upperCI from the bootstrap distribution.
      cor_value <- mean(r_distribution)
      ci_low <- quantile(r_distribution, 0.025)
      ci_high <- quantile(r_distribution, 0.975)
      lang_type <- substr(deparse(substitute(data)), 1, 3)
      
      
      rawr <- Eng_corr_MLOC2FAD[Eng_corr_MLOC2FAD$Study == var_xx & Eng_corr_MLOC2FAD$SubStudy == var_yx, "cor_value"]
      result <- is_outlier(rawr, r_distribution)
      
      
      result_df <- rbind(result_df, data.frame(
        Study = var_xx,
        SubStudy = var_yx,
        Type = lang_type,
        cor_value = cor_value,
        lowerCI = ci_low,
        upperCI = ci_high,
        ifout = result$is_outlier,
        ifout_p = result$p_value
      ))
    }
  }
  
  return(result_df)
}


#######BFI#########
# Rename columns for BFI data and calculate correlations for CHN data
name_mapping <- c("CHN_BFI_E" = "BFI_E", 
                  "CHN_BFI_A" = "BFI_A", 
                  "CHN_BFI_C" = "BFI_C", 
                  "CHN_BFI_N" = "BFI_N", 
                  "CHN_BFI_O" = "BFI_O")

colnames(CHN_BFI_FAD_Final) <- sapply(colnames(CHN_BFI_FAD_Final), function(var_name) {
  if (var_name %in% names(name_mapping)) {
    return(name_mapping[var_name])
  } else {
    return(var_name)
  }
})

CHN_corr_BFI2FAD <- cor_with_ci_to_dataframe_BFI(
  data = CHN_BFI_FAD_Final,
  vars_x = BFI_name,
  vars_y = FAD_name
)




#########MLOC#########
# Calculate MLOC scores and correlations for CHN data
re_CHN_FAD_MLOC <- reCHN_cal_data[,c("FD","SD","UP","FW",fadnames)]

CHN_MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
re_CHN_MLOC_I <- apply(reCHN_cal_data[,CHN_MLOC_INames]+3,1,sum)

CHN_MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
re_CHN_MLOC_P <- apply(reCHN_cal_data[,CHN_MLOC_PNames]+3,1,sum)


CHN_MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")
re_CHN_MLOC_C <- apply(reCHN_cal_data[,CHN_MLOC_CNames]+3,1,sum) 

re_CHN_MLOCS <- cbind(re_CHN_MLOC_I,re_CHN_MLOC_P,re_CHN_MLOC_C)

re_CHN_MLOC_FAD_Final <- cbind(re_CHN_MLOCS,re_CHN_FAD_MLOC)

name_mapping <- c("re_CHN_MLOC_I" = "MLOC_I", 
                  "re_CHN_MLOC_P" = "MLOC_P", 
                  "re_CHN_MLOC_C" = "MLOC_C")

colnames(re_CHN_MLOC_FAD_Final) <- sapply(colnames(re_CHN_MLOC_FAD_Final), function(var_name) {
  if (var_name %in% names(name_mapping)) {
    return(name_mapping[var_name])
  } else {
    return(var_name)
  }
})


CHN_corr_MLOC2FAD <- cor_with_ci_to_dataframe_MLOC(
  data = re_CHN_MLOC_FAD_Final,
  vars_x = MLOC_name,
  vars_y = FAD_name
)

CHN_corr_MLOC2FAD$Type <- rep("CHN", 12)



## FRN
# Rename columns for FRN data and calculate correlations

name_mapping <- c("Extraversion" = "BFI_E", 
                  "Agreabilite" = "BFI_A", 
                  "Conscience" = "BFI_C", 
                  "EmotionsNegatives" = "BFI_N", 
                  "Ouverture" = "BFI_O")

BFI_name <- c("BFI_E","BFI_A","BFI_C","BFI_N","BFI_O")

colnames(FRN_BFI) <- sapply(colnames(FRN_BFI), function(var_name) {
  if (var_name %in% names(name_mapping)) {
    return(name_mapping[var_name])
  } else {
    return(var_name)
  }
})


FRN_corr_BFI2FAD <- cor_with_ci_to_dataframe_BFI(
  data = FRN_BFI,
  vars_x = BFI_name,
  vars_y = FAD_name
)  

FRN_corr_MLOC2FAD <- CHN_corr_MLOC2FAD
FRN_corr_MLOC2FAD$Type <- rep("FRN", 12)

FRN_corr_MLOC2FAD$cor_value <- rep(NaN, 12)
FRN_corr_MLOC2FAD$lowerCI <- rep(NaN, 12)
FRN_corr_MLOC2FAD$upperCI <- rep(NaN, 12) 
FRN_corr_MLOC2FAD$ifout <- rep(NaN, 12)  
FRN_corr_MLOC2FAD$ifout_p <- rep(NaN, 12)  





### Plotting functions and parameters
library(ggplot2)
library(dplyr)

title_size <- 30
axis_text_size <- 25
legend_title_size <- 20
legend_text_size <- 20
point_size <- 12  
errorbar_height <- 1
facet_label_size_x <- 25
facet_label_size_y <- 25
axis_ticks_size <- 20

# Function to create and save a ggplot for correlation values
# Input: data_subset - data frame, file_name - name of the output file
# Output: saves the plot as a PDF file
plot_func <- function(data_subset, file_name) {
  p <- ggplot(data_subset, aes(x = cor_value, y = interaction(Study, Type, sep = "\n"), color = Type)) +
    geom_point(aes(shape = factor(shape_cond)), size = point_size) +  
    geom_errorbarh(aes(xmin = lowerCI, xmax = upperCI), height = errorbar_height) +
    scale_color_manual(values = color_mapping, name = 'Language') +
    scale_shape_manual(values = c('16' = 16, '1' = 1), name = "", 
                       labels = c('Significant difference','No significant difference from original')) +
    scale_x_continuous(limits = c(-0.5, 0.75)) +
    facet_grid(Study ~ SubStudy, scales = 'free', space = 'free_x', switch = 'y') +
    theme_minimal(base_size = 20) +  
    theme(
      plot.title = element_text(size = title_size),
      axis.title = element_text(size = axis_text_size),
      legend.title = element_text(size = legend_title_size),
      legend.text = element_text(size = legend_text_size),
      legend.position = 'bottom',
      strip.text.x = element_text(size = facet_label_size_x),
      axis.ticks.length = unit(0.5, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = axis_ticks_size),
      strip.text.y = element_text(size = facet_label_size_y),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    labs(title = 'Correlation Values by FAD-Plus, BFI, and MLOC',
         x = 'Person Correlation (r)',
         y = '') +
    guides(color = guide_legend(title = "Language"),
           shape = guide_legend(override.aes = list(size = 6)))  
  
  p <- p + geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 3)
  
  
  ggsave(file_name, plot = p, device = "pdf", width = 16, height = 12, units = "in")
}


# Combine and plot MLOC data
data <- rbind(CHN_corr_MLOC2FAD, 
              Eng_corr_MLOC2FAD,
              FRN_corr_MLOC2FAD)




# Define shape and color mappings
color_mapping <- c('ORG(ENG)' = '#56B4E9', 'FRN' = '#009E73', 'CHN' = '#E69F00') # Low saturation colors
shape_mapping <- c('FD' = 16, 'FW' = 16, 'UP' = 16, 'SD' = 16) 
data$shape_cond <- ifelse(data$ifout == 0, 16, 1)  #16 is solid, 1 is hollow


data$Study <- factor(data$Study, levels = c("MLOC_C", "MLOC_I", "MLOC_P"))
data$position <- as.numeric(factor(data$Type, levels = c("ORG(ENG)", "CHN", "FRN")))



plot_func(data[data$Study %in% MLOC_name, ], "MLOC_plot_V2.pdf")



# Combine and plot BFI data
data <- rbind(CHN_corr_BFI2FAD, 
              Eng_corr_BFI2FAD,
              FRN_corr_BFI2FAD)




# Define shape and color mappings
color_mapping <- c('ORG(ENG)' = '#56B4E9', 'FRN' = '#009E73', 'CHN' = '#E69F00') # Low saturation colors
shape_mapping <- c('FD' = 16, 'FW' = 16, 'UP' = 16, 'SD' = 16) 
data$shape_cond <- ifelse(data$ifout == 0, 16, 1)  #16 is solid, 1 is hollow


data$position <- as.numeric(factor(data$Type, levels = c("ORG(ENG)", "CHN", "FRN")))
data$Study <- factor(data$Study, levels = c("BFI_A", "BFI_C", "BFI_E", "BFI_N", "BFI_O"))

plot_func(data[data$Study %in% BFI_name, ], "BFI_plot_V2.pdf")


########################################
###############   NMF   ################
########################################
# install_github('linxihui/NNLM')
# devtools::install_github("zdebruine/RcppML")
pacman::p_unload("all")
pacman::p_load(NNLM, doParallel, foreach, gplots, ggplot2, reshape2, cluster, RcppML, cowplot, Matrix)


# Load data & filter useful columns
# Input: filename - string, name of the CSV file to load
# Output: data frame containing only the columns specified in fadnames
# Purpose: Load and filter relevant columns from the dataset

load_data <- function(filename) {
  fad <- read.csv(filename, encoding = "UTF-8", header = TRUE)
  fadnames <- c(
    "FD1","FD5","FD9","FD13","FD17",
    "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
    "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
    "FW4","FW8","FW12","FW16","FW21","FW23","FW26"
  )
  return(fad[, fadnames])
}

CHN_Clean <- load_data("CHN_230711.csv")
ENG_Clean <- load_data("ENG.csv")
FRN_Clean <- load_data("FRN.csv")
JPN_1_Clean <- load_data("JPN_1.csv")
JPN_2_Clean <- load_data("JPN_2.csv")

# Function to convert a data frame to a sparse matrix
# Input: df - data frame
# Output: sparse matrix of class dgCMatrix
# Purpose: Convert a data frame to a sparse matrix for crossValidate
convert_to_sparse <- function(df) {
  # Ensure the data frame is numeric
  df[] <- lapply(df, as.numeric)
  
  # Convert to a matrix
  mat <- as.matrix(df)
  
  # Convert the matrix to a sparse matrix of class dgCMatrix
  sparse_mat <- Matrix(mat, sparse = TRUE)
  
  return(sparse_mat)
}

# Function to perform cross-validation using the "impute" method, compute mean values, and create plots
# Input: data - data frame, data_name - string, name of the dataset
# Output: ggplot object representing the cross-validation results
# Purpose: Determine the optimal number of latent factors for NMF using cross-validation
# https://www.zachdebruine.com/post/cross-validation-for-nmf-rank-determination/
# https://stats.stackexchange.com/questions/111205/how-to-choose-an-optimal-number-of-latent-factors-in-non-negative-matrix-factori
create_cv_plots_impute <- function(data, data_name) {
  data_sparse <- convert_to_sparse(data)
  
  cv_impute <- crossValidate(data_sparse, k = as.integer(1:9), method = "impute", reps = 1000, seed = 777)
  
  # Calculate mean values
  data_mean <- aggregate(value ~ k, data = cv_impute, FUN = mean)
  
  # Create plot with mean values
  plot_impute <- ggplot(data_mean, aes(x = k, y = value)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    ggtitle(data_name) + 
    scale_y_continuous(trans = "log10") + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(1, 9, by = 1)) +
    ylim(0.5, 1.3) # Set uniform y-axis limits
  
  return(plot_impute)
}

# Define datasets and names
datasets <- list(
  CHN_Clean = CHN_Clean,
  ENG_Clean = ENG_Clean,
  FRN_Clean = FRN_Clean,
  JPN_1_Clean = JPN_1_Clean,
  JPN_2_Clean = JPN_2_Clean
)
data_names <- names(datasets)

# Create plots for all datasets
all_plots <- lapply(seq_along(datasets), function(i) {
  create_cv_plots_impute(datasets[[i]], data_names[i])
})

# Combine plots into a 3x2 grid and add a main title
combined_plots <- plot_grid(
  plotlist = all_plots, 
  ncol = 3, 
  nrow = 2
)

main_title <- ggdraw() + 
  draw_label(
    "Cross Validation of Rank for All Datasets", 
    fontface = 'bold', 
    x = 0.5, 
    hjust = 0.5, 
    size = 20
  )

final_plot <- plot_grid(
  main_title, 
  combined_plots, 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

# Save the combined plot to a PDF file
pdf("cross_validation_results_impute_mean_2000.pdf", width = 15, height = 10)
print(final_plot)
dev.off()



# Function to perform NMF Analysis using NNLM
# Input: data - data frame, data_name - string, name of the dataset, title_prefix - string, prefix for plot titles, nruns - integer, number of iterations, method - string, NMF method, loss - string, loss function, best_k - integer, optimal number of factors
# Output: Saves heatmap plots of the NMF components to PDF files
# Purpose: Perform NMF analysis and visualize the results
nmf_analysis <- function(data, data_name, title_prefix, nruns, method, loss, best_k) {
  
  data_t <- t(data)
  
  # Use all cores except one for parallel computation
  numCores <- parallel::detectCores()
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  cat("Using", numCores, "cores for parallel computation\n")
  
  set.seed(777)
  
  # Perform NMF
  res_data <- NNLM::nnmf(data_t, k = 4, method = method, loss = loss, max.iter = nruns)
  
  # Standardize W to 0-1 range
  W <- res_data$W
  W_norm <- t(apply(W, 1, function(x) (x - min(x)) / (max(x) - min(x))))
  
  # Visualize and save basis map as heatmap
  heatmap.2(W_norm, trace = "none", col = bluered(100), margins = c(6, 6),
            main = paste('4 components of', title_prefix), cex.main = 1.5, dendrogram = "none", scale = "none", key.title = "Color Key")
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  # Perform NMF with the best k (based on cross-validation)
  res_data_best <- NNLM::nnmf(data_t, k = best_k, method = method, loss = loss, max.iter = nruns) #123
  
  # Standardize the best W matrix to 0-1 range
  W_best_norm <- t(apply(res_data_best$W, 1, function(x) (x - min(x)) / (max(x) - min(x))))
  
  # Visualize and save the best basis map
  heatmap.2(W_best_norm, trace = "none", col = bluered(100), margins = c(6,6),
            main = paste('Best Rank', best_k, ' of', title_prefix), cex.main = 1.5, dendrogram = "none", scale = "none", key.title = "Color Key")
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_best_NMF.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  stopCluster(cl)
}

nruns <- 1000
method <- "scd"
loss <- "mse"

# Run NMF analysis on all datasets
nmf_analysis(CHN_Clean, "CHN_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 3)
nmf_analysis(ENG_Clean, "ENG_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 4)
nmf_analysis(FRN_Clean, "FRN_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 4)
nmf_analysis(JPN_1_Clean, "JPN_1_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 3)
nmf_analysis(JPN_2_Clean, "JPN_2_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 3)

