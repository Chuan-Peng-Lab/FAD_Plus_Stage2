rm(list = ls())
setwd("E:/Psy_Zchhh/NNU/FAD_New_Start/FAD_submission/final_submission/FAD_R1_0912/FAD_R1_0912/Stage 2-2/3_2_Analayses/3_2_Analayses/3_2_3_Save_points")

library(CTT)
library(dplyr)
library(psych)
library(stringr)
library(lavaan)
library(semPlot)
library(semTools)
library(ltm)



# for ENG
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











CHN <- read.csv("CHN_230711.csv")
re_CHN <- read.table("MLOC6_FAD_filtered.txt")
ENG <- read.csv("ENG.csv")
FRN <- read.csv("FRN.csv")
JPN_1 <- read.csv("JPN_1.csv")
JPN_2 <- read.csv("JPN_2.csv")

ALLdata <- list(CHN,ENG,FRN,JPN_1,JPN_2)

CHN1 <- CHN[which(CHN[,"datasetNO"]==1),] 
CHN2 <- CHN[which(CHN[,"datasetNO"]==2),] 
CHN3 <- rbind(CHN[which(CHN[,"datasetNO"]==3.1),],
              CHN[which(CHN[,"datasetNO"]==3.2),])


CHN3.2 <- CHN[which(CHN[,"datasetNO"]==3.2),] #in which will match the retest dataset
-
#存在未匹配的情况，原因可能是：
#最开始筛查发放retest信息的时候，只是简单的通过两个注意力检测check，后期有可能被有na值，重复值，或者其他条件给删掉了
#其中三个MLOC的回收回复为空，决定删掉不用这4个subjects的response
which(is.na(match(re_CHN[,"ID"],CHN3.2[,"ID"])))
reCHN_cal_data <- re_CHN[-c(48,64,72,111),]

#function用来匹配retestdata
findretest <- function(n){
  retest_loc <- match(n,CHN3.2[,"ID"])
  res <- CHN3.2[retest_loc,]
  return(res)
}
reCHN_ori_data <- t(mapply(findretest, reCHN_cal_data[,"ID"]))

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

BFI_name <- c("BFI_E","BFI_A","BFI_C","BFI_N","BFI_O")
FAD_name <- c("FD", "SD", "UP", "FW")
MLOC_name <- c("MLOC_I","MLOC_P","MLOC_C")


# Define a function to calculate the correlation coefficient between two variables.
calculate_r <- function(x, y) {
  cor(x, y, use = "complete.obs")
}

# Define a function to create a distribution of r values using bootstrap sampling.
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
#CHN
#function for BIG5 5 dimensions' scores
CHN_BFIS <- function(datos,nombre,key){
  databfi <- datos[,nombre]
  data_corrected <- matrix(t(apply(databfi,1,function(x){x*key})),ncol = length(key))
  datafinal <- na.exclude(data_corrected)
  bfiscores <- apply(datafinal, 1, function(x){sum(x)/length(key)})
  return(bfiscores)
}  

#外向性 (Extraversion): 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
CHN_BFI_Enames <- c("BFI1","BFI6", "BFI11", "BFI16", "BFI21", "BFI26", "BFI31", "BFI36", "BFI41", "BFI46", "BFI51", "BFI56")
CHN_BFI_Ekeys <- c(1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
CHN_BFI_E <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Enames,CHN_BFI_Ekeys)


#宜人性 (Agreeableness): 2, 7, 12R, 17R, 22R, 27, 32, 37R, 42R, 47R, 52, 57
CHN_BFI_Anames <- c("BFI2","BFI7", "BFI12", "BFI17", "BFI22", "BFI27", "BFI32", "BFI37", "BFI42","BFI47", "BFI52", "BFI57")
CHN_BFI_Akeys <- c(1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1)
CHN_BFI_A <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Anames,CHN_BFI_Akeys)


#尽责性 (Conscientiousness): 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
CHN_BFI_Cnames <- c("BFI3","BFI8", "BFI13", "BFI18", "BFI23", "BFI28", "BFI33", "BFI38", "BFI43","BFI48", "BFI53", "BFI58")
CHN_BFI_Ckeys <- c(-1, -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
CHN_BFI_C <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Cnames,CHN_BFI_Ckeys)


#负性情绪/神经质 (Negative Emotionality): 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
CHN_BFI_Nnames <- c("BFI4","BFI9", "BFI14", "BFI19", "BFI24", "BFI29", "BFI34", "BFI39", "BFI44","BFI49", "BFI54", "BFI59")
CHN_BFI_Nkeys <- c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
CHN_BFI_N <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Nnames,CHN_BFI_Nkeys)


#开放性 (Open-Mindedness): 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
CHN_BFI_Onames <- c("BFI5","BFI10", "BFI15", "BFI20", "BFI25", "BFI30", "BFI35", "BFI40", "BFI45","BFI50", "BFI55", "BFI60")
CHN_BFI_Okeys <- c(-1, 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
CHN_BFI_O <- CHN_BFIS(ALLdata[[1]],CHN_BFI_Onames,CHN_BFI_Okeys)

CHN_BFIS <- cbind(CHN_BFI_E,CHN_BFI_A,CHN_BFI_C,CHN_BFI_N,CHN_BFI_O)

CHN_BFI_FAD_Final <- cbind(CHN_BFIS,ALLdata[[1]][,c("FD","SD","UP","FW")])
#corCHN_BFI_FAD <- cor(CHN_BFI_FAD_Final[,c("CHN_BFI_E","CHN_BFI_A","CHN_BFI_C","CHN_BFI_N","CHN_BFI_O")],CHN_BFI_FAD_Final[,c("FD","SD","UP","FW")])

name_mapping <- c("CHN_BFI_E" = "BFI_E", 
                  "CHN_BFI_A" = "BFI_A", 
                  "CHN_BFI_C" = "BFI_C", 
                  "CHN_BFI_N" = "BFI_N", 
                  "CHN_BFI_O" = "BFI_O")

# Rename the columns in CHN_BFI_FAD_Final
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
#select retest cases with MLOC data



re_CHN_FAD_MLOC <- reCHN_cal_data[,c("FD","SD","UP","FW",fadnames)]

#calculate MLOC scores
CHN_MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
re_CHN_MLOC_I <- apply(reCHN_cal_data[,CHN_MLOC_INames]+3,1,sum)

CHN_MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
re_CHN_MLOC_P <- apply(reCHN_cal_data[,CHN_MLOC_PNames]+3,1,sum)


CHN_MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")
re_CHN_MLOC_C <- apply(reCHN_cal_data[,CHN_MLOC_CNames]+3,1,sum) 

re_CHN_MLOCS <- cbind(re_CHN_MLOC_I,re_CHN_MLOC_P,re_CHN_MLOC_C)

re_CHN_MLOC_FAD_Final <- cbind(re_CHN_MLOCS,re_CHN_FAD_MLOC)
#cor_re_CHN_MLOC_FAD <- cor(re_CHN_MLOC_FAD_Final[,c("re_CHN_MLOC_I","re_CHN_MLOC_P","re_CHN_MLOC_C","FD","SD","UP","FW")])


name_mapping <- c("re_CHN_MLOC_I" = "MLOC_I", 
                  "re_CHN_MLOC_P" = "MLOC_P", 
                  "re_CHN_MLOC_C" = "MLOC_C")

# Rename the columns in CHN_BFI_FAD_Final
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
fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
FRN_BFI <- FRN[-c(which(is.na(FRN[,"BFI_1"]))),]

name_mapping <- c("Extraversion" = "BFI_E", 
                  "Agreabilite" = "BFI_A", 
                  "Conscience" = "BFI_C", 
                  "EmotionsNegatives" = "BFI_N", 
                  "Ouverture" = "BFI_O")

BFI_name <- c("BFI_E","BFI_A","BFI_C","BFI_N","BFI_O")
# Rename the columns in CHN_BFI_FAD_Final
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





### version 8
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
    labs(title = 'Correlation Values by FAD+, BFI, and MLOC',
         x = 'Person Correlation (r)',
         y = '') +
    guides(color = guide_legend(title = "Language"),
           shape = guide_legend(override.aes = list(size = 6)))  
  
  p <- p + geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 3)
  
  
  ggsave(file_name, plot = p, device = "pdf", width = 16, height = 12, units = "in")
}

# Simulate data


## MLOC
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



## BFI
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
