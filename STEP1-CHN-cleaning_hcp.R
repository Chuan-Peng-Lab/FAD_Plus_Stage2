# datacleanning of FAD-Plus-newstart
#

rm(list = ls())

# options(repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/") # using the fastest mirror
# install.packages("pacman") if not installed
if (!requireNamespace('pacman', quietly = TRUE)) {
      install.packages('pacman')
}

# using pacman for loading packages
pacman::p_load(tidyverse, psych, CTT, lavaan, semPlot, semTools) 

# prepare colnames
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

names_used <- c("datasetNO","check1","check2","ID","gender","age","ethnic",
                "edu","SES","objSES","subSES","abroad","abroad_c","abroad_t",
                FADnames, BFInames, MLOCnames)

# define the data dir within the R Proj
data_dir <- here::here("3_2_1_Standardized_Data")

# load data
data_DZL <- read.csv(here::here(data_dir,"CHN_DZL.csv")) %>%
      dplyr::rename_with(~FADnames, .cols = starts_with("FAD")) %>%
      dplyr::rename_with(~BFInames, .cols = starts_with("BFI")) %>%
      dplyr::rename_with(~MLOCnames, .cols = starts_with("MLOC")) %>%
      dplyr::mutate(datasetNO = 1) %>%
      # rename columns
      dplyr::rename(check1 = 'check.1',
                    check2 = 'check.2',
                    ID = ResponseId,   
                    gender = "Q3.gender",
                    age = "Q4.age",
                    "ethnic" = "Q5.ethnic.groups" ,
                    "edu" = "Q6.educational.atta.",
                    "SES" = "Q7.SES.",
                    "objSES" = "Q8.objSES_1",
                    "subSES" = "Q9.subSES_1",
                    "abroad" = "Q10.abroad",
                    "abroad_c" = "Q11.abroad",
                    "abroad_t" = "Q12.abroad") %>%
      dplyr::select(all_of(names_used))

data_LJG <- read.csv(here::here(data_dir,"CHN_LJG.csv"))  %>%
      dplyr::rename_with(~FADnames, .cols = starts_with("FAD")) %>%
      dplyr::rename_with(~BFInames, .cols = starts_with("BFI")) %>%
      dplyr::rename_with(~MLOCnames, .cols = starts_with("MLOC")) %>%
      dplyr::mutate(datasetNO = 2) %>%
      # rename columns
      dplyr::rename(check1 = 'check.1',
                    check2 = 'check.2',
                    ID = "Q1.ID",   
                    gender = "Q3.gender",
                    age = "Q4.age",
                    "ethnic" = "Q5.ethnic.groups" ,
                    "edu" = "Q6.educational.atta.",
                    "SES" = "Q7.SES.",
                    "objSES" = "Q8.objSES_1",
                    "subSES" = "Q9.subSES_1",
                    "abroad" = "Q10.abroad",
                    "abroad_c" = "Q11.abroad",
                    "abroad_t" = "Q12.abroad") %>%
      dplyr::select(all_of(names_used))

data_HCP_S <- read.csv(here::here(data_dir,"CHN_HCP_Student.csv")) %>%
      dplyr::rename_with(~FADnames, .cols = starts_with("FAD")) %>%
      dplyr::rename_with(~BFInames, .cols = starts_with("BFI")) %>%
      dplyr::rename_with(~MLOCnames, .cols = starts_with("MLOC")) %>%
      dplyr::mutate(datasetNO = 3.1) %>%
      # rename columns
      dplyr::rename(check1 = 'check.1',
                    check2 = 'check.2',
                    ID = "Q1.ID",   
                    gender = "Q3.gender",
                    age = "Q4.age",
                    "ethnic" = "Q5.ethnic.groups" ,
                    "edu" = "Q6.educational.atta.",
                    "SES" = "Q7.SES.",
                    "objSES" = "Q8.objSES_1",
                    "subSES" = "Q9.subSES_1",
                    "abroad" = "Q10.abroad",
                    "abroad_c" = "Q11.abroad",
                    "abroad_t" = "Q12.abroad") %>%
      dplyr::select(all_of(names_used))

data_HCP_A <- read.csv(here::here(data_dir,"CHN_HCP_1_2.csv")) %>%
      dplyr::rename_with(~FADnames, .cols = starts_with("FAD")) %>%
      dplyr::rename_with(~BFInames, .cols = starts_with("BFI")) %>%
      dplyr::rename_with(~MLOCnames, .cols = starts_with("MLOC")) %>%
      dplyr::mutate(datasetNO = 3.2) %>%
      # rename columns
      dplyr::rename(check1 = 'check.1',
                    check2 = 'check.2',
                    ID = "Q1.ID",   
                    gender = "Q3.gender",
                    age = "Q4.age",
                    "ethnic" = "Q5.ethnic.groups" ,
                    "edu" = "Q6.educational.atta.",
                    "SES" = "Q7.SES.",
                    "objSES" = "Q8.objSES_1",
                    "subSES" = "Q9.subSES_1",
                    "abroad" = "Q10.abroad",
                    "abroad_c" = "Q11.abroad",
                    "abroad_t" = "Q12.abroad") %>%
      dplyr::select(all_of(names_used))

# replication data
re_data_HCP_A <- read.csv(here::here(data_dir,"CHN_retest_Q0804_MLOC.csv")) %>%
      dplyr::rename_with(~FADnames, .cols = starts_with("FAD")) %>%
      dplyr::rename_with(~BFInames, .cols = starts_with("BFI")) %>%
      dplyr::rename_with(~MLOCnames, .cols = starts_with("MLOC")) %>%
      dplyr::mutate(datasetNO = 9) %>% # replication data as 9
      # rename columns
      dplyr::rename(ID = "Q1.ID",   
                    gender = "Q3.gender",
                    age = "Q4.age",
                    "ethnic" = "Q5.ethnic.groups" ,
                    "edu" = "Q6.educational.atta.",
                    "SES" = "Q7.SES.",
                    "objSES" = "Q8.objSES_1",
                    "subSES" = "Q9.subSES_1",
                    "abroad" = "Q10.abroad",
                    "abroad_c" = "Q11.abroad",
                    "abroad_t" = "Q12.abroad") %>%
      dplyr::select(-c("Q139","times"))

# combine all first-test data
CHN_alldata_ori <- rbind(data_DZL, data_LJG, data_HCP_A, data_HCP_S) %>%
      dplyr::filter(check1 == "不合理" & check2 == 5) %>%
      dplyr::mutate(years = as.numeric(substring(str_remove_all(age,"[^0-9]"), 1, 4))) %>%
      # check birth year: CHN_alldata_ori$years[which(nchar(CHN_alldata_ori$years)!=4)];
      # CHN_alldata_ori$years[which(CHN_alldata_ori$years > 2023)]
      # if not with 4 digit, mutate use case_when
      dplyr::mutate( 
            years = case_when(years %in% 908 ~ 1990,
                              years %in%  69 ~ 1969,
                              years %in%  97 ~ 1997,
                              years %in% 9103 ~ 1991,
                              years %in%  9806 ~ 1998,
                              TRUE ~ years ),
            ) %>%
      dplyr::mutate(years = ifelse(years < 1900, NA, years),
                    age = 2023-years) %>%
      # recode FAD+' response to numeric
      dplyr::mutate_at(vars(starts_with("FW") | starts_with("FD") | starts_with("SD") | starts_with("UP")), 
                       ~ str_remove_all(., "[^0-9]")) %>%
      dplyr::mutate_at(vars(starts_with("FW") | starts_with("FD") | starts_with("SD") | starts_with("UP")), 
                       ~ as.numeric(.)) %>%
      dplyr::mutate(across(starts_with("BFI"), ~ recode(.x, 
                                                        "非常不同意" = '1',
                                                        "不太同意"= '2',
                                                        "态度中立"= '3',
                                                        "比较同意" = '4',
                                                        "非常同意"= '5'))) %>%
      dplyr::mutate_at(vars(starts_with("BFI")), ~ as.numeric(.)) %>%
      dplyr::mutate(across(starts_with("MLOC"), ~ recode(.x, 
                                                        "很不同意" = '-3',
                                                        "不同意" = "-2",
                                                        "不太同意"= '-1',
                                                        "态度中立" = '0',
                                                        "同意"= '1',
                                                        "比较同意" = '2',
                                                        "很同意"= '3'))) %>%
      dplyr::mutate_at(vars(starts_with("MLOC")), ~ as.numeric(.))

# ??
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

CHN_FAD_cleaned <- cleaning_OmissionSames(CHN_alldata_ori,FADnames)

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

lapply(dup_locs, showresponse, m=CHN_FAD_BFI_MLOC_cleaned)


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