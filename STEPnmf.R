# NMF Analysis for FAD Plus
# Using Brunet's Standard NMF based on the Kullback-Leibler divergence
# It uses simple multiplicative updates from Lee et al. (2001), enhanced to avoid numerical underflow.

# Set the environment for analysis
rm(list = ls())

# install.packages("pacman") if not installed
if (!requireNamespace('pacman', quietly = TRUE)) {
      install.packages('pacman')
}

curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)

# pay attention to the message from biocManager, it will ask you to update the packages or not.
if (!requireNamespace('Biobase', quietly = TRUE)) {
      BiocManager::install("Biobase", force = TRUE)
}

# Load the necessary package
pacman::p_load(BiocManager, Biobase, NMF)

# Load data & filter useful columns
load_data <- function(filename) {
  fad <- read.csv(filename, encoding = "UTF-8", header = T)
  fadnames <- c(
    "FD1","FD5","FD9","FD13","FD17",
    "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
    "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
    "FW4","FW8","FW12","FW16","FW21","FW23","FW26"
  )
  return(fad[, fadnames])
}

# need to check the file names
CHN_Clean <- load_data(here::here("3_2_3_Save_points","CHN_Cleaned.csv"))
ENG_Clean <- load_data(here::here("3_2_3_Save_points", "ENG_Cleaned.csv"))
FRN_Clean <- load_data(here::here("3_2_3_Save_points", "FRN_Cleaned.csv"))
JPN_1_Clean <- load_data(here::here("3_2_3_Save_points", "JPN_1_Cleaned.csv"))
JPN_2_Clean <- load_data(here::here("3_2_3_Save_points", "JPN_2_Cleaned.csv"))

# Function to perform NMF Analysis
nmf_analysis <- function(data, data_name, title_prefix) {
  data_t <- t(data)
  
  res_data <- NMF::nmf(data_t, 4, nrun=500, seed=777, .opt="vP31")
  #pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
  basismap(res_data, main = paste('4 components of', title_prefix))
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  estim_data <- NMF::nmf(data_t, 2:9, nrun=500, seed=777, .opt="vP31")
  plot(estim_data, main = paste('NMF estim of', title_prefix))
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_estim.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  coph <- estim_data$measures$cophenetic
  coph_diff <- c(diff(coph))
  k.best <- which.max(coph_diff) + 1
  
  
  res_data_best <- NMF::nmf(data_t, k.best, seed=9527)
  basismap(res_data_best, main = paste('Best NMF is ',k.best,' of',title_prefix))
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_best_NMF.pdf"))
  replayPlot(curr_plot)
  dev.off()
}


nmf_analysis(CHN_Clean, "CHN_Clean", "FADplus_NMF")
nmf_analysis(ENG_Clean, "ENG_Clean", "FADplus_NMF")
nmf_analysis(FRN_Clean, "FRN_Clean", "FADplus_NMF")
nmf_analysis(JPN_1_Clean, "JPN_1_Clean", "FADplus_NMF")
nmf_analysis(JPN_2_Clean, "JPN_2_Clean", "FADplus_NMF")


## test
consensusmap(estim_data, annCol=data_t, labCol=NA, labRow=NA)

##
Half_Clean <- load_data(here::here("3_2_3_Save_points","halftestnmf.csv"))

##
data <- Half_Clean
data_t <- t(data)


title_prefix <- "FADplus_NMF"
data_name <- "Half_Clean"

res_data <- NMF::nmf(data_t, 4, nrun=500, seed=9527, .opt="vP31")

#pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
basismap(res_data, main = paste('4 components of', title_prefix))

curr_plot <- recordPlot()
pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
replayPlot(curr_plot)
dev.off()

estim_data <- NMF::nmf(data_t, 2:9, nrun=500, seed=9527, .opt="vP31")
plot(estim_data, main = paste('NMF estim of', title_prefix))
curr_plot <- recordPlot()
pdf(paste0(title_prefix, "_", data_name, "_estim.pdf"))
replayPlot(curr_plot)
dev.off()

coph <- estim_data$measures$cophenetic
coph_diff <- c(diff(coph))
k.best <- which.max(coph_diff) + 1


res_data_best <- NMF::nmf(data_t, k.best,nrun=500,  seed=9527)
basismap(res_data_best, main = paste('Best NMF is ',k.best,' of',title_prefix))
curr_plot <- recordPlot()
pdf(paste0(title_prefix, "_", data_name, "_best_NMF.pdf"))
replayPlot(curr_plot)
dev.off()

