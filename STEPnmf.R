# NMF Analysis for FAD Plus
# Using Brunet's Standard NMF based on the Kullback-Leibler divergence
# It uses simple multiplicative updates from Lee et al. (2001), enhanced to avoid numerical underflow.

# Set the environment for analysis
rm(list = ls())

# install.packages("pacman") if not installed
if (!requireNamespace('pacman', quietly = TRUE)) {
      install.packages('pacman')
}

if (!requireNamespace('NMF', quietly = TRUE)) {
      install.packages('NMF', version = "0.27", repos = 'http://cran.r-project.org')
}

# pay attention to the message from biocManager, it will ask you to update the packages or not.
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")

#   BiocManager::install("NMF")
#   BiocManager::install("Biobase")


# Load the necessary package
pacman::p_load(BiocManager, Biobase, NMF, doParallel, foreach)

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


CHN_Clean <- load_data(here::here("3_2_3_Save_points","CHN_230711.csv"))
ENG_Clean <- load_data(here::here("3_2_3_Save_points", "ENG.csv"))
FRN_Clean <- load_data(here::here("3_2_3_Save_points", "FRN.csv"))
JPN_1_Clean <- load_data(here::here("3_2_3_Save_points", "JPN_1.csv"))
JPN_2_Clean <- load_data(here::here("3_2_3_Save_points", "JPN_2.csv"))


# Function to perform NMF Analysis
nmf_analysis <- function(data, data_name, title_prefix, nruns) {
  data_t <- t(data)
  
  # I am using windows with 14900k but only with 96GB memory, 
  # so I am not going to use all cores
  numCores <- parallel::detectCores() - 2  # Use all cores except two
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  cat("Using", numCores, "cores for parallel computation\n")
  
  
  options_string <- paste("p", numCores, sep="")  
  
  # if you are using macOS
  # register(MulticoreParam(workers = parallel::detectCores() - 2))
  # I am using windows with 14900k, so I cll SnowParam()
  # bp_param <- SnowParam(workers = parallel::detectCores() - 2, type = "SOCK")
  # register(bp_param)

  res_data <- NMF::nmf(data_t, 
                       rank = 4,
                       method = "brunet",
                       nrun=nruns, 
                       seed=777,
                       #.opt='v',
                       .options = list(backend = "doParallel",
                                       num_threads = 6,
                                       verbose = TRUE)
                       )
  
  #pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
  NMF::basismap(res_data, main = paste('4 components of', title_prefix))
  grDevices::curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  estim_data <- NMF::nmf(data_t, 
                         2:9, 
                         nrun=nruns, 
                         seed=777)
  
  
  plot(estim_data, main = paste('NMF estim of', title_prefix))
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_estim.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  coph <- estim_data$measures$cophenetic
  coph_diff <- c(diff(coph))
  k.best <- which.max(coph_diff) + 1
  
  
  res_data_best <- NMF::nmf(data_t, 
                            k.best, 
                            nrun=nruns, 
                            seed=777) 
  
  
  basismap(res_data_best, main = paste('Best NMF is ',k.best,' of',title_prefix))
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_best_NMF.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  
  stopCluster(cl)
}

nruns <- 200
nmf_analysis(CHN_Clean, "CHN_Clean", "FADplus_NMF", nruns)
nmf_analysis(ENG_Clean, "ENG_Clean", "FADplus_NMF", nruns)
nmf_analysis(FRN_Clean, "FRN_Clean", "FADplus_NMF", nruns)
nmf_analysis(JPN_1_Clean, "JPN_1_Clean", "FADplus_NMF", nruns)
nmf_analysis(JPN_2_Clean, "JPN_2_Clean", "FADplus_NMF", nruns)



## consensus_map
nmf_consensus_map <- function(data, data_name, title_prefix, rank_range, nruns) {
  data_t <- t(data)
  
  numCores <- parallel::detectCores() - 2  # Use all cores except two
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  cat("Using", numCores, "cores for parallel computation\n")
  
  nmf_fit <- NMF::nmf(data_t, 
                      rank=rank_range, 
                      nrun=nruns, 
                      seed=777) 
  
  pdf(paste0(title_prefix, "_", data_name, "_consensus_map.pdf"))
  consensusmap(nmf_fit, main=paste0('Consensus Map for ', data_name))
  dev.off()
  
  stopCluster(cl)
  
}
rank_range <- 2:9
nmf_consensus_map(CHN_Clean, "CHN_Clean", "FADplus_NMF", rank_range, nruns)
nmf_consensus_map(ENG_Clean, "ENG_Clean", "FADplus_NMF", rank_range, nruns)
nmf_consensus_map(FRN_Clean, "FRN_Clean", "FADplus_NMF", rank_range, nruns)
nmf_consensus_map(JPN_1_Clean, "JPN_1_Clean", "FADplus_NMF", rank_range, nruns)
nmf_consensus_map(JPN_2_Clean, "JPN_2_Clean", "FADplus_NMF", rank_range, nruns)







## no use ##
## no use ##
## no use ##
Half_Clean <- load_data(here::here("3_2_3_Save_points","halftestnmf.csv"))

##
data <- Half_Clean
data_t <- t(data)


title_prefix <- "FADplus_NMF"
data_name <- "Half_Clean"

res_data <- NMF::nmf(data_t, 4, nrun=1000, seed=777, .opt="vP31")
#pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
basismap(res_data, main = paste('4 components of', title_prefix))
curr_plot <- recordPlot()
pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
replayPlot(curr_plot)
dev.off()

estim_data <- NMF::nmf(data_t, 2:9, nrun=1000, seed=777, .opt="vP31")
plot(estim_data, main = paste('NMF estim of', title_prefix))
curr_plot <- recordPlot()
pdf(paste0(title_prefix, "_", data_name, "_estim.pdf"))
replayPlot(curr_plot)
dev.off()

coph <- estim_data$measures$cophenetic
coph_diff <- c(diff(coph))
k.best <- which.max(coph_diff) + 1


res_data_best <- NMF::nmf(data_t, k.best,nrun=1000,  seed=777)
basismap(res_data_best, main = paste('Best NMF is ',k.best,' of',title_prefix))
curr_plot <- recordPlot()
pdf(paste0(title_prefix, "_", data_name, "_best_NMF.pdf"))
replayPlot(curr_plot)
dev.off()

