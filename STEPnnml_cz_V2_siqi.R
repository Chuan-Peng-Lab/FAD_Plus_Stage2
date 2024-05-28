# library(devtools)
# install_github('linxihui/NNLM')

# Set the environment for analysis
rm(list = ls())
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)

pacman::p_load(NNLM,doParallel,foreach,gplots,ggplot2,reshape2,cluster)

# Load data & filter useful columns
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

CHN_Clean <- load_data(here::here("3_2_3_Save_points","CHN_230711.csv"))
ENG_Clean <- load_data(here::here("3_2_3_Save_points","ENG.csv"))
FRN_Clean <- load_data(here::here("3_2_3_Save_points","FRN.csv"))
JPN_1_Clean <- load_data(here::here("3_2_3_Save_points","JPN_1.csv"))
JPN_2_Clean <- load_data(here::here("3_2_3_Save_points","JPN_2.csv"))

# Function to perform NMF Analysis using NNLM
nmf_4dim <- function(data, data_name, title_prefix, nruns, method, loss, k_range) {
      data_t <- t(data)
      
      # Validate input data
      if (any(is.na(data_t))) stop("Input data contains NA values.")
      
      # Validate k_range
      if (!is.numeric(k_range) || any(k_range <= 0)) stop("k_range must be a positive numeric vector.")
      
      # Use all cores except one for parallel computation
      numCores <- parallel::detectCores() - 1
      cl <- makeCluster(numCores)
      registerDoParallel(cl)
      cat("Using", numCores, "cores for parallel computation\n")
      
      set.seed(777)
      
      # Perform NMF using NNLM with specified method and loss function
      res_data <- NNLM::nnmf(data_t, k = 4, method = method, loss = loss, max.iter = nruns)
      
      # Standardize W to 0-1 range
      W <- res_data$W
      W_norm <- t(apply(W, 1, function(x) (x - min(x)) / (max(x) - min(x))))
      
      # Visualize and save basis map as heatmap
      heatmap.2(W_norm, trace = "none", col = bluered(100), margins = c(6, 6),
                main = paste('4 components of', title_prefix), cex.main = 1.5, 
                dendrogram = "none", scale = "none", key.title = "Color Key")
      curr_plot <- recordPlot()
      pdf(paste0(title_prefix, "_", data_name, "_4_components.pdf"))
      replayPlot(curr_plot)
      dev.off()
      stopCluster(cl)
}

nmf_bestrank <- function(data, data_name, title_prefix, nruns, method, loss, k_range) {
  data_t <- t(data)
  
  # Validate input data
  if (any(is.na(data_t))) stop("Input data contains NA values.")
  
  # Validate k_range
  if (!is.numeric(k_range) || any(k_range <= 0)) stop("k_range must be a positive numeric vector.")
  
  # Use all cores except one for parallel computation
  numCores <- parallel::detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  cat("Using", numCores, "cores for parallel computation\n")
  
  set.seed(777)
  
  # Estimating the best number of components
  estim_data <- lapply(k_range, function(k) {
    replicate(nruns, NNLM::nnmf(data_t, k = k, method = method, loss = loss, max.iter = nruns), simplify = FALSE)
  })
  
  # Initialize lists to store measures
  cophenetic <- numeric(length(k_range))
  dispersion <- numeric(length(k_range))
  evar <- numeric(length(k_range))
  residuals <- numeric(length(k_range))
  rss <- numeric(length(k_range))
  sparseness <- numeric(length(k_range))
  
  for (i in seq_along(estim_data)) {
    nmf_runs <- estim_data[[i]]
    k <- k_range[i]
    
    # Calculate consensus matrix
    consensus_matrix <- matrix(0, nrow = nrow(data_t), ncol = ncol(data_t))
    for (run in nmf_runs) {
      W <- run$W
      H <- run$H
      clusters <- apply(H, 2, which.max)
      for (c in unique(clusters)) {
        members <- which(clusters == c)
        
        # Debugging: Print members and consensus_matrix dimensions
        print(paste("k =", k, "clusters =", c, "members =", members))
        print(paste("consensus_matrix dimensions =", dim(consensus_matrix)))
        
        # Check if members are within bounds
        if (any(members > nrow(consensus_matrix)) || any(members < 1)) {
          print(paste("Error: members out of bounds. k =", k, "members =", members))
          next
        }
        consensus_matrix[members, members] <- consensus_matrix[members, members] + 1
      }
    }
    consensus_matrix <- consensus_matrix / nruns
    
    # Calculate cophenetic correlation
    dist_matrix <- as.dist(1 - consensus_matrix)
    hc <- hclust(dist_matrix)
    
    # Ensure dimensions match before calculating correlation
    cophenetic_matrix <- cophenetic(hc)
    data_dist_matrix <- as.dist(1 - cor(t(data_t)))  # Changed to use correlation distance
    
    if (length(cophenetic_matrix) == length(data_dist_matrix)) {
      cophenetic[i] <- cor(cophenetic_matrix, data_dist_matrix, method = "pearson")
    } else {
      cat("Dimension mismatch: cophenetic_matrix length =", length(cophenetic_matrix), 
          "data_dist_matrix length =", length(data_dist_matrix), "\n")
      cophenetic[i] <- NA
    }
    
    # Calculate other measures
    dispersion[i] <- sd(as.vector(consensus_matrix)) / mean(as.vector(consensus_matrix))
    consensus <- nmf_runs[[1]]$W %*% nmf_runs[[1]]$H
    evar[i] <- sum((data_t - consensus)^2) / sum(data_t^2)
    residuals[i] <- sum((data_t - consensus)^2)
    rss[i] <- sum((data_t - consensus)^2)
    sparseness[i] <- sum(nmf_runs[[1]]$W != 0) / length(nmf_runs[[1]]$W)
  }
  
  # Combine measures into a data frame
  measures <- data.frame(
    k = k_range,
    cophenetic = cophenetic,
    dispersion = dispersion,
    evar = evar,
    residuals = residuals,
    rss = rss,
    sparseness = sparseness
  )
  
  # Melt data for ggplot2
  measures_melt <- melt(measures, id.vars = "k")
  
  # Plot measures
  ggplot(measures_melt, aes(x = k, y = value, color = variable)) +
    geom_line() +
    geom_point() +
    facet_wrap(~variable, scales = "free_y") +
    ggtitle(paste('NMF estimation of', title_prefix)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, hjust = 0.5))
  
  pdf(paste0(title_prefix, "_", data_name, "_estim.pdf"))
  print(ggplot(measures_melt, aes(x = k, y = value, color = variable)) +
          geom_line() +
          geom_point() +
          facet_wrap(~variable, scales = "free_y") +
          ggtitle(paste('NMF estimation of', title_prefix)) +
          theme_minimal())
  dev.off()
  
  # Determine the best k based on the cophenetic correlation
  coph_diff <- c(diff(measures$cophenetic))
  k.best <- k_range[which.max(coph_diff) + 1]
  
  # Perform NMF with the best k
  res_data_best <- NNLM::nnmf(data_t, k = k.best, method = method, loss = loss, max.iter = nruns)
  
  # Standardize the best W matrix to 0-1 range
  W_best_norm <- t(apply(res_data_best$W, 1, function(x) (x - min(x)) / (max(x) - min(x))))
  
  # Visualize and save the best basis map
  heatmap.2(W_best_norm, trace = "none", col = bluered(100), margins = c(6,6),
            main = paste('Best NMF is', k.best, 'components of', title_prefix), cex.main = 1.5, dendrogram = "none", scale = "none", key.title = "Color Key")
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_best_NMF.pdf"))
  replayPlot(curr_plot)
  dev.off()
  
  stopCluster(cl)
}

nruns <- 2000
method <- "scd"
loss <- "mse"
k_range <- 2:9

# for test function
# data <- CHN_Clean
# data_name <- "CHN_Clean"
title_prefix <- "FADplus_NNLM"

# run NMF with 4-factor
nmf_4dim(CHN_Clean, "CHN_Clean", "FADplus_NNLM", nruns, method, loss, k_range)

nmf_analysis(CHN_Clean, "CHN_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(ENG_Clean, "ENG_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(FRN_Clean, "FRN_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(JPN_1_Clean, "JPN_1_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(JPN_2_Clean, "JPN_2_Clean", "FADplus_NNLM", nruns, method, loss, k_range)