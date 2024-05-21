# options(repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# install.packages('devtools', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
# library(devtools)
# install.packages('RcppProgress', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
# install.packages('gplots', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
# devtools::install_github('linxihui/NNLM')

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
nmf_analysis <- function(data, data_name, title_prefix, nruns, method, loss, k_range) {
  data_t <- t(data)
  
  # Use all cores except one for parallel computation
  numCores <- parallel::detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  cat("Using", numCores, "cores for parallel computation\n")
  
  set.seed(9527)
  
  # Perform NMF using NNLM with specified method and loss function
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
  
  # Estimating the best number of components
  estim_data <- lapply(k_range, function(k) {
    set.seed(777)
    NNLM::nnmf(data_t, k = k, method = method, loss = loss, max.iter = nruns)
  })
  
  # Initialize lists to store measures
  cophenetic <- numeric(length(k_range))
  dispersion <- numeric(length(k_range))
  evar <- numeric(length(k_range))
  residuals <- numeric(length(k_range))
  rss <- numeric(length(k_range))
  # silhouette <- numeric(length(k_range))
  sparseness <- numeric(length(k_range))
  
  for (i in seq_along(estim_data)) {
    H <- estim_data[[i]]$H
    W <- estim_data[[i]]$W
    k <- k_range[i]
    
    # Calculate cophenetic correlation
    # Computed as the Spearman correlation between the consensus matrix (obtained from 𝑊×𝐻) and the original data matrix
    # The best rank is usually where the cophenetic correlation shows a significant drop (determined by the largest difference)
    consensus <- W %*% H
    cophenetic[i] <- cor(as.vector(consensus), as.vector(data_t), method = "spearman")
    
    # Calculate dispersion
    # Measures the variability within the data clusters. Lower dispersion indicates tighter clusters.
    # Lower dispersion values are better, indicating more compact clusters.
    dispersion[i] <- sd(as.vector(consensus)) / mean(as.vector(consensus))
    
    # Calculate explained variance
    # Proportion of the total variance in the data explained by the model.
    # Higher explained variance indicates better fitting models.
    evar[i] <- sum((data_t - consensus)^2) / sum(data_t^2)
    
    # Calculate residuals
    # Sum of the squared differences between the original data and the reconstructed data.
    # Lower residual values indicate better model fit.
    residuals[i] <- sum((data_t - consensus)^2)
    
    # Calculate RSS
    # Similar to residuals, measures the discrepancy between the data and the model.
    # Lower RSS indicates a better fit.
    rss[i] <- sum((data_t - consensus)^2)
    
    # Calculate silhouette (a proxy calculation); not included
    # clusters <- apply(H, 2, which.max)
    # silhouette[i] <- mean(silhouette(clusters, dist(data_t))[, 3])
    
    # Calculate sparseness
    # Measures the sparsity of the factorization. High sparsity means fewer non-zero elements.
    # Depends on the context; higher sparsity might be desired in some applications.
    sparseness[i] <- sum(W != 0) / length(W)
  }
  
  # Combine measures into a data frame
  measures <- data.frame(
    k = k_range,
    cophenetic = cophenetic,
    dispersion = dispersion,
    evar = evar,
    residuals = residuals,
    rss = rss,
    #silhouette = silhouette,
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
    theme(plot.title = element_text(size = 14, hjust = 0.5)) # Adjust title size and position
  
  
  pdf(paste0(title_prefix, "_", data_name, "_estim.pdf"))
  print(ggplot(measures_melt, aes(x = k, y = value, color = variable)) +
          geom_line() +
          geom_point() +
          facet_wrap(~variable, scales = "free_y") +
          ggtitle(paste('NMF estimation of', title_prefix)) +
          theme_minimal())
  dev.off()
  
  # Determine the best k based on the cophenetic correlation
  # the rank after the largest increase in cophenetic correlation.
  # It assumes that the largest increase indicates a significant improvement in stability, and the next rank is where this improvement is maximized.
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
  
  
  # Determine the best k based on the cophenetic correlation before drop
  # find the rank just before the cophenetic correlation starts to level off or improve significantly
  k.best <- k_range[which.max(coph_diff)]
  
  # Perform NMF with the best k
  res_data_best <- NNLM::nnmf(data_t, k = k.best, method = method, loss = loss, max.iter = nruns)
  
  # Standardize the best W matrix to 0-1 range
  W_best_norm <- t(apply(res_data_best$W, 1, function(x) (x - min(x)) / (max(x) - min(x))))
  
  # Visualize and save the best basis map
  heatmap.2(W_best_norm, trace = "none", col = bluered(100), margins = c(6,6),
            main = paste('Best-1 NMF is', k.best, 'components of', title_prefix), cex.main = 1.5, dendrogram = "none", scale = "none", key.title = "Color Key")
  curr_plot <- recordPlot()
  pdf(paste0(title_prefix, "_", data_name, "_best-1_NMF.pdf"))
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
# title_prefix <- "FADplus_NNLM"


nmf_analysis(CHN_Clean, "CHN_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(ENG_Clean, "ENG_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(FRN_Clean, "FRN_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(JPN_1_Clean, "JPN_1_Clean", "FADplus_NNLM", nruns, method, loss, k_range)
nmf_analysis(JPN_2_Clean, "JPN_2_Clean", "FADplus_NNLM", nruns, method, loss, k_range)