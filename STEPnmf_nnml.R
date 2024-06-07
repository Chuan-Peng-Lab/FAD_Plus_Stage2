# Set the environment for analysis
rm(list = ls())
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)

options(repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/") # using the fastest mirror

# remotes::install_github("zdebruine/RcppML")

# Load necessary libraries
# library(devtools)
# install_github('linxihui/NNLM')
# devtools::install_github("zdebruine/RcppML")
pacman::p_load(NNLM, doParallel, foreach, gplots, ggplot2, reshape2, cluster, RcppML, cowplot, Matrix)

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

# Function to convert a data frame to a sparse matrix
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

# for test function
# data <- CHN_Clean
# data_name <- "CHN_Clean"
# title_prefix <- "FADplus_NNLM"


# Run NMF analysis on all datasets
nmf_analysis(CHN_Clean, "CHN_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 3)
nmf_analysis(ENG_Clean, "ENG_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 4)
nmf_analysis(FRN_Clean, "FRN_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 4)
nmf_analysis(JPN_1_Clean, "JPN_1_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 3)
nmf_analysis(JPN_2_Clean, "JPN_2_Clean", "FADplus_NNLM", nruns, method, loss, best_k = 3)
