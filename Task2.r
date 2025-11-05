
library(cluster)
library(ggplot2)
library(plotly)

#  Generate shell clusters 
generate_shell_clusters <- function(n_shells, k_per_shell, max_radius,
                                    noise_sd = 0.1, inner_radius = 0.5) {
  radius <- seq(from = inner_radius, to = max_radius, length.out = n_shells)
  shell_data <- matrix(NA, nrow = n_shells * k_per_shell, ncol = 3)
  shell_label <- integer(n_shells * k_per_shell)
  row_index <- 1
  for (i in seq_len(n_shells)) {
    r_i <- radius[i]
    for (j in 1:k_per_shell) {
      direction <- rnorm(3)
      direction <- direction / sqrt(sum(direction^2))
      r <- r_i + rnorm(1, mean = 0, sd = noise_sd)
      shell_data[row_index, ] <- direction * r
      shell_label[row_index] <- i
      row_index <- row_index + 1
    }
  }
  colnames(shell_data) <- c("x", "y", "z")
  return(list(data = shell_data, labels = shell_label, radius = radius))
}

#Plot 3D clusters 
shell_plotly <- function(shell_data, shell_label) {
  df <- data.frame(shell_data, cluster = factor(shell_label))
  p <- plot_ly(df, x = ~x, y = ~y, z = ~z, color = ~cluster, colors = "Set2",
               type = "scatter3d", mode = "markers", marker = list(size = 2.5)) %>%
    layout(scene = list(xaxis = list(title = "x"),
                        yaxis = list(title = "y"),
                        zaxis = list(title = "z")))
  return(p)
}

#  Spectral clustering
spectral_cluster <- function(x, k, d_threshold = 1.0) {
  n <- nrow(x)
  dist_mat <- as.matrix(dist(x))
  A <- matrix(0, n, n)
  A[dist_mat < d_threshold] <- 1
  
  deg <- rowSums(A)
  deg_adj <- ifelse(deg == 0, 1e-8, deg)
  D <- diag(1 / sqrt(deg_adj))
  L_sym <- diag(n) - D %*% A %*% D
  
  eigenv <- eigen(L_sym, symmetric = TRUE)
  chosen_idx <- head(order(eigenv$values), k)
  U <- eigenv$vectors[, chosen_idx, drop = FALSE]
  
  set.seed(123)
  kmean <- kmeans(U, centers = k, nstart = 20, iter.max = 50)
  return(list(cluster = as.integer(kmean$cluster)))
}

# Simulation wrapper 
simulation_spectral <- function(max_radius_values = seq(10, 0, by = -1),
                                n_shells = 4, k_per_shell = 100, noise_sd = 0.1,
                                d_threshold = 1.0) {
  output <- data.frame(max_radius = numeric(),
                       detected_k = integer(),
                       stringsAsFactors = FALSE)
  
  spectral_clusGap <- function(x, k) {
    result <- spectral_cluster(x, k, d_threshold)
    return(list(cluster = result$cluster))
  }
  
  for (R in max_radius_values) {
    cat("Running max_radius =", R, " (d_threshold =", d_threshold, ")\n")
    data_gen <- generate_shell_clusters(n_shells = n_shells, k_per_shell = k_per_shell,
                                        max_radius = R, noise_sd = noise_sd)
    X <- data_gen$data
    set.seed(123)
    gap_spec <- clusGap(X, FUN = spectral_clusGap, K.max = n_shells + 2)
    best_k <- maxSE(gap_spec$Tab[, "gap"], gap_spec$Tab[, "SE.sim"], method = "Tibs2001SEmax")
    output <- rbind(output, data.frame(max_radius = R, detected_k = best_k))
    cat("  -> estimated k =", best_k, "\n")
  }
  return(output)
}

#  Visualization 
plot_results <- function(res_df, true_k = 4) {
  p <- ggplot(res_df, aes(x = max_radius, y = detected_k)) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    geom_hline(yintercept = true_k, linetype = "dashed", color = "red") +
    labs(title = "Estimated number of clusters vs max_radius",
         subtitle = "d_threshold = 1.0",
         x = "Max radius",
         y = "Estimated cluster count") +
    theme_minimal(base_size = 14)
  return(p)
}

# Main execution
main_output <- simulation_spectral()
p_main <- plot_results(main_output, true_k = 4)
print(p_main)

failure_point <- function(resu, true_k = 4) {
  idx <- which(resu$detected_k < true_k)
  ifelse(length(idx) == 0, NA, resu$max_radius[min(idx)])
}

fail_r <- failure_point(main_output, true_k = 4)
cat("Failure radius =", fail_r, "\n")

#  Save results
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

write.csv(main_output, "results/task2_result.csv", row.names = FALSE)
ggsave("figures/task2_plot.png", plot = p_main, width = 7, height = 5, dpi = 300)

cat("✅ Task2 completed. Results saved to results/task2_result.csv, figure saved to figures/task2_plot.png\n")
