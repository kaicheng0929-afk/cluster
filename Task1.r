

library(cluster)
library(ggplot2)

# Generate data
generate_hypercube_clusters <- function(n, k, side_length, noise_sd = 1.0) {
  center <- diag(rep(side_length, n)) # generate centres
  cluster_list <- list()
  for (i in 1:n) {
    cluster_point <- matrix(rnorm(k * n, mean = 0, sd = noise_sd), nrow = k, ncol = n)
    cluster_point <- cluster_point + matrix(rep(center[i, ], each = k), nrow = k)
    cluster_list[[i]] <- cluster_point
  }
  data <- do.call(rbind, cluster_list)
  return(data)
}

# Estimate number of clusters 
cluster_estimation <- function(data, max_K) {
  set.seed(1)
  gap <- clusGap(data,
                 FUN = function(x, k) kmeans(x, centers = k, nstart = 20, iter.max = 50),
                 K.max = max_K)
  best_k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")
  return(list(gap = gap, best_k = best_k))
}

#Simulation loop
simu_result <- data.frame(
  dimension = integer(),
  side_length = numeric(),
  esti_num = integer()
)

for (n in seq(6, 2, by = -1)) {
  cat(sprintf("Dimension = %d\n", n))
  for (L in 10:1) {
    data <- generate_hypercube_clusters(n, 100, L, 1.0)
    esti <- cluster_estimation(data, max_K = n + 2)
    simu_result <- rbind(
      simu_result,
      data.frame(dimension = n, side_length = L, esti_num = esti$best_k)
    )
    cat(sprintf("(n,L) = (%d, %d) → estimated number = %d\n", n, L, esti$best_k))
  }
}
# Plot results 
p1 <- ggplot(simu_result, aes(x = side_length, y = esti_num,
                              color = factor(dimension), group = dimension)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_reverse(breaks = 10:1) +
  geom_hline(aes(yintercept = dimension, color = factor(dimension)),
             linetype = "dashed", size = 0.8, alpha = 0.5) +
  labs(title = "Estimated Number of Clusters vs. Side Length",
       x = "Side Length",
       y = "Estimated Number of Clusters",
       color = "Dimension") +
  theme_minimal(base_size = 14)

print(p1)

#Save results 
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

write.csv(simu_result, "results/task1_result.csv", row.names = FALSE)
ggsave("figures/task1_plot.png", plot = p1, width = 7, height = 5, dpi = 300)

cat("✅ Task1 completed. Results saved to results/task1_result.csv, figure saved to figures/task1_plot.png\n")
