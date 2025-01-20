#Does not work for now.

# Clustered Variance-Covariance Matrix Function
clustered_vcov_maxLik <- function(model, cluster) {
  # Extract scores (gradients of the log-likelihood)
  scores <- model$gradient
  
  # Ensure cluster is a factor
  cluster <- as.factor(cluster)
  unique_clusters <- levels(cluster)
  G <- length(unique_clusters)  # Number of clusters
  N <- nrow(scores)            # Number of observations
  k <- ncol(scores)            # Number of parameters
  
  # Check if clusters are valid
  if (G < 30) {
    warning("The number of clusters is small; clustered standard errors may be unreliable.")
  }
  
  # Compute cluster-level score sums
  cluster_sums <- matrix(0, nrow = G, ncol = k)
  for (i in seq_along(unique_clusters)) {
    cluster_indices <- which(cluster == unique_clusters[i])
    cluster_sums[i, ] <- colSums(scores[cluster_indices, , drop = FALSE])
  }
  
  # Compute clustered variance
  meat <- crossprod(cluster_sums) / G
  bread <- solve(-model$hessian / N)  # Standard variance-covariance scaling
  
  # Sandwich estimator
  vcov_clustered <- bread %*% meat %*% bread / N
  return(vcov_clustered)
}

# Apply the function to your maxLik result
city_clusters <- data$CITY
clustered_se <- clustered_vcov_maxLik(result_refined, cluster = city_clusters)

# Extract standard errors
standard_errors <- sqrt(diag(clustered_se))

# Display clustered standard errors
print("Clustered Standard Errors:")
print(standard_errors)
