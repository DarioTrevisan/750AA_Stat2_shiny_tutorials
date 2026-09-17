final_clustering_logo_description <- ""

final_clustering_description <- "# Welcome to Final Clustering!

Embark on an exciting journey through the world of clustering techniques! In **Final Clustering**, you will explore various clustering methods, create custom datasets, and solve quests that challenge your understanding of these powerful algorithms. This game is designed for adventurers of all skill levels, from beginners to seasoned data explorers.

## Game Overview

In **Final Clustering**, you are free to explore an open world where you can craft your own datasets and apply different clustering algorithms. The game consists of two main parts:

### 1. Adventurer's Workshop

In the **Adventurer's Workshop** tab, you can:

- **Draw Points**: Create your own datasets by placing points in a canvas. Experiment with different shapes and distributions to see how they affect clustering results.
- **Load/Save Configurations**: Save your handcrafted datasets and load them later, allowing you to refine your quests and share your creations with fellow players.
- **Quests**: Explore various quests that guide you in applying clustering techniques. Note that while there are no automated checks for solutions, you are encouraged to verify your results through exploration and analysis.

### 2. Clusterize!

The **Clusterize!** tab is where the magic happens. Here, you can:

- **Test Different Methods**: Apply various clustering algorithms to your chosen dataset, including K-means, hierarchical clustering, and more.
- **Visualize Results**: View important metrics such as silhouette scores and dendrograms for hierarchical clustering. These visual aids will help you understand how well your clustering methods are performing.
- **Solve Quests**: Use the insights gained from your analyses to tackle the quests laid out in the Adventurer's Workshop.

### Extra Help Session

To assist you on your journey, an additional **Help** session is available. Here, you can access the main manual pages for the selected clustering methods, offering you guidance and deeper insights into their workings.

## Getting Started

To begin your adventure:

1. Navigate to the **Adventurer's Workshop** tab to create or load a dataset.
2. Explore the various quests and choose one that piques your interest.
3. Switch to the **Clusterize!** tab to apply clustering methods and analyze your results.
4. Use the **Help** session whenever you need clarification on clustering techniques.

## Conclusion

**Final Clustering** is not just a game; it's an educational tool designed to enhance your understanding of clustering algorithms in a fun and interactive way. As you embark on this adventure, remember that exploration and experimentation are key to mastering clustering techniques.

Happy clustering, brave adventurer!"

cluster_algos <- c(
  "K-means clustering" = "kmeans",
  "Partition Around Medoids (Euclidean)" = "pam",
  "Partition Around Medoids (Manhattan)" = "pam_manhattan",
  "Single linkage HC (agnes)" = "single",
  "Average linkage  HC (agnes)" = "average",
  "Complete linkage  HC (agnes)" = "complete",
  "DIvisive ANAlysis Clustering (diana)" = "diana"
)

# Function to get description of the clustering method
get_clustering_description <- function(method) {
  descriptions <- c(
    "kmeans" = "K-means clustering partitions data into K clusters based on distance to centroids.",
    "pam" = "Partition Around Medoids (Euclidean) uses actual data points as cluster centers.",
    "pam_manhattan" = "Partition Around Medoids (Manhattan) uses medoids based on Manhattan distance.",
    "single" = "Single linkage HC (agnes) clusters based on the shortest distance between clusters.",
    "average" = "Average linkage HC (agnes) clusters based on the average distance between points.",
    "complete" = "Complete linkage HC (agnes) clusters based on the farthest distance between clusters.",
    "diana" = "DIvisive ANAlysis Clustering (diana) starts with all points in one cluster and splits iteratively."
  )
  return(descriptions[[method]])
}

get_clustering_function <- function(method) {
  descriptions <- c(
    "kmeans" = "kmeans",
    "pam" = "pam",
    "pam_manhattan" = "pam",
    "single" = "agnes",
    "average" = "agnes",
    "complete" = "agnes",
    "diana" = "diana"
  )
  return(descriptions[[method]])
}

# algorithms based on trees

algos_with_dendro <- c("single", "average", "complete", "diana")


quest_list <- c(
  "Silhouette Challenge" = "Create a dataset where, for k=2 clusters, the average silhouette width is greater than 0.9.",
  "Dendrogram Divergence" = "Find a dataset where single linkage and complete linkage hierarchical clustering yield different clusters, even after relabeling.",
  "K-means Exploration" = "Generate a dataset where k=3 clusters computed with K-means clustering have a silhouette width averaging at least 0.7.",
  "Manhattan vs. Euclidean" = "Construct a dataset where the PAM algorithm using Manhattan distance results in different clusters compared to Euclidean distance.",
  "Dunn Index Showdown"= "Design a dataset where the Dunn Index is maximized (greater than 0.7) for k=4 clusters, demonstrating well-separated clusters.",
  "Inertia Inflation" = "Generate a dataset where the Within-Cluster Sum-of-Squares (inertia) for k=4 clusters is notably high (greater than 4), suggesting poor cluster compactness.",
  "Cluster Overlap Chaos" = " Create a dataset with overlapping clusters that results in a Dunn Index below 0.1, indicating poor separation.",
  "Outlier Detection" = "Design a dataset where outliers significantly affect the K-means clustering results, causing changes in cluster centroids.",
  "Cluster Size Variability" = "Create a dataset with k=2 evident clusters of complex structure such that K-means fails to capture them.",
  "Cluster Relabeling" = "Find a scenario where hierarchical clustering (single linkage) and K-means clustering yield the same clusters but require relabeling for consistency.",
  "High Silhouette with Noise" = "Generate a dataset where the average silhouette is high (> 0.8) despite the presence of evidently noisy points.",
  "Dendrogram Interpretation" = "Create a dataset where the dendrogram shows three clear clusters, and demonstrate how to interpret the merge points effectively.",
  "Cluster Overlap" = "Design a dataset with overlapping clusters and show that PAM can still accurately identify the correct clusters.",
  "Dynamic Clustering" = "Create a dataset where adding a single point dramatically changes the clusters identified by K-means.",
  "Comparative Analysis" = "Generate two datasets with the same shape but different distributions and compare the clustering results using K-means and PAM.",
  "Silhouette for HC" = "Find data points such that the average silhouette for clusters formed using hierarchical clustering exceeds 0.85.",
  "Cluster Consistency" = "Create a dataset where K-means and hierarchical clustering produce consistent results across multiple runs with different initializations.",
  "Diana vs. Agnes" = "Explore a dataset where DIANA and Agnes methods identify different clusters.",
  "Cluster Shape Exploration" = "Design a dataset with non-convex clusters and show how K-means fails to capture the true structure.",
  "Hierarchical Insights" = "Create a dataset where the dendrogram reveals nested clusters, and explain how this relates to the cluster structure.",
  "Agnes Method Comparison" = "Design a dataset where single linkage, average linkage, and complete linkage hierarchical clustering produce distinctly different clusters.",
  "Poor Silhouette Quality" = "Create a dataset where the average silhouette width for k=3 clusters is less than 0.2, indicating poor clustering quality.",
  "Cluster Shape Sensitivity" = "Generate a dataset with crescent-shaped clusters and demonstrate how K-means struggles to identify the correct cluster boundaries."
)


# Indicators

# withinclass sum of squares
wcss <- function( data_points_and_clusters){
  # transform clusters vector into factor 
  data_points <- data_points_and_clusters[,1:2]
  clusters <- factor(data_points_and_clusters[,3])
  wcss_computed <- 0
  for( column in colnames(data_points)){
    for(i in levels(clusters)){
      mean_cluster <- mean(data_points[clusters==i, column])
      # compute the sse for the cluster
      sse <- sum( (data_points[clusters==i, column] - mean_cluster)^2 )
      # add the sse to wcss
      wcss_computed <- wcss_computed +sse
    }
  }
  return(wcss_computed)
}
  

# Dunn Index

dunn_index <- function( data_points_and_cluster){
  # the third column contains the cluster labels
  data_points <- data_points_and_cluster[,1:2]
  clusters <- factor(data_points_and_cluster[,3])
  D <- dist(data_points, diag=TRUE, upper=TRUE)
  diam <- 0
  delta <- max(D)
  for( i in levels(clusters)){
    for(j in levels(clusters)){
      if(i != j){
        delta <- min(delta, min( D[clusters==i][clusters==j], na.rm=TRUE))
      } else{
        diam <- max(diam, max(D[clusters==i][clusters==j], na.rm=TRUE))
      }
      
      }
    }
  return( delta/diam )
}


  