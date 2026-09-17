pca_man_logo_description <- " logo opening splash screen for a game called Ms PCA-MAN insipired by pac man games with points and principal component analysis in background graphics 70s arcade games early 3D lines vectorial"

pca_man_description <- "# Welcome to Ms PCA-Man!

## Overview
Step into the exciting world of **Ms PCA-Man**, a unique mini-game designed to challenge your understanding of principal components and directional variance! This interactive game invites you to visually approximate the first principal component direction of real datasets from R's standard dataset collection. Whether you're a statistics enthusiast or a curious newcomer, Ms PCA-Man offers a fun and educational experience.

## Game Objectives
In Ms PCA-Man, you will:
- **Familiarize Yourself with Principal Components**: Learn about the concept of directional variance and how it relates to the principal component analysis (PCA) technique.
- **Engage with Real Datasets**: Explore various datasets, each presenting unique point cloud shapes and distributions. As you play, you’ll gain insights into the diverse structures of real-world data.
- **Compete for Accuracy**: Your score is based on how closely your estimated line aligns with the actual first principal component direction. The closer your approximation, the higher your score!

## Gameplay Features
- **Dynamic Datasets**: Choose from a selection of standard datasets, each with its own characteristics. Observe how different shapes and distributions affect your PCA challenges. 
- **Interactive Drawing Tool**: Use our drawing tool to create your own set of points! Experiment with different configurations and see how your choices impact the principal component's direction.
- **Visual Feedback**: As you draw, watch how the principal component changes in real-time. The game also displays a normal ellipse that approximates your data, helping you visualize the variance along various angles.
- **Directional Variance Display**: Gain deeper insights into your approximations with a visual representation of directional variance at different angles, allowing you to understand how variance is distributed in your data (demo only).
- **Dataset Descriptions**: Each dataset you encounter in the challenge comes with a detailed description in a separate tab. Take a pause from the game rush by clicking on the *Dataset Description* tab to learn more about standard R datasets and their contexts.


## Why Play Ms PCA-Man?
Ms PCA-Man is more than just a game; it’s a learning tool that bridges the gap between theory and practice in statistics. By engaging with real datasets and visualizing the effects of your choices, you’ll develop a stronger intuition for PCA and the significance of variance in data analysis.

## Get Started!
Are you ready to embark on your PCA adventure? Click the *Play Ms PCA-Man!* tab to dive into the world of Ms PCA-Man, or start with the *Demo*. Challenge yourself, track your progress, and become a PCA master!"


# Load necessary library

# Function to sample a random dataset and get its description
sample_dataset <- function() {
  dataset_names <- ls("package:datasets")  # Get all dataset names in the 'datasets' package
  valid_dataset <- NULL  # Initialize variable to store the valid dataset
  
  while (is.null(valid_dataset)) {
    # Sample a random dataset name
    random_name <- sample(dataset_names, 1)
    
    # Try to get the dataset and check for errors
    tryCatch({
      valid_dataset <- get(random_name)  # Attempt to retrieve the dataset
      message(paste("Successfully sampled dataset:", random_name))
      
      # Return both the dataset and its name
      return(list("data" =valid_dataset, "name" = random_name ))
    }, error = function(e) {
      message(paste("Error with dataset:", random_name, "->", e$message))
      valid_dataset <<- NULL  # Ensure valid_dataset remains NULL to continue the loop
    })
  }
}

sample_data_frame_numeric <- function(){
  #   all <-  ls("package:datasets")
  n =1
  while (n<2){
    #   # sample until we find a dataset with at least two numeric columns
    data<-sample_dataset()
    dataset <- na.omit(data$data)
    dataset <- dataset[sapply(dataset, is.numeric)]
    n <- ncol(dataset)
    if (is.null(n)){
      n<-0
    }
  }
  #   # further sample two random columns and return a list with dataset and original name
  list( "data" = dataset[, sample(n, 2)],  "name" = data$name) 
}



# function that computes the points in the bean

compute_pac_man <- function(M, mean){
 theta <- seq(0, 1, by=0.01)
  values <- numeric()
  pac_man <- data.frame(x=numeric(), y=numeric())

 for( i in theta){
   v<- c(cos(2*pi*i), sin(2*pi*i))
   sigma <- sqrt(sum(v * (M %*% v)))
   v <- data.frame( x=mean$x+sigma * v[1], y= mean$y+sigma*v[2] )
   pac_man <- rbind(pac_man, v)
 }
  pac_man
}


# 
# 
# # 
# sample <- sample_data_frame_numeric()
# dataset <- drop_na(sample$data)
# 
# name_dataset <- names(dataset) 
# 
# dataset <- scale(data.frame(x=dataset[,1], y=dataset[,2]))
# # 
# dataset_pca <- prcomp(dataset)$rotation[,1]
# # 
# M <- cov(dataset)
# 
# 
# 
# pac_man <- compute_pac_man(M)
# 
# plt<- ggplot(dataset, aes(x,y))+geom_point(color="blue")+
#   stat_ellipse(type="norm", geom="polygon", fill='green', color="black", alpha=0.5, level=0.39)+
#   geom_polygon(data=pac_man, fill="yellow", color="black", alpha=0.8)+
#   geom_point(aes(0,0), colour="black", size=4)+
#   geom_segment( aes(x=0, y=0, xend=dataset_pca[1], yend=dataset_pca[2]), arrow = arrow(type="closed"), color="red")+
#   labs(x= paste(name_dataset[1], " (standardized)"), y=paste(name_dataset[2], " (standardized)"))+
#   ggtitle(paste("From dataset: ", sample$name))
#   
# 
#  plt
# 
