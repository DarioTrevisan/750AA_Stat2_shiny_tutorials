game_logo_description <- "a logo opening splash screen for a game called Hello Neighbor (K-Nearest) insipired by by hello neighbor series game with aerial view sim city style of suburbs monochrome houses in various clusters of colours in backgound graphics games
"

game_description <- "# Welcome to Hello Neighbor (k-nearest)

Dive into the thrilling world of **Hello Neighbor (k-nearest)**, where you'll not only face challenges but also master one of the simplest yet powerful classification algorithms: the k-nearest neighbor (k-NN) method! In this engaging mini-game, your mission is to classify new data points using the wisdom of your closest *neighbors* from real-world datasets sampled from the MedDataSets R collection.

## Game Overview

In **Hello Neighbor (k-nearest)**, every decision counts! You’ll explore various values of *k* to determine how many nearest neighbors to consider when classifying new points. Your success hinges on your ability to apply the k-NN algorithm correctly—get it right, and you’ll gain lives; misclassify a point, and you won’t earn any lives at all! Missteps can be costly, and if you apply the algorithm incorrectly, you risk losing lives and ultimately facing game over.

### What Awaits You:

- **Real Datasets**: Tackle authentic datasets and apply your knowledge of k-NN in an interactive setting.
- **Strategic Choices**: Experiment with different values of *k* to see how they influence your classification results.
- **Visual Insights**: Utilize a dynamic graph that displays the mean errors over training points, cross-validation error, and mean error over test points. This visual feedback will help you grasp crucial concepts like the bias-variance tradeoff, guiding you in making strategic decisions.
- **Demo Mode**: Unleash your creativity in the demo mode, where you can draw your own datasets and test various *k* values to explore how they affect classification outcomes.
- **Help Tab**: Access additional information on the sampled dataset, helping you get acquainted with the features of real-world data and enhancing your understanding of the context you’re working within. Mind in particular the (possibly) different scales of the coordinates!


## Are You Ready?

Put your skills to the test and navigate the complexities of data classification in a fun, interactive way. Will you be able to classify correctly and rise to the top, or will your misclassifications lead to a swift end? Embrace the challenge in **Hello Neighbor (k-nearest)**, where learning meets adventure!"

# Function to sample a random dataset and get its description
sample_dataset <- function() {
  dataset_names <- ls("package:MedDataSets")  # Get all dataset names in the 'datasets' package
  valid_dataset <- NULL  # Initialize variable to store the valid dataset
  
  while (is.null(valid_dataset)) {
    # Sample a random dataset name
    random_name <- sample(dataset_names, 1)
    
    # Try to get the dataset and check for errors
    tryCatch({
      valid_dataset <- get(random_name)  # Attempt to retrieve the dataset
      #message(paste("Successfully sampled dataset:", random_name))
      # Return both the dataset and its name
      return(list("data" = valid_dataset, "name" = random_name))
    }, error = function(e) {
      #message(paste("Error with dataset:", random_name, "->", e$message))
      valid_dataset <<- NULL  # Ensure valid_dataset remains NULL to continue the loop
    })
  }
}

# sample a dataset with at least two numeric columns and one factor (for classification)

sample_data_frame_numeric_class <- function() {
  #   all <-  ls("package:datasets")
  m = TRUE
  while (m) {
    n = TRUE
    while (n) {
      ## sample until we find a dataset with at least two numeric columns and one factor
      data <- sample_dataset()
      dataset <- na.omit(data$data)
      n <- !(sum(sapply(data$data, is.numeric)) > 1 &&
               sum(sapply(data$data, is.factor)) > 0)
    }
    #remove NA and select only one factor and two numeric columns
    dataset_tidy <- na.omit(cbind(data$data[sapply(data$data, is.numeric)], data$data[sapply(data$data, is.factor)]))
    dataset_numeric <- dataset_tidy[sapply(dataset_tidy, is.numeric)]
    dataset_numeric <- dataset_numeric[, sample(ncol(dataset_numeric), 2)]
    # choose only one factor column
    dataset_class <- dataset_tidy[sapply(dataset_tidy, is.factor)]
    dataset_class <- dataset_class[sample(ncol(dataset_class), 1)]
    # check if the number of levels is not too large, otherwise repeat
    num_factors <- length(levels(dataset_class[, 1]))
    if (num_factors < 7 && num_factors > 1)
      m <- FALSE
  }
  # join the columns to obtain the final dataset
  dataset <- cbind(dataset_numeric, dataset_class)
  ## return a list with data points (numeric), classes (factor, one sampled at random) and original name
  list("data" = dataset, "name" = data$name)
}


# example
