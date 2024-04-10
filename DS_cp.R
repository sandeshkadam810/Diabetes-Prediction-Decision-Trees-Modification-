# Load necessary libraries
library(data.table)

# Load the dataset
data=read.csv('C:\\diabetes_test.csv')


# Function to calculate Gini index
calculate_gini <- function(target) {
  # Count occurrences of each class
  class_counts <- table(target)
  
  # Calculate the probability of each class
  class_probs <- class_counts / sum(class_counts)
  
  # Calculate Gini index using the custom formula
  gini <- 1 - sum(class_probs * (class_probs + 1) * (2 * class_probs + 1)) / 6
  
  return(gini)
}

# Function to calculate Gini impurity for a split
calculate_split_gini <- function(left_target, right_target) {
  n_left <- length(left_target)
  n_right <- length(right_target)
  total <- n_left + n_right
  
  # Calculate Gini index for left and right nodes
  gini_left <- calculate_gini(left_target)
  gini_right <- calculate_gini(right_target)
  
  # Calculate weighted Gini impurity for the split
  gini_impurity <- (n_left / total) * gini_left + (n_right / total) * gini_right
  
  return(gini_impurity)
}

# Function to find the best split
find_best_split <- function(data, target) {
  best_gini <- Inf
  best_feature <- NULL
  best_value <- NULL
  
  for (col in names(data)) {
    if (col != target) {
      values <- unique(data[[col]])
      
      for (value in values) {
        left_indices <- which(data[[col]] <= value)
        right_indices <- which(data[[col]] > value)
        
        # Check for empty splits
        if (length(left_indices) == 0 | length(right_indices) == 0) {
            # Skip this value if either split is empty
        }
        
        left_target <- data[[target]][left_indices]
        right_target <- data[[target]][right_indices]
        
        gini_impurity <- calculate_split_gini(left_target, right_target)
        
        if (gini_impurity < best_gini) {
          best_gini <- gini_impurity
          best_feature <- col
          best_value <- value
        }
      }
    }
  }
  
  return(list(feature = best_feature, value = best_value, gini = best_gini))
}




# Function to build decision tree
build_tree <- function(data, target, max_depth = 10, min_samples_split = 2, current_depth = 0) {
  if (length(unique(data[[target]])) == 1) {
    return(list(class = as.character(as.integer(round(mean(data[[target]]))))))  # Create leaf node with class label
  }
  
  if (nrow(data) < min_samples_split) {
    return(as.character(as.integer(round(mean(data[[target]])))))
  }
  
  if (current_depth >= max_depth) {
    return(as.character(as.integer(round(mean(data[[target]])))))
  }
  
  split <- find_best_split(data, target)
  
  if (is.null(split)) {
    return(as.character(as.integer(round(mean(data[[target]])))))
  }
  
  print(paste("Split Feature:", split$feature))
  print(paste("Split Value:", split$value))
  
  left_indices <- which(data[[split$feature]] <= split$value)
  right_indices <- which(data[[split$feature]] > split$value)
  
  print(paste("Left Indices:", left_indices))
  print(paste("Right Indices:", right_indices))
  
  left_data <- data[left_indices, ]
  right_data <- data[right_indices, ]
  
  return(list(
    feature = split$feature,
    value = split$value,
    left = build_tree(left_data, target, max_depth, min_samples_split, current_depth + 1),
    right = build_tree(right_data, target, max_depth, min_samples_split, current_depth + 1)
  ))
}




# Split data into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(data), 0.8*nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build decision tree
tree <- build_tree(train_data, "Diabetes_binary")
summary(tree)
str(tree)

# Check if tree exists before making predictions
if (exists("tree")) {
  # Make predictions
  predictions <- sapply(1:nrow(test_data), function(i) predict_tree(tree, test_data[i, ]))
  
  # Calculate accuracy
  accuracy <- sum(predictions == test_data$Diabetes_binary) / nrow(test_data)
  print(paste("Accuracy:", round(accuracy, 2)))
} else {
  print("Error: Decision tree not built.")
}
