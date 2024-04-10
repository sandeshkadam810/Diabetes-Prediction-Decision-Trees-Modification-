# Load necessary libraries
library(rpart)
library(class)

# Load the sample data (replace with your actual data loading)
data=read.csv('C:\\diabetes_test.csv')

# Function to calculate Gini impurity
# Function to calculate Gini impurity
calculate_gini <- function(data, class_variable) {
  # Get class labels
  classes <- unique(data[, class_variable])
  
  # Count occurrences of each class
  class_counts <- table(data[, class_variable])
  
  # Calculate the probability of each class
  class_probs <- class_counts / sum(class_counts)
  
  # Calculate Gini impurity
  gini <-  gini <- 1 - sum(class_probs * (class_probs + 1) * (2 * class_probs + 1)) / 6
  return(gini)
}


# Function to find the best split
find_best_split <- function(data, feature, class_variable) {
  # Get unique values for the feature
  unique_values <- unique(data[, feature])
  
  # Initialize best split variables
  best_gini <- Inf
  best_split_value <- NA
  best_left_class <- NA
  best_right_class <- NA
  
  # Loop through unique values and find the best split
  for (value in unique_values) {
    # Split data based on the value
    left_data <- data[data[, feature] <= value, ]
    right_data <- data[data[, feature] > value, ]
    
    # Calculate Gini impurity for left and right nodes
    left_gini <- calculate_gini(left_data, class_variable)
    right_gini <- calculate_gini(right_data, class_variable)
    
    # Calculate weighted Gini impurity
    weighted_gini <- (nrow(left_data) / nrow(data)) * left_gini + 
      (nrow(right_data) / nrow(data)) * right_gini
    
    # Update best split if current split is better
    if (weighted_gini < best_gini) {
      best_gini <- weighted_gini
      best_split_value <- value
      best_left_class <- ifelse(nrow(left_data) == 0, NA, table(left_data[, class_variable]))
      best_right_class <- ifelse(nrow(right_data) == 0, NA, table(right_data[, class_variable]))
    }
  }
  
  return(list(best_gini = best_gini, best_split_value = best_split_value, 
              left_class = best_left_class, right_class = best_right_class))
}

# Build the decision tree
# Function to build the decision tree
build_tree <- function(data, class_variable, min_split = 2, max_depth = Inf) {
  # Check if all observations belong to the same class
  if (length(unique(data[, class_variable])) == 1) {
    # Create a leaf node with the majority class label
    return(list(node_type = "leaf", prediction = names(sort(table(data[, class_variable]), decreasing = TRUE))[1]))
  }
  
  # Check if maximum depth or minimum split criteria are met
  if (max_depth == 0 || nrow(data) < min_split) {
    # Create a leaf node with the majority class label
    return(list(node_type = "leaf", prediction = names(sort(table(data[, class_variable]), decreasing = TRUE))[1]))
  }
  
  # Initialize variables to store best split information
  best_split <- NULL
  best_gini <- Inf
  
  # Exclude the class variable from candidate features
  features <- setdiff(names(data), class_variable)
  
  # Loop through candidate features to find the best split
  for (feature in features) {
    split_info <- find_best_split(data, feature, class_variable)
    if (split_info$best_gini < best_gini) {
      best_gini <- split_info$best_gini
      best_split <- list(
        split_variable = feature,
        split_value = split_info$best_split_value,
        left_child_data = data[data[, feature] <= split_info$best_split_value, ],
        right_child_data = data[data[, feature] > split_info$best_split_value, ]
      )
    }
  }
  
  # Check if the best split was found
  if (is.null(best_split)) {
    # Create a leaf node with the majority class label
    return(list(node_type = "leaf", prediction = names(sort(table(data[, class_variable]), decreasing = TRUE))[1]))
  }
  
  # Recursively build left and right subtrees
  left_child <- build_tree(best_split$left_child_data, class_variable, min_split, max_depth - 1)
  right_child <- build_tree(best_split$right_child_data, class_variable, min_split, max_depth - 1)
  
  # Return a node representing the best split
  return(list(
    node_type = "split",
    split_variable = best_split$split_variable,
    split_value = best_split$split_value,
    left_child = left_child,
    right_child = right_child
  ))
}

# Now you can call this function to build your decision tree
tree <- build_tree(data, "Diabetes_binary")
summary(tree)
str(tree)
print(tree)


# Assuming your decision tree is already built and stored in the 'tree' variable
# Structure your new data in a data frame format
test_data <- data.frame(
  Diabetes_binary = 0,  # Replace with the actual class value if known, otherwise use NA
  HighBP = 1,
  HighChol = 1,
  CholCheck = 1,
  BMI = 40,
  Smoker = 1,
  Stroke = 0,
  HeartDiseaseorAttack = 0,
  PhysActivity = 0,
  Fruits = 0,
  Veggies = 1,
  HvyAlcoholConsump = 0,
  AnyHealthcare = 1,
  NoDocbcCost = 0,
  GenHlth = 5,
  MentHlth = 18,
  PhysHlth = 15,
  DiffWalk = 1,
  Sex = 0,
  Age = 9,
  Education = 4,
  Income = 3
)


# Function to predict using the decision tree
predict_tree <- function(tree, new_data) {
  predictions <- vector("character", length = nrow(new_data))  # Initialize vector to store predictions
  for (i in 1:nrow(new_data)) {
    current_node <- tree  # Start at the root node
    # Traverse the tree until reaching a leaf node
    while (!is.null(current_node$left_child) || !is.null(current_node$right_child)) {
      # Determine which branch to follow based on the split criterion
      if (new_data[i, current_node$split_variable] <= current_node$split_value) {
        current_node <- current_node$left_child
      } else {
        current_node <- current_node$right_child
      }
    }
    # Assign the prediction of the leaf node to the corresponding observation
    leaf_predictions <- current_node$prediction
    # Handle cases where the leaf node prediction is missing or empty
    if (length(leaf_predictions) == 0) {
      # If no prediction, assign NA
      predictions[i] <- NA
    } else if (length(leaf_predictions) == 1) {
      # If single prediction, assign it
      predictions[i] <- leaf_predictions
    } else {
      # If multiple predictions, select the most frequent one
      predictions[i] <- names(sort(table(leaf_predictions), decreasing = TRUE))[1]
    }
  }
  return(predictions)
}

# Make predictions using the implemented decision tree
#manual prediction test_predictions <- predict_tree(tree, test_data)
test_predictions <- predict_tree(tree, test_data)

# Display the predictions
print(test_predictions)
