### Run this to create the functions

## Function that returns the frequency of each repetition length
# Input: "dataset" = name of dataset to check
repetition_length <- function(dataset) {
  count_repeats <- function(row_values) {
    repeat_lengths <- rle(row_values)$lengths
    repeat_lengths[repeat_lengths > 1]
  }
  
  all_repeat_lengths <- lapply(1:nrow(dataset), function(row_index) {
    count_repeats(as.numeric(dataset[row_index, ]))
  })
  
  length_frequencies <- table(unlist(all_repeat_lengths))
  return(length_frequencies)
}



## Function that replaces repetitions with NA, except the first (few)
# Input: "dataset" = name of dataset to correct, "max_length" = the maximum repetition length
repetition_replace <- function(dataset, max_length) {
  check_values <- function(row_values) {
    n <- length(row_values)
    store_values <- rep(FALSE, n)
    
    i <- 1
    while (i <= (n - max_length + 1)) {
      if (all(row_values[i:(i + max_length - 1)] == row_values[i])) {
        j <- i + max_length
        while (j <= n && row_values[j] == row_values[i]) {
          store_values[j] <- TRUE
          j <- j + 1
        }
        i <- j
      } else {
        i <- i + 1
      }
    }
    return(store_values)
  }
  
  corrected_dataset <- t(apply(dataset, 1, function(row_values) {
    logical_vector <- check_values(as.numeric(row_values))
    row_values[logical_vector] <- NA
    return(row_values)
  }))
  
  return(as.data.frame(corrected_dataset))
}



## Function that calculates pressure (MPa) with given force (N) surface area (mm^2)
# Input: "dataset" = name of dataset to recalculate, "surface" = surface area in mm^2
force_to_pressure <- function(dataset, surface_area){
  recalculated <- dataset/surface_area
  
  return(recalculated)
}


## Function that calculates the mean & standard deviation per given amount of rows and columns
# Input: "dataset" = name of dataset to calculate, "n_rows" = number of rows per calculation, "n_columns" = number of columns per calculation
calc_mean <- function(dataset, n_rows, n_columns) {
  num_cols <- ncol(dataset) - (ncol(dataset) %% n_columns)
  num_rows <- nrow(dataset) - (nrow(dataset) %% n_rows)
  
  num_col_groups <- num_cols / n_columns
  num_row_groups <- num_rows / n_rows
  
  mean_matrix <- matrix(nrow = num_row_groups, ncol = num_col_groups)
  
  for (i in seq_len(num_row_groups)) {
    row_range <- (n_rows * (i - 1) + 1):(n_rows * i)
    
    for (j in seq_len(num_col_groups)) {
      col_range <- (n_columns * (j - 1) + 1):(n_columns * j)
      group_data <- as.matrix(dataset[row_range, col_range])
      
      mean_matrix[i, j] <- mean(group_data, na.rm = TRUE)
    }
  }
  
  rownames(mean_matrix) <- paste0("RowGroup", seq_len(num_row_groups))
  colnames(mean_matrix) <- paste0(seq_len(n_cols*num_col_groups-n_cols),"-", seq_len(n_cols*num_col_groups))
  
  mean_df <- as.data.frame(mean_matrix)
  
  return(mean_df)
}

## Function that calculates the mean & standard deviation per given amount of rows and columns
# Input: "dataset" = name of dataset to calculate, "n_rows" = number of rows per calculation, "n_columns" = number of columns per calculation
calc_sd <- function(dataset, n_rows, n_columns) {
  num_cols <- ncol(dataset) - (ncol(dataset) %% n_columns)
  num_rows <- nrow(dataset) - (nrow(dataset) %% n_rows)
  
  num_col_groups <- num_cols / n_columns
  num_row_groups <- num_rows / n_rows
  
  sd_matrix <- matrix(nrow = num_row_groups, ncol = num_col_groups)
  
  for (i in seq_len(num_row_groups)) {
    row_range <- (n_rows * (i - 1) + 1):(n_rows * i)
    
    for (j in seq_len(num_col_groups)) {
      col_range <- (n_columns * (j - 1) + 1):(n_columns * j)
      group_data <- as.matrix(dataset[row_range, col_range])
      
      sd_matrix[i, j] <- sd(group_data, na.rm = TRUE)
    }
  }
  
  rownames(sd_matrix) <- paste0("RowGroup", seq_len(num_row_groups))
  colnames(sd_matrix) <- paste0(seq_len(n_cols*num_col_groups-n_cols),"-", seq_len(n_cols*num_col_groups))
  
  sd_df <- as.data.frame(sd_matrix)
  
  return(sd_df)
}
