
# Function to calculate the mode of a vector
get_mode <- function(v, bins = 10) {
  # Remove NA and infinite values
  v <- v[is.finite(v)]
  v <- na.omit(v)
  
  # Handle character or factor vectors
  if (is.character(v) || is.factor(v)) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
    
    # Handle numeric vectors
  } else if (is.numeric(v)) {
    if (length(unique(v)) > bins) {
      breaks <- seq(min(v), max(v), length.out = bins + 1)
      bin_counts <- cut(v, breaks, include.lowest = TRUE)
      return(levels(bin_counts)[which.max(tabulate(bin_counts))])
    } else {
      uniqv <- unique(v)
      return(uniqv[which.max(tabulate(match(v, uniqv)))])
    }
    
    # Handle logical vectors
  } else if (is.logical(v)) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
    
    # Handle date/time vectors
  } else if (inherits(v, c("IDate", "Date", "POSIXct"))) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
    
    # Unsupported data type
  } else {
    stop("Unsupported data type")
  }
}

# Function to summarize a dataframe
summarise_dataframe <- function(df) {
  gc()  # Garbage collection to free up memory
  
  df <- df %>% 
    mutate_if(is.character, ~ na_if(., ""))  # Replace empty strings with NA
  
  summary <- data.frame(
    Feature = character(),
    Min = character(),
    Max = character(),
    Mode = character(),
    Proportion_Missing = numeric(),
    Data_Type = character(),
    Levels = character(),
    stringsAsFactors = FALSE
  )
  
  suppressWarnings(
    for (col in colnames(df)) {
      data_type <- class(df[[col]])
      col_data <- df[[col]]
      
      if (any(data_type %in% c("numeric", "integer"))) {
        min_val <- as.character(round(min(col_data, na.rm = TRUE), 3))
        max_val <- as.character(round(max(col_data, na.rm = TRUE), 3))
        mode_val <- as.character(round(as.numeric(get_mode(col_data)), 3))
        levels_val <- NA
      } else if (any(data_type %in% c("factor", "character"))) {
        min_val <- NA
        max_val <- NA
        mode_val <- as.character(get_mode(col_data))
        levels_val <- paste(levels(as.factor(col_data)), collapse = ", ")
      } else if (any(data_type %in% c("POSIXct", "IDate", "Date"))) {
        min_val <- as.character(as.Date(min(col_data, na.rm = TRUE)))
        max_val <- as.character(as.Date(max(col_data, na.rm = TRUE)))
        mode_val <- as.character(as.Date(get_mode(col_data)))
        levels_val <- NA
      } else {
        min_val <- NA
        max_val <- NA
        mode_val <- NA
        levels_val <- NA
      }
      
      proportion_missing <- mean(is.na(col_data))
      
      summary <- rbind(summary, data.frame(
        Feature = col,
        Min = min_val,
        Max = max_val,
        Mode = mode_val,
        Proportion_Missing = proportion_missing,
        Data_Type = data_type,
        Levels = levels_val
      ))
    }
  )
  
  return(summary)
}




