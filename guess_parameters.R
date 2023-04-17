library(stringr)

library(readxl)
library(readstata13)
library(haven)

read_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  
  switch(file_extension,
         csv = {data <- read.csv(file_path)},
         txt = {data <- read.table(file_path, header = TRUE, sep = "", quote = "\"'", dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)},
         xls = {data <- read_excel(file_path)},
         xlsx = {data <- read_excel(file_path)},
         dta = {data <- read_dta(file_path)},
         sav = {data <- read_sav(file_path)},
         stop("Unsupported file format")
  )
  
  return(data)
}


library(stringr)

find_parameters <- function(repo_path, pattern = "d\\$") {
  files <- list.files(repo_path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  param_dict <- list()
  
  for (file in files) {
    script <- readLines(file)
    matches <- lapply(str_extract_all(script, paste0(pattern, "\\w+")), unique)
    matches <- matches[lengths(matches) > 0]
    
    if (length(matches) > 0) {
      for (match in matches) {
        standardized_name <- match[1]
        non_standard_names <- match[-1]
        
        if (standardized_name %in% names(param_dict)) {
          param_dict[[standardized_name]] <- unique(c(param_dict[[standardized_name]], non_standard_names))
        } else {
          param_dict[[standardized_name]] <- non_standard_names
        }
      }
    }
  }
  
  results <- data.frame(standardized_name = names(param_dict), 
                        raw_name = I(param_dict), stringsAsFactors = FALSE)
  return(results)
}

# Example usage
repo_path <- "~/Documents/codes/carob"
parameters <- find_parameters(repo_path)

write.table(parameters, "parameters.txt", sep = "\t")


f <- readLines(file)
regex_pattern <- 'd\\[,c\\((.*)\\]'

# Extract the matched words
qq <- grep(regex_pattern, f)
matched_words <- str_match(f[qq], regex_pattern)[, 2]
# Remove double quotes and split the string by commas
column_names <- strsplit(gsub('"', '', matched_words), split = ",\\s*")[[1]]


# get the codes for synthesis
extract_lines_between <- function(text, start_line, end_line) {
  lines <- strsplit(text, split = "\n")[[1]]
  start_index <- which(lines == start_line)
  end_index <- which(lines == end_line)
  if (length(start_index) == 0 || length(end_index) == 0) {
    return("Start or end line not found.")
  }
  extracted_lines <- lines[(start_index + 1):(end_index - 1)]
  return(paste(extracted_lines, collapse = "\n"))
}

start_line <- 'd <- carobiner::read.excel(f)'
end_line <- 'd$dataset_id <- dataset_id'

extracted_code <- extract_lines_between(f, start_line, end_line)
cat(extracted_code)

# now get all the column names to get an idea of the possible standardized names

# find the closest match
# Find the closest match using fuzzy matching
find_closest_match <- function(field_name, parameter_table) {
  # Implement fuzzy matching
  # Calculate string distance between the field_name and parameter_table
  distances <- stringdist::stringdist(field_name, parameter_table, method = "jw")
  
  # Get the index of the minimum distance
  min_index <- which.min(distances)
  
  # Suggest the closest match and ask for user confirmation
  suggested_match <- parameter_table[min_index]
  confirmation_prompt <- sprintf("Is '%s' the correct match for '%s'? (yes/no): ", suggested_match, field_name)
  user_confirmation <- get_user_input(confirmation_prompt)
  
  if (tolower(user_confirmation) == "yes") {
    # If the user confirms, return the suggested match
    return(suggested_match)
  } else {
    # If the user does not confirm, prompt them to select from the list of non-standard parameters
    cat("Please choose one of the following non-standard parameters:\n")
    cat(paste(parameter_table, collapse = "\n"))
    
    # Get user input and validate their choice
    while (TRUE) {
      selected_param <- get_user_input("\nEnter the exact non-standard parameter you would like to use: ")
      if (selected_param %in% parameter_table) {
        return(selected_param)
      } else {
        cat("Invalid selection. Please try again.\n")
      }
    }
  }
}

