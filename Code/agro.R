library(rvest)
library(dplyr)
library(chromote)

OUTPUT_FILE_PATH <- "Results/agro.csv"

homepage <- read_html("https://www.agrobank.com.my/product/takaful")

urls <- 
  homepage %>% 
  html_nodes(".related_pages ul li a") %>%
  html_attr("href")

# Visit product pages and scrape data points

results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(results) <- c("company", "product_type", "product_name", "product_description", "insurance_type")

takaful_mentioned <- function(content, word) {
  any(grepl(word, content))
}

is_pdf <- function(link) {
  grepl("\\.pdf($|#)", link, ignore.case = TRUE)
}

for (i in 1:length(urls)){

  full_link <- paste0("https://www.agrobank.com.my", urls[i])
  
  if (is_pdf(full_link)) {
    warning(sprintf("Skipping PDF file: %s", full_link))
    next
  }
  
  html <- read_html(full_link)
  
  content <- html %>%
    html_nodes(".content") %>%
    html_text2() %>%
    tolower()

  if(takaful_mentioned(content, "takaful")){
    product_name <- html %>%
      html_nodes(".page-title h2") %>%
      html_text()
    
    product_desc <- html %>%
      html_nodes(".content h2") %>%
      html_text2() %>%
      paste(collapse = " ")
    
    new_row <- data.frame(company = "Syarikat Takaful", product_type = "To be filled", product_name = product_name, product_description = product_desc, insurance_type = "To be filled")
  
    results <- rbind(results, new_row)
    
    new_row <- NA
  } else if (takaful_mentioned(tolower(urls[i]), "takaful")) {
    product_name <- html %>%
      html_nodes(".page-title h2") %>%
      html_text()
    
    product_desc <- html %>%
      html_nodes(".content h2") %>%
      html_text2() %>%
      paste(collapse = " ")
    
    new_row <- data.frame(company = "Syarikat Takaful", product_type = "To be filled", product_name = product_name, product_description = product_desc, insurance_type = "To be filled")
    
    results <- rbind(results, new_row)
    
    new_row <- NA
  }
  
}  

# Manual add skipped PDF file (For now, only Agro Takaful Flexi is a pdf file)
# results <- rbind(results, data.frame(company = "Syarikat Takaful", product_type = "To be filled", product_name = "Agro Takaful Flexi", product_description = "To your family, you are everything. We understand that securing their lives and
#                                      future is crucial, regardless of any unexpected life events.", insurance_type = "To be filled"))

# Enhance this part to compare the results with last week's result. New product's product_type and insurance_type have to be filled in manually (if any).
date_value2 <- Sys.Date() - 7

last_weeks_month <- format(date_value2, "%m")
last_weeks_year <- as.numeric(format(date_value2, "%Y"))
last_weeks_week <- ceiling(as.numeric(format(date_value2, "%d")) / 7)
last_week_path <- paste("Results", last_weeks_month, last_weeks_year, "Week", last_weeks_week, sep = "_")
last_week_csv <- read.csv(paste0(last_week_path, "/agro.csv"))

this_week_names <- tolower(results$product_name)
last_week_names <- tolower(last_week_csv$product_name)
last_week_names <- head(last_week_names, -1)

for (i in 1:nrow(results)) {
  # Check if the current product name exists in last week's data
  if (this_week_names[i] %in% last_week_names) {
    # Find the corresponding row in last week's data
    last_week_row <- last_week_csv[last_week_names == this_week_names[i], ]
    
    # Fill in the company, product_type, and insurance_type if they are "To be filled"
    if (results$company[i] == "To be filled") {
      results$company[i] <- last_week_row$company
    }
    
    if (results$product_type[i] == "To be filled") {
      results$product_type[i] <- last_week_row$product_type
    }
    
    if (results$insurance_type[i] == "To be filled") {
      results$insurance_type[i] <- last_week_row$insurance_type
    }
  } else {
    # Handle case where product is not found in last week's data
    results$company[i] <- ifelse(results$company[i] == "To be filled", NA, results$company[i])
    results$product_type[i] <- ifelse(results$product_type[i] == "To be filled", NA, results$product_type[i])
    results$insurance_type[i] <- ifelse(results$insurance_type[i] == "To be filled", NA, results$insurance_type[i])
  }
}

  
# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)