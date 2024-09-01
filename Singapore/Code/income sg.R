library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/income sg.csv"

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

HOMEPAGE <- "https://www.income.com.sg/"

home <- read_html(HOMEPAGE)

category_url <- home %>%
  html_elements(".tile.notSquare.evgBoxIngp") %>%
  html_attr("href")

product_types <- home %>%
  html_nodes("a.tile.notSquare div h6") %>%
  html_text2()

URL_list <- data.frame(product_types = product_types, urls = category_url)

URL_list <- URL_list %>%
  mutate(urls = case_when(
    urls == "/promotions/motorcycle-insurance-promotion" ~ "/motorcycle-insurance",
    TRUE ~ urls
  ))

for (i in 1:nrow(URL_list)){
  
  url <- paste0("https://www.income.com.sg", URL_list$urls[i])
  
  html <- read_html(url)
  
  product_type <- URL_list$product_types[i]
  
  if (product_type %in% c("Travel Insurance", "Car Insurance", "Home Insurance", "Domestic Helper Insurance", "Pet Insurance", "Motorcycle Insurance")){
    
    product_name <- html %>%
      html_elements(".container-fluid.article.bnr-article") %>%
      html_nodes("h1") %>%
      html_text2()
    
    product_desc <- html %>%
      html_elements(".text-center") %>%
      html_text2() %>%
      .[1]
    
  } else if(product_type == "Motorcycle Insurance"){
    
    product_name <- ""
  }
  
  else{
    
    product_name <- html %>%
      html_nodes("figcaption h4") %>%
      html_text2()
    
    product_desc <- html %>%
      html_elements("p.tes") %>%
      html_text2()
    
  }
  
  results <- rbind(results, data.frame(insurer = "Income", bank_name = "-", product_name = product_name, product_type = product_type, product_description = product_desc, insurance_type = ""))
}

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Health Insurance" ~ "H&P",
    product_type == "Personal Accident Insurance" ~ "H&P",
    product_type == "Life Insurance" ~ "Life",
    product_type == "Travel Insurance" ~ "Travel",
    product_type == "Car Insurance" ~ "Motor",
    product_type == "Savings & Investments" & (grepl("link", tolower(product_name)) | grepl("invest", tolower(product_name))) ~ "Investment",
    product_type == "Savings & Investments" & !(grepl("link", tolower(product_name)) | grepl("invest", tolower(product_name))) ~ "Savings",
    product_type == "Home Insurance" ~ "Home",
    product_type == "Domestic Helper Insurance" ~ "Misc.",
    product_type == "Group Insurance" ~ "To be filled",
    product_type == "Commercial Insurance" ~ "To be filled",
    product_type == "Group Insurance for Schools & MOE Personnel" ~ "To be filled",
    product_type == "Motorcycle Insurance" ~ "Motor",
    product_type == "Pet Insurance" ~ "Misc.",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "H&P", "Savings", "Investment") ~ "Life Insurance",
    product_type %in% c("Travel", "Motor", "Home", "Property", "Liability", "Misc.") ~ "General Insurance",
    product_type == "To be filled" ~ "To be filled",
    TRUE ~ NA_character_
  ))

results <- results %>%
  group_by(product_name) %>%
  summarise(
    insurer = first(insurer),
    bank_name = first(bank_name),
    product_type = paste(unique(product_type), collapse = ", "),
    product_description = paste(unique(product_description), collapse = ", "),
    insurance_type = paste(unique(insurance_type), collapse = ", ")
  )

results <- results[, c(2, 3, 4, 1, 5, 6)] # Rearrange columns, summarise() changes their order

# Enhance this part to compare the results with last week's result. New product's product_type and insurance_type have to be filled in manually (if any).
date_value2 <- Sys.Date() - 7

last_weeks_month <- format(date_value2, "%m")
last_weeks_year <- as.numeric(format(date_value2, "%Y"))
last_weeks_week <- ceiling(as.numeric(format(date_value2, "%d")) / 7)
last_week_path <- paste("Results", last_weeks_month, last_weeks_year, "Week", last_weeks_week, sep = "_")
last_week_csv <- read.csv(paste0(last_week_path, "/income sg.csv"))

this_week_names <- tolower(results$product_name)
last_week_names <- tolower(last_week_csv$product_name)
last_week_names <- head(last_week_names, -1)

for (i in 1:nrow(results)) {
  # Check if the current product name exists in last week's data
  if (this_week_names[i] %in% last_week_names) {
    # Find the corresponding row in last week's data
    last_week_row <- last_week_csv[last_week_names == this_week_names[i], ]
    
    # Fill in the product_type, and insurance_type if they are "To be filled"
    if (results$product_type[i] == "To be filled") {
      results$product_type[i] <- last_week_row$product_type
    }
    
    if (results$insurance_type[i] == "To be filled") {
      results$insurance_type[i] <- last_week_row$insurance_type
    }
  } else {
    # Handle case where product is not found in last week's data
    results$product_type[i] <- ifelse(results$product_type[i] == "To be filled", NA, results$product_type[i])
    results$insurance_type[i] <- ifelse(results$insurance_type[i] == "To be filled", NA, results$insurance_type[i])
  }
}

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
