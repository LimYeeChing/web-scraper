# If captcha error occurs, use html$view() to solve the captcha then rerun the code.
# read_html_live("https://www.zurich.com.my/insurance-products/protection")$view()

library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/zurich.csv"

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

product_types <- data.frame("product_type" = c("Protection", "Savings", "Investment"), 
                            "url" = c("https://www.zurich.com.my/en/insurance-products/protection", 
                                      "https://www.zurich.com.my/en/insurance-products/savings", 
                                      "https://www.zurich.com.my/en/insurance-products/investment"))

url_list <- vector(mode = "character")

# Scrape urls of sub-category pages

for (i in 1:nrow(product_types)) {
  html <- read_html_live(product_types$url[i])
  
  urls1 <-
    html %>% 
    html_elements(".component.col-md-4") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls2 <- 
    html %>%
    html_elements(".component.col-md-6") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls <- c(urls1, urls2)
  
  url_list <- c(urls, url_list)
  
  if (length(urls) == 0) {
    stop("An error occurred. Likely Zurich Website Captcha.", call. = FALSE, .exit = 1)
  }

}

url_list <- paste0("https://www.zurich.com.my", unique(url_list))

# Loop through all product sub-category, visiting each page and scraping info

for (j in 1:length(url_list)) {
  html <- read_html_live(url_list[j])
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements("h5#headline.card-headline") %>%
    html_text2()

  # Scrape product descriptions
 
  product_description <-
    html %>%
    html_elements(".card-body") %>%
    html_text2()
  
  # Obtain product category from url 

  product_category <- gsub("https://www.zurich.com.my/insurance-products/", "", url_list[j])
  product_category <- gsub("/", ", ", product_category)
  product_category <- gsub("for-my-", "", product_category)
  product_category <- gsub(", future", "", product_category)

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_category, product_name, product_description)) 
  } else{
    stop("An error occurred. Likely Zurich Website Captcha.", call. = FALSE, .exit = 1)
  }
  
}

results$product_name[results$product_name == "Zurich"] <- "Zurich ValueLife Junior"

# Add insurance_type 

results <- results %>%
  mutate(insurance_type = case_when(
    product_type == "investment, family" ~ "Life Insurance",
    product_type == "investment" ~ "Life Insurance",
    product_type == "savings" ~ "Life Insurance",
    product_type == "protection, health" ~ "Life Insurance",
    product_type == "protection, life" ~ "Life Insurance",
    product_type == "protection, safety" ~ "General Insurance",
    product_type == "protection, family" ~ "TBD (Conventional)",
    product_type == "protection, property" ~ "General Insurance",
    product_type == "protection, vehicle" ~ "General Insurance",
    product_type == "protection, travel" ~ "General Insurance",
    product_type == "protection, golf" ~ "General Insurance",
    TRUE ~ "TBD"
  ))

# Scrape takaful products ----

takaful_results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(takaful_results) <- c("product_type", "product_name", "product_description")

takaful_product_types <- product_types 

takaful_product_types$url <- gsub("insurance", "takaful", takaful_product_types$url)

takaful_url_list <- vector(mode = "character")

# Scrape urls of sub-category pages

for (i in 1:nrow(takaful_product_types)) {
  html <- read_html_live(takaful_product_types$url[i])
  
  urls1 <-
    html %>% 
    html_elements(".component.col-md-4") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls2 <- 
    html %>%
    html_elements(".component.col-md-6") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls3 <- 
    html %>%
    html_elements(".component.col-10.col-sm-6.me-auto.ms-auto.me-sm-auto.ms-sm-auto") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls <- c(urls1, urls2, urls3)

  takaful_url_list <- c(urls, takaful_url_list)
  
  if (length(urls) == 0) {
    stop("An error occurred. Likely Zurich Website Captcha.", call. = FALSE, .exit = 1)
  }

}

takaful_url_list <- paste0("https://www.zurich.com.my", unique(takaful_url_list))

# Loop through all product sub-category, visiting each page and scraping info

for (url in takaful_url_list) {
  html <- read_html_live(url)
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements("h5#headline.card-headline") %>%
    html_text2()
  
  # Scrape product descriptions
  
  product_description <-
    html %>%
    html_elements(".card-body") %>%
    html_text2()
  
  # Obtain product category from url 

  product_type <- gsub("https://www.zurich.com.my/takaful-products/", "", url)
  product_type <- gsub("/", ", ", product_type)
  product_type <- gsub("for-my-", "", product_type)
  product_type <- gsub(", future", "", product_type)

  if (length(product_name) != 0 && length(product_description) != 0){
    takaful_results <- rbind(takaful_results, data.frame(product_type, product_name, product_description)) 
  } else{
    stop("An error occurred. Likely Zurich Website Captcha.", call. = FALSE, .exit = 1)
  }
  
}

takaful_results$product_name[takaful_results$product_name == "Z-HomeProtect"] <- "Z-HomeProtect Takaful"

takaful_results <- takaful_results %>%
  mutate(insurance_type = case_when(
    product_type == "investment, family" ~ "Family Takaful",
    product_type == "investment" ~ "Family Takaful",
    product_type == "savings" ~ "Family Takaful",
    product_type == "protection, health" ~ "Family Takaful",
    product_type == "protection, life" ~ "Family Takaful",
    product_type == "protection, safety" ~ "General Takaful",
    product_type == "protection, family" ~ "TBD (Takaful)",
    product_type == "protection, property" ~ "General Takaful",
    product_type == "protection, vehicle" ~ "General Takaful",
    product_type == "protection, travel" ~ "General Takaful",
    product_type == "protection, golf" ~ "General Takaful",
    TRUE ~ "TBD"
  ))

results <- rbind(results, takaful_results)

# Remove row with product_name "Enhance Your Coverage With Our Riders"
# This is not a product

row_to_remove <- results$product_name == "Enhance Your Coverage With Our Riders"
results <- results[!row_to_remove, ]

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "investment, family" ~ "Investment",
    product_type == "investment" ~ "Investment",
    product_type == "savings" ~ "Savings",
    product_type == "protection, health" ~ "TBD (H&P/CI/Medical)",
    product_type == "protection, life" ~ "Life",
    product_type == "protection, safety" ~ "H&P",
    product_type == "protection, family" ~ "TBD",
    product_type == "protection, property" ~ "TBD",
    product_type == "protection, vehicle" ~ "Motor",
    product_type == "protection, travel" ~ "Travel",
    product_type == "protection, golf" ~ "Misc.",
    TRUE ~ product_type
  ))

# Combine duplicate results
# some products may fall into more than one category, thus appearing in multiple pages 

results <- results %>%
  group_by(product_name) %>%
  summarise(product_type = paste(unique(product_type), collapse = ", "),
            product_description = first(product_description),
            insurance_type = paste(unique(insurance_type), collapse = ", "))

results <- results[, c(2, 1, 3, 4)]

# Add timestamp to last row ----

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

