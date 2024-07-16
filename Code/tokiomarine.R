library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/tokiomarine.csv"

PRODUCTS_URL_LIFE <- "https://www.tokiomarine.com/my/en/life.html" 
PRODUCTS_URL_GI   <- "https://www.tokiomarine.com/my/en/non-life/products/personal.html"

# Scrape product types and URLs from life index page

homepage_life <- read_html_live(PRODUCTS_URL_LIFE)

product_type_life <-
  homepage_life %>% 
  html_elements(".quick-help-custom__left") %>%
  html_text2()

url_life <-
  homepage_life %>% 
  html_elements(".quick-help-custom__item a") %>%
  html_attr("href")

product_types <- data.frame(product_type = product_type_life, url = url_life, insurance_type = "Life Insurance")

# Scrape from non-life index page 

homepage_gi <- read_html_live(PRODUCTS_URL_GI)

product_type_gi <-
  homepage_gi %>% 
  html_elements("h5.signpost-custom-card__item__title") %>%
  html_text2()

url_gi <-
  homepage_gi %>% 
  html_elements(".btn.btn-custom.signpost-custom-card__item__link.comparison-signpost-link") %>%
  html_attr("href")

product_types <- rbind(product_types, data.frame(product_type = product_type_gi, url = url_gi, insurance_type = "General Insurance"))

# Scraped links do not include site name and protocol

product_types$url[startsWith(product_types$url, "/")] <- paste0("https://www.tokiomarine.com", product_types$url[startsWith(product_types$url, "/")]) 

# Adjust product_type to align with other companies, TBD used if unclear

product_types <- product_types %>%
  mutate(product_type = case_when(
    product_type == "General Savings" ~ "Savings",
    product_type == "For Your Retirement" ~ "Savings/Investment",
    product_type == "Health" ~ "H&P/Medical/CI",
    product_type == "For Education" ~ "Savings/Investment",
    product_type == "Employee Benefits" ~ "TBD",
    product_type == "Personal Accident" ~ "H&P", 
    product_type == "Travel and Sports" ~ "TBD (Travel/Sports)", 
    product_type == "Home and Property" ~ "Home",
    TRUE ~ product_type
  ))

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description", "insurance_type")

# Remove the product type "Promotions"
# This page doesn't contain products, but rather promotional campaigns 

product_types <- product_types[!product_types$product_type == "Promotions", ]

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html_live(product_types$url[i])
  
  insurance_type <- product_types$insurance_type[i]

  product_name <-
    html %>% 
    html_elements("h5.signpost-custom-card__item__title") %>%
    html_text2()
 
  product_description <-
    html %>%
    html_elements("p.signpost-custom-card__item__text") %>%
    html_text2()
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description, insurance_type)) 
  }
  
}

# Superscripted characters cause issues with write.csv()'s character encoding

product_names <- results$product_name
product_names <- gsub("¹", "", product_names)
product_names <- gsub("²", "", product_names)
product_names <- gsub("³", "", product_names)
results$product_name <- product_names 

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("product_type" = "Scraped at", "product_name" = ":", "product_description" = formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
