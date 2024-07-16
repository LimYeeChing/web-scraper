library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/prudential.csv"

HOMEPAGE_URL <- "https://www.prudential.com.my/en/" 

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_url")

homepage <- read_html_live(HOMEPAGE_URL)

# Click on 'Products' button to open products menu 

homepage$click('[data-header-popup="nav0"]')

columns <- 
  homepage %>%
  html_elements('[data-nav-content="nav0"]') %>%
  html_elements("div.col-mini") 

# Scrape product types, product names and URLs from homepage menu columns

for (i in 1:length(columns))
{

  product_type <- 
    columns[i] %>% 
    html_elements("a.parent-link") %>%
    html_text2()

  product <-
    columns[i] %>%
    html_elements("a.child-link")

  product_name <- html_text2(product)
  product_url  <- html_attr(product, "href")

  if(!any(length(product_type) == 0, length(product_name) == 0, length(product_url) == 0)){
    results <- rbind(results, data.frame(product_type, product_name, product_url)) 
  }

}

# Clean product urls

results$product_url <- paste0("https://www.prudential.com.my", results$product_url)

# Visit each product url to scrape product descriptions

for (i in 1:length(results$product_url)){

  html <- read_html(results$product_url[i])

  product_description <-
    html %>% 
    html_elements(".page-title__content--desc") %>%
    html_text2()

  # if statement to deal with pages where description has different css selector
  
  if(length(product_description) == 0){

    product_description <-
      html %>% 
      html_elements("div.hero-banner__content--desc") %>%
      html_text2()

  }

  results[i, "product_description"] <- product_description
  
}

# Remove product_url and add insurance_type

results$product_url <- NULL

results <- cbind(results, "insurance_type" = "Life Insurance")

# Remove "Wealth Insurance" results, they are to be separately scraped elsewhere 

rows_to_remove <- results$product_name == "Savings & Investment Plans" | results$product_name == "Investment-linked Funds Information"
results <- results[!rows_to_remove, ]

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Medical Insurance" ~ "Medical",
    product_type == "Critical Illness Insurance" ~ "Critical Illness",
    product_type == "Life Insurance" ~ "Life",
    product_type == "Infant & Child Insurance" ~ "Baby",
    product_type == "My First Insurance" ~ "Life, Medical, H&P",
    product_type == "EPF i-Lindung" ~ "TBD",
    product_type == "Wealth Insurance" ~ "Investment", 
    TRUE ~ product_type
  ))

# Scrape from Prudential BSN Takaful's Website

TAKAFUL_URLS <- c("Life" = "https://www.prubsn.com.my/en/protection/",
                  "Savings" = "https://www.prubsn.com.my/en/wealth/",
                  "TBD (CI/H&P/Medical)" = "https://www.prubsn.com.my/en/health/")

takaful_results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(takaful_results) <- c("product_type", "product_name", "product_description")

for (i in 1:length(TAKAFUL_URLS)){

  html <- read_html(TAKAFUL_URLS[i])

  product_type <- names(TAKAFUL_URLS)[i]

  product_cards <- 
    html %>%
    html_elements("a") %>%
    html_elements(".dynamic-card-content, .card-static__content") 

  product_name <- 
    product_cards %>%
    html_elements(".title-content-bold, .card-static__content--title") %>% 
    html_text2()

  product_description <- 
    product_cards %>%
    html_elements(".content, .card-static__content--desc") %>% 
    html_text2()

  takaful_results <- rbind(takaful_results, data.frame(product_type, product_name, product_description))

}

takaful_results <- cbind(takaful_results, "insurance_type" = "Family Takaful")

results <- rbind(results, takaful_results)

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))
# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

