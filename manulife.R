library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/manulife.csv"

PRODUCTS_URL <- "https://www.manulife.com.my/en/individual/products.html" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Scrape product types and URLs from category index page

product_type <-
  homepage %>% 
  html_elements(".products-container div.cmp-text a") %>%
  html_text2()

url <-
  homepage %>% 
  html_elements(".products-container div.cmp-text a") %>%
  html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.manulife.com.my", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url)

# Adding links that are not in index page for some reason... 
# Loop through all product types, visiting each type's page and scraping additional links
# Manulife's site has sub-categories under each category, e.g. 'Medical and Hospitalisation' , 'Critical Illness' under'Health'. 
# Clicking on 'Health' leads you to 'Medical and Hospitalisation' only, so the link for 'Critical Illness' needs to be scraped from this 'Medical and Hospitalisation' site. 

updated_product_types <- product_types

for (i in 1:nrow(product_types)) {

  html <- read_html(product_types$url[i])

  url <- 
  html %>%
  html_elements(".cmp-list__cta--tab-list .cmp-list .cmp-list__item-link") %>%
  html_attr("href")

  product_type <- 
  html %>%
  html_elements(".cmp-list__cta--tab-list .cmp-list .cmp-list__item-link") %>%
  html_text2()

  if (length(url) != 0 && length(product_type) != 0){
      updated_product_types <- rbind(updated_product_types, data.frame(url, product_type))
  }

}

updated_product_types$url[startsWith(updated_product_types$url, "/en/")] <- paste0("https://www.manulife.com.my", updated_product_types$url[startsWith(updated_product_types$url, "/")])
updated_product_types <- updated_product_types[!duplicated(updated_product_types$url), ]

# Loop through all product types, visiting each type's page and scraping product info

for (i in 1:nrow(updated_product_types)) {
  html <- read_html(updated_product_types$url[i])
  
  product_info <-
    html %>% 
    html_elements(".cmp-productteaser__link") %>%
    html_attr("data-elbl")

  if (length(product_info) != 0){
    # Split each string in the vector using the "|" delimiter
    split_strings <- lapply(product_info, function(x) strsplit(x, "|", fixed = TRUE)[[1]])

    product_name <- sapply(split_strings, function(x) x[1])
    product_description <- sapply(split_strings, function(x) x[2])
    
    # product names end up with '<p>' and '</p>' for some reason. Cleaning 
    product_name <- gsub("<p>", "", product_name)
    product_name <- gsub("</p>", "", product_name)
    
    results <- rbind(results, data.frame(product_type = updated_product_types$product_type[i], product_name, product_description)) 
  }
  
}

# Combine duplicate results with different product_type

results <- results %>%
  group_by(product_name, product_description) %>%
  summarise(product_type = paste(unique(product_type), collapse = ", "))
  
# Rearrange columns since summarising mixes the order

results <- results[, c(3, 1, 2)]

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

