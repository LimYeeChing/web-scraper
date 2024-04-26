library(rvest)

OUTPUT_FILE_PATH <- "results/tokiomarineLIFE.csv"

PRODUCTS_URL <- "https://www.tokiomarine.com/my/en/life.html" 

homepage <- read_html_live(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Scrape product types and URLs from category index page

product_type <-
  homepage %>% 
  html_elements(".quick-help-custom__left") %>%
  html_text2()

url <-
  homepage %>% 
  html_elements(".quick-help-custom__item a") %>%
  html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.tokiomarine.com", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url)

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html_live(product_types$url[i])
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements("h5.signpost-custom-card__item__title") %>%
    html_text2()

  # Scrape product descriptions
 
  product_description <-
    html %>%
    html_elements("p.signpost-custom-card__item__text") %>%
    html_text2()
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }
  
}

# Write results to .csv file

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

