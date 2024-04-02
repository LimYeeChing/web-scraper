library(rvest)

OUTPUT_FILE_PATH <- "results/GE.csv"

PRODUCTS_URL <- "https://www.greateasternlife.com/my/en/personal-insurance/our-products.html" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Scrape product types and URLs from category index page

product_type <-
  homepage %>% 
  html_elements(".ge-headline5.ge-headline--primary") %>%
  html_text2()

url <-
  homepage %>% 
  html_elements("a.solution-item") %>%
  html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.greateasternlife.com", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url)

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html(product_types$url[i])
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements(".ge-headline2.ge-headline--red") %>%
    html_text2()

  # Scrape product descriptions
 
  product_description <-
    html %>%
    html_elements(".solution-detail") %>%
    html_text2()
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }
  
}

# Write results to .csv file

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

