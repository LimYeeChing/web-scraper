library(rvest)

OUTPUT_FILE_PATH <- "results/mcis.csv"

PRODUCTS_URL <- "https://www.mcis.my/our-products" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Scrape product types and URLs from category index page

product_type <-
  homepage %>% 
  html_elements("h4.slider-img-text.mcis-text-white") %>%
  html_text2()

url <-
  homepage %>% 
  html_elements("a.btn.mcis-btn-transparent-white.btn-rounded.text-uppercase") %>%
  html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.mcis.my", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url)

# Loop through all product types, visiting each product page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html(product_types$url[i])
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements("h4.slider-img-text") %>%
    html_text2()

  # Scrape product urls
 
  product_url <-
    html %>% 
    html_elements("a.btn.mcis-btn-transparent-white.btn-rounded.text-uppercase") %>%
    html_attr("href") 

  product_url <- paste0("https://www.mcis.my", product_url[startsWith(product_url, "/")])

  # Visit each `product_url` to scrape product descriptions
 
  product_description <- vector(mode = "character", length = length(product_url))

  for (j in 1:length(product_url)){
    product_html <- read_html(product_url[j])

    product_desc <- 
      product_html %>%
      html_elements(".mcis-text-bold") %>%
      html_text2() 
      
    product_description[j] <- paste(product_desc[!product_desc %in% c("Learn More", "Footnotes", "Disclaimer")], collapse = "\n")
  }
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }

}

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

