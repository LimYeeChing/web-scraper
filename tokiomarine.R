library(rvest)

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

product_type <- c(product_type_life, product_type_gi)
url <- c(url_life, url_gi)

url[startsWith(url, "/")] <- paste0("https://www.tokiomarine.com", url[startsWith(url, "/")]) # Scraped links do not include site name and protocol

product_types <- data.frame(product_type, url)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html_live(product_types$url[i])
  
  product_name <-
    html %>% 
    html_elements("h5.signpost-custom-card__item__title") %>%
    html_text2()
 
  product_description <-
    html %>%
    html_elements("p.signpost-custom-card__item__text") %>%
    html_text2()
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }
  
}

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
