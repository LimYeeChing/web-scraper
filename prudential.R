library(rvest)

OUTPUT_FILE_PATH <- "results/prudential.csv"

HOMEPAGE_URL <- "https://www.prudential.com.my/en/" 

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_url")

homepage <- read_html_live(HOMEPAGE_URL)

# Click on 'Products' button to open products menu 

homepage$click('[data-header-popup="nav1"]')

columns <- 
  homepage %>%
  html_elements('[data-nav-content="nav1"]') %>%
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

# Remove product_url from results before writing to csv

results$product_url <- NULL

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

