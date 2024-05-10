# If captcha error occurs, use html$view() to solve the captcha then rerun the code.
# read_html_live("https://www.zurich.com.my/en/insurance-products/protection")$view()

library(rvest)

OUTPUT_FILE_PATH <- "results/zurich.csv"

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

product_types <- data.frame("product_type" = c("Protection", "Savings", "Investment"), "url" = c("https://www.zurich.com.my/en/insurance-products/protection", "https://www.zurich.com.my/en/insurance-products/savings", "https://www.zurich.com.my/en/insurance-products/investment"))

url_list <- vector(mode = "character")

# Scrape urls of sub-category pages

for (i in 1:nrow(product_types)) {
  html <- read_html_live(product_types$url[i])
  
  urls <-
    html %>% 
    html_elements("div.teaser--wrapper a") %>%
    html_attr("href")

  url_list <- c(urls, url_list)
  
  if (length(urls) == 0) {
    stop("An error occurred. Likely Zurich Website Captcha.", call. = FALSE, .exit = 1)
  }

}

url_list <- paste0("https://www.zurich.com.my", unique(url_list))

# Loop through all product sub-category, visiting each page and scraping info

for (url in url_list) {
  html <- read_html_live(url)
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements(".teaser__content .field-headline") %>%
    html_text2()

  # Scrape product descriptions
 
  product_description <-
    html %>%
    html_elements(".field-promotext") %>%
    html_text2()
  
  # Obtain product category from url 

  product_category <- gsub("https://www.zurich.com.my/en/insurance-products/", "", url)
  product_category <- gsub("/", ", ", product_category)
  product_category <- gsub("for-my-", "", product_category)

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_category, product_name, product_description)) 
  } else{
    stop("An error occurred. Likely Zurich Website Captcha.", call. = FALSE, .exit = 1)
  }
  
}

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

