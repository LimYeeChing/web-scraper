library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/sunlife.csv"

PRODUCTS_URL <- "https://www.sunlifemalaysia.com/insurance-and-takaful/life-insurance/" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Scrape product types and URLs from navigation bar

html <- read_html(PRODUCTS_URL)
  
navigation_buttons <- 
  html %>%
  html_elements(".nav-link.p-md-0.pb-0")

product_type <- 
  navigation_buttons %>%
  html_text2() 

# Clean product_type
product_type <- gsub("\r", "", product_type)
product_type <- gsub("\n", "", product_type)

url <- html_attr(navigation_buttons, "href")

url[startsWith(url, "/")] <- paste0("https://www.sunlifemalaysia.com", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url)

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html(product_types$url[i])

  product_name <-
    html %>% 
    html_elements("div.card-product h5.sl-bold") %>%
    html_text2()
 
  product_description <-
    html %>%
    html_elements("div.card-product div.l2-inner") %>%
    html_text2()
  
  # Stickers contain extra information about the product's category
  # We use these stickers to further filter out takaful products

  # sticker <- 
  #   html %>% 
  #   html_elements("div.card-product .sub-cat1 .m-0") %>%
  #   html_text2()

  # non_takaful_products <- !grepl("takaful", sticker, fixed = TRUE)

  # product_name <- product_name[non_takaful_products]
  # product_description <- product_description[non_takaful_products]

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }

}

# Combine duplicate results
# some products fall into more than one category, thus appearing in multiple pages 

results <- results %>%
  group_by(product_name, product_description) %>%
  summarise(product_type = paste(unique(product_type), collapse = ", "))

results <- results[, c(3, 1, 2)]

# Filter out takaful products
results <- results[!grepl("takaful", results$product_type,  ignore.case = TRUE), ]

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("product_type" = "Scraped at", "product_name" = ":", "product_description" = formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

