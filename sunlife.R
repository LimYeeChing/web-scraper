library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/sunlife.csv"

PRODUCTS_URL <- "https://www.sunlifemalaysia.com/plans/protection-needs/" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Scrape product types and URLs from category index page

html <- read_html("https://www.sunlifemalaysia.com/plans/protection-needs/")
  
  product_type <-
    html %>% 
    html_elements(".main-col h2") %>%
    html_text2()

  url <-
    html %>% 
    html_elements(".main-col a") %>%
    html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.sunlifemalaysia.com", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url)

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)) {
  html <- read_html(product_types$url[i])
  
  # Scrape additional urls
  
  urls <-
    html %>% 
    html_elements(".year-list-wrapper a") %>%
    html_attr("href") 
    
  urls <- gsub(" ", "%20", urls)
  urls <- paste0("https://www.sunlifemalaysia.com", urls)

  product_name <-
    html %>% 
    html_elements(".package-detail h4") %>%
    html_text2()
 
  product_description <-
    html %>%
    html_elements(".package-detail ul") %>%
    html_text2()

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }

  for (url in urls){

    # Visit newly scraped links 
   
    html <- read_html(url)

    product_name <-
      html %>% 
      html_elements(".package-detail h4") %>%
      html_text2()
    
    product_description <-
      html %>%
      html_elements(".package-detail ul") %>%
      html_text2()

    if (length(product_name) != 0 && length(product_description) != 0){
        results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
    }

  }
  
}

# Combine duplicate results
# some products fall into more than one category, thus appearing in multiple pages 

results <- results %>%
  group_by(product_name, product_description) %>%
  summarise(product_type = paste(unique(product_type), collapse = ", "))

results <- results[, c(3, 1, 2)]

# Clean out takaful products (though I believe not all of them will be filtered with this method)

results <- results[!grepl("takaful", results$product_name,  ignore.case = TRUE), ]
results <- results[!grepl("takaful", results$product_description,  ignore.case = TRUE), ]

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

