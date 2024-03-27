library(rvest)

OUR_PRODUCTS_URL <- "https://www.greateasternlife.com/my/en/personal-insurance/our-products.html"

homepage <- read_html(OUR_PRODUCTS_URL)

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

# The 'takaful' category produces an edge case, as it's at www.greateasterntakaful.com

url <- paste0("https://www.greateasternlife.com", url[startsWith(url, "/")])

# Removing the last product_type -- this solution only works under the assumption that 'takaful' is the last item, change in future if needed

product_types <- data.frame(product_type[1:length(url)], url)

# Loop through all product types, visiting each type's page and scraping information

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
  
  results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description))
  
}

# Write results to .csv file

write.csv(results, file = "results/GE.csv", row.names = FALSE)