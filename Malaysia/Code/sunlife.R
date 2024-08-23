library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/sunlife.csv"

PRODUCTS_URL <- "https://www.sunlifemalaysia.com/insurance-and-takaful/life-insurance/" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("bank_name", "product_type", "product_name", "product_description", "src", "distribution")

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
  
  link <-
    html %>%
    html_elements(".col-12.col-sm-6.col-md-6.col-lg-4.mb-4") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  src <- 
    html %>%
    html_elements(".col-md-6") %>%
    html_nodes("img") %>%
    html_attr("src")
  
  src <- src[-((length(src)-3):length(src))]
  
  distribution_channel <-
    html %>%
    html_elements("p.m-0") %>%
    html_text2()
  
  distribution_channel <- distribution_channel[-c(1:4, (length(distribution_channel)-1):length(distribution_channel))]

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(bank_name = "-", product_type = product_types$product_type[i], product_name, product_description, src = src, distribution = distribution_channel)) 
  }

}

results$product_description <- trimws(results$product_description)

results <- results %>%
  mutate(bank_name = case_when(
    grepl("banca", tolower(distribution)) & grepl("cimb", tolower(src)) ~ "CIMB",
    grepl("banca", tolower(distribution)) & grepl("rajhi", tolower(src)) ~ "Al-Rajhi Bank",
    TRUE ~ "-"
  ))

results <- results[, c("bank_name", "product_type", "product_name", "product_description")]

# Label insurance_type for conventional and takaful products
results$insurance_type <- "Life Insurance"

results[grepl("takaful", results$product_type,  ignore.case = TRUE), "insurance_type"] <- "Family Takaful"

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life Insurance" ~ "Life",
    product_type == "Family Takaful" ~ "Life",
    product_type == "Protection" ~ "H&P",
    product_type == "Health" ~ "H&P",
    TRUE ~ product_type
  ))

# Remove tags "Education" and "Retirement" -- these products are duplicated and we don't need these tags in product_type

rows_to_remove <- results$product_type == "Education" | results$product_type == "Retirement"
results <- results[!rows_to_remove, ]

# Combine duplicate results
# some products fall into more than one category, thus appearing in multiple pages 

results <- results %>%
  group_by(bank_name, product_name, product_description) %>%
  summarise(
    product_type = paste(unique(product_type), collapse = ", "),
    insurance_type = paste(unique(insurance_type), collapse = ", ")
  )

# Sometimes duplicates result in insurance_type being 'Life Insurance, Family Takaful'

results$insurance_type[results$insurance_type == "Family Takaful, Life Insurance"] <- "Family Takaful"

# Summarise rearranges columns so arranging them back

results <- results[, c(1, 4, 2, 3, 5)]

# Store timestamp at last row of results

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("bank_name" = "", "product_type" = "Scraped at", "product_name" = ":", "product_description" = formatted_timestamp, "insurance_type" = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

