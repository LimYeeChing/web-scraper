library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/ge.csv"

INSURANCE_URL <- "https://www.greateasternlife.com/my/en/personal-insurance/our-products.html" 
TAKAFUL_URL   <- "https://www.greateasterntakaful.com/en/personal-takaful/our-products.html"

homepage <- read_html(INSURANCE_URL)

# Scrape product types and URLs from insurance index page

product_type <-
  homepage %>% 
  html_elements(".ge-headline5.ge-headline--primary") %>%
  html_text2()

url <-
  homepage %>% 
  html_elements("a.solution-item") %>%
  html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.greateasternlife.com", url[startsWith(url, "/")])

product_types <- data.frame(product_type, url, "insurance_type" = "Life Insurance")

# Visit greateasterntakaful.com

homepage <- read_html(TAKAFUL_URL)

# Scrape product types and URLs from takaful index page

product_type <-
  homepage %>% 
  html_elements(".ge-headline5.ge-headline--primary") %>%
  html_text2()

url <-
  homepage %>% 
  html_elements("a.solution-item") %>%
  html_attr("href")

url[startsWith(url, "/")] <- paste0("https://www.greateasterntakaful.com", url[startsWith(url, "/")])

product_types <- rbind(product_types, data.frame(product_type, url, "insurance_type" = "Family Takaful"))

product_types <- product_types %>%
  mutate(product_type = case_when(
    product_type == "Life Insurance" ~ "Life",
    product_type == "Health Insurance" ~ "H&P",
    product_type == "Wealth Accumulation" ~ "Savings/Investment",
    product_type == "Personal Accident Insurance" ~ "H&P",
    product_type == "Travel Insurance" ~ "Travel",
    product_type == "Motor Insurance" ~ "Motor", 
    product_type == "Home Insurance" ~ "Home",
    product_type == "Hospital & Surgical Insurance" ~ "Medical",
    product_type == "Sports Insurance" ~ "Misc.",
    product_type == "Other Insurance" ~ "Misc.",
    product_type == "Family Takaful" ~ "Life", 
    product_type == "Health Protection" ~ "TBD(Health/Medical/CI)", 
    product_type == "Online Takaful Plan" ~ "TBD (Online)",
    TRUE ~ product_type
  ))

# Loop through all product types, visiting each type's page and scraping info

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)) {
  html <- read_html(product_types$url[i])
  
  # Scrape product information 
  
  product_name <-
    html %>% 
    html_elements(".ge-headline2.ge-headline--red") %>%
    html_text2()
 
  product_name <- gsub("\n", "", product_name)

  product_description <-
    html %>%
    html_elements(".solution-detail") %>%
    html_text2()
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description, insurance_type = product_types$insurance_type[i])) 
  }
  
}

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

