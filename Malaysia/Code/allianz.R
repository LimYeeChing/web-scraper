library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/allianz.csv"

HOMEPAGE_URLS <- c("Life Insurance" = "https://www.allianz.com.my/personal/life-health-and-savings.html",
                   "General Insurance" = "https://www.allianz.com.my/personal/home-motor-and-travel.html")

# Initialise data frames to store results

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description", "insurance_type") 

product_types <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(product_types) <- c("type", "type_url", "insurance_type")

for (i in 1:length(HOMEPAGE_URLS)){
  html <- read_html(HOMEPAGE_URLS[i])

  # Select html which contains links to different product types

  product_types_html <- 
    html %>%
    html_elements('[style="background-color:#FFFFFF;"]')

  # Scrape product type and corresponding urls

  type <- 
    product_types_html %>%
    html_elements("h3.c-heading.c-heading--subsection-medium.c-teaser__headline") %>%
    html_text2()

  type_url <- 
    product_types_html %>%
    html_elements(".c-button.c-button--link.c-button-link-center-align") %>%
    html_attr("href")
  
  insurance_type <- names(HOMEPAGE_URLS[i])

  # Add results to product_types

  product_types <- rbind(product_types, data.frame(type, type_url, insurance_type))
}

# Clean product urls

product_types$type_url <- paste0("https://www.allianz.com.my", product_types$type_url)

# Visit each page 

for (i in 1:nrow(product_types)){
    html <- read_html(product_types$type_url[i])

    product_name <-
      html %>% 
      html_elements("div.multi-column-grid") %>%
      html_elements(".c-heading") %>%
      html_text2()

    product_description <-
      html %>% 
      html_elements("ul.c-list.c-list--icon") %>%
      html_text2()

    insurance_type <- vector(mode = "character", length = length(product_name))

    insurance_type[] <- product_types$insurance_type[i]

    # Add data to results

    results <- rbind(results, data.frame(product_type = product_types$type[i], product_name, product_description, insurance_type))
}

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life Protection" ~ "Life",
    product_type == "Medical & Hospitalisation" ~ "Medical",
    product_type == "Savings, Investments & Waivers" ~ "Savings/Investment",
    product_type == "Roadside Assistance" ~ "Motor",
    product_type == "Car & Motorcycle" ~ "Motor",
    product_type == "Personal Accident" ~ "H&P", 
    product_type == "Living" ~ "Misc.",
    product_type == "Travel Insurance" ~ "Travel",
    TRUE ~ product_type
  ))

# Remove '\n' from product names

results$product_name <- gsub("\n", "", results$product_name)
results$product_name <- trimws(results$product_name)

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

