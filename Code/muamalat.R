library(rvest)
library(dplyr)
library(chromote)

OUTPUT_FILE_PATH <- "Results/muamalat.csv"

url <- "https://www.muamalat.com.my/investment/personal/wealth-protection/"

gen_insurance_tab <- read_html_live(url)

gen_product_name <- gen_insurance_tab %>%
  html_elements(".card-title.color-orange.mb-2") %>%
  html_text2()

gen_results <- data.frame(company = "Syarikat Takaful", product_type = "", product_name = gen_product_name, product_description = "", insurance_type = "General Takaful")

gen_results <- gen_results %>%
  mutate(product_type = case_when(
    grepl("home", tolower(product_name)) ~ "Home",
    grepl("pa", tolower(product_name)) ~ "H&P",
    grepl("motor", tolower(product_name)) ~ "Motor",
    TRUE ~ NA_character_
  ))

gen_insurance_tab$click(css = "#tab-2-tab", n_clicks = 1)
Sys.sleep(5)

life_insurance_tab <- read_html_live(url)

life_product_name <- life_insurance_tab %>%
  html_elements(".color-.mb-3.text-start") %>%
  html_text2()

life_results <- data.frame(company = "Syarikat Takaful", product_type = "Life", product_name = life_product_name, product_description = "", insurance_type = "Family Takaful")

results <- rbind(life_results, gen_results)

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

