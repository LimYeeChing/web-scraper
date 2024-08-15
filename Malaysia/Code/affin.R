library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/affin.csv"

HOMEPAGE_URL <- "https://www.affinalways.com/en/my-protection"

homepage <- read_html_live(HOMEPAGE_URL)

# Each product_type is in a different carousel 

product_types <- 
  homepage %>% 
  html_elements(".flex-container") %>% 
  html_text2()

product_types <- gsub("Protect My ", "", product_types)

carousels <- 
  homepage %>% 
  html_elements(".carousel")

# Intitialise results data frame 

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

for (i in 1:length(carousels)){

  product_name <- 
    carousels[i] %>% 
    html_elements(".card.card-radius .card-text") %>% 
    html_text2()

  product_description <- 
    carousels[i] %>% 
    html_elements(".card.card-radius .text-left") %>% 
    html_text2()    

  results <- rbind(results, data.frame(product_type = product_types[i], product_name, product_description))

}

# Change product_types to match other companies' results

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Health" ~ "H&P/CI/Medical", 
    product_type == "Protect Myself and Family" ~ "H&P", 
    product_type == "Vehicle" ~ "Motor", 
    TRUE ~ product_type
  ))

# Figure out insurance_type 
# Affin bank website doesn't show underwriting insurance company... 
# For simplicity, we assume Generali for conventional and Syarikat Takaful for islamic
# Also assume all takaful products have "Takaful" in name

results$insurance_type <- "Insurance"

takaful_index <- grepl("takaful", results$product_name, ignore.case = TRUE) | 
                 grepl("takaful", results$product_description, ignore.case = TRUE)

results[takaful_index, "insurance_type"] <- "Takaful"

results <- cbind(company = "To be filled", results)

results <- results %>%
  mutate(company = case_when(
    insurance_type == "Insurance" ~ "Generali", 
    insurance_type == "Takaful" ~ "Syarikat Takaful", 
    TRUE ~ "TBD"
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    insurance_type == "Insurance" & product_type == "Life" ~ "Life Insurance", 
    insurance_type == "Insurance" & product_type == "H&P" ~ "General Insurance", 
    insurance_type == "Insurance" & product_type == "Motor" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Travel" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Home" ~ "General Insurance",
    insurance_type == "Takaful" & product_type == "Life" ~ "Family Takaful", 
    insurance_type == "Takaful" & product_type == "H&P" ~ "General Takaful", 
    insurance_type == "Takaful" & product_type == "Motor" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Travel" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Home" ~ "General Takaful",
    TRUE ~ insurance_type
  ))

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)