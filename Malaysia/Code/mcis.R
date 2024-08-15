library(rvest)
library(dplyr)
library(stringr)

OUTPUT_FILE_PATH <- "results/mcis.csv"

PRODUCTS_URL <- "https://www.mcis.my/our-products" 

homepage <- read_html(PRODUCTS_URL)

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description", "product_url")

url <-
  homepage %>% 
  html_elements(".nav-link-style.stretched-link") %>%
  html_attr("href") %>%
  unique()

url[startsWith(url, "/")] <- paste0("https://www.mcis.my", url[startsWith(url, "/")])

for (i in 1:length(url)){
  
  html <- read_html(url[i])
  
  product_type <- str_extract(url[i], "(?<=our-products/)[^/]+")
  
  product_desc <- html %>%
    html_elements(".fw-light.pb-1") %>%
    html_text2()
  
  if (length(product_desc) == 0){
    product_desc <- html %>%
      html_elements("h3.mcis-text-purple.mcis-text-bold") %>%
      html_text2()
    
    product_name <- html %>%
      html_elements(".mcis-page-title.pt-5.text-center") %>%
      html_text2()
    
  } else {
    
    product_name <- str_extract(product_desc, "(?<=with ).*") 
    
  }
  
  if (is.na(product_name)){
   
    extracted_name <- str_extract(url[i], "[^/]+$")
    words <- unlist(str_split(product_desc, "\\s+"))
    
    for (j in 1:length(words)){
      
      if (extracted_name == tolower(words[j])){
        product_name <- words[j]
      } else {
        extracted_name <- str_replace_all(extracted_name, "-", " ")
        extracted_name_indexed <- unlist(str_split(extracted_name, "\\s+"))
        if (extracted_name_indexed[1] == tolower(words[j])){
          if (j + length(extracted_name_indexed) - 1 <= length(words)) {
            product_name <- str_c(words[j:(j + length(extracted_name_indexed) - 1)], collapse = " ")
            break
          }
      }
      }
    }
     
  }
  
  results <- rbind(results, data.frame(product_type = product_type, product_name = product_name, product_description = product_desc, product_url = url[i]))
}

results$product_name[results$product_name == "EzySaver today"] <- "EzySaver"

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "savings" ~ "Savings",
    product_type == "health-and-medical-protection" ~ "H&P",
    product_type == "investment-linked-insurance" ~ "Investment",
    product_type == "life-insurance" ~ "Life",
    product_type == "group-insurance" ~ "TBD",
    product_type == "mcis-for-life" ~ "TBD",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(product_type = case_when(
    (product_type == "TBD" & any(grepl("life", tolower(product_name)))) ~ "Life",
    (product_type == "TBD" & any(grepl("critical", tolower(product_name)))) ~ "Critical Illness",
    (product_type == "TBD" & any(grepl("hospital", tolower(product_name)))) ~ "Medical",
    (product_type == "TBD" & any(grepl("medi", tolower(product_name)))) ~ "Medical",
    (product_type == "TBD" & any(grepl("sme", tolower(product_name)))) ~ "Life, Medical",
    TRUE ~ product_type
  ))

# Add insurance_type 

results <- cbind(results, "insurance_type" = "Life Insurance")

# Rearrange columns, remove unnecessary ones 

results <- results[, c("product_type", "product_name", "product_description", "insurance_type")]

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

