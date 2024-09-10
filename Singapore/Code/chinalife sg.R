library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/chinalife sg.csv"

URL_list <- c("https://www.chinalife.com.sg/products/endowment-plans",
              "https://www.chinalife.com.sg/products/retirement-annuity-plans",
              "https://www.chinalife.com.sg/products/legacy-planning",
              "https://www.chinalife.com.sg/products/protection",
              "https://www.chinalife.com.sg/products/riders")

PRODUCT_TYPE <- c("Endowment", "Retirement", "Legacy Planning", "Protection", "Riders")

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL_list)

product_list <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(product_list) <- c("product_type", "product_name", "product_url")

for (i in 1:nrow(product_types)){
  
  product_type <- product_types$product_type[i]
  
  html <- read_html(product_types$url[i])
  
  product_name <- html %>%
    html_elements(".field-content") %>%
    html_nodes("a") %>%
    html_text2()
  
  product_name <- product_name[product_name != ""]
  product_name <- product_name[product_name != "Know more"]
  
  product_url <- html %>%
    html_elements(".field-content") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique()
  
  if (product_type != "Riders"){
    product_name <- product_name[product_name != "riders"]
    product_url <- product_url[!(grepl("riders", product_url))]
  }
  
  product_list <- rbind(product_list, data.frame(product_type = product_type, product_name = product_name, product_url = product_url))
  
}

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_list)){
  
  product_name <- product_list$product_name[i]
  product_type <- product_list$product_type[i]
  bank_name <- "-"
  
  link <- paste0("https://www.chinalife.com.sg", product_list$product_url[i])
  
  if (grepl("DIRECT", link)){
    
    html <- read_html_live(link)
    
    for(n in 1:2){
      Sys.sleep(4)
    }
    
    product_desc <- html %>%
      html_elements(".clearfix.text-formatted.field.field--name-body.field--type-text-with-summary.field--label-hidden.field__item") %>%
      html_nodes("p") %>%
      html_text2()
    
    product_desc <- product_desc[which(grepl(product_name, product_desc))]
    product_desc <- product_desc[1]
    
  } else if(product_type == "Riders"){
    
    html <- read_html_live(link)
    
    for(n in 1:2){
      Sys.sleep(4)
    }
    
    product_desc <- html %>%
      html_elements(".field__item") %>%
      html_text2()
    
    product_desc <- product_desc[which(grepl(product_name, product_desc))]
    product_desc <- product_desc[1]

  } else{
  
    html <- read_html(link)
    
    product_desc <- html %>%
      html_elements(".field__item") %>%
      html_text2()

    end_index <- which(product_desc == "Product Features")
    product_desc <- product_desc[end_index - 1]
    
  }
  
  results <- rbind(results, data.frame(insurer = "China Life", bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = "Life Insurance"))
}

results$product_description <- gsub("<[^>]+>", "", results$product_description)
results$product_description <- gsub("<!--.*?-->", "", results$product_description)
results$product_description <- gsub("\\n", " ", results$product_description)
results$product_description <- gsub("\\*/", "", results$product_description)
results$product_description <- gsub("\\s+", " ", results$product_description)
results$product_description <- trimws(results$product_description)

results <- results %>%
  mutate(product_type = case_when(
    product_type %in% c("Endowment", "Retirement") ~ "Savings",
    product_type == "Legacy Planning" ~ "Legacy",
    product_type %in% c("Protection", "Riders") & (grepl("medi", tolower(product_name))) ~ "Medical",
    product_type %in% c("Protection", "Riders") & (grepl("accident", tolower(product_name))) ~ "H&P",
    product_type %in% c("Protection", "Riders") & (grepl("critic", tolower(product_name))) ~ "Critical Illness",
    product_type %in% c("Protection", "Riders") & (grepl("cancer", tolower(product_name))) ~ "Critical Illness",
    product_name == "SOPHY" ~ "Critical Illness",
    TRUE ~ "Life"
  ))

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
