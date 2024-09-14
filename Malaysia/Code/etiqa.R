library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/etiqa.csv"

HOMEPAGE_URL <- "https://www.etiqa.com.my/v2/homepage" 

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

homepage <- read_html(HOMEPAGE_URL)

menu_html <- 
  homepage %>%
  html_elements(".col-md-6")

personal_insurance_html <- menu_html[1]
             
# Scrape product types and URLs from category index page

products <-
  personal_insurance_html %>% 
  html_elements("div.col-md-3") %>% # Select columns containing product info
  html_elements("a.nav-link.active") 

product_type <- html_text2(products)
url <- html_attr(products, "href")

# Clean URLs

url <- paste0("https://www.etiqa.com.my", url)

product_types <- data.frame(product_type, url)

for (i in 1:nrow(product_types)) {
  html <- read_html(product_types$url[i])
  
  # Scrape product names, descriptions, bullet points
  
  product_name <-
    html %>% 
    html_elements("h3.bold.mb-3") %>%
    html_text2()
 
  product_description <-
    html %>%
    html_elements("p.text-left , .mt-3 div") %>%
    html_text2()
  
  # Clean product descriptions

  product_description <- product_description[product_description != "" & product_description != " "]
  
  if(length(product_description) > length(product_name)){
    print(paste("Warning: There seems to be more product descriptions than product names on", product_types$url[i]))
    print("Auto-combining excess descriptions into 1, please check if this is correct")

    product_description[1] <- paste(product_description[1:(1 + length(product_description) - length(product_name))], collapse = '\n')
    product_description[2:length(product_name)] <- product_description[(2 + length(product_description) - length(product_name)):length(product_description)]
    product_description <- product_description[1:length(product_name)]
  }

  product_bullets <- 
    html %>%
    html_elements(".list-group") %>%
    html_text2()

  product_description <- paste0(product_description, "\n\r\n", product_bullets)
  
  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description)) 
  }

}

securepro <- "https://www.etiqa.com.my/v2/investment-linked-insurance-takaful/securepro"

html <- read_html(securepro)

rider_name <- html %>%
  html_elements(".text-color") %>%
  html_text2()

rider_name <- rider_name[grepl("IL Ultimate Health",rider_name)|grepl("IL Lady Care", rider_name)|grepl("IL Infinite Care", rider_name)]

rider_list <- data.frame(rider_name = c("IL Ultimate Health","IL Lady Care","IL Infinite Care"),
                         rider_type = c("Medical","Critical Illness","Critical Illness"))

for (i in 1:nrow(rider_list)){
  
  for (j in 1:length(rider_name)){
  
  if (grepl(rider_list$rider_name[i], rider_name[j])){
    results <- rbind(results, data.frame(product_type = rider_list$rider_type[i], product_name = rider_list$rider_name[i], product_description = ""))
  }
  }}

results$product_description <- gsub('StartFragment','', results$product_description)
results$product_description <- gsub('EndFragment','', results$product_description)

# Label takaful products 

results$insurance_type <- "Conventional"
results[grepl("takaful", results$product_name,  ignore.case = TRUE), "insurance_type"] <- "Takaful"

# Label these products as 'General' or 'Life' to further specify insurance_type

results <- results %>%
  mutate(general_life = case_when(
    product_type == "Car" ~ "General",
    product_type == "Motorcycle" ~ "General",
    product_type == "Car EV" ~ "General",
    product_type == "Pets" ~ "General",
    product_type == "Life and Family" ~ "Life",
    product_type == "Savings" ~ "Life",
    product_type == "Investment" ~ "Life",
    product_type == "Personal Accident" ~ "General", 
    product_type == "Home" ~ "General", 
    product_type == "Travel" ~ "General", 
    product_type == "Health" ~ "Life",
    product_type == "Education" ~ "Life",
    product_type == "Medical" ~ "Life",
    product_type == "Critical Illness" ~ "Life",
    product_type == "Public Servants" ~ "TBD",
    TRUE ~ product_type
  ))

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Car" ~ "Motor",
    product_type == "Motorcycle" ~ "Motor",
    product_type == "Car EV" ~ "Motor",
    product_type == "Pets" ~ "Misc.",
    product_type == "Life and Family" ~ "Life",
    product_type == "Personal Accident" ~ "H&P", 
    product_type == "Health" ~ "H&P/Medical/CI",
    product_type == "Education" ~ "Savings/Investment",
    product_type == "Public Servants" ~ "TBD",
    TRUE ~ product_type
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    insurance_type == "Conventional" & general_life == "Life" ~ "Life Insurance", 
    insurance_type == "Conventional" & general_life == "General" ~ "General Insurance", 
    insurance_type == "Takaful" & general_life == "Life" ~ "Family Takaful",
    insurance_type == "Takaful" & general_life == "General" ~ "General Takaful", 
    TRUE ~ insurance_type
  ))

results$general_life <- NULL

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
