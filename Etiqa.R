library(rvest)

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

results$product_description <- gsub('StartFragment','', results$product_description)
results$product_description <- gsub('EndFragment','', results$product_description)

# Clean out takaful products 

results <- results[!grepl("takaful", results$product_name,  ignore.case = TRUE), ]

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
