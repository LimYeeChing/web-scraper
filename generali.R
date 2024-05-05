library(rvest)

# Scraped urls don't contain the protocol and site name as they are relative
complete_url <- function(url){

  paste0("https://www.generali.com.my", url)

}

OUTPUT_FILE_PATH <- "results/generali.csv"

HOMEPAGE_URL <- "https://www.generali.com.my/" 

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

homepage <- read_html(HOMEPAGE_URL)

dropdown_lists <- 
  homepage %>%
  html_elements(".dropdown-list.first-layer") 

# The second dropdown list contains links to Generali's products
products <- 
  dropdown_lists[2] %>%
  html_elements(".desktop a.link.dropdown")

product_name <- html_text2(products)
product_url  <- complete_url(html_attr(products, "href"))

product_urls <- data.frame(product_name, product_url)

# Generali's website has category-specific pages for Medical & Health and Life Protection products
category_pages <- product_urls[product_urls$product_name == "View All", ]
product_urls <- product_urls[product_urls$product_name != "View All", ]

for (page in category_pages$product_url){

  html <- read_html(page)

  product_name <-
    html %>%
    html_elements(".modal-wrapper.outlined h2") %>%
    html_text2() 

  product_url <-
    html %>% 
    html_elements(".g-button.primary.small.w-button") %>%
    html_attr("href") %>%
    complete_url()

  product_urls <- rbind(product_urls, data.frame(product_name, product_url))
}

product_urls <- unique(product_urls)

# Product descriptions ought to be scraped from each individual product page due to the design of the website

for (i in 1:nrow(product_urls)){

  html <- read_html(product_urls$product_url[i])

  product_type <-   
    html %>%
    html_elements(".subtitle") %>%
    html_text2() 

  # invisible_class <- 
  #   html %>%
  #   html_elements(".w-condition-invisible, .hide") 

  # invisible_elements <- vector(mode = "character")

  # Mark the 1st to 5th children of any invisible elements as invisible 
  # This loop doesn't work, I have no idea why...
  # for (j in 1:5){

  #   invisible_children <- invisible_class

  #   for (k in 1:j){invisible_children <- html_children(invisible_children)}

  #   invisible_elements <- union(invisible_elements, invisible_children)

  # }
  
  # invisible_child3 <- 
  #   html %>% 
  #   html_elements(".w-condition-invisible, .hide") %>%
  #   html_children() %>%
  #   html_children() %>%
  #   html_children() 

  # invisible_child4 <- 
  #   html %>% 
  #   html_elements(".w-condition-invisible, .hide") %>%
  #   html_children() %>%
  #   html_children() %>%
  #   html_children() %>%
  #   html_children()

  # invisible_child5 <-
  #   html %>%
  #   html_elements(".w-condition-invisible, .hide") %>%
  #   html_children() %>%
  #   html_children() %>%
  #   html_children() %>%
  #   html_children() %>%
  #   html_children()

  # invisible_elements <- union(invisible_child3, invisible_child4)

  description_heading <-
    html %>%
    html_elements("h2.h2") %>%
    html_text2() 

  description_body <-
    html %>%
    html_elements("div.paragraph-subtitle.w-richtext") %>%
    html_text2() 

  product_description <- paste(description_heading[1], description_body[1], sep = "\n") 

  results <- rbind(results, data.frame("product_type" = product_type[1], "product_url" = product_urls$product_name[i], product_description))
}

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
