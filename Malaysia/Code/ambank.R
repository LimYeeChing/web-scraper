library(rvest)
library(dplyr)
library(stringr)

OUTPUT_FILE_PATH <- "Results/ambank.csv"

insurance_type <- c("General Insurance","Life Insurance")

gen_url <- read_html("https://www.ambank.com.my/eng/Liberty-General-Product")
life_url <- read_html("https://www.ambank.com.my/eng/insurance")

gen_product_types <- gen_url %>%
  html_nodes("h3") %>%
  html_text()

gen_product_names <- gen_url %>%
  html_elements(".caption") %>%
  html_nodes("h5") %>%
  html_text()

gen_product_description <- gen_url %>%
  html_elements(".caption") %>%
  html_nodes("p") %>%
  html_text()

gen_product_description <- data.frame(gen_product_description)
gen_product_description <- gen_product_description[!(gen_product_description$gen_product_description %in% c("Learn more", "Learn More")), , drop = TRUE]

gen_product_counts <- gen_url %>%
  html_nodes(".custom-grid-container") %>%
  lapply(function(node) node %>%
    html_nodes(".custom-grid-item") %>%
    length()) %>%
  unlist()

grouped_counts <- c(gen_product_counts[1], sum(gen_product_counts[2:3]), gen_product_counts[-c(1:3)])

if (length(grouped_counts) != 6) {
  warning("WARNING: Check number of products in each category!")
}

gen_results <- data.frame(matrix(nrow = sum(grouped_counts), ncol = 3))
colnames(gen_results) <- c("product_type", "product_name", "product_description")

current_row <- 1
for (i in seq_along(gen_product_types)) {
  product_type <- gen_product_types[i]
  each_product_count <- grouped_counts[i]
  
  # Assign product_type, product_name, and product_description to gen_results
  gen_results[current_row:(current_row + each_product_count - 1), "product_type"] <- rep(product_type, each_product_count)
  gen_results[current_row:(current_row + each_product_count - 1), "product_name"] <- gen_product_names[current_row:(current_row + each_product_count - 1)]
  gen_results[current_row:(current_row + each_product_count - 1), "product_description"] <- gen_product_description[current_row:(current_row + each_product_count - 1)]
  
  # Move to the next set of rows
  current_row <- current_row + each_product_count
}

gen_results <- cbind(company = "Liberty", gen_results, insurance_type = "General Insurance")

gen_results <- gen_results %>%
  mutate(product_type = case_when(
    product_type == "Motor" ~ "Motor",
    product_type == "Personal Accident" ~ "H&P",
    product_type == "Travel Insurance" ~ "Travel",
    product_type == "Home Insurance" ~ "Home",
    product_type == "Commercial & Industry" ~ "Fire",
    product_type == "Miscellaneous" ~ "Misc."
  ))

containers <- life_url %>%
  html_nodes(".carousels-container")

container_titles <- list()
container_bodies <- list()

# Loop through each container
for (i in 1:length(containers)) {
  container <- containers[[i]]
  
  # Extract slides within the current container
  slides <- container %>%
    html_nodes(".slide")
  
  # Initialize vector to store titles for current container
  titles <- character(length(slides) - 1)
  bodies <- character(length(slides) - 1)
  
  # Loop through slides starting from the second slide
  for (j in 2:length(slides)) {
    slide <- slides[[j]]
    
    # Extract title from current slide
    title <- slide %>%
      html_node(".title") %>%
      html_text()
    
    # Extract body content from the second <p> tag
    body <- slide %>%
      html_elements(".body") %>%
      html_nodes("p") %>%
      html_text() %>%
      .[4]
    
    # Store title and body in vectors
    titles[j - 1] <- title
    bodies[j - 1] <- body
  }
  
  # Store titles and bodies in the lists
  container_titles[[i]] <- titles
  container_bodies[[i]] <- bodies
}

life_product_names <- unique(unlist(container_titles))
life_product_description <- unique(unlist(container_bodies))

life_results <- data.frame(company = "AmMetLife", product_type = "", product_name = life_product_names, product_description = life_product_description, insurance_type = "Life Insurance")

life_results <- life_results %>%
  mutate(product_type = case_when(
    grepl("medical", tolower(product_description)) | grepl("medical", tolower(product_name)) ~ "Medical",
    grepl("mortgage", tolower(product_description)) | grepl("mortgage", tolower(product_name)) ~ "Property",
    grepl("investment", tolower(product_description)) ~ "Investment",
    TRUE ~ "Life"
  ))

results <- rbind(gen_results, life_results)

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

