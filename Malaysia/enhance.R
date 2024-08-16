library(dplyr)

MASTER_CSV <- "all_products.csv"

# Compare master list with latest compilation file
date_value <- Sys.Date() - 6

formatted_date <- format(date_value, format = "%d-%b-%Y")
month <- format(date_value, "%m")
year <- as.numeric(format(date_value, "%Y"))
week <- ceiling(as.numeric(format(date_value, "%d")) / 7)

this_week_path <- paste("Results", month, year, "Week", week, sep = "_")
this_week_csv <- read.csv(paste0(this_week_path, "/new_COMPILATION.csv"))

# Manual override of csv files to be read
# this_week_csv <- read.csv("Results_May_2024_Week_3/new_COMPILATION.csv")

compilation_filtered <- this_week_csv %>%
  select(insurer, product_name)

duplicate_counts_summary <- compilation_filtered %>%
  group_by(insurer, product_name) %>%
  filter(n() > 1) %>%
  summarise(occurrences = n(), .groups = 'drop')

result_with_bank_name <- this_week_csv %>%
  semi_join(duplicate_counts_summary %>%
              select(insurer, product_name),
            by = c("insurer", "product_name")) %>%
  left_join(duplicate_counts_summary, by = c("insurer", "product_name"))

write.csv(result_with_bank_name, file = "Duplicates in Compilation.csv", row.names = FALSE)

# Proceed to manual adjustment

# Change master list to standardized column names
master_df <- read.csv(MASTER_CSV)
colnames(master_df)[which(names(master_df) == "company")] <- "insurer"

master_df <- master_df %>%
  mutate(bank_name = case_when(
    discovered_place == "Insurer" ~ "-",
    TRUE ~ "TBD"
  ))

master_df <- master_df[, c("insurer", "bank_name", "product_type", "product_name", "insurance_type", "discovered_place", "status", "date_discovered", "date_retired")]

# Get bank_name from compilation to fill in master list
compilation_bank_only <- this_week_csv %>%
  filter(discovered_place == "Bank")

for (i in 1:nrow(master_df)){
  
  if (master_df$bank_name[i] == "TBD"){
    
    search_for_name <- master_df$product_name[i]
    search_for_insurer <- master_df$insurer[i]
    
    for (j in 1:nrow(compilation_bank_only)){
      
      if ((compilation_bank_only$product_name[j] == search_for_name) & (compilation_bank_only$insurer[j] == search_for_insurer)){
        
        master_df$bank_name[i] <- compilation_bank_only$bank_name[j]
      }
    }
  }
}

# Retired products with no bank_name are ignored (not important)
for (i in 1:nrow(master_df)){
  if ((master_df$status[i] == "Retired") & (master_df$bank_name[i] == "TBD")){
    master_df$bank_name[i] <- "-"
  }
}

# Check what is left
master_df[master_df$bank_name == "TBD", ]

# Do manual adjustment (if necessary)

write.csv(master_df, "Updated_all_products.csv", row.names = FALSE)
# End

# ======This is supplementary check======
# not_in_master_df <- data.frame()
# not_in_compilation <- data.frame()
# 
# for (i in 1:nrow(master_df)){
#   
#   if (master_df$status[i] != "Retired"){
#     
#     filtered <- this_week_csv %>%
#       filter(product_name == master_df$product_name[i])
#     
#     if (nrow(filtered) == 0){
#       
#       not_in_compilation <- rbind(not_in_compilation, master_df[i, ])
#       
#     } else {
#       
#       for (j in 1:nrow(filtered)){
#         if (filtered$insurer[j] != master_df$insurer[i]){
#           if (filtered$discovered_place[j] != master_df$discovered_place[i]){
#             not_in_master_df <- rbind(not_in_master_df, filtered[j, ])
#           }
#         }
#       }
#     }
#   }
# }
