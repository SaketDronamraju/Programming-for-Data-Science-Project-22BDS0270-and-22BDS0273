library(rvest)
library(dplyr)

final_table <- data.frame()

# Loop through pages 1 to 5
for (i in 1:5) {
  url <- paste0("https://growthlist.co/india-startups/?page=", i)
  page <- read_html(url)
  
  # Extract table
  table <- page %>%
    html_element("table") %>%
    html_table(fill = TRUE)
  
  final_table <- bind_rows(final_table, table)
  
  message("✅ Scraped page ", i)
}

# Save CSV
save_path <- "C:/Users/Saket/OneDrive/Desktop/pds project/india_startups2.csv"
write.csv(final_table, save_path, row.names = FALSE)
cat("✅ Saved content to:", save_path, "\n")
