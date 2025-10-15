library(httr)
library(jsonlite)

# Reddit post JSON URL
url <- "https://www.reddit.com/r/DACXI/comments/1lrk9q0/india_ranks_3rd_globally_in_fintech_startup/.json"

# Set user agent (Reddit blocks requests without one)
res <- GET(url, user_agent("Mozilla/5.0"))

# Prevent simplification so nested structure is preserved
json_data <- fromJSON(rawToChar(res$content), simplifyVector = FALSE)

# Navigate the nested structure
title <- json_data[[1]]$data$children[[1]]$data$title
body <- json_data[[1]]$data$children[[1]]$data$selftext

# Combine
full_text <- paste("Title:", title, "\n\nContent:\n", body)

# Save
save_path <- "C:/Users/Saket/OneDrive/Desktop/pds project/reddit_post.txt"
writeLines(full_text, save_path)

cat("Saved to:", save_path)
