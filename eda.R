# --- Load libraries ---
install.packages(c("tidyverse", "lubridate", "scales"))
library(tidyverse)
library(lubridate)
library(scales)

# --- Load dataset ---
data <- read.csv("C:/Users/Saket/OneDrive/Desktop/pds project/india_startups2.csv", check.names = FALSE)

# --- Clean column names ---
colnames(data) <- str_replace_all(colnames(data), "[^[:alnum:]_]", "_")

# --- Handle missing or invalid data ---
data <- data %>%
  mutate(across(everything(), ~ na_if(., ""))) %>%
  drop_na()

# --- Clean Funding Amount (USD) ---
funding_col <- grep("Funding", names(data), value = TRUE)[1]
data[[funding_col]] <- gsub("[$,]", "", data[[funding_col]])
data[[funding_col]] <- as.numeric(data[[funding_col]])

# --- Convert Date ---
if("Last_Funding_Date" %in% names(data)) {
  data$Last_Funding_Date <- parse_date_time(data$Last_Funding_Date, orders = c("dmy", "ymd", "my", "bY"))
}

# --- Top 15 Industries by Startup Count ---
industry_counts <- data %>%
  group_by(Industry) %>%
  summarise(Startups = n()) %>%
  arrange(desc(Startups)) %>%
  slice_head(n = 15)

# Treemap or Lollipop (better for readability)
# Option 1️⃣: Lollipop Chart
ggplot(industry_counts, aes(x = reorder(Industry, Startups), y = Startups)) +
  geom_segment(aes(xend = Industry, y = 0, yend = Startups), color = "#1f77b4", linewidth = 1.2) +
  geom_point(color = "#ff7f0e", size = 4) +
  coord_flip() +
  labs(title = "Top 15 Industries by Startup Count", x = "Industry", y = "Number of Startups") +
  theme_minimal()

# Option 2️⃣: Treemap (requires treemapify)
# install.packages("treemapify")
# library(treemapify)
# ggplot(industry_counts, aes(area = Startups, fill = Industry, label = Industry)) +
#   geom_treemap() +
#   geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
#   labs(title = "Startup Count by Industry (Treemap)") +
#   theme(legend.position = "none")

# --- Type of Funding Analysis ---
ggplot(data, aes(x = Funding_Type, fill = Funding_Type)) +
  geom_bar() +
  labs(title = "Distribution of Funding Types", x = "Funding Type", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Distribution of Funding Amounts ---
ggplot(data, aes(x = !!sym(funding_col))) +
  geom_histogram(binwidth = 500000, fill = "#ff7f0e", color = "black") +
  labs(title = "Distribution of Funding Amounts", x = "Funding Amount (USD)", y = "Frequency") +
  scale_x_continuous(labels = comma) +
  theme_minimal()

# --- Top 10 Industries by Average Funding ---
top_industries <- data %>%
  group_by(Industry) %>%
  summarise(Average_Funding = mean(!!sym(funding_col), na.rm = TRUE)) %>%
  arrange(desc(Average_Funding)) %>%
  slice_head(n = 10)

ggplot(top_industries, aes(x = reorder(Industry, Average_Funding), y = Average_Funding)) +
  geom_col(fill = "#2ca02c") +
  coord_flip() +
  labs(title = "Top 10 Industries by Average Funding", x = "Industry", y = "Average Funding (USD)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# --- Average Funding by Funding Type ---
avg_funding_type <- data %>%
  group_by(Funding_Type) %>%
  summarise(Average_Funding = mean(!!sym(funding_col), na.rm = TRUE)) %>%
  arrange(desc(Average_Funding))

ggplot(avg_funding_type, aes(x = reorder(Funding_Type, Average_Funding), y = Average_Funding, fill = Funding_Type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Funding by Funding Type", x = "Funding Type", y = "Average Funding (USD)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# --- Correlation Heatmap (for numeric variables) ---
num_data <- data %>% select(where(is.numeric))
if (ncol(num_data) > 1) {
  corr_matrix <- cor(num_data, use = "complete.obs")
  corr_df <- as.data.frame(as.table(corr_matrix))
  
  ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = round(Freq, 2)), color = "white") +
    scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0) +
    labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
    theme_minimal()
}

# --- Save cleaned dataset ---
write.csv(data, "C:/Users/Saket/OneDrive/Desktop/pds project/india_startups_cleaned.csv", row.names = FALSE)
