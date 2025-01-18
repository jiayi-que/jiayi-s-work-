install.packages(c("ggplot2", "dplyr", "lubridate", "tidytext", "wordcloud", 
                   "reshape2", "RColorBrewer", "tm", "tidyr", "stringr","lubridate","fmsb","scales","circlize","networkD3"))
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidytext)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(tm)
library(tidyr)
library(stringr)
library(lubridate)
library(fmsb)
library(scales)
library(circlize)
library(networkD3)

News_data<-read.csv("D:/Desktop/MSC data science/IDS assignment/MN-DS-news-classification.csv")
View(News_data)
summary(News_data)

# Data preprocessing
clean_text <- function(text) {
  text <- tolower(text)  
  text <- removePunctuation(text)  
  text <- removeNumbers(text) 
  text <- removeWords(text, stopwords("en"))  
  text <- stripWhitespace(text)  
  return(text)
}

Sys.setlocale("LC_TIME", "C") 
News_data$title_clean <- sapply(News_data$title, clean_text)
News_data$content_clean <- sapply(News_data$content, clean_text)
News_data$hour <- hour(as.POSIXct(News_data$published_utc, origin = "1970-01-01", tz = "UTC"))
News_data$title_length <- nchar(News_data$title)
News_data$content_length <- nchar(News_data$content)
View(News_data)
summary(News_data)


# Distribution of categories

category_level_1_freq <- table(News_data$category_level_1) 
category_level_2_freq <- table(News_data$category_level_2) 

# Primary Category Distribution
primary_df <- data.frame(Category = names(category_level_1_freq), Frequency = as.numeric(category_level_1_freq))
ggplot(primary_df, aes(x = reorder(Category, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Frequency), vjust = -0.3, size = 3) +  
  labs(title = "Primary Category Distribution", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
# Secondary Category Distribution
secondary_df <- data.frame(Category = names(category_level_2_freq), Frequency = as.numeric(category_level_2_freq))
ggplot(secondary_df, aes(x = reorder(Category, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = scales::comma(Frequency)), 
            vjust = 0.5, hjust = -0.1, size = 4, angle = 90) +  # Rotate numbers vertically
  labs(title = "Secondary Category Distribution", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))



# Time Series Analysis: Monthly Trend of News Articles
News_data$month <- floor_date(as.POSIXct(News_data$published_utc, origin = "1970-01-01"), "month")
monthly_trend <- News_data %>%
  group_by(month) %>%
  summarise(Count = n())

ggplot(monthly_trend, aes(x = month, y = Count)) +
  geom_line(color = "blue") +
  geom_point(color = "red") + 
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +  
  labs(
    title = "Monthly Trend of News Articles",
    x = "Month",
    y = "Number of Articles"
  ) +
  theme_minimal()

# Content Length Distribution by Primary Category
News_data$category_level_1 <- str_wrap(News_data$category_level_1, width = 10)

ggplot(News_data, aes(x = category_level_1, y = content_length)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Content Length Distribution by Primary Category",
    x = "Primary Category",
    y = "Content Length"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8) 
  )


# Average Content Length by News Category
# Calculate average content length
category_length_avg <- News_data %>%
  group_by(category_level_1) %>%
  summarise(avg_content_length = mean(content_length))

# Convert data into the format suitable for radar chart
spider_data <- as.data.frame(t(category_length_avg$avg_content_length))
colnames(spider_data) <- category_length_avg$category_level_1

# Set the maximum and minimum values for the radar chart
spider_data <- rbind(rep(max(spider_data), length(spider_data)), 
                     rep(min(spider_data), length(spider_data)), 
                     spider_data)

# Plot the radar chart
par(mar = c(1, 1, 1, 1))
radarchart(spider_data, 
           axistype = 1, 
           pcol = rgb(0.2, 0.5, 0.5, 0.9), 
           pfcol = rgb(0.2, 0.5, 0.5, 0.5), 
           plwd = 2, 
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, max(spider_data[1, ]), length.out = 5), 
           cglwd = 0.8, 
           vlcex = 0.8)




# Word Frequency Analysis: Title Word Cloud
title_words <- News_data %>%
  unnest_tokens(word, title_clean) %>%
  count(word, sort = TRUE) %>%
  filter(n > 50)  # Adjust threshold as needed

wordcloud(words = title_words$word, freq = title_words$n, max.words = 100, colors = brewer.pal(8, "Dark2"))

# Heatmap Analysis: Category vs. Hour of Publication
heatmap_data <- News_data %>%
  count(category_level_1, hour) %>%
  spread(hour, n, fill = 0)

heatmap_data_melt <- melt(heatmap_data, id.vars = "category_level_1")
colnames(heatmap_data_melt) <- c("Category", "Hour", "Frequency")

ggplot(heatmap_data_melt, aes(x = Hour, y = Category, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Heatmap of News Categories by Hour of Publication", x = "Hour", y = "Category") +
  theme_minimal()

# Sentiment Analysis by Category
sentiment_lexicon <- get_sentiments("bing")  # Choose sentiment lexicon as needed

sentiment_analysis <- News_data %>%
  unnest_tokens(word, content_clean) %>%
  inner_join(sentiment_lexicon, by = "word") %>%
  count(category_level_1, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

sentiment_analysis$category_level_1 <- str_wrap(sentiment_analysis$category_level_1, width = 10)

ggplot(sentiment_analysis, aes(x = category_level_1, y = sentiment_score, fill = category_level_1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sentiment_score), vjust = -0.3, size = 3) + 
  labs(
    title = "Sentiment Analysis by News Category",
    x = "Category",
    y = "Sentiment Score",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

# Sankey diagram for content & title-category relationship
# Extract top 5 frequent words from the title
title_words <- News_data %>%
  unnest_tokens(word, title_clean) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)  # Filter words that appear more than 10 times for clarity

top_title_words <- title_words %>%
  top_n(5, n)  # Get top 5 frequent words from the title

# Extract top 5 frequent words from the content
content_words <- News_data %>%
  unnest_tokens(word, content_clean) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)  # Filter words that appear more than 10 times for clarity

top_content_words <- content_words %>%
  top_n(5, n)  # Get top 5 frequent words from the content

# Create relationship table for title with primary category
title_category_relationship <- News_data %>%
  unnest_tokens(word, title_clean) %>%
  filter(word %in% top_title_words$word) %>%
  count(category_level_1, word) %>%
  spread(word, n, fill = 0)

# Create relationship table for content with primary category
content_category_relationship <- News_data %>%
  unnest_tokens(word, content_clean) %>%
  filter(word %in% top_content_words$word) %>%
  count(category_level_1, word) %>%
  spread(word, n, fill = 0)

# Prepare data for Sankey diagram for title-category relationship
title_sankey_data <- title_category_relationship %>%
  gather(key = "word", value = "frequency", -category_level_1) %>%
  filter(frequency > 0) %>%
  mutate(source = category_level_1,
         target = paste0("Title: ", word),
         value = frequency) %>%
  select(source, target, value)

# Prepare data for Sankey diagram for content-category relationship
content_sankey_data <- content_category_relationship %>%
  gather(key = "word", value = "frequency", -category_level_1) %>%
  filter(frequency > 0) %>%
  mutate(source = category_level_1,
         target = paste0("Content: ", word),
         value = frequency) %>%
  select(source, target, value)

# Combine title and content Sankey data
combined_sankey_data <- bind_rows(title_sankey_data, content_sankey_data)

# Create a list of nodes for Sankey diagram (combining categories and words)
nodes <- data.frame(name = unique(c(combined_sankey_data$source, combined_sankey_data$target)))

# Add a rowid (index) to the nodes data frame
nodes$rowid <- 0:(nrow(nodes) - 1)

# Create a list of links (Sankey diagram data)
links <- combined_sankey_data %>%
  left_join(nodes, by = c("source" = "name")) %>%
  rename(source_id = rowid) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  rename(target_id = rowid) %>%
  select(source_id, target_id, value)

# Create Sankey diagram (without orientation argument)
sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source_id", Target = "target_id", Value = "value", NodeID = "name", units = "Frequency", fontSize = 12, nodeWidth = 30, nodePadding = 10)

# Render the Sankey diagram
sankey
