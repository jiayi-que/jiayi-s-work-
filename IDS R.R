install.packages(c("dplyr", "ggplot2", "lubridate", "tm", "reshape2", "wordcloud", "GGally"))
library(dplyr)
library(ggplot2)
library(lubridate)
library(tm)
library(reshape2)
library(wordcloud)
library(GGally)

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

News_data$title_clean <- sapply(News_data$title, clean_text)
News_data$content_clean <- sapply(News_data$content, clean_text)
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
  labs(title = "Secondary Category Distribution", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))


# Relationship of sources and authors to categories
# Chi-square test for source and categories
source_category_table <- table(News_data$source, News_data$category_level_1)
chi_test_source <- chisq.test(source_category_table)
print(chi_test_source)

# Chi-square test for authors and categories
author_category_table <- table(News_data$author, News_data$category_level_1)
chi_test_author <- chisq.test(author_category_table)
print(chi_test_author)


# Textual Feature Analysis
install.packages('tidytext')
library(tidytext)

# Word Frequency
# Extract the top 5 frequent words from the title and content
top_n_words_title <- title_tokens %>%
  count(word, sort = TRUE) %>%
  top_n(5)

top_n_words_content <- content_tokens %>%
  count(word, sort = TRUE) %>%
  top_n(5)

# View the top 5 words in title and content
print(top_n_words_title)
print(top_n_words_content)

# Create binary indicator variables for the presence of top 5 words
# For title
for (word in top_n_words_title$word) {
  News_data[[paste0("title_contains_", word)]] <- ifelse(grepl(word, News_data$title_clean), 1, 0)
}

# For content
for (word in top_n_words_content$word) {
  News_data[[paste0("content_contains_", word)]] <- ifelse(grepl(word, News_data$content_clean), 1, 0)
}

# Chi-Square Test to check if there's a relationship between the words and the society_category
# Chi-Square Test for Title Words
for (word in top_n_words_title$word) {
  word_col <- paste0("title_contains_", word)
  chi_test_title <- chisq.test(table(News_data$society_category, News_data[[word_col]]))
  print(paste("Chi-Square Test for word:", word, "in title"))
  print(chi_test_title)
}

# Chi-Square Test for Content Words
for (word in top_n_words_content$word) {
  word_col <- paste0("content_contains_", word)
  chi_test_content <- chisq.test(table(News_data$society_category, News_data[[word_col]]))
  print(paste("Chi-Square Test for word:", word, "in content"))
  print(chi_test_content)
}

# Title
News_data$title_contains_uk <- ifelse(grepl("uk", News_data$title_clean), 1, 0)
News_data$title_contains_trump <- ifelse(grepl("trump", News_data$title_clean), 1, 0)
News_data$title_contains_weather <- ifelse(grepl("weather", News_data$title_clean), 1, 0)

# Content
News_data$content_contains_people <- ifelse(grepl("people", News_data$content_clean), 1, 0)
News_data$content_contains_government <- ifelse(grepl("government", News_data$content_clean), 1, 0)
News_data$content_contains_president <- ifelse(grepl("president", News_data$content_clean), 1, 0)

# Logistic Regression Model (with only significant words)
# Logistic Regression for Title
logistic_model_title <- glm(society_category ~ title_contains_uk + title_contains_trump + title_contains_weather, 
                            data = News_data, 
                            family = binomial())

# Logistic Regression for Content
logistic_model_content <- glm(society_category ~ content_contains_people + content_contains_government + content_contains_president, 
                              data = News_data, 
                              family = binomial())

# ANOVA (Analysis of Variance) on Logistic Regression Models
# ANOVA for Title Model
anova_title <- anova(logistic_model_title, test = "Chisq")
print("ANOVA for Title Model")
print(anova_title)

# ANOVA for Content Model
anova_content <- anova(logistic_model_content, test = "Chisq")
print("ANOVA for Content Model")
print(anova_content)

summary(logistic_model_title)
summary(logistic_model_content)

