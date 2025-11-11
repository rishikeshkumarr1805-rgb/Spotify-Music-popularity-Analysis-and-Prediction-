# Install required libraries (run once)
install.packages(c("tidyverse", "corrplot", "GGally", "caret", "ggthemes"))

# Load libraries
library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(ggthemes)

# Read data
spotify <- read.csv("C:/Users/ACER/Desktop/try/spotify.csv")


# View first few rows
head(spotify)

# Select relevant columns
spotify_clean <- spotify %>%
  select(artist_name, track_name, popularity, danceability, energy, loudness,
         speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>%
  na.omit()

# Summary
summary(spotify_clean)

# Check for missing values
colSums(is.na(spotify_clean))


ggplot(spotify_clean, aes(x = popularity)) +
  geom_histogram(fill = "#1DB954", bins = 30, color = "black") +
  ggtitle("Distribution of Song Popularity on Spotify") +
  xlab("Popularity Score") + ylab("Number of Songs") +
  theme_minimal()


# Numeric columns
num_cols <- spotify_clean %>% select(popularity, danceability, energy, loudness,
                                     speechiness, acousticness, instrumentalness,
                                     liveness, valence, tempo)

# Correlation matrix
corr_matrix <- cor(num_cols)

# Plot correlation heatmap
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8,
         title = "Correlation Heatmap of Audio Features", mar = c(0,0,2,0))


# Popularity vs Danceability
ggplot(spotify_clean, aes(x = danceability, y = popularity)) +
  geom_point(color = "#1DB954", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Relationship between Danceability and Popularity") +
  theme_minimal()

# Popularity vs Energy
ggplot(spotify_clean, aes(x = energy, y = popularity)) +
  geom_point(color = "#ff4757", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Relationship between Energy and Popularity") +
  theme_minimal()


# Split data (80% train, 20% test)
set.seed(123)
trainIndex <- createDataPartition(spotify_clean$popularity, p = 0.8, list = FALSE)
train <- spotify_clean[trainIndex, ]
test <- spotify_clean[-trainIndex, ]

# Build linear regression model
model <- lm(popularity ~ danceability + energy + loudness + valence + tempo, data = train)
summary(model)

# Predict on test data
pred <- predict(model, test)

# Evaluate model performance
rmse <- sqrt(mean((pred - test$popularity)^2))
cat("RMSE:", rmse)


top_artists <- spotify_clean %>%
  group_by(artist_name) %>%
  summarise(avg_popularity = mean(popularity)) %>%
  arrange(desc(avg_popularity)) %>%
  slice(1:10)

ggplot(top_artists, aes(x = reorder(artist_name, avg_popularity), y = avg_popularity)) +
  geom_col(fill = "#1DB954") +
  coord_flip() +
  ggtitle("Top 10 Most Popular Artists on Spotify") +
  xlab("Artist") + ylab("Average Popularity") +
  theme_minimal()


top_artists <- spotify_clean %>%
  group_by(artist_name) %>%
  summarise(avg_popularity = mean(popularity)) %>%
  arrange(desc(avg_popularity)) %>%
  slice(1:10)

ggplot(top_artists, aes(x = reorder(artist_name, avg_popularity), y = avg_popularity)) +
  geom_col(fill = "#1DB954") +
  coord_flip() +
  ggtitle("Top 10 Most Popular Artists on Spotify") +
  xlab("Artist") + ylab("Average Popularity") +
  theme_minimal()


