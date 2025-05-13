# Dataset: Netflix Movies till 2025
# Source: https://www.kaggle.com/datasets/bhargavchirumamilla/netflix-movies-and-tv-shows-till-2025
nf = read.csv("netflix_movies_detailed_up_to_2025.csv")

# packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(maps)
library(paletteer)
library(ggthemes)


# TILE PLOT: BUDGET VS REVENUE AND AVERAGE RATING OF NETFLIX MOVIES

# Data cleaning:
# remove movies with budget and venue = 0 (probably incomplete data)
# remove movies that have no rating vote
# convert budget and revenue to millions
nf1 = nf %>%
  filter(budget > 0, revenue > 0, vote_count > 0) %>%
  mutate(budget = budget / 1000000) %>%
  mutate(revenue = revenue / 1000000000)

# Plot 2d binned tileplot of budget vs revenue and color code by average rating
ggplot(nf1, aes(x = budget, y = revenue)) +
  stat_summary_2d(aes(z = rating), bins = 30, fun = mean) +
  scale_fill_paletteer_c(name = "Average Rating","ggthemes::Classic Red") +
  labs(title = "Budget vs Revenue and Average Rating of Netflix Movies",
       x = "Budget ($ Millions)",
       y = "Revenue ($ Billions)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(fill = guide_colorbar(barheight = 0.5, barwidth = 10))

  

# WORLD MAP: AVERAGE BUDGET OF NETFLIX MOVIES AROUND THE WORLD

# Data cleaning: 
# remove movies with budget = 0
# convert budget to millions
# use only first country listed if more than one
# rename USA to match world map name format
# average budget grouped by country
# remove blank countries
nf2 = nf[, c("country", "budget")]
nf2 = nf2 %>%
  filter(budget > 0) %>%
  mutate(budget = budget / 1000000) %>%
  mutate(country = str_trim(str_split_fixed(country, ",", 2)[, 1]))
nf2$country = ifelse(grepl("^United States", nf2$country), "USA", nf2$country)
nf2 = nf2 %>%
  group_by(country) %>%
  summarise(avg_budget = mean(budget, na.rm = TRUE)) %>%
  filter(country != "")

# Merge world map data with netflix data and remove Antarctica as it's empty
world = map_data("world")
nf2 = left_join(world, nf2, by = c("region" = "country"))
nf2 = nf2 %>% 
  filter(region != "Antarctica") %>%
  mutate(avg_budget = ifelse(is.na(avg_budget), 0, avg_budget))

# Plot the world map, and color code the countries by the average budget of their movies
ggplot(nf2, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = avg_budget)) +
  scale_fill_paletteer_c("ggthemes::Classic Red") +
  labs(title = "Average Budget of Netflix Movies Around the World",
       subtitle = "By Country of Origin",
       fill = "Average Budget ($ Millions)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barheight = 0.5, barwidth = 10))



# FACET PLOT: COUNT OF NETFLIX MOVIES RELEASED OVER THE YEARS BY GENRE

# Data cleaning:
# use only first genre if there is more than one and remove blank genres
# total number of movies grouped by release year and genre
# only selected a subset of some popular genres
nf3 = nf[, c("show_id", "release_year", "genres")]
nf3 = nf3 %>%
  mutate(genres = str_trim(str_split_fixed(genres, ",", 2)[, 1])) %>%
  filter(genres != "") %>%
  group_by(release_year, genres) %>%
  summarise(movie_count = n(), .groups = "drop") %>%
  filter(genres %in% c("Action", "Animation", "Comedy", "Documentary",
                       "Drama", "Horror", "Romance", "Thriller"))

# Plot the number of movies vs the year released, facet by the movie genre
ggplot(nf3, aes(x = release_year, y = movie_count)) +
  geom_line(color = "#E50914", size = 0.8) +
  facet_wrap(~genres, ncol = 4) +
  labs(title = "Count of Netflix Movies Released Over the Years by Genre",
       x = "Release Year", y = "Movie Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        panel.spacing = unit(1.25, "lines"))



# R GRAPHING FUNDAMENTALS BAR CHART: TOP 5 NEFLIX MOVIES BY REVENUE

# Data cleaning:
# get the top 5 movies by revenue
nf4 <- nf %>%
  arrange(desc(revenue)) %>%
  slice_head(n = 5)

# Bar chart function using R graphing fundamentals
barchart = function(data, title, movies, revenues)
{
  rect(0, 0, 10, 10, border = NA)
  
  # main title
  text(5, 9.4, title, font = 2, cex = 1.2)
  
  # convert revenues to billions, and normalize the values
  revenues = revenues / 1000000000
  ratios = revenues / max(revenues)
  
  # draw the bars
  for (i in 1:5){

    # calculate positions
    y_movie = 8.4 - (i - 1) * 1.7
    y_bar_top = y_movie - 0.5
    y_bar_bottom = y_bar_top - 0.7
    
    bar_length = ratios[i] * 8
    
    # write movie names
    text(0.3, y_movie, labels = movies[i], adj = 0, cex = 0.9)
    
    # draw bars
    rect(xleft = 0.3, xright = 1.5 + bar_length, ybottom = y_bar_bottom, ytop = y_bar_top,
         col = "#E50914", border = NA)
    
    # write revenues inside bars
    revenues[i] = round(revenues[i], 2)
    text(x = bar_length + 0.6, y = (y_bar_top + y_bar_bottom) / 2,
         labels = paste0("$", revenues[i], "B"), 
         adj = 0, cex = 0.8, col = "white", font = 2)
  }
}

# Plot barchart of top 5 netflix movies by revenue
plot.new()
par(mar = c(0, 0, 0, 0))
plot.window(xlim = c(0,10), ylim = c(0,10))
barchart(title = "Top 5 Netflix Movies by Revenue ($ Billions)", 
         movies = nf4$title, revenues = nf4$revenue)
