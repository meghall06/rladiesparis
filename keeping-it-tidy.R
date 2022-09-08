library(tidyverse)

# Workshop data----

# read in the two data files
breed_rank <- read_csv("https://raw.githubusercontent.com/meghall06/rladiesparis/master/breed_rank.csv")
breed_traits <- read_csv("https://raw.githubusercontent.com/meghall06/rladiesparis/master/breed_traits.csv")

# Basic data cleaning----

library(janitor)

# change all the variable names to snake case
breed_traits <- breed_traits |> 
  clean_names()

# some data verification
breed_traits |> 
  count(shedding_level)

# find the wonky observation
breed_traits |> 
  filter(shedding_level == 0) |> 
  select(breed, shedding_level, coat_grooming_frequency, 
         drooling_level)

# and remove it
breed_traits <- breed_traits |> 
  filter(shedding_level != 0)

# create a new data frame and variable for untidy_score
untidy_scores <- breed_traits |> 
  mutate(untidy_score = shedding_level + 
           coat_grooming_frequency + drooling_level) |> 
  select(breed, untidy_score)

# view highest and lowest scores
untidy_scores |> 
  arrange(untidy_score)

untidy_scores |> 
  arrange(desc(untidy_score))

# Bar chart----

# special theme for custom versions
library(showtext) # helps with custom fonts
font_add_google("Prompt", "prompt") # load preferred Google fonts
showtext_auto()

theme_tidy_dog <- function () { 
  theme_linedraw(base_size=13, base_family="prompt") %+replace% 
    theme(
      # justify axis titles
      axis.title = element_text(hjust = 0),
      # backgrounds to match website
      panel.background = element_rect(fill='#F9E0D9', color = NA),
      plot.background = element_rect(fill='#F9E0D9', color=NA),
      legend.background = element_rect(fill="transparent", color=NA),
      legend.key = element_rect(fill="transparent", color=NA),
      # I hate axis ticks and lines :shrug:
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.3), 
      panel.grid.minor = element_blank(),
      # make tweaks to the title and subtitle
      plot.title = element_text(size = 15, hjust = 0, vjust = 0.5, face = "bold", 
                                margin = margin(b = 0.2, unit = "cm")),
      plot.subtitle = element_text(size = 10, hjust = 0, vjust = 0.5, 
                                   margin = margin(b = 0.2, unit = "cm")),
    )
}

# new data frame for the top 6 observations
untidy_dogs <- untidy_scores |> 
  slice_max(untidy_score, n = 6, with_ties = FALSE)

# basic version
untidy_dogs |> 
  ggplot(aes(x = untidy_score, y = breed)) +
  geom_bar(stat = "identity")

# custom version
untidy_dogs |> 
  # can reorder the breeds based on the untidy score
  ggplot(aes(x = untidy_score, y = reorder(breed, untidy_score), 
             label = untidy_score)) +
  # fill colors the bars
  geom_bar(stat = "identity", fill = "#6A395B") +
  # add the data labels
  geom_label(family = "prompt") +
  # ensure the bars go all the way to the axis line
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "The untidiest dogs",
       subtitle = "Based on drooling, shedding, and grooming frequency",
       x = "Untidiness score", y = NULL) +
  theme_tidy_dog()

# Tidy data----

# make the data tidy
ranks_pivoted <- breed_rank |> 
  pivot_longer(`2013 Rank`:`2020 Rank`,
               names_to = "year",
               values_to = "rank")

# and do a bit more cleaning
ranks_pivoted <- ranks_pivoted |> 
  rename(breed = Breed) |> 
  mutate(year = parse_number(year))

# Dot plot----

# basic version
ranks_pivoted |> 
  filter(str_detect(breed, "Bernese")) |> 
  ggplot(aes(x = year, y = rank, label = rank)) +
  geom_point(size = 3) +
  geom_text(vjust = 2)

# custom version
ranks_pivoted |> 
  filter(str_detect(breed, "Bernese")) |> 
  ggplot(aes(x = year, y = rank, label = rank)) +
  geom_point(size = 3) +
  geom_text(vjust = 2) +
  # flip the y-axis
  scale_y_reverse(limits = c(50, 1)) +
  # specify the breaks on the x-axis
  scale_x_continuous(breaks = seq(2013, 2020, 1)) +
  labs(x = NULL, y = "Popularity Rank",
       title = "Popularity of Bernese Mountain Dogs") +
  theme_tidy_dog()

# Line graph----

# keep only the untidiest dog breeds
untidy_popularity <- ranks_pivoted |> 
  filter(breed %in% untidy_dogs$breed)

# check that the filter worked as expected
untidy_popularity |> 
  count(breed)

# basic version
untidy_popularity |> 
  ggplot(aes(x = year, y = rank, group = breed, color = breed)) +
  geom_line() +
  geom_point(size = 3)

# custom version
untidy_popularity |> 
  # create a label that applies for the last point only
  mutate(label = ifelse(year == 2020, breed, NA)) |> 
  ggplot(aes(x = year, y = rank, group = breed, color = breed,
             label = label)) +
  geom_line() +
  geom_point(size = 3) +
  # apply the label so I don't have to use a legend
  geom_text(hjust = -0.1, family = "prompt") +
  # change the color scale
  scale_color_viridis_d(option = "A") +
  # control the width of the x-axis (so I have room for labels)
  # and specify breaks
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.5)),
                     breaks = seq(2013, 2020, 1)) +
  # flip the y-axis so better rankings are "higher"
  scale_y_reverse() +
  labs(title = "Popularity over time of the untidiest dogs",
       subtitle = "Based on drooling, shedding, and grooming frequency",
       x = NULL,
       y = "Popularity Rank") +
  theme_tidy_dog() +
  # remove the legend since I used labels instead
  theme(legend.position = "none")

# Relational data----

# find average ranks
avg_ranks <- ranks_pivoted |> 
  group_by(breed) |> 
  summarize(avg_rank = mean(rank, na.rm = TRUE))

# join in data from untidy_scores
tidy_and_rank <- avg_ranks |> 
  left_join(untidy_scores, by = "breed")

# check that it worked the way we wanted it to
tidy_and_rank |> 
  count(untidy_score)

# find the NA observation
tidy_and_rank |> 
  filter(is.na(untidy_score))

# and remove it
tidy_and_rank <- tidy_and_rank |> 
  filter(!is.na(untidy_score))

# Jitter plot----

# basic version
tidy_and_rank |> 
  ggplot(aes(x = untidy_score, y = avg_rank)) +
  geom_jitter(width = 0.1)

# custom version
tidy_and_rank |>  
  ggplot(aes(x = untidy_score, y = avg_rank)) +
  # specify the labels of the x-axis
  scale_x_continuous(breaks = seq(3, 11, 1)) +
  # control the size, transparency, and color of the points
  geom_jitter(size = 3, width = 0.1, alpha = 0.7, color = "#6A395B") +
  # flip the y-axis and control the labels
  scale_y_reverse(breaks = c(200, 150, 100, 50, 1)) +
  labs(title = "Dog breed popularity compared to their untidiness score",
       subtitle = "Ranking based on 2013-2020 data; tidy score based on drooling, shedding, grooming frequency",
       x = "Untidiness Score",
       y = "Average Popularity Rank") +
  theme_tidy_dog()

# Dumbbell plot----

# new data frame with only the biggest jumps from 2013 to 2020
rank_change <- ranks_pivoted |>  
  filter(year == min(year) | year == max(year)) |> 
  pivot_wider(names_from = "year",
              values_from = "rank") |>  
  mutate(change = `2013` - `2020`) |> 
  filter(`2020` <= 50) |>  
  slice_max(change, n = 6)

# basic version
rank_change |>  
  ggplot(aes(y = breed)) +
  geom_segment(aes(yend = breed, x = `2013`, xend = `2020`)) +
  geom_point(aes(x = `2013`), color = "#c991b8", size = 3) +
  geom_point(aes(x = `2020`), color = "#6A395B", size = 3) +
  geom_label(aes(x = `2020`, label = `2020`), vjust = -0.5) +
  geom_label(aes(x = `2013`, label = `2013`), vjust = -0.5)

# custom version
rank_change |>  
  # create a new variable to find the value of the middle
  mutate(middle = `2020` + (change / 2)) |>  
  # order the breeds by their 2020 rank
  ggplot(aes(y = reorder(breed, -`2020`))) +
  geom_segment(aes(yend = reorder(breed, -`2020`), x = `2013`, xend = `2020`), 
               color = "grey20") +
  geom_point(aes(x = `2013`), color = "#c991b8", size = 3) +
  geom_point(aes(x = `2020`), color = "#6A395B", size = 3) +
  geom_label(aes(x = `2020`, label = `2020`), family = "prompt", vjust = -0.5) +
  geom_label(aes(x = `2013`, label = `2013`), family = "prompt", vjust = -0.5) +
  # label the value of the change at the middle of the line
  geom_text(aes(x = middle, label = str_c("+", change)), family = "prompt", 
            vjust = -0.75, size = 3.5) +
  # flip the x-axis
  scale_x_reverse() +
  labs(x = "Popularity Ranking",
       y = NULL,
       title = "Dog breeds with the biggest jump in popularity from 2013",
       subtitle = "Among the top 50 in 2020") +
  theme_tidy_dog() +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot")

