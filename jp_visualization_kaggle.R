# Loading packages
pacman::p_load(tidyverse, plotly)

# Classifying emperors by sex and legendary status
## Creating new df
leg_sex <- japanese_emperors %>%
  count(legendary, sex)

## Calculating percentage of each category
total_cases <- sum(leg_sex$n)

leg_sex <- leg_sex %>%
  mutate(percent_total = (n / total_cases) * 100)

head(leg_sex)

## Creating graph
ggplot(leg_sex, aes(x = legendary, y = n, fill = sex)) +
  geom_col(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF")) +
  labs(
    title = "Distribution of Legendary and Non-Legendary Cases by Sex",
    x = "Category",
    y = "Number of Cases",
    fill = "Sex"
  ) +  
  theme_minimal() 

# Life duration
## Histogram with life duration
japanese_emperors %>%
  ggplot(aes(x = life_duration_y)) +
  geom_histogram(bins = 20, fill = "black", color = "white") +
  labs(
    title = "Histogram - Life Duration of Japanese Emperors",
    x = "Life Duration (years)"
  ) +
  theme_minimal()

## Geompoint by emperor
japanese_emperors %>%
  ggplot(aes(x = posthumous_name, y = life_duration_y, color = legendary, text = posthumous_name)) +
  geom_point(size = 2) +
  labs(
    title = "Life Duration of Japanese Emperors by legendary status",
    y = "Life Duration (years)",
    color = "Legendary Status"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) -> life_duration_jp_emp

### Making the plot interactive
ggplotly(life_duration_jp_emp, tooltip = "text")

## Separating by periods
japanese_emperors %>%
  ggplot(aes(x = japanese_periods, y = life_duration_y, text = posthumous_name)) +
  geom_point(size = 2) +
  labs(
    title = "Life Duration of Japanese Emperors by japanese periods",
    y = "Life Duration (years)",
    color = "Japanese periods"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) -> life_duration_jp_emp_jp_period

### Making the plot interactive
ggplotly(life_duration_jp_emp_jp_period, tooltip = "text")

# Years in office
## histogram
japanese_emperors %>%
  ggplot(aes(x = reign_duration_y)) +
  geom_histogram(bins = 20, fill = "black", color = "white") +
  labs(
    title = "Histogram - Years in office of Japanese Emperors",
    x = "Years in office"
  ) +
  theme_minimal()

## Geompoint by emperor
japanese_emperors %>%
  ggplot(aes(x = posthumous_name, y = reign_duration_y, color = legendary, text = posthumous_name)) +
  geom_point(size = 2) +
  labs(
    title = "Reign Duration of Japanese Emperors",
    y = "Reign Duration (years)",
    color = "Legendary Status"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) -> p

### Making the plot interactive
ggplotly(p, tooltip = "text")

## Separating by periods
japanese_emperors %>%
  ggplot(aes(x = japanese_periods, y = reign_duration_y, text = posthumous_name)) +
  geom_point(size = 1) +
  labs(
    title = "Reign Duration of Japanese Emperors by japanese periods",
    y = "Reign Duration (years)",
    color = "Japanese periods"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) -> life_duration_jp_emp_jp_period

### Making the plot interactive
ggplotly(life_duration_jp_emp_jp_period, tooltip = "text")


# Succession status
## Creating the dfs
succession_status_leg <- japanese_emperors %>%
  count(succession_status, legendary)
succession_status_periods <- japanese_emperors %>%
  count(succession_status, japanese_periods)

### Creating graph with legendary
ggplot(succession_status_df, aes(x = succcession_status, y = n, fill = legendary)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1))

### Creating graph with succession
ggplot(succession_status_periods, aes(x = japanese_periods, y = n, fill = succession_status)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
# Mean eras per Japanese period
## Creating new df with the mean
era_means <- japanese_emperors %>%
  group_by(japanese_periods) %>%
  summarize(mean_eras = mean(eras_n))

## Creating graph
ggplot(era_means, aes(x = japanese_periods, y = mean_eras)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Mean Reign Duration (years)", x = "Japanese Periods")
