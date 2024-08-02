pacman::p_load(rvest, httr, xml2, readr, openxlsx, tidyverse)

# URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_emperors_of_Japan"

# Read the HTML content of the page
page <- read_html(url)

# Extract the single table from the page
japanese_emperors <- page %>%
  html_node("table") %>%
  html_table()

# Print the first few rows of the table
head(japanese_emperors)

# selecting columns
japanese_emperors <- japanese_emperors %>%
  select(No., `Posthumous name`, `Reign and era names[8][9][i]`, `Life details`)

# Changing the names of the columns
japanese_emperors <- japanese_emperors %>%
  rename(no. = No.,
         posthumous_name = `Posthumous name`,
         reign_era = `Reign and era names[8][9][i]`, 
         life_details = `Life details`)

# Transforming the observations in X column into lower case
japanese_emperors$life_details <- tolower(japanese_emperors$life_details)

# Adding a space before the first japanese kanji
japanese_emperors$posthumous_name <- str_replace(japanese_emperors$posthumous_name, "([一-龯])", " \\1")

# Removing square brackets 
japanese_emperors$posthumous_name <- gsub("\\[.*?\\]", "", japanese_emperors$posthumous_name)
japanese_emperors$reign_era <- gsub("\\[.*?\\]", "", japanese_emperors$reign_era)
japanese_emperors$life_details <- gsub("\\[.*?\\]", "", japanese_emperors$life_details)

# Identifying the sex
japanese_emperors <- japanese_emperors %>%
  mutate(sex = case_when(
    grepl("Emperor|(Living)", posthumous_name) ~ "Male",
    grepl("Empress", posthumous_name) ~ "Female"))

# Identifying if the emperor was legendary
japanese_emperors <- japanese_emperors %>%
  mutate(legendary = case_when(
    grepl("non-legendary", life_details) ~ "Non-legendary",
    grepl("legendary", life_details) ~ "Legendary",
    TRUE ~ "Non-legendary"
  ))
  
# Extracting the duration of the reign
japanese_emperors$reign_duration_y <- gsub(".*\\((.*?)\\).*", "\\1", japanese_emperors$reign_era) 
japanese_emperors$reign_duration_y <- sub(",.*", "", japanese_emperors$reign_duration_y)

## Maintaining only numeric information
japanese_emperors$reign_duration_y <- parse_number(japanese_emperors$reign_duration_y)

# Extracting the emperors' life duration
japanese_emperors$life_duration_y <- gsub(".*\\((.*?)\\).*", "\\1", japanese_emperors$life_details)

## Keeping the maximum possible ages for Emperor Jimmu and Nintoku
japanese_emperors[1, 8] <- '136'
japanese_emperors[17, 8] <- '109'

## Removing c. for Emperor Go-Kameyama
japanese_emperors[100, 8] <- '77'

## Maintaining only numeric information
japanese_emperors$life_duration_y <- parse_number(japanese_emperors$life_duration_y)

# Extracting the ruling period
japanese_emperors$reign_period <- sub("\\(.*", "", japanese_emperors$reign_era)

# Finding the cause of death
japanese_emperors <- japanese_emperors %>%
  mutate(cause_death = case_when(
    grepl("assassin|murdered|killed", life_details) & !grepl("battle", life_details) ~ "Assassinated",
    grepl("natural", life_details) ~ "Natural Causes",
    grepl("suicide", life_details) ~ "Suicide",
    grepl("executed|beheaded", life_details) ~ "Executed",
    grepl("battle", life_details) ~ "Killed in Battle",
    grepl("poison", life_details) ~ "Poisoned",
    grepl("unknown", life_details) | grepl("\\)$", life_details) ~ "Other/Unknown",
    grepl("illness", life_details) ~ "Illness",
    grepl("tuberculosis|edema|gout|carbuncle|dysentery|epilepsy", life_details) ~ "Illness",
    grepl("(Living)", posthumous_name) ~ "Living",
    TRUE ~ "Other/Unknown"
  ))

# Finding how the emperors reign ended
japanese_emperors <- japanese_emperors %>%
  mutate(succession_status = case_when(
    grepl("abdicated", life_details) ~ "Abdicated",
    grepl("assassinated", life_details) ~ "Assassinated",
    grepl("deposed", life_details) ~ "Deposed",
    grepl("forced to abdicate", life_details) ~ "Forced to abdicate",
    grepl("(Living)", posthumous_name) ~ "Current emperor",
    TRUE ~ "Died while emperor"
  ))

# Adding personal names of living emperors
japanese_emperors[132, 2] <- 'Akihito'
japanese_emperors[133, 2] <- 'Naruhito'

# Extracting how many eras each emperor had
japanese_emperors$eras <- sub(".*\\)\\s*", "", japanese_emperors$reign_era)

## Adjusting the eras of emperor Go-Daigo
japanese_emperors[97, 12] <- 'Bunpō, Genō, Gonko Shōchū, Karyaku, Gentoku, Genkō, Kenmu, Engen'

## Counting eras
japanese_emperors <- japanese_emperors %>%
  mutate(
    eras_n = case_when(
      eras == "" ~ 0,
      !grepl(",", eras) ~ 1,
      grepl(",", eras) ~ 1 + str_count(eras, ",")
    )
  )

# Adding the Japanese periods
japanese_periods <- c(rep('Yayoi', 15), rep('Kofun', 14), rep('Asuka', 15), rep('Nara', 7), rep('Heian', 22), rep('Kamakura', 24), rep('Muromachi', 3),
                      rep('Kamakura', 1), rep('Muromachi', 12), rep('Azuchi-Momoyama', 1), rep('Edo', 14), rep('Meiji', 1), rep('Taisho', 1), rep('Showa', 1),
                      rep('Heisei', 1), rep('Reina', 1))

japanese_emperors$japanese_periods <- japanese_periods

# Transforming japanese_periods into factors to help with the graphs
japanese_emperors$japanese_periods <- factor(japanese_emperors$japanese_periods, levels = unique(japanese_emperors$japanese_periods))

write.csv(japanese_emperors, "japanese_emperors.csv", row.names = FALSE)
write.xlsx(japanese_emperors, file = "japanese_emperors.xlsx")
