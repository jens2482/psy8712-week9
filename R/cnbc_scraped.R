# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rvest)
library(tidyverse)

# Data Import and Cleaning
sources <- c("Business", "Investing", "Tech", "Politics") #identify four sources
for (source in sources) {  #create a tibble for each source page that includes title and the source
  headlines <- read_html(paste0("https://www.cnbc.com/", tolower(source))) %>%
    html_elements(".Card-title") %>%
    html_text () %>%
    tibble(
      headline = .,
      source = source
    ) 
  assign(paste0("headlines_", source), headlines) #each tibble gets named based on the source
}
combined_headlines <- bind_rows(headlines_Business,headlines_Investing, headlines_Tech, headlines_Politics) #combine all individual tibbles into one
cnbc_tbl <- combined_headlines %>% #mutate on a column for length and reorder columns to match the order in the directions
  mutate(
    length = str_count(headline, "\\S+")
  )%>%
  select(headline, length, source)

# Visualization
ggplot(cnbc_tbl, aes(source,length, fill = source)) +
  geom_col() + 
  theme(legend.position = "none") +
  xlab("Headline Source") + #easier to understand label
  ylab("Number of Words in Headline") #easier to understand label

# Analysis
anova_result <- aov(length ~ source, data = cnbc_tbl) %>%
  summary()%>%
  print()

# Publication
