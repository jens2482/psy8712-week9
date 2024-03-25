# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rvest)
library(tidyverse)

# Data Import and Cleaning
sources <- c("Business", "Investing", "Tech", "Politics") #identify four sources
for (source in sources) {  #create a tibble for each source page that includes title and the source
  headlines <- read_html(paste0("https://www.cnbc.com/", tolower(source))) %>% #needed to make source name lowercase in order to create correct URL
    html_elements(".Card-title") %>% #Used the tool you showed us to get the code for the headlines
    html_text () %>% #convert to text
    tibble(
      headline = .,  #this changed the name of the column with imported data to headline
      source = source #added a column with the name of the source
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
ggplot(cnbc_tbl, aes(source,length, fill = source)) + #thought a bar plot would be best to compare here. I didn't need to add the colors but I like it when the bars are different colors
  geom_col() + #geom_col instead of geom_bar so that stat would default to identity...I think...if I did it right
  theme(legend.position = "none") + #since the labels appear underneath the columns I figured I did not also need a legend
  xlab("Headline Source") + #easier to understand label
  ylab("Number of Words in Headline") #easier to understand label

# Analysis
anova_result <- aov(length ~ source, data = cnbc_tbl) %>% #I don't have a lot of experience with ANOVAS (a gap in my stats education...we just skipped to regression) so hopefully this is right
  summary()%>%
  print()

# Publication
formatted_F <- formatC(anova_result[[1]]$'F value'[[1]], format = "f", digits = 2) #2 decimal places
formatted_p_value <- str_replace(formatC(anova_result[[1]]$'Pr(>F)'[[1]], format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero
significance_outcome <- ifelse(anova_result[[1]]$'Pr(>F)'[[1]] <= 0.05, "was", "was not")
cat("The results of an ANOVA comparing lengths across sources was F(", anova_result[[1]]$Df[[1]],",", anova_result[[1]]$Df[[2]], ") = ", formatted_F, ", p = ", formatted_p_value, ". This test ", significance_outcome, " statistically significant.", sep ="")
# The results of an ANOVA comparing lengths across sources was F(3,130) = 3.39, p = .02. This test was statistically significant.
