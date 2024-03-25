# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(tidyverse)

# Data Import and Cleaning
io_output1 <- fromJSON("https://www.reddit.com/r/rstats/.json?limit=100&after=t3_1bkemcp") #scrape first 100 posts
io_output2 <- fromJSON("https://www.reddit.com/r/rstats/.json?limit=100&after=t3_1b0idff") #input next 100 posts - I am not sure how to deal with this if the posts are in a different order or if there are more added later
io_df1 <- io_output1$data$children$data #convert to df
io_df2 <- io_output2$data$children$data #convert to df
rstats_tbl1 <- io_df1 %>% #select rows and rename 
  select(c(title, ups, num_comments)) %>%
  rename(c(post = title, upvotes = ups, comments = num_comments))
rstats_tbl2 <- io_df2 %>% #select rows and rename - I did this twice because my data frames from reddit were for some reason I was getting an error (names do not match previous names) when I tried to bind the original data frames before condensing the columns
  select(c(title, ups, num_comments)) %>%
  rename(c(post = title, upvotes = ups, comments = num_comments))
rstats_tbl <- rbind(rstats_tbl1, rstats_tbl1) #combine the two separate data frames into one

# Visualization
ggplot(rstats_tbl, aes(upvotes,comments)) +
  geom_jitter() + #there was quite a bit of point overlap so I included some jitter to be able to see the data a little better
  geom_smooth(method = "lm") + # I wanted to be able to visualize the trend in the relationship between these two values
  xlab("Number of Upvotes") + #easier to understand label
  ylab("Number of Comments") #easier to understand label

# Analysis
correlation <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments) #correlation test (gets correlation and p-value in one step)
cat("Correlation coefficient:", correlation$estimate, "\n", "P-value:", correlation$p.value) #I wasn't sure if display was different than print...so I made it look a little fancier with cat()

# Publication
formatted_correlation <- str_replace(formatC(correlation$estimate, format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero
formatted_p_value <- str_replace(formatC(correlation$p.value, format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero
significance_outcome <- ifelse(correlation$p.value <= 0.05, "was", "was not")
cat("The correlation between upvotes and comments was r(", correlation$parameter, ") = ", formatted_correlation, ", p = ", formatted_p_value, ". This test ", significance_outcome, " statistically significant.", sep ="")
# The correlation between upvotes and comments was r(198) = .60, p = .00. This test was statistically significant.