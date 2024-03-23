# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(tidyverse)


# Data Import and Cleaning
io_output1 <- fromJSON("https://www.reddit.com/r/rstats/.json?limit=100") #scrape first 100 posts
io_output2 <- fromJSON("https://www.reddit.com/r/rstats/.json?limit=100&after=t3_1b1mhmv") #input next 100 posts - I am not sure how to deal with this if the posts are in a different order or if there are more added later
io_df1 <- io_output1$data$children$data #convert to df
io_df2 <- io_output2$data$children$data #convert to df
rstats_tbl1 <- io_df1 %>% #select rows and rename 
  select(c(title, ups, num_comments)) %>%
  rename(c(post = title, upvotes = ups, comments = num_comments))
rstats_tbl2 <- io_df2 %>% #select rows and rename - I did this twice because my data frames from reddit were for some reason coming out with a different number of columns so it didn't work to rbind them before creating the same number of columns
  select(c(title, ups, num_comments)) %>%
  rename(c(post = title, upvotes = ups, comments = num_comments))
rstats_tbl <- rbind(rstats_tbl1, rstats_tbl1) #combine the two separate data frames into one

# Visualization
ggplot(rstats_tbl, aes(upvotes,comments)) +
  geom_jitter() + #there was quite a bit of point overlap so I included some jitter to be able to see the data a little better
  geom_smooth(method = "lm") + # I wanted to be able to visualize the trend in the relationship between these two values
  xlab("Number of Upvotes") + 
  ylab("Number of Comments") 

# Analysis

# Publication
