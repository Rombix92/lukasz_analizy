library(ggplot2)
library(dplyr)
library(readr)

performance_url <- "https://assets.datacamp.com/production/course_5977/datasets/performance_data.csv"
hr_url <- "https://assets.datacamp.com/production/course_5977/datasets/hr_data.csv"

hr_data <- read.csv(hr_url)
performance_data <-read.csv(performance_url)

#In many organizations, employees at higher job levels in the organization are more likely to be considered high performers. That is, the distribution of performance ratings is not always the same at different job levels.

#First, check the difference in job level distribution by gender to see if there is any potential for job level being an omitted variable we need to consider. Then plot the high performer distribution by both gender and job level. Does it look like the gender difference disappears?
  

#Now that you've looked at hr_data and performance_data, it's time to join them. Use the techniques from the video to put all the data you need in one data frame. Once you have the final data frame, check to see whether the average performance rating is higher for men or women.

# Join the two tables
joined_data <- left_join(hr_data, performance_data, by = "employee_id")

# Examine the result
summary(joined_data)

# Check whether the average performance rating differs by gender 
joined_data %>% group_by(gender) %>% summarize(avg_rating=mean(rating))


# Add the high_performer column
performance <- joined_data %>%  
  mutate(high_performer = ifelse(rating >= 4, 1, 0))

# Test whether one gender is more likely to be a high performer
chisq.test(performance$gender, performance$high_performer)   

# Do the same test, and tidy the output
library(broom)
chisq.test(performance$gender, performance$high_performer) %>% 
  tidy()



# Visualize the distribution of job_level by gender
performance %>%
  ggplot(aes(x = gender, fill = job_level)) +
  geom_bar(position = "fill")

# Test whether men and women have different job level distributions
chisq.test(performance$gender, performance$job_level) 

# Visualize the distribution of high_performer by gender, faceted by job level
performance %>% 
  ggplot(aes(x=gender, fill=factor(high_performer)))+
  geom_bar(position="fill")+
  facet_wrap(~job_level)

#Now that you're warmed up on logistic regression, you can use it to statistically test what you saw in an earlier graph - are women less likely to be labeled a high performer, even when taking differences in job level into account? You can test this by adding job_level into the glm() formula and looking at the new p-value. The process is the same as adding a new variable to a linear regression.

# Run a multiple logistic regression
logistic_multiple <- glm(high_performer ~ gender + job_level, family = "binomial", data = performance)

# View the result with summary() or tidy()
logistic_multiple %>%
  tidy()

# Is the result significant?  
significant <- TRUE

#You've seen that even when differences in job levels are taken into account, women are less likely to be labeled high performers than men in this invented company dataset.
#Which of the following recommendations is the best to make based on this result?
#Undertake an initiative to ensure performance ratings are given fairly.



