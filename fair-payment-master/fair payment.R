library(broom)
library(readr)
load(ggplot2)

database_url <- "https://assets.datacamp.com/production/course_5977/datasets/fair_pay_data.csv"

pay <- read_csv(database_url)

pay %>% group_by(new_hire) %>% summarize(avg_salary=mean(salary))


# The new hires have a higher average salary than the more tenured employees,
# but I don't yet know whether that difference is statistically significant.

t_test_result <- t.test(salary ~ new_hire, data=pay) %>% tidy()
significant <- t_test_result$p.value<0.05

#One variable you didn't consider in your earlier statistical test is job level. 
#Different job levels tend to have different pay levels, so if the mix of job levels for new hires 
#is different than the mix of job levels for current employees, you could be dealing with omitted 
#variable bias.

#Check whether the job level composition is the same for new hires as it is for current employees.

ggplot(data=pay, aes(x=new_hire, fill=job_level))+
  geom_bar(position="fill")


#It appears that the job level mix is different for new hires. 
#New hires are less likely to be hourly employees, and more likely to be salaried or managers. 
#Do new hires have a higher average salary than current employees when job level is taken into account? 
#Calculate the average salaries, and then recreate the bar chart from earlier in the chapter, 
#adding faceting to split it up by the three job levels. Are the bar heights closer together 
#than they were in the first plot?


# Calculate the average salary for each group of interest
pay_grouped <- pay %>% 
  group_by(new_hire, job_level) %>% 
  summarize(avg_salary = mean(salary))

# Graph the results using facet_wrap()  
pay_grouped %>%
  ggplot(aes(x=new_hire, y=avg_salary))+
  geom_col()+
  facet_wrap(~job_level, scale="free")


#the graph shows a small difference in the average salaries for hourly workers. 
#Test whether a significant pay difference exists between hourly new hires and hourly current employees.

# Filter the data to include only hourly employees
pay_filter <- pay %>% filter(job_level=="Hourly")

# Test the difference in pay
t_test_result_2 <- t.test(salary ~ new_hire, data=pay_filter) %>% tidy()

# Is the result significant?
significant <- t_test_result_2$p.value<.05
