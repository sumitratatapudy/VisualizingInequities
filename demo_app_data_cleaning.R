


library(dplyr)
library(stringr)
library(tibble)

#load your data file
df <-read.csv("Template_Inequities_In_Course_Performance.csv") %>% mutate(course.grade = as.numeric(course.grade)) %>% na.omit()

#Change Quarters to  Winter, Spring, Summer,Fall
df$course.quarter <- with(df,
                                                    factor(course.quarter,
                                                           levels = 1:4,
                                                           labels = c("Winter", "Spring","Summer","Fall")))
#replace course titles

#100 level courses
df<- df %>% mutate(across('course', str_replace, 'CLASS.120', 'Course10B'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.142', 'Course10C'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.152', 'Course10D'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.162', 'Course10E'))

#200 level courses
df<- df %>% mutate(across('course', str_replace, 'CLASS.220', 'Course20A'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.221', 'Course20B'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.237', 'Course20C'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.238', 'Course20D'))
df<- df %>% mutate(across('course', str_replace, 'CLASS.239', 'Course20E'))


write.csv(df, "Template_Inequities_In_Course_Performance_Cleaned.csv", row.names=TRUE)