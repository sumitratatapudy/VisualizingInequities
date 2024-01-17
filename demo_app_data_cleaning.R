
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

df1 <- unique(df[c("course.year","course","course.quarter")])
df2 <- unique(df1[c("course","course.quarter")])

#creating a vector with instructors based on the unique quarters and courses

#create empty vector with the length of unique courses and quarter
instructor <- rep(NA,length(df2$course))

#forloop to create vector with instructor 1, instructor 2, etc.
for (i in 1:length(df2$course)) {
  instructor[i] =  paste("instructor",i)
}

#add instructor to data frame with course and quarter
df2$instructors <- instructor

# Using merge to assign instructor to each quarter and course in original dataset
df3 <- merge(x=df,y=df2, 
             by=c("course.quarter","course"))

#create cleaned file
write.csv(df3, "Template_Inequities_In_Course_Performance_Cleaned.csv", row.names=TRUE)