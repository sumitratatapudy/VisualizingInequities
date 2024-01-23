###### Instructions for modification - see supplemental for details ######
# There are two ways to interact with this code
# places where you need to make changes in order to run the app with your own 
# data are tagged with the word "huskies"
# places where you can make changes to add features to the app are tagged 
# with the word "cougars"
# use command f and search for either "huskies" or "cougars"
# to find all the places you need to change


# load necessary packages
library(shiny)
library(dplyr)
library(tidyverse)

## load your data file
data.years.names.substituteR <- read.csv("Template_Inequities_In_Course_Performance_Cleaned.csv") #huskies: make sure the text in quotations exactly matches your csv file name
 

# ##just for development WILL REMOVE LATER!: COUGARS add additional variable (values randomly generated)
# data.years.names.substituteR$Additional_Var = rbinom(length(data.years.names.substituteR$course.grade), 1, 0.5)

## Define authentication credentials
user_base <- tibble::tibble(
  user = c("user1", "user2"), #creates a list of authorized user names
  password = c("pass1", "pass2"), #creates a list of authorized passwords
  permissions = c("standard", "standard"), #sets permissions for each user
  name = c("User One", "User Two")# sets display name for each user
)

## Design all tabs present in UI
login_tab <- tabPanel(title = icon("lock"), 
                      value = "login",
                      shinyauthr::loginUI("login"))
#UI code
# creates the home page
home_tab <-
  tabPanel(
    
    title = "Approach towards visualizing inequities",
    h4("Systemic inequities are often invisible and challenging to identify. You are taking a crucial step in working towards mitigating inequities in your classroom just by being here. 
      Welcome!"),
    strong("What this app is:"),
    br(),
    tags$ul(
      tags$li("This app is intended as a tool for self-reflection: Instructors know their class environment best; this tool aids identification of equitable outcomes persist and provides an opportunity to make changes."), 
      tags$li("Comparing performance before/after intervention in class by examining outcomes over time/terms.")), 
    strong("Larger context:"),
    tags$ul(
      tags$li("Differences between student groups are due to systemic barriers that present opportunity gaps, not due to inherent differences in abilities."), 
      tags$li("While you may not have control over the larger systemic issues, your classroom is a space where you could have control to mitigate these differences with thoughtful/reflective pedagogy."),
      tags$li("It can be intimidating to start. The results from this app can be a way to identify where to focus those efforts. You know your class best, and then you can customize your interventions. ")
    )  
    )
  

# creates the data tab
data_tab <- tabPanel(title = "Data",
                     fluidPage(
                       titlePanel("Student grade distributions"),
                       sidebarLayout(
                         # the following code sets up the options for dis aggregating data
                         sidebarPanel( 
                           width = 4,
                           
                           # cougars, the following codes creates a slider to select what years you want 
                           # to display in the visual. You can use this code as a template to create a 
                           # similar slider with other variables of interest
                           sliderInput( 
                             "year",
                             "Choose year",
                             min = 2001, #huskies, update this with the minimum year from your data
                             max = 2016, #huskies, update this with the maximum year from your data
                             value = c(2010, 2014), #huskies, sets the initial date range to be displayed
                             #make sure the years in the line above are found in your data
                             sep = ""
                           ), 
                           
                           # cougars, the following code creates a drop-down to select which course
                           # you want to visualize
                           selectInput(
                             "course",
                             "Choose course",
                             choices = c(unique(data.years.names.substituteR$course)),
                             selected = "Course10C" #huskies, sets the initial course to be displayed
                             #change the course name in quotations to a course name found in your data
                           ),
                           
                           # cougars, creates a drop-down to select which quarter you want to visualize
                           selectInput( 
                             "quarter",
                             "Choose quarter",
                             
                             # huskies, you will need to change the items in the quotations marks in the
                             # following choices list to match the names in the course.quarter column in your data
                             choices = c(
                               "None selected",
                               "Winter",
                               "Spring",
                               "Summer",
                               "Fall"
                             ),
                             selected = "None selected"
                           ),
                           
                           # creates a drop-down to select which minoritized group you would like to disaggreagate by
                           selectizeInput(
                             "minoritized_how", 
                             "Which minoritized group would you like data disggreagated by?",
                             
                             # the following code defines the options for disaggregating the data
                             # huskies, ensure the following list matches column names in your data
                             # you can also add column names
                             choices = c( 
                               "None selected",
                               "Racially Minoritized",
                               "Binary Gender",
                               "First Generation Status"#, #COUGARS (comma only)
                               #"Additional Variable" #COUGARS
                             ),
                             selected = "None selected"
                           ),
                           textOutput("minoritized_how_info"),
                           textOutput("minoritized_how_info2"),
                           textOutput("minoritized_how_info3")#, #COUGAR (comma only)
                           #textOutput("minoritized_how_info4") #COUGAR
                         ),
                         
                         mainPanel(
                           plotOutput("plot1"),
                           textOutput("intervention_info"),
                           textOutput("intervention_info2"),
                           textOutput("intervention_info3"),
                           #textOutput("intervention_info4"), #COUGAR
                           uiOutput("url1"),
                           uiOutput("url2"),
                           uiOutput("url3"),
                           uiOutput("url1.2"),
                           uiOutput("url2.2"),
                           uiOutput("url3.2"),
                           uiOutput("url1.3"),
                           uiOutput("url2.3"),
                           uiOutput("url3.3"), #COUGAR (just the comma)
                           uiOutput("url4.3")
                           #uiOutput("url1.4"), #COUGAR
                           #uiOutput("url2.4"), #COUGAR
                           #uiOutput("url3.4") #COUGAR
                         )
                         
                         
                       )
                     ))


#Define how UI will be displayed on app
ui <- navbarPage(title = "Visualizing inequities in student performance",
                 id = "tabs",
                 collapsible = TRUE,
                 login_tab)


#FUNCTION code
server <- function(input, output, session) {
  #logout button UI
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(class = "nav navbar-nav navbar-right",
                 tags$li(
                   div(style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
                       shinyauthr::logoutUI("logout"))
                 ))
  )
  
  ## Providing access to authenticated users to view app UI and data
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      removeTab("tabs", "login")
      
      appendTab("tabs", home_tab, select = TRUE)
      
      output$user_data <-
        renderPrint({
          dplyr::glimpse(credentials()$info)
        })
      
      appendTab("tabs", data_tab)
      
    }
  })
  
  #login and logout functions
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))
  observeEvent(input$logout, {
    shinyjs::alert("Thank you!")
  })

  
  ## Resources and descriptions associated with graphs
  
  #URM Resources
  url1 <- a("Re-Envisioning the Culture of Undergraduate Biology Education to Foster Black Student Success", href = "https://www.lifescied.org/doi/full/10.1187/cbe.22-09-0175")
  url2 <- a("Teaching About Racial Equity in Introductory Physics Courses", href = "https://pubs.aip.org/aapt/pte/article-abstract/55/6/328/782527/Teaching-About-Racial-Equity-in-Introductory?redirectedFrom=fulltext")
  url3 <- a("Student-Authored Scientist Spotlights", href = "https://www.lifescied.org/doi/full/10.1187/cbe.21-03-0060")

  #Binary Gender Resources
  url4 <- a("Gender Gaps in Achievement and Participation in Intro Biology", href = "https://www.google.com/url?q=https://www.lifescied.org/doi/full/10.1187/cbe.13-10-0204&sa=D&source=docs&ust=1706046973389795&usg=AOvVaw0bli17OADKj5hdiAx5p6wt")
  url5 <- a("Transgender, Nonbinary, Gender Nonconforming, and Questioning Students in Biology", href = "https://www.lifescied.org/doi/full/10.1187/cbe.21-12-0343")
  url6 <- a("Reducing the gender gap in the physics classroom", href = "https://pubs.aip.org/aapt/ajp/article-abstract/74/2/118/1039347/Reducing-the-gender-gap-in-the-physics-classroom?redirectedFrom=fulltext")
  url7 <- a("Values Affirmation to Reduce Gender Acheivement Gap in Science", href = "https://www.science.org/doi/10.1126/science.1195996")
  
  #First Gen Resources
  url8 <-a("How to design a high-structure class", href = "https://files.eric.ed.gov/fulltext/EJ1268125.pdf")
  url9 <- a("Sample materials from a high-structure class", href = "https://www.lifescied.org/doi/10.1187/cbe.14-03-0050")
  url10 <-a("Resources from UW Teach", href = "https://teaching.washington.edu/inclusive-teaching/supporting-specific-student-groups/first-generation-students/")
  
  ##RACIALLY MINORITIZED
  reactive_function2 <- eventReactive(input$minoritized_how, {
    req(input$minoritized_how == "Racially Minoritized")
  })
  
  output$minoritized_how_info2 <- renderText({
    reactive_function2()
    # huskies, make sure the text below matches the definition of "racially minoritized"
    # in your data
    paste(
      "Students who identify as Black, African-American, Latinx, Hawaiian/Pacific Islander and/or American Indian
"
    )
  })
  
  output$intervention_info2 <- renderText({
    reactive_function2()
    paste(
      "Studies multiple strategies for creating a more equitable classroom environment, including acknowledging racial equity topics in the classroom, intentionally providing equitable access to resources, and instructors self reflecting and coming up with action plans to address biases."
    )
  })
  

  output$url1.2 <- renderUI({
    reactive_function2()
    tagList(url1)
    
  })
  
  output$url2.2 <- renderUI({
    reactive_function2()
    tagList(url2)
    
  })
  
  output$url3.2 <- renderUI({
    reactive_function2()
    tagList(url3)
    
  })
  
  ##BINARY GENDER
  reactive_function3 <- eventReactive(input$minoritized_how, {
    req(input$minoritized_how == "Binary Gender")
  })
  
  output$minoritized_how_info3 <- renderText({
    reactive_function3()
    # huskies, make sure the text below matches the definition of gender in your data
    paste(
      "Binary gender is the classification of gender into two distinct forms of masculine and feminine, whether by social system, cultural belief, or both simultaneously
"
    )
  })
  
  output$intervention_info3 <- renderText({
    reactive_function3()
    paste(
      "Many studies reflect on how to reduce gender gaps by encorporating active learning and changing grading stuctures. However, it is also important to acknowledge the erasure of nonbinary and transgender invidividuals in the majority of gender based education studies."
    )
  })
  
  output$url1.3 <- renderUI({
    reactive_function3()
    tagList(url4)
    
  })
  
  output$url2.3 <- renderUI({
    reactive_function3()
    tagList(url5)
    
  })
  
  output$url3.3 <- renderUI({
    reactive_function3()
    tagList(url6)
    
  })
  
  output$url4.3 <- renderUI({
    reactive_function3()
    tagList(url7)
    
  })
  
  
  
  ## FIRST GEN STATUS
  reactive_function <- eventReactive(input$minoritized_how, {
    req(input$minoritized_how == "First Generation Status")
  })
  
  
  
  
  output$minoritized_how_info <- renderText({
    reactive_function()
    # huskies, make sure the text below matches the definition of first generation used in your data
    paste("Students whose parents did not complete a 4-year college or university degree")
  })
  
  output$intervention_info <- renderText({
    reactive_function()
    paste(
      "Literature on learning tells us that first-generation students benefit from high structure classes.

Here are some ways to incorporate high structure in your course: "
    )
  })
  output$url1 <- renderUI({
    reactive_function()
    tagList(url8)
    
  })
  
  output$url2 <- renderUI({
    reactive_function()
    tagList(url9)
    
  })
  
  output$url3 <- renderUI({
    reactive_function()
    tagList(url10)
    
  })
  
  
#   ## COUGARS -- ADDITIONAL VARIABLE
#   reactive_function4 <- eventReactive(input$minoritized_how, {
#     req(input$minoritized_how == "Additional Variable")
#   })
# 
#   output$minoritized_how_info4 <- renderText({
#     reactive_function4()
#     paste(
#       "[Give a definition of your variable here]
# "
#     )
#   })
# 
#   output$intervention_info4 <- renderText({
#     reactive_function4()
#     paste(
#       "[Insert a summary of suggestions/interventions and some links below]: "
#     )
#   })
#   #NOTE -- link to something other than urls 7,8,9... create new urls relevant to your additional variable
#   output$url1.4 <- renderUI({
#     reactive_function4()
#     tagList(url7)
# 
#   })
# 
#   output$url2.4 <- renderUI({
#     reactive_function4()
#     tagList(url8)
# 
#   })
# 
#   output$url3.4 <- renderUI({
#     reactive_function4()
#     tagList(url9)
# 
#   })
  
## The following code creates the violin plot that is rendered above
  
  #Data disaggregated by students majoritized and minoritized on basis of race when no course quarter selected -- WRONG it's when a Q is selected
  output$plot1 <- renderPlot({
    req(credentials()$user_auth)
    if (input$quarter != "None selected") {
      if (input$minoritized_how == "Racially Minoritized") {
        data.years.names.substitute.subset <-
          data.years.names.substituteR %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          select(course.year, Racially_Minoritized, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        ggplot(
          data.years.names.substitute.subset,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(Racially_Minoritized)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.2,
          linetype = "dotted"
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.2,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + 
          ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_fill_manual(values = c("#d5edf6", "#55b9dd"), 
                            name=NULL,
                            labels = c(
                              'Not Racially Minoritized',
                              'Racially Minoritized',
                              'Did not indicate'
                            ))
      }
      #Data disaggregated by students gender when no course quarter selected
      else if (input$minoritized_how == "Binary Gender") {
        data.years.names.substitute.subset2 <-
          data.years.names.substituteR %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          select(course.year, Gender, course.grade) %>%
          na.omit(data.years.names.substitute.subset2)
        ggplot(
          data.years.names.substitute.subset2,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(Gender)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.2,
          linetype = "dotted"
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.2,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_fill_manual(values = c("#d9f4d7", "#67d35f"), 
                            name=NULL,
                            labels = c(
                              'Male',
                              'Female',
                              'Did not indicate'
                            ))
      }
      #Data disaggregated by students first generation status when no course quarter selected
      else if (input$minoritized_how == "First Generation Status") {
        data.years.names.substitute.subset3 <-
          data.years.names.substituteR %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          select(course.year, First_Generation, course.grade) %>%
          na.omit(data.years.names.substitute.subset3)
        ggplot(
          data.years.names.substitute.subset3,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(First_Generation)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.2,
          linetype = "dotted"
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.2,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_fill_manual(values = c("#fad1d2", "#ed5e61"), 
                            name=NULL,
                            labels = c(
                              'Not First Generation Student',
                              'First Generation Student'
                            ))
      }
      
      # #COUGARS -- Data disaggregated by additional variable when no course quarter selected
      # else if (input$minoritized_how == "Additional Variable") {
      #   data.years.names.substitute.subset3 <-
      #     data.years.names.substituteR %>%
      #     filter(
      #       course == input$course,
      #       course.quarter == input$quarter,
      #       course.year >= input$year[1] &
      #         course.year <= input$year[2]
      #     ) %>%
      #     select(course.year, Additional_Var, course.grade) %>%
      #     na.omit(data.years.names.substitute.subset3)
      #   ggplot(
      #     data.years.names.substitute.subset3,
      #     aes(
      #       x = as.factor(course.year),
      #       y = course.grade,
      #       fill = as.factor(Additional_Var)
      #     )
      #   ) + geom_violin(
      #     draw_quantiles = c(0.25, 0.75),
      #     position = position_dodge(0.5),
      #     width =
      #       0.2,
      #     linetype = "dotted"
      #   ) + geom_violin(
      #     draw_quantiles = .5,
      #     position = position_dodge(0.5),
      #     width =
      #       0.2,
      #     alpha = 0
      #   ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
      #     theme(plot.title = element_text(hjust = 0.5)) +
      #     scale_fill_manual(values = c("#ffe6cc", "#ff901a"),
      #                       name=NULL,
      #                       labels = c(
      #                         'Group 1',
      #                         'Group 2'
      #                       ))
      # }
      
      #Data not disaggregated when no course quarter selected
      else {
        data.years.names.substituteR %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          ggplot(aes(x = as.factor(course.year),
                     y = course.grade,
                     #fill = course.year,
                     group = course.year)) + geom_violin(
                       draw_quantiles = c(0.25, 0.75),
                       position = position_dodge(0.5),
                       width =
                         0.2,
                       linetype = "dotted"
                     ) + geom_violin(
                       draw_quantiles = .5,
                       position = position_dodge(0.5),
                       width =
                         0.2,
                       alpha = 0
                     ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5))
        
      }
      
    }
    else {
      #Data disaggregated by students majoritized and minoritized on basis of race when specific course quarter selected
      if (input$minoritized_how == "Racially Minoritized") {
        data.years.names.substitute.subset4 <-
          data.years.names.substituteR %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          select(course.year, Racially_Minoritized, course.grade) %>%
          na.omit(data.years.names.substitute.subset4)
        ggplot(
          data.years.names.substitute.subset4,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(Racially_Minoritized)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.2,
          linetype = "dotted"
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.2,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + 
          ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_fill_manual(values = c("#d5edf6", "#55b9dd"), 
                            name=NULL,
                            labels = c(
                              'Not Racially Minoritized',
                              'Racially Minoritized',
                              'Did not indicate'
                            ))
      }
      
      #Data disaggregated by students gender when specific course quarter selected
      else if (input$minoritized_how == "Binary Gender") {
        data.years.names.substitute.subset5 <-
          data.years.names.substituteR %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          select(course.year, Gender, course.grade) %>%
          na.omit(data.years.names.substitute.subset5)
        ggplot(
          data.years.names.substitute.subset5,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(Gender)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.2,
          linetype = "dotted"
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.2,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5))+
          scale_fill_manual(values = c("#d9f4d7", "#67d35f"), 
                            name=NULL,
                            labels = c(
                              'Male',
                              'Female',
                              'Did not indicate'
                            ))
      }
      
      #Data disaggregated by students first generation status when specific course quarter selected
      else if (input$minoritized_how == "First Generation Status") {
        data.years.names.substitute.subset6 <-
          data.years.names.substituteR %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          select(course.year, First_Generation, course.grade) %>%
          na.omit(data.years.names.substitute.subset6)
        ggplot(
          data.years.names.substitute.subset6,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(First_Generation)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.2,
          linetype = "dotted"
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.2,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA")  + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_fill_manual(values = c("#fad1d2", "#ed5e61"), 
                            name=NULL,
                            labels = c(
                              'Not First Generation Student',
                              'First Generation Student'
                            ))
      }
      
      # #COUGAR -- Data disaggregated by additional varaible when specific course quarter selected
      # else if (input$minoritized_how == "Additional Variable") {
      #   data.years.names.substitute.subset6 <-
      #     data.years.names.substituteR %>%
      #     filter(course == input$course,
      #            course.year >= input$year[1] &
      #              course.year <= input$year[2]) %>%
      #     select(course.year, Additional_Var, course.grade) %>%
      #     na.omit(data.years.names.substitute.subset6)
      #   ggplot(
      #     data.years.names.substitute.subset6,
      #     aes(
      #       x = as.factor(course.year),
      #       y = course.grade,
      #       fill = as.factor(Additional_Var)
      #     )
      #   ) + geom_violin(
      #     draw_quantiles = c(0.25, 0.75),
      #     position = position_dodge(0.5),
      #     width =
      #       0.2,
      #     linetype = "dotted"
      #   ) + geom_violin(
      #     draw_quantiles = .5,
      #     position = position_dodge(0.5),
      #     width =
      #       0.2,
      #     alpha = 0
      #   ) + labs(x = "Course Year", y = "GPA")  + ggtitle("Student academic performance") +
      #     theme(plot.title = element_text(hjust = 0.5))+
      #         scale_fill_manual(values = c("#ffe6cc", "#ff901a"),
      #                           name=NULL,
      #                           labels = c(
      #                             'Group 1',
      #                             'Group 2'
      #                           ))
      # }
      
      #Data not disaggregated when specific course quarter selected
      else {
        data.years.names.substituteR %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          ggplot(aes(x = as.factor(course.year),
                     y = course.grade,
                     #fill = course.year,
                     group = course.year)) + geom_violin(
                       draw_quantiles = c(0.25, 0.75),
                       position = position_dodge(0.5),
                       width =
                         0.2,
                       linetype = "dotted"
                     ) + geom_violin(
                       draw_quantiles = .5,
                       position = position_dodge(0.5),
                       width =
                         0.2,
                       alpha = 0
                     ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5))
        
      }
      
    }
    
    
    
    
  })
}
  
shinyApp(ui = ui, server = server)
