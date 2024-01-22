#load necessary packages
library(shiny)
library(dplyr)
library(tidyverse)

#load your data file
data.years.names.substituteR <-
  read.csv("Template_Inequities_In_Course_Performance_Cleaned.csv")

# ##just for development WILL REMOVE LATER!: COUGARS add additional variable (values randomly generated)
# data.years.names.substituteR$Additional_Var = rbinom(length(data.years.names.substituteR$course.grade), 1, 0.5)

#Define authentication credentials
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("standard", "standard"),
  name = c("User One", "User Two")
)

#Design all tabs present in UI
login_tab <- tabPanel(title = icon("lock"),
                      value = "login",
                      shinyauthr::loginUI("login"))
#UI code
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
  
data_tab <- tabPanel(title = "Data",
                     fluidPage(
                       titlePanel("Student grade distributions"),
                       sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           sliderInput(
                             "year",
                             "Choose year",
                             min = 2001,
                             max = 2016,
                             value = c(2010, 2014),
                             sep = ""
                           ),
                           selectInput(
                             "course",
                             "Choose course",
                             choices = c(unique(data.years.names.substituteR$course)),
                             selected = "Course10C"
                           ),
                           selectInput(
                             "quarter",
                             "Choose quarter",
                             choices = c(
                               "None selected",
                               "Winter",
                               "Spring",
                               "Summer",
                               "Fall"
                             ),
                             selected = "None selected"
                           ),
                           selectizeInput(
                             "minoritized_how",
                             "Which minoritized group would you like data disggreagated by?",
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
                           uiOutput("url3.3")#, #COUGAR (just the comma)
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
  
  #Providing access to authenticated users to view app UI and data
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

  
  #Resources and descriptions associated with graphs
  url7 <-
    a("How to design a high-structure class", href = "https://files.eric.ed.gov/fulltext/EJ1268125.pdf")
  url8 <-
    a("Sample materials from a high-structure class", href = "https://www.lifescied.org/doi/10.1187/cbe.14-03-0050")
  url9 <-
    a("Resources from UW Teach", href = "https://teaching.washington.edu/inclusive-teaching/supporting-specific-student-groups/first-generation-students/")
  
  ##RACIALLY MINORITIZED
  reactive_function2 <- eventReactive(input$minoritized_how, {
    req(input$minoritized_how == "Racially Minoritized")
  })
  
  output$minoritized_how_info2 <- renderText({
    reactive_function2()
    paste(
      "Students who identify as Black, African-American, Latinx, Hawaiian/Pacific Islander and/or American Indian
"
    )
  })
  
  output$intervention_info2 <- renderText({
    reactive_function2()
    paste(
      "[Insert minoritized race stuff here (and replace the links)]: "
    )
  })
  
  #NOTE -- link to something other than urls 7,8,9... create new urls relevant to race
  output$url1.2 <- renderUI({
    reactive_function2()
    tagList(url7)
    
  })
  
  output$url2.2 <- renderUI({
    reactive_function2()
    tagList(url8)
    
  })
  
  output$url3.2 <- renderUI({
    reactive_function2()
    tagList(url9)
    
  })
  
  ##BINARY GENDER
  reactive_function3 <- eventReactive(input$minoritized_how, {
    req(input$minoritized_how == "Binary Gender")
  })
  
  output$minoritized_how_info3 <- renderText({
    reactive_function3()
    paste(
      "Binary gender is the classification of gender into two distinct forms of masculine and feminine, whether by social system, cultural belief, or both simultaneously
"
    )
  })
  
  output$intervention_info3 <- renderText({
    reactive_function3()
    paste(
      "[Insert binary gender stuff here (and replace the links)]: "
    )
  })
  #NOTE -- link to something other than urls 7,8,9... create new urls relevant to binary gender
  output$url1.3 <- renderUI({
    reactive_function3()
    tagList(url7)
    
  })
  
  output$url2.3 <- renderUI({
    reactive_function3()
    tagList(url8)
    
  })
  
  output$url3.3 <- renderUI({
    reactive_function3()
    tagList(url9)
    
  })
  
  
  ## FIRST GEN STATUS
  reactive_function <- eventReactive(input$minoritized_how, {
    req(input$minoritized_how == "First Generation Status")
  })
  
  
  
  
  output$minoritized_how_info <- renderText({
    reactive_function()
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
    tagList(url7)
    
  })
  
  output$url2 <- renderUI({
    reactive_function()
    tagList(url8)
    
  })
  
  output$url3 <- renderUI({
    reactive_function()
    tagList(url9)
    
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(
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
      #   ) + labs(x = NULL, y = "GPA") + geom_violin(
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
          ggplot(aes(x = course.year,
                     y = course.grade,
                     #fill = course.year,
                     group = course.year)) + labs(x = NULL, y = "GPA") + geom_violin(
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
                     ) + ggtitle("Student academic performance") +
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(
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
      #   ) + labs(x = NULL, y = "GPA") + geom_violin(
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
          ggplot(aes(x = course.year,
                     y = course.grade,
                     #fill = course.year,
                     group = course.year)) + labs(x = NULL, y = "GPA") + geom_violin(
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
                     ) + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5))
        
      }
      
    }
    
    
    
    
  })
}
  
shinyApp(ui = ui, server = server)
