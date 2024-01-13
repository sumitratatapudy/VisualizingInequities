#load necessary packages
library(shiny)
library(dplyr)
library(tidyverse)

#load your data file
data.years.names.substituteR <-
  read.csv("Template_Inequities_In_Course_Performance.csv") %>% mutate(course.grade = as.numeric(course.grade)) %>% na.omit(data.years.names.substituteR)

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
    "Systemic inequities are often invisible and challenging to identify.
                     You are taking a crucial step in working towards mitigating inequities in your classroom just by being here.
                     Welcome!
                     [[More goals - Need advice]]"
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
                             selected = "CLASS.142"
                           ),
                           selectInput(
                             "quarter",
                             "Choose quarter",
                             choices = c(
                               "None selected",
                               unique(data.years.names.substituteR$course.quarter)
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
                               "First Generation Status"
                             ),
                             selected = "None selected"
                           ),
                           textOutput("minoritized_how_info"),
                           textOutput("minoritized_how_info2"),
                           textOutput("minoritized_how_info3")
                         ),
                         mainPanel(
                           plotOutput("plot1"),
                           textOutput("intervention_info"),
                           textOutput("intervention_info2"),
                           textOutput("intervention_info3"),
                           uiOutput("url1"),
                           uiOutput("url2"),
                           uiOutput("url3"),
                           uiOutput("url1.2"),
                           uiOutput("url2.2"),
                           uiOutput("url3.2"),
                           uiOutput("url1.3"),
                           uiOutput("url2.3"),
                           uiOutput("url3.3")
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
      "[Insert minoritized race stuff here]: "
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
      "[Insert binary gender stuff here]Literature on learning tells us that first-generation students benefit from high structure classes.

Here are some ways to incorporate high structure in your course: "
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
  
  
  
  #Data disaggregated by students majoritized and minoritized on basis of race when no course quarter selected
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(position = position_dodge(0.5), width =
                                                       0.2) + labs(x = "Course Year", y = "GPA") + scale_fill_hue(
                                                         labels = c(
                                                           "Not Racially Minoritized",
                                                           "Racially Minoritized",
                                                           "Did not indicate"
                                                         )
                                                       ) + scale_fill_discrete(
                                                         name = NULL,
                                                         labels = c(
                                                           'Not Racially Minoritized',
                                                           'Racially Minoritized',
                                                           'Did not indicate'
                                                         )
                                                       ) + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5))
      }
      
      #Data disaggregated by students gender when no course quarter selected
      else {
        if (input$minoritized_how == "Binary Gender") {
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
          ) + labs(x = NULL, y = "GPA") + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), position = position_dodge(0.5), width =
                                                         0.2) + labs(x = "Course Year", y = "GPA") + scale_fill_hue(labels = c("Male", "Female", "Did not indicate")) + scale_fill_discrete(name = NULL,
                                                                                                                                                                                            labels = c('Male', 'Female', 'Did not indicate')) + ggtitle("Student academic performance") +
            theme(plot.title = element_text(hjust = 0.5))
        }
        
        #Data disaggregated by students first generation status when no course quarter selected
        else {
          if (input$minoritized_how == "First Generation Status") {
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
            ) + labs(x = NULL, y = "GPA") + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), position = position_dodge(0.5), width =
                                                           0.2) + labs(x = "Course Year", y = "GPA") + scale_fill_hue(labels = c(
                                                             "Not First Generation Student",
                                                             "First Generation Student"
                                                           )) + scale_fill_discrete(
                                                             name = NULL,
                                                             labels = c(
                                                               'Not First Generation Student',
                                                               'First Generation Student'
                                                             )
                                                           ) + ggtitle("Student academic performance") +
              theme(plot.title = element_text(hjust = 0.5))
          }
          
          
          #Data not disaggregated when no course quarter selected
          else {
            data.years.names.substituteR %>%
              filter(
                course == input$course,
                course.quarter == input$quarter,
                course.year >= input$year[1] &
                  course.year <= input$year[2]
              ) %>%
              ggplot(
                aes(
                  x = course.year,
                  y = course.grade,
                  #fill = course.year,
                  group = course.year
                )
              ) + labs(x = NULL, y = "GPA") + geom_violin(fill="gray", draw_quantiles = c(0.25, 0.5, 0.75), width = 0.2) + ggtitle("Student academic performance") +
              theme(plot.title = element_text(hjust = 0.5))
            
          }
          
          
        }
        
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
        ) + labs(x = NULL, y = "GPA") + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), position = position_dodge(0.5), width =
                                                       0.2) + labs(x = "Course Year", y = "GPA") + scale_fill_hue(
                                                         labels = c(
                                                           "Not Racially Minoritized",
                                                           "Racially Minoritized",
                                                           "Did not indicate"
                                                         )
                                                       ) + scale_fill_discrete(
                                                         name = NULL,
                                                         labels = c(
                                                           'Not Racially Minoritized',
                                                           'Racially Minoritized',
                                                           'Did not indicate'
                                                         )
                                                       ) + ggtitle("Student academic performance") +
          theme(plot.title = element_text(hjust = 0.5))
      }
      
      else {
        #Data disaggregated by students gender when specific course quarter selected
        if (input$minoritized_how == "Binary Gender") {
          data.years.names.substitute.subset5 <-
            data.years.names.substituteR %>%
            filter(
              course == input$course,
              course.year >= input$year[1] &
                course.year <= input$year[2]
            ) %>%
            select(course.year, Gender, course.grade) %>%
            na.omit(data.years.names.substitute.subset5)
          ggplot(
            data.years.names.substitute.subset5,
            aes(
              x = as.factor(course.year),
              y = course.grade,
              fill = as.factor(Gender)
            )
          ) + labs(x = NULL, y = "GPA") + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), position = position_dodge(0.5), width =
                                                         0.2) + labs(x = "Course Year", y = "GPA") + scale_fill_hue(labels = c("Male", "Female", "Did not indicate")) + scale_fill_discrete(name = NULL,
                                                                                                                                                                                            labels = c('Male', 'Female', 'Did not indicate')) + ggtitle("Student academic performance") +
            theme(plot.title = element_text(hjust = 0.5))
        }
        else {
          #Data disaggregated by students first generation status when specific course quarter selected
          if (input$minoritized_how == "First Generation Status") {
            data.years.names.substitute.subset6 <-
              data.years.names.substituteR %>%
              filter(
                course == input$course,
                course.year >= input$year[1] &
                  course.year <= input$year[2]
              ) %>%
              select(course.year, First_Generation, course.grade) %>%
              na.omit(data.years.names.substitute.subset6)
            ggplot(
              data.years.names.substitute.subset6,
              aes(
                x = as.factor(course.year),
                y = course.grade,
                fill = as.factor(First_Generation)
              )
            ) + labs(x = NULL, y = "GPA") + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), position = position_dodge(0.5), width =
                                                           0.2) + labs(x = "Course Year", y = "GPA") + scale_fill_hue(labels = c(
                                                             "Not First Generation Student",
                                                             "First Generation Student"
                                                           )) + scale_fill_discrete(
                                                             name = NULL,
                                                             labels = c(
                                                               'Not First Generation Student',
                                                               'First Generation Student'
                                                             )
                                                           ) + ggtitle("Student academic performance") +
              theme(plot.title = element_text(hjust = 0.5))
          }
          
          #Data not disaggregated when specific course quarter selected
          else {
            data.years.names.substituteR %>%
              filter(
                course == input$course,
                course.year >= input$year[1] &
                  course.year <= input$year[2]
              ) %>%
              ggplot(
                aes(
                  x = course.year,
                  y = course.grade,
                  #fill = course.year,
                  group = course.year
                )
              ) + labs(x = NULL, y = "GPA") + geom_violin(fill="gray", draw_quantiles = c(0.25, 0.5, 0.75), width = 0.2) + ggtitle("Student academic performance") +
              theme(plot.title = element_text(hjust = 0.5))
            
          }
          
        }
        
        
        
      }
      
    }
    
  })
}
shinyApp(ui = ui, server = server)
