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
class.data <- read.csv("Template_Inequities_In_Course_Performance_Cleaned.csv") #huskies: make sure the text in quotations exactly matches your csv file name


library(shiny)

home_tab <- tabPanel(
  title = "Background",
  tags$style(HTML("
    .custom-text { color: black; line-height: 1.5; }
    .custom-bg-1 { background-color: #fad1d2; padding: 20px; border-radius: 10px; margin-bottom: 10px; }
    .custom-bg-2 { background-color: #fbb2b3; padding: 20px; border-radius: 10px; margin-bottom: 10px; }
    .custom-bg-3 { background-color: #f8999d; padding: 20px; border-radius: 10px; margin-bottom: 10px; } /* Color between #fbb2b3 and #ed5e61 */
    .custom-no-bg { margin-bottom: 10px; }
    .flex-container { display: flex; }
    .left-boxes { flex: 1; }
    .right-image { flex: 1; padding-left: 20px; }
    .half-width-image { max-width: 50%; }
  ")),
  
 h4(class = "custom-text", "Educational inequities remain one of the most persistent, intractable, and most crucial problems in our society. Interrogating the manifestation of these inequities in higher education classes is essential and urgent. You are taking the first step."),
  
  div(class = "flex-container",
      div(class = "left-boxes",
          div(
            class = "custom-bg-1",
            strong("What this app is:"),
            br(),
            tags$ul(
              tags$li("This app is intended as a tool for self-reflection: instructors know their class environment best but interrogating the manifestation of educational inequities is not always simple."), 
              tags$li("This tool aims to address this shortcoming. By selecting years, terms, and courses, instructors can disaggregate their data by several student identities. Because without disaggregating data, we are only measuring the majority. "), 
              tags$li("With intentional, guided self-reflection, and carefully curated resources, instructors can practice equity-minded reflection practices and develop strategies that disrupt inequities in their classrooms.")
            )
          ),
          
          div(
            class = "custom-bg-2",
            strong("Broader context:"),
            tags$ul(
              tags$li("Differences between student groups are due to systemic barriers, not due to inherent differences in abilities. Said another way, we need to fix our institutions, not our students."), 
              tags$li("While a single instructor does not always have control over larger systemic issues, the classroom is one place where they do have control."),
              tags$li("It can be intimidating to start. And once you do, please don’t stop.")
            )
          ),
          
          div(
            class = "custom-bg-3",
            strong("Data presented and intention"),
            tags$ul(
              tags$li("The data shown here come from a freely available dataset of student outcomes. The data originally came from institutional data and have been further anonymized to obscure courses, instructors, terms, and years. The students and the patterns in student outcomes are real."), 
              tags$li("The intention is that instructors or administrators will download and edit the code to develop a data processing application to explore their own data. We use these data here for illustrative purposes only to demonstrate the power and potential of such a tool."), 
              tags$li("One can read more about the application here: [link to publication coming soon]")
            )
          )
      ),
      
      div(class = "right-image half-width-image", 
            strong("Key to Reading Kernel (Violin) Plots:"),
          imageOutput("myImage")
      )
  )
)

  

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
                           sliderInput( #"integer slider",
                             "year",
                             "Choose year",
                             min = min(class.data$course.year), #huskies, update this with the minimum year from your data
                             max = max(class.data$course.year), #huskies, update this with the maximum year from your data
                             value = c(2002,2003), #huskies, sets the initial date range to be displayed
                             #make sure the years in the line above are found in your data
                             sep = "",
                             step = 1
                             #round = 0
                           ), 
                           
                           # cougars, the following code creates a drop-down to select which course
                           # you want to visualize
                           selectInput(
                             "course",
                             "Choose course",
                             choices = c(unique(class.data$course))
                             #selected = "Course10C" #huskies, sets the initial course to be displayed
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
                           uiOutput("reflection_questions"),
                           uiOutput("reflection_questions1"),
                           uiOutput("reflection_questions2"),
                           uiOutput("intervention_info"),
                           uiOutput("intervention_info2"),
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
                           uiOutput("url3.3"), 
                           uiOutput("url4.3")#, #COUGAR (just the comma)
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
                 home_tab,
                 data_tab
                 )


#FUNCTION code
server <- function(input, output, session) { 
  
  # Render the image
  output$myImage <- renderImage({
    list(src = "images/app_image.png",
         alt = "Your Image Alt Text",
         width = 500, height = 400)
  }, deleteFile = FALSE)
  
  #class.data <- subset(class.data, (instructors == users$username)) #filter data to match intstructor 
  # Reactive values to store logged-in user's data
  #user_data <- reactiveVal(NULL)
  
  # observeEvent(input$login, {
  #   username <- input$username
  #   password <- input$password
  #   
  #   # Check if credentials match
  #   user <- users[users$username == username & users$password == password, ]
  #   
  #   if (nrow(user) == 1) {
  #     showModal(modalDialog(
  #       title = "Login Successful",
  #       "Welcome to the app!"
  #       
  #     ))
  #     
  #     # Filter data based on logged-in user
  #     
  #     
     
  #     
  #   } else {
  #     showModal(modalDialog(
  #       title = "Login Failed",
  #       "Invalid username or password. Please try again."
  #     ))
  #   }
  #   
  #   
  #   
  # })
  
  
  
  ## Resources and descriptions associated with graphs
  
  #URM Resources
  url1 <- a("Re-Envisioning the Culture of Undergraduate Biology Education to Foster Black Student Success", href = "https://www.lifescied.org/doi/full/10.1187/cbe.22-09-0175")
  url2 <- a("UW Teach Inclusive Teaching", href = "https://teaching.washington.edu/inclusive-teaching/")
  url3 <- a("Student-Authored Scientist Spotlights", href = "https://www.lifescied.org/doi/full/10.1187/cbe.21-03-0060")
  
  #Binary Gender Resources
  url4 <- a("Gender Gaps in Achievement and Participation in Intro Biology", href = "https://www.google.com/url?q=https://www.lifescied.org/doi/full/10.1187/cbe.13-10-0204&sa=D&source=docs&ust=1706046973389795&usg=AOvVaw0bli17OADKj5hdiAx5p6wt")
  url5 <- a("Transgender, Nonbinary, Gender Nonconforming, and Questioning Students in Biology", href = "https://www.lifescied.org/doi/full/10.1187/cbe.21-12-0343")
  url6 <- a("Reducing the gender gap in the physics classroom", href = "https://pubs.aip.org/aapt/ajp/article-abstract/74/2/118/1039347/Reducing-the-gender-gap-in-the-physics-classroom?redirectedFrom=fulltext")
  url7 <- a("Values Affirmation to Reduce Gender Acheivement Gap in Science", href = "https://www.science.org/doi/10.1126/science.1195996")
  
  #First Gen Resources
  url8 <-a("How to design a high-structure class", href = "https://files.eric.ed.gov/fulltext/EJ1268125.pdf")
  url9 <- a("Sample materials from a high-structure class", href = "https://www.lifescied.org/doi/10.1187/cbe.14-03-0050")
  url10 <-a("First generation resources from UW Teach", href = "https://teaching.washington.edu/inclusive-teaching/supporting-specific-student-groups/first-generation-students/")
  
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
  
  
  output$reflection_questions <- renderUI({
    reactive_function2()
    HTML("<p><u><strong>Questions for reflection:</u></p>
       <p><strong>1) Are there in equities?</strong><br>
       <strong>2) What might be contributing to inequities in the context of this class?</strong><br>
       <strong>3) With whom can you reflect on these data?</strong></p>")
  })

  output$intervention_info2 <- renderUI({
    reactive_function2()
    HTML("<p>Studies suggest multiple strategies for creating a more equitable classroom environment, including acknowledging racial equity topics in the classroom, intentionally providing equitable access to resources, and instructors self reflecting and coming up with action plans to address biases.</p>")
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
      "Binary classification of gender is outdated, but institutional data lag behind."
    )
  })
  
  output$reflection_questions1 <- renderUI({
    reactive_function3()
    HTML("<p><u><strong>Questions for reflection:</u></p>
       <p><strong>1) Are there in equities?</strong><br>
       <strong>2) What might be contributing to inequities in the context of this class?</strong><br>
       <strong>3) With whom can you reflect on these data?</strong></p>")
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
  
  output$reflection_questions2 <- renderUI({
    reactive_function()
    HTML("<p><u><strong>Questions for reflection:</u></p>
       <p><strong>1) Are there in equities?</strong><br>
       <strong>2) What might be contributing to inequities in the context of this class?</strong><br>
       <strong>3) With whom can you reflect on these data?</strong></p>")
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
  
  #class.data <- subset(class.data, (instructors == "instructor 1"))
  output$plot1 <- renderPlot({
    if (input$quarter != "None selected") {
      if (input$minoritized_how == "Racially Minoritized") {
        data.years.names.substitute.subset <-
          class.data %>%
          filter(
            #instructors == input$instructors
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          select(course.year, Racially_Minoritized, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        
        #find which combinations of "Racially_Minoritized" and course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year, Racially_Minoritized) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year, Racially_Minoritized)) %in% 
                   interaction(high_n_combos))
        
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
            0.4,
          linewidth=.3
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.75,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + 
          ggtitle("Student academic performance") +
          theme_bw(base_size=18) +
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
          scale_fill_manual(values = c("#d5edf6", "#55b9dd"), 
                            name=NULL,
                            labels = c(
                              'Racially Majoritized',
                              'Racially Minoritized',
                              'Did not indicate'
                            )) 
      }
      #Data disaggregated by students gender when no course quarter selected
      else if (input$minoritized_how == "Binary Gender") {
        data.years.names.substitute.subset <-
          class.data %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          select(course.year, Gender, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        
        #find which combinations of "Gender" and course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year, Gender) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year, Gender)) %in% 
                   interaction(high_n_combos))
        
        ggplot(
          data.years.names.substitute.subset,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(Gender)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.3
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.75,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme_bw(base_size=18) +
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
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
        data.years.names.substitute.subset <-
          class.data %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          ) %>%
          select(course.year, First_Generation, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        
        #find which combinations of "First_Generation" and course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year, First_Generation) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year, First_Generation)) %in% 
                   interaction(high_n_combos))
        
        ggplot(
          data.years.names.substitute.subset,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(First_Generation)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.3
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.75,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme_bw(base_size=18)+
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
          scale_fill_manual(values = c("#fad1d2", "#ed5e61"), 
                            name=NULL,
                            labels = c(
                              'Continuing Generation Student',
                              'First Generation Student'
                            ))
      }
      
      # #COUGARS -- Data disaggregated by additional variable when no course quarter selected
      # else if (input$minoritized_how == "Additional Variable") {
      #   data.years.names.substitute.subset <-
      #     class.data %>%
      #     filter(
      #       course == input$course,
      #       course.quarter == input$quarter,
      #       course.year >= input$year[1] &
      #         course.year <= input$year[2]
      #     ) %>%
      #     select(course.year, Additional_Var, course.grade) %>%
      #     na.omit(data.years.names.substitute.subset)
      #   
      #   #find which combinations of "Additional_Var" and course.year have n>=10
      #   count_n <- data.years.names.substitute.subset %>% 
      #     group_by(course.year, Additional_Var) %>% summarize(enough=n()>=10)
      #   #isolate the combinations with high enough n to keep
      #   high_n_combos <- count_n %>% 
      #     filter(enough) %>% select(-enough)
      #   #the number of combinations omitted due to small sample size
      #   num_omissions <- sum(!count_n$enough)
      #   #filter to remove data points of combinations with n>=10
      #   data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
      #     filter(interaction(data.frame(course.year, Additional_Var)) %in% 
      #              interaction(high_n_combos))
      #   
      #   ggplot(
      #     data.years.names.substitute.subset,
      #     aes(
      #       x = as.factor(course.year),
      #       y = course.grade,
      #       fill = as.factor(Additional_Var)
      #     )
      #   ) + geom_violin(
      #     draw_quantiles = c(0.25, 0.75),
      #     position = position_dodge(0.5),
      #     width =
      #       0.4,
      #     linewidth=.3
      #   ) + geom_violin(
      #     draw_quantiles = .5,
      #     position = position_dodge(0.5),
      #     width =
      #       0.4,
      #     linewidth=.75
      #     alpha = 0
      #   ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
      #     theme_bw(base_size=18) +
      #     theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
      #     scale_fill_manual(values = c("#ffe6cc", "#ff901a"),
      #                       name=NULL,
      #                       labels = c(
      #                         'Group 1',
      #                         'Group 2'
      #                       ))
      # }
      
      #Data not disaggregated when no course quarter selected
      else {
        data.years.names.substitute.subset <- class.data %>%
          filter(
            course == input$course,
            course.quarter == input$quarter,
            course.year >= input$year[1] &
              course.year <= input$year[2]
          )
        
        #find which course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year)) %in% 
                   interaction(high_n_combos))
        
        ggplot(data.years.names.substitute.subset, aes(x = as.factor(course.year),
                                                       y = course.grade,
                                                       #fill = course.year,
                                                       group = course.year)) + geom_violin(
                                                         draw_quantiles = c(0.25, 0.75),
                                                         position = position_dodge(0.5),
                                                         width =
                                                           0.4,
                                                         linewidth=.3
                                                       ) + geom_violin(
                                                         draw_quantiles = .5,
                                                         position = position_dodge(0.5),
                                                         width =
                                                           0.4,
                                                         linewidth=.75,
                                                         alpha = 0
                                                       ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") + theme_bw(base_size=18) +
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
        
      }
      
    }
    else {
      #Data disaggregated by students majoritized and minoritized on basis of race when specific course quarter selected
      if (input$minoritized_how == "Racially Minoritized") {
        data.years.names.substitute.subset <-
          class.data %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          select(course.year, Racially_Minoritized, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        
        #find which combinations of "Racially_Minoritized" and course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year, Racially_Minoritized) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year, Racially_Minoritized)) %in% 
                   interaction(high_n_combos))
        
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
            0.4,
          linewidth=.3
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.75,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + 
          ggtitle("Student academic performance") +
          theme_bw(base_size=18) +
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
          scale_fill_manual(values = c("#d5edf6", "#55b9dd"), 
                            name=NULL,
                            labels = c(
                              'Racially Majoritized',
                              'Racially Minoritized',
                              'Did not indicate'
                            ))
      }
      
      #Data disaggregated by students gender when specific course quarter selected
      else if (input$minoritized_how == "Binary Gender") {
        data.years.names.substitute.subset <-
          class.data %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          select(course.year, Gender, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        
        #find which combinations of "Gender" and course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year, Gender) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year, Gender)) %in% 
                   interaction(high_n_combos))
        
        ggplot(
          data.years.names.substitute.subset,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(Gender)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.3
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.75,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") +
          theme_bw(base_size=18) + 
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
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
        data.years.names.substitute.subset <-
          class.data %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2]) %>%
          select(course.year, First_Generation, course.grade) %>%
          na.omit(data.years.names.substitute.subset)
        
        #find which combinations of "First_Generation" and course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year, First_Generation) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year, First_Generation)) %in% 
                   interaction(high_n_combos))
        
        ggplot(
          data.years.names.substitute.subset,
          aes(
            x = as.factor(course.year),
            y = course.grade,
            fill = as.factor(First_Generation)
          )
        ) + geom_violin(
          draw_quantiles = c(0.25, 0.75),
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.3
        ) + geom_violin(
          draw_quantiles = .5,
          position = position_dodge(0.5),
          width =
            0.4,
          linewidth=.75,
          alpha = 0
        ) + labs(x = "Course Year", y = "GPA")  + ggtitle("Student academic performance") +
          theme_bw(base_size=18) +
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
          scale_fill_manual(values = c("#fad1d2", "#ed5e61"), 
                            name=NULL,
                            labels = c(
                              'Continuing Generation Student',
                              'First Generation Student'
                            ))
      }
      
      # #COUGAR -- Data disaggregated by additional varaible when specific course quarter selected
      # else if (input$minoritized_how == "Additional Variable") {
      #   data.years.names.substitute.subset <-
      #     class.data %>%
      #     filter(course == input$course,
      #            course.year >= input$year[1] &
      #              course.year <= input$year[2]) %>%
      #     select(course.year, Additional_Var, course.grade) %>%
      #     na.omit(data.years.names.substitute.subset)
      #   
      #   #find which combinations of "Additional_Var" and course.year have n>=10
      #   count_n <- data.years.names.substitute.subset %>% 
      #     group_by(course.year, Additional_Var) %>% summarize(enough=n()>=10)
      #   #isolate the combinations with high enough n to keep
      #   high_n_combos <- count_n %>% 
      #     filter(enough) %>% select(-enough)
      #   #the number of combinations omitted due to small sample size
      #   num_omissions <- sum(!count_n$enough)
      #   #filter to remove data points of combinations with n>=10
      #   data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
      #     filter(interaction(data.frame(course.year, Additional_Var)) %in% 
      #              interaction(high_n_combos))
      #   
      #   ggplot(
      #     data.years.names.substitute.subset,
      #     aes(
      #       x = as.factor(course.year),
      #       y = course.grade,
      #       fill = as.factor(Additional_Var)
      #     )
      #   ) + geom_violin(
      #     draw_quantiles = c(0.25, 0.75),
      #     position = position_dodge(0.5),
      #     width =
      #       0.4,
      #     linewidth=.3
      #   ) + geom_violin(
      #     draw_quantiles = .5,
      #     position = position_dodge(0.5),
      #     width =
      #       0.4,
      #     linewidth=.75
      #     alpha = 0
      #   ) + labs(x = "Course Year", y = "GPA")  + ggtitle("Student academic performance") +
      #     theme_bw(base_size=18) +
      #     theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
      #         scale_fill_manual(values = c("#ffe6cc", "#ff901a"),
      #                           name=NULL,
      #                           labels = c(
      #                             'Group 1',
      #                             'Group 2'
      #                           ))
      # }
      
      #Data not disaggregated when specific course quarter selected
      else {
        data.years.names.substitute.subset <- class.data %>%
          filter(course == input$course,
                 course.year >= input$year[1] &
                   course.year <= input$year[2])
        
        #find which course.year have n>=10
        count_n <- data.years.names.substitute.subset %>% 
          group_by(course.year) %>% summarize(enough=n()>=10)
        #isolate the combinations with high enough n to keep
        high_n_combos <- count_n %>% 
          filter(enough) %>% select(-enough)
        #the number of combinations omitted due to small sample size
        num_omissions <- sum(!count_n$enough)
        #filter to remove data points of combinations with n>=10
        data.years.names.substitute.subset <- data.years.names.substitute.subset %>% 
          filter(interaction(data.frame(course.year)) %in% 
                   interaction(high_n_combos))
        
        ggplot(data.years.names.substitute.subset, aes(x = as.factor(course.year),
                                                       y = course.grade,
                                                       #fill = course.year,
                                                       group = course.year)) + geom_violin(
                                                         draw_quantiles = c(0.25, 0.75),
                                                         position = position_dodge(0.5),
                                                         width =
                                                           0.4,
                                                         linewidth=.3
                                                       ) + geom_violin(
                                                         draw_quantiles = .5,
                                                         position = position_dodge(0.5),
                                                         width =
                                                           0.4,
                                                         linewidth=.75,
                                                         alpha = 0
                                                       ) + labs(x = "Course Year", y = "GPA") + ggtitle("Student academic performance") + theme_bw(base_size=18) +
          theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
        
      }
      
    }
    
    
    
    
  })
  
}

shinyApp(ui = ui, server = server)


