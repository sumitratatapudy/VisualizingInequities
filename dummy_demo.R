library(shiny)
library(ggplot2)

# Dummy dataset containing data for multiple users
data <- data.frame(
  username = c("user1", "user1", "user1", "user2", "user2", "user2"),
  id = c(1, 2, 3, 4, 5, 6),
  value = c(10, 20, 30, 40, 50, 60)
)

# Dummy user database
users <- data.frame(
  username = c("user1", "user2"),
  password = c("password1", "password2"),
  stringsAsFactors = FALSE
)

# Define UI
ui <- navbarPage(
  title = "Shiny App with Login",
  tabPanel(
    "Login",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          textInput("username", "Username:"),
          passwordInput("password", "Password:"),
          actionButton("login", "Login")
        ),
        mainPanel(
          plotOutput("data_plot")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store logged-in user's data
  user_data <- reactiveVal(NULL)
  
  observeEvent(input$login, {
    username <- input$username
    password <- input$password
    
    # Check if credentials match
    user <- users[users$username == username & users$password == password, ]
    
    if (nrow(user) == 1) {
      showModal(modalDialog(
        title = "Login Successful",
        "Welcome to the app!"
      ))
      
      # Filter data based on logged-in user
      user_data(data[data$username == username, ])
    } else {
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password. Please try again."
      ))
    }
  })
  
  # Display user-specific violin plot
  output$data_plot <- renderPlot({
    req(user_data())
    ggplot(user_data(), aes(x = username, y = value, fill = username)) +
      geom_violin() +
      labs(title = "User-specific Violin Plot", x = "Username", y = "Value") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
