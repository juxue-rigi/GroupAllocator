library(shiny)

# Define UI for Login Page
login_ui <- fluidPage(
  titlePanel("Login Page"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter your username:"),
      textInput("course", "Enter your course name:"),
      actionButton("go", "Go to Project Set-up")
    ),
    mainPanel(
      textOutput("greeting")
    )
  )
)

# Define UI for Project Setup Page
project_setup_ui <- fluidPage(
  titlePanel("Project Set-up"),
  
  # Profile section at top right corner
  div(style = "display: flex; justify-content: flex-end; align-items: center; gap: 10px; padding: 10px; position: relative;",
    tags$img(src = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAMAAAAJbSJIAAAAM1BMVEXk5ueutLeqsbTn6eqpr7PJzc/j5ebf4eLZ3N2wtrnBxsjN0NLGysy6v8HT1tissra8wMNxTKO9AAAFDklEQVR4nO2d3XqDIAxAlfivoO//tEOZWzvbVTEpic252W3PF0gAIcsyRVEURVEURVEURVEURVEURVEURVEURVEURVEURflgAFL/AirAqzXO9R7XNBVcy9TbuMHmxjN6lr92cNVVLKEurVfK/zCORVvW8iUBnC02dj+Wpu0z0Y6QlaN5phcwZqjkOkK5HZyPAjkIjSO4fIdfcOwFKkJlX4zPu7Ha1tIcwR3wWxyFhRG6g4Je0YpSPDJCV8a2Sv2zd1O1x/2WMDZCwljH+clRrHfWCLGK8REMiql//2si5+DKWKcWeAGcFMzzNrXC/0TUwQ2s6+LhlcwjTMlYsUIQzPOCb7YBiyHopyLXIEKPEkI/TgeuiidK/R9FniUDOjRDpvm0RhqjMyyXNjDhCfIMYl1gGjIMIuYsnGEYRMRZOMMunaLVwpWRW008v6fYKDIzxCwVAeNSO90BJW6emelYBRF/kHpYGVaoxTDAaxOFsfP9y8hpJ4xd7gOcij7JNGQ1EYFgkPJa1jQEiYZXRaRINKxSDUW9n+FT82lSKadkiru9/4XPqSLWOekGPoY05TAvLm9orm+YWuwHoBHkZKijNBJGmeb61eL6Ff/6q7bLr7yvv3vKGhpDRjvgjGaPz+gUg6YgcvpyAR2FIZ9U6nEEyZRTovmEU32KichpGn7C17XrfyH9gK/c0CMP05HZIM2uf9sEveizKveBy9/6Qt7o89ne33D525cfcIMW6ab+TMEukQbQbu+xu7X3A9bChmWaCeAkG17bpntwXgWxHaMzGPmUaR5dQZiKqRVeUZ3047fi3nAu28h4CHxCsZAgmEH8Y27jJAhm8c+5RQzRQNVGhVFSfxOYIjp/pP7RxzjevYXVGf4eLt+BJ1vCuLuLkrgABgCGXZ2wik5uty+oBvNirI6mkzhAf4Gsb58Hcm67Jzd+KwD10BYPLL3e0MjvKrgAULnOfveF/O4N2Xb9BZom3gJes3F9X5Zze8/6Yt09b4CrqsEjUv8oFBaR2rl+6CZr2xVrp24o/WitBKuGrrpl1+bFkmK2qXTON4VpbdfLa7o7y/WdLxG7lm2Lqh2clOwTegbvc/vj2U78CwhA87Bn8G5Nk3eOb0Nsr9flz3sG78UUtue4kpv1xvjg3TMay62BMlTlP+vrOMnJsRmt/ze0jsfkPPYdAH57hK+34PeOyc8XIXu5xT2HsUkdZz+adwg8HGFfQ3K5jtDvbUiO4Di9/ywHGrL88pDizZ++oTp+an+SMX/ndymUCwmHMdO7yuOx83pUx/eEMU0AvxWndwgidAqOZ8ypCwdEfvvEo6D9HwpA8wzvmOJEqAg9ySu8g4x0Hb9hSB/BANEKJ+LbPBU0lzbAJs4xt1AoshKkUGQmiH8/jJ0gdhTTLmSegHlPE0oOdXALnqDjKYh3px//fSgSWG8UqfrrIICzYYSJXRr9BSPbpNzw7gBjKjKOYI7ReIGqQRIap5+5MdjyvuDkExvGeXSlONWZAP3/AZBwJohU7QJRGU+cTVH18ELmRPNBmibW6MT/k1b0XhdkRBvyT6SB6EYv/GvhSmRNpGngRULsAlxMCGNXp7w3FfdEbTEEDdLI9TdIKRUzUesa3I461ER8cpNT7gMRhpKmYVS9ELOgCUQsa4SsulciKiLbY+AnHD8cpuhISsnxpamI84sbDq9qYJgf8wiiOBrC7Ml7M7ZECCqKoiiKoiiKoiiKoijv5AvJxlZRyNWWLwAAAABJRU5ErkJggg==", height = "40px", width = "40px", style = "border-radius: 50%; cursor: pointer;", id = "profile_pic"),
    textOutput("profile_name")
  ),
  
  div(id = "sidebar", style = "display: none; position: fixed; top: 0; right: 0; width: 250px; height: 100%; background: white; border-left: 1px solid #ddd; padding: 20px; box-shadow: -2px 0 5px rgba(0,0,0,0.2); z-index: 100;",
    actionButton("logout", "Log Out", style = "width: 100%; background-color: red; color: white;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("project_name", "Enter project name:"),
      selectInput("project_type", "Select project type:", choices = c("Research", "Development", "Experiment")),
      textAreaInput("project_description", "Describe your project:"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("confirmation")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  current_page <- reactiveVal("login")
  user_name <- reactiveVal("")
  
  observeEvent(input$go, {
    if (input$username != "" && input$course != "") {
      user_name(input$username)
      current_page("project_setup")
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter both your username and course name.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Success!",
      paste("Project", input$project_name, "has been set up successfully!"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$logout, {
    current_page("login")
  })
  
  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,", user_name())
  })
  
  output$main_ui <- renderUI({
    if (current_page() == "login") {
      login_ui
    } else {
      project_setup_ui
    }
  })
}

# Run the application
shinyApp(ui = fluidPage(
  tags$head(
    tags$script(HTML("$(document).ready(function(){
      $(document).on('click', '#profile_pic', function(event){
        event.stopPropagation();
        $('#sidebar').toggle();
      });
      $(document).on('click', function(event){
        if (!$(event.target).closest('#sidebar').length && !$(event.target).is('#profile_pic')) {
          $('#sidebar').hide();
        }
      });
    });"))
  ),
  uiOutput("main_ui")
), server = server)