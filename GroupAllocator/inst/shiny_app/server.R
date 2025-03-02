library(shiny)
server <- function(input, output, session) {
  current_page <- reactiveVal("login")
  user_name <- reactiveVal("")
  projects <- reactiveVal(list())
  skills <- reactiveVal(list())

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

  observeEvent(input$add_project, {
    req(input$project_name)
    new_projects <- projects()
    new_projects <- c(new_projects, input$project_name)
    projects(new_projects)
    updateTextInput(session, "project_name", value = "")
  })

  observeEvent(input$add_skill, {
    req(input$skill_name)
    new_skills <- skills()
    new_skills <- c(new_skills, input$skill_name)
    skills(new_skills)
    updateTextInput(session, "skill_name", value = "")
  })

  output$project_list <- renderUI({
    project_items <- lapply(seq_along(projects()), function(i) {
      fluidRow(
        column(8, strong(projects()[[i]])),
        column(4, actionButton(paste0("remove_project_", i), "Remove",
                               class = "btn-danger btn-sm",
                               style = "float: right;"))
      )
    })
    do.call(tagList, project_items)
  })

  output$skill_list <- renderUI({
    skill_items <- lapply(seq_along(skills()), function(i) {
      fluidRow(
        column(8, strong(skills()[[i]])),
        column(4, actionButton(paste0("remove_skill_", i), "Remove",
                               class = "btn-danger btn-sm",
                               style = "float: right;"))
      )
    })
    do.call(tagList, skill_items)
  })

  observe({
    lapply(seq_along(projects()), function(i) {
      observeEvent(input[[paste0("remove_project_", i)]], {
        new_projects <- projects()
        new_projects <- new_projects[-i]
        projects(new_projects)
      })
    })
  })

  observe({
    lapply(seq_along(skills()), function(i) {
      observeEvent(input[[paste0("remove_skill_", i)]], {
        new_skills <- skills()
        new_skills <- new_skills[-i]
        skills(new_skills)
      })
    })
  })

  observeEvent(input$logout, {
    current_page("login")
  })

  output$welcome_message <- renderText({
    req(user_name(), input$course)
    paste("Hello",
          user_name(),
          ", Welcome to the Project Set-up Page for Course",
          input$course)
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
