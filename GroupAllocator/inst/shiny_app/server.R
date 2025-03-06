library(shiny)

server <- function(input, output, session) {

  current_page <- reactiveVal("login")
  user_name <- reactiveVal("")
  projects <- reactiveVal(list())
  skills <- reactiveVal(list())
  subgroups <- reactiveVal(list())

  ### Navigation Logic
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

  observeEvent(input$logout, {
    current_page("login")
  })

  ### projects
  observeEvent(input$add_project, {
    req(input$project_name)
    projects(c(projects(), input$project_name))
    updateTextInput(session, "project_name", value = "")
  })

  output$project_list <- renderUI({
    lapply(seq_along(projects()), function(i) {
      fluidRow(
        column(8, strong(projects()[[i]])),
        column(4, actionButton(paste0("remove_project_", i), "Remove", 
                               class = "btn-danger btn-sm", style = "float: right;"))
      )
    })
  })

  observe({
    lapply(seq_along(projects()), function(i) {
      observeEvent(input[[paste0("remove_project_", i)]], {
        projects(projects()[-i])
      })
    })
  })

  ### Sub-group Function 
  observeEvent(input$add_subgroup, {
    req(input$subgroup_name)
    subgroups(c(subgroups(), input$subgroup_name))
    updateTextInput(session, "subgroup_name", value = "")
  })

  output$subgroup_list <- renderUI({
    lapply(seq_along(subgroups()), function(i) {
      fluidRow(
        column(8, strong(subgroups()[[i]])),
        column(4, actionButton(paste0("remove_subgroup_", i), "Remove", 
                               class = "btn-danger btn-sm", style = "float: right;"))
      )
    })
  })

  observe({
    lapply(seq_along(subgroups()), function(i) {
      observeEvent(input[[paste0("remove_subgroup_", i)]], {
        subgroups(subgroups()[-i])
      })
    })
  })

  ### Skills
  observeEvent(input$add_skill, {
    req(input$skill_name)
    skills(c(skills(), input$skill_name))
    updateTextInput(session, "skill_name", value = "")
  })

  output$skill_list <- renderUI({
    lapply(seq_along(skills()), function(i) {
      fluidRow(
        column(8, strong(skills()[[i]])),
        column(4, actionButton(paste0("remove_skill_", i), "Remove", 
                               class = "btn-danger btn-sm", style = "float: right;"))
      )
    })
  })

  observe({
    lapply(seq_along(skills()), function(i) {
      observeEvent(input[[paste0("remove_skill_", i)]], {
        skills(skills()[-i])
      })
    })
  })

  ### Student Survey
  observeEvent(input$generate_survey, {
    current_page("survey")
  })

  ### Student Survey UI 
  output$project_ranking <- renderUI({
    req(projects())
    lapply(1 : min(4, length(projects())), function(i) {
      selectInput(paste0("rank_project_", i), 
                  paste0(i, "st choice"), 
                  choices = projects(), 
                  selected = NULL)
    })
  })

  output$subgroup_selection <- renderUI({
    req(subgroups())  # Using user-entered sub-group functions
    lapply(1:min(4, length(projects())), function(i) {
      selectInput(paste0("subgroup_choice_", i), 
                  paste0(i, "st choice - Sub-group function"), 
                  choices = subgroups(),  # filled from project set-up
                  selected = NULL)
    })
  })

    output$student_inputs <- renderUI({
     req(input$sub_group_size)  # Ensure sub-group size is set
     num_students <- as.numeric(input$sub_group_size)  # Convert input to numeric
     student_fields <- lapply(1:num_students, function(i) {
        tagList(
            textInput(paste0("student_name_", i), paste("Student", i, "Name (optional):")),
            textInput(paste0("student_id_", i), paste("Student", i, "ID (optional):"))
        )
    })
    do.call(tagList, student_fields)
  })


  ### Save Student Survey Data
    observeEvent(input$submit_survey, {
    num_students <- as.numeric(input$sub_group_size)
    
    student_data <- lapply(1:num_students, function(i) {
        name <- input[[paste0("student_name_", i)]]
        id <- input[[paste0("student_id_", i)]]
        
        if (!is.null(name) && name != "" && !is.null(id) && id != "") {
        return(data.frame(Name = name, StudentID = id))
        } else {
        return(NULL)  # Ignore empty fields
        }
    })
    
    student_data <- do.call(rbind, student_data)  # Combine filled student entries
    
    if (nrow(student_data) == 0) {
        showModal(modalDialog(title = "Error", "At least one student must be entered.", easyClose = TRUE))
        return()
    }
    
    project_ranks <- sapply(1:min(4, length(projects())), function(i) input[[paste0("rank_project_", i)]])
    subgroup_choices <- sapply(1:min(4, length(subgroups())), function(i) input[[paste0("subgroup_choice_", i)]])
    
    response_data <- data.frame(
        StudentData = I(list(student_data)),  # Store list of student names & IDs
        ProjectRanking = paste(project_ranks, collapse = " | "),
        SubgroupChoices = paste(subgroup_choices, collapse = " | ")
    )
    
    write.table(response_data, file = "student_survey_responses.csv", 
                append = TRUE, sep = ",", row.names = FALSE, col.names = !file.exists("student_survey_responses.csv"))
    
    output$survey_confirmation <- renderText("Survey submitted successfully!")
    })

  ### UI Switching Logic
  output$main_ui <- renderUI({
    if (current_page() == "login") {
      login_ui
    } else if (current_page() == "project_setup") {
      project_setup_ui
    } else if (current_page() == "survey") {
      survey_ui
    }
  })

  ### text display
  output$welcome_message <- renderText({
    req(user_name(), input$course)
    paste("Hello", user_name(), ", Welcome to the Project Set-up Page for Course", input$course)
  })

  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,", user_name())
  })
}