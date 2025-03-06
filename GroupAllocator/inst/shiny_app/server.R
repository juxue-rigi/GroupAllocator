library(shiny)

server <- function(input, output, session) {

  current_page <- reactiveVal("login")
  user_name <- reactiveVal("")
  projects <- reactiveVal(list())
  skills <- reactiveVal(list())
  subgroups <- reactiveVal(list())

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
                               class = "btn-danger btn-sm",
                               style = "float: right;"))
      )
    })
  })

  observe({
    lapply(seq_along(projects()), function(i) {
      observeEvent(input[[paste0("remove_project_", i)]], {
        projects(projects()[-i])
      }, ignoreInit = TRUE)
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
                               class = "btn-danger btn-sm",
                               style = "float: right;"))
      )
    })
  })

  observe({
    lapply(seq_along(subgroups()), function(i) {
      observeEvent(input[[paste0("remove_subgroup_", i)]], {
        subgroups(subgroups()[-i])
      }, ignoreInit = TRUE)
    })
  })

  ### Skills
  observeEvent(input$add_skill, {
    req(input$skill_name)
    current_skills <- skills()
    skills(c(current_skills, input$skill_name))
    updateTextInput(session, "skill_name", value = "")
  })

  output$skill_list <- renderUI({
    lapply(seq_along(skills()), function(i) {
      fluidRow(
        column(8, strong(skills()[[i]])),
        column(4, actionButton(paste0("remove_skill_", i), "Remove",
                               class = "btn-danger btn-sm",
                               style = "float: right;"))
      )
    })
  })

  observe({
    lapply(seq_along(skills()), function(i) {
      observeEvent(input[[paste0("remove_skill_", i)]], {
        current_skills <- skills()
        skills(skills()[-i])
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$generate_survey, {
    current_page("survey")
  })

  ### Student Survey UI
  output$project_ranking <- renderUI({
    req(projects())
    proj_list <- projects()
    # Only show rank choices if there are projects
    if (length(proj_list) > 0) {
      lapply(1:min(4, length(proj_list)), function(i) {
        selectInput(paste0("rank_project_", i),
                    paste0(i, "st choice"),
                    choices = proj_list,
                    selected = NULL)
      })
    }
  })

  output$subgroup_selection <- renderUI({
    req(subgroups())
    sg_list <- subgroups()
    # Only show subgroup choices if there are subgroups
    if (length(sg_list) > 0) {
      lapply(1:min(4, length(sg_list)), function(i) {
        selectInput(paste0("subgroup_choice_", i),
                    paste0(i, "st choice - Sub-group function"),
                    choices = sg_list,
                    selected = NULL)
      })
    }
  })

  output$student_inputs <- renderUI({
    req(input$sub_group_size)  # Ensure sub-group size is set
    num_students <- as.numeric(input$sub_group_size) # Convert input to numeric
    student_fields <- lapply(1:num_students, function(i) {
      tagList(
        textInput(paste0("student_name_", i),
                  paste("Student", i, "Name:")),
        textInput(paste0("student_id_", i),
                  paste("Student", i, "ID:"))
      )
    })
    do.call(tagList, student_fields)
  })

  # Skills rating UI
  output$skill_ratings <- renderUI({
    req(skills())
    skill_list <- skills()

    if (length(skill_list) == 0) {
      return(p("No skills have been defined for this project."))
    }
    lapply(seq_along(skill_list), function(i) {
      skill_name <- skill_list[[i]]
      # A container for each skill's label + radio buttons
      div(
        style = "margin-bottom: 30px;",
        tags$strong(skill_name),
        # Put radio buttons & labels in a separate div
        div(
          style = "position: relative; 
                   display: inline-block; 
                   margin-left: 10px;",
          radioButtons(
            inputId = paste0("skill_rating_", i),
            label = NULL,
            choices = 1:5,
            inline = TRUE
          ),
          tags$div(
            style = "
              position: absolute;
              top: 2.0em;  /* adjust if needed */
              width: 100%;
              display: flex;
              justify-content: space-between;",
            tags$span("Beginner"),
            tags$span("Expert")
          )
        )
      )
    })
  })

  ### Save Student Survey Data
  observeEvent(input$submit_survey, {
    num_students <- as.numeric(input$sub_group_size)
    student_data <- lapply(1:num_students, function(i) {
      name <- input[[paste0("student_name_", i)]]
      id <- input[[paste0("student_id_", i)]]
      if (!is.null(name) && name != "" && !is.null(id) && id != "") {
        data.frame(Name = name, StudentID = id)
      } else {
        NULL # Ignore empty fields
      }
    })
    student_data <- do.call(rbind, student_data) #combine filled student entries
    if (is.null(student_data) || nrow(student_data) == 0) {
      showModal(modalDialog(title = "Error",
                            "At least one student must be entered.",
                            easyClose = TRUE))
      return()
    }

    # Get project rankings
    project_ranks <- character(0)
    if (length(projects()) > 0) {
      project_ranks <- sapply(1:min(4, length(projects())),
                              function(i) {
                                if (!is.null(input[[paste0("rank_project_", i)]])) {
                                  input[[paste0("rank_project_", i)]]
                                } else {
                                  NA
                                }
                              })
    }

    # Get subgroup choices
    subgroup_choices <- character(0)
    if (length(subgroups()) > 0) {
      subgroup_choices <- sapply(1:min(4, length(subgroups())),
                                 function(i) {
                                   if (!is.null(input[[paste0("subgroup_choice_", i)]])) {
                                     input[[paste0("subgroup_choice_", i)]]
                                   } else {
                                     NA
                                   }
                                 })
    }

    # Get skill ratings
    skill_ratings <- list()
    if (length(skills()) > 0) {
      skill_list <- skills()
      skill_ratings <- sapply(seq_along(skill_list), function(i) {
        skill_name <- skill_list[[i]]
        rating <- input[[paste0("skill_rating_", i)]]
        if (is.null(rating)) {
          paste0(skill_name, ": Not rated")
        } else {
          paste0(skill_name, ": ", rating)
        }
      })
    }

    # Create response data frame
    response_data <- data.frame(
      StudentData = I(list(student_data)), # store list of student names & IDs
      ProjectRanking = paste(project_ranks, collapse = " | "),
      SubgroupChoices = paste(subgroup_choices, collapse = " | "),
      SkillRatings = paste(skill_ratings, collapse = " | ")
    )
    # Write to file
    write.table(response_data, file = "student_survey_responses.csv",
                append = TRUE,
                sep = ",",
                row.names = FALSE,
                col.names = !file.exists("student_survey_responses.csv"))
    output$survey_confirmation <- renderText("Survey submitted successfully!")
  })

  ### text display
  output$welcome_message <- renderText({
    req(user_name(), input$course)
    paste("Hello",
          user_name(),
          ", Welcome to the Project Set-up Page for Course", input$course)
  })

  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,",
          user_name())
  })
}
