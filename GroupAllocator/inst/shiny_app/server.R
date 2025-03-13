source("../../R/optimization_model.R")
source("../../R/import_data.R")

server <- function(input, output, session) {
  
  # Track which page is displayed
  current_page <- reactiveVal("login")
  
  # Store username for display
  user_name <- reactiveVal("")
  course_name <- reactiveVal("")
  
  # Reactive values for data
  params <- reactiveValues(
    c_team = NULL,
    b_subteam = NULL,
    x_topic_teams = NULL,
    survey_uploaded = FALSE,     # Track if user has uploaded CSV
    final_assignments = NULL,    # Store final optimization result
    error_message = NULL         # Store any error messages
  )
  
  # Render dynamic UI based on current page
  output$main_ui <- renderUI({
    switch(
      current_page(),
      "login"         = login_ui,
      "project_setup" = project_setup_ui,
      "csv_upload"    = csv_upload_ui,
      "result"        = result_ui
    )
  })

  # Handle download of results
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("student_assignments_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(params$final_assignments)
      write.csv(params$final_assignments, file, row.names = FALSE)
    }
  )
  
  # --------------------------------------------------------------------------
  # Page Navigation
  # --------------------------------------------------------------------------
  
  # 1) Login -> Project Setup
  observeEvent(input$go, {
    if (input$username != "" && input$course != "") {
      user_name(input$username)
      course_name(input$course)
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
  
  # 2) "Back" from Project Setup -> Login
  observeEvent(input$back_to_login, {
    current_page("login")
  })
  
  # 3) "Back" from CSV Upload -> Project Setup
  observeEvent(input$back_to_setup, {
    current_page("project_setup")
  })
  
  # 4) "Back" from Result -> CSV Upload
  observeEvent(input$back_to_upload, {
    current_page("csv_upload")
  })
  
  # 5) Next Step -> CSV Upload
  observeEvent(input$next_step, {
    # Validate inputs first
    if (is.na(input$c_team) || is.na(input$b_subteam) || is.na(input$x_topic_teams)) {
      showModal(modalDialog(
        title = "Error",
        "Please enter valid values for all fields.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Save numeric inputs to user_inputs.csv
    # so read_project_data() can read them
    data_to_save <- data.frame(
      c_team        = input$c_team,
      b_subteam     = input$b_subteam,
      x_topic_teams = input$x_topic_teams
    )
    
    # Store values in reactive values for later use
    params$c_team <- input$c_team
    params$b_subteam <- input$b_subteam
    params$x_topic_teams <- input$x_topic_teams
    
    # Save to CSV
    tryCatch({
      write.table(
        data_to_save,
        file = "user_inputs.csv",
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,  # Changed to FALSE as expected by read_project_data
        append = FALSE
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to save settings:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    })
    
    current_page("csv_upload")
  })
  
  # 6) Open Microsoft Form in new tab
  observeEvent(input$go_survey, {
    # Optionally also save numeric inputs here if needed
    form_url <- "https://forms.office.com/your-form-url"
    js <- sprintf("window.open('%s', '_blank');", form_url)
    session$sendCustomMessage(type = "jsCode", js)
  })
  
  # --------------------------------------------------------------------------
  # Parameter Change Logic on Results Page
  # --------------------------------------------------------------------------
  
  # Toggle parameter panel on result page
  observeEvent(input$change_params, {
    shinyjs::toggle("params_panel")
    
    # Initialize new parameter inputs with current values
    if (!is.null(params$c_team)) {
      updateNumericInput(session, "new_c_team", value = params$c_team)
      updateNumericInput(session, "new_b_subteam", value = params$b_subteam)
      updateNumericInput(session, "new_x_topic_teams", value = params$x_topic_teams)
    }
  })
  
  # Run model again with new parameters
  observeEvent(input$run_again, {
    # Validate inputs
    if (is.na(input$new_c_team) || is.na(input$new_b_subteam) || is.na(input$new_x_topic_teams)) {
      showModal(modalDialog(
        title = "Error",
        "Please enter valid values for all fields.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Save new parameters
    data_to_save <- data.frame(
      c_team        = input$new_c_team,
      b_subteam     = input$new_b_subteam,
      x_topic_teams = input$new_x_topic_teams
    )
    
    # Update reactive values
    params$c_team <- input$new_c_team
    params$b_subteam <- input$new_b_subteam
    params$x_topic_teams <- input$new_x_topic_teams
    
    # Save to CSV
    tryCatch({
      write.table(
        data_to_save,
        file = "user_inputs.csv",
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = FALSE
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to save settings:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    })
    
    # Show progress while running optimization
    withProgress(message = "Running optimization with new parameters...", {
      # Run the optimization with error handling
      tryCatch({
        # Pass the path to the uploaded CSV file
        res <- run_optimization("survey_data.csv")
        
        # Store the results
        params$final_assignments <- res$assignments
        params$error_message <- NULL
        
        # Hide the parameter panel
        shinyjs::hide("params_panel")
        
        # Show success message
        showModal(modalDialog(
          title = "Success",
          "Model ran successfully with new parameters!",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }, error = function(e) {
        params$error_message <- e$message
        showModal(modalDialog(
          title = "Optimization Error",
          paste("Failed to generate allocation with new parameters:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })
  
  # --------------------------------------------------------------------------
  # CSV Upload Logic
  # --------------------------------------------------------------------------
  
  # Upload CSV -> Save to "survey_data.csv"
  observeEvent(input$upload_csv, {
    req(input$survey_csv)
    
    # Copy the uploaded file to a known location
    dest_path <- "survey_data.csv"
    
    tryCatch({
      file.copy(input$survey_csv$datapath, dest_path, overwrite = TRUE)
      params$survey_uploaded <- TRUE
      
      # Enable the generate allocation button
      shinyjs::enable("generate_allocation")
      
      showModal(modalDialog(
        title = "Upload Successful",
        paste("CSV has been saved to", dest_path),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Upload Error",
        paste("Failed to save the CSV file:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # --------------------------------------------------------------------------
  # Generate Allocation -> Run Optimization -> Go to Result Page
  # --------------------------------------------------------------------------
  observeEvent(input$generate_allocation, {
    req(params$survey_uploaded)  # Ensure user uploaded CSV
    
    # Show progress indicator
    withProgress(message = "Running optimization...", {
      
      # Run the optimization with error handling
      tryCatch({
        # Pass the path to the uploaded CSV file
        res <- run_optimization("survey_data.csv")
        
        # Store the results
        params$final_assignments <- res$assignments
        params$error_message <- NULL
        
        # Go to results page
        current_page("result")
      }, error = function(e) {
        params$error_message <- e$message
        showModal(modalDialog(
          title = "Optimization Error",
          paste("Failed to generate allocation:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })

  # --------------------------------------------------------------------------
  # Display the Allocation Table on the Result Page
  # --------------------------------------------------------------------------
  output$allocation_table <- renderTable({
    req(params$final_assignments)
    params$final_assignments
  })
  
  # Display optimization parameters on result page
  output$optimization_params <- renderText({
    req(params$c_team, params$b_subteam, params$x_topic_teams)
    paste("Current Settings: Team Size =", params$c_team, 
          ", Subteam Size =", params$b_subteam, 
          ", Max Teams per Topic =", params$x_topic_teams)
  })
  
  # Enable/disable the Generate Allocation button based on whether CSV is uploaded
  observe({
    shinyjs::toggleState("generate_allocation", condition = params$survey_uploaded)
  })
  
  # --------------------------------------------------------------------------
  # Display text
  # --------------------------------------------------------------------------
  output$welcome_message <- renderText({
    req(user_name(), course_name())
    paste("Hello", user_name(), ", Welcome to the Project Set-up Page for Course", course_name())
  })
  
  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,", user_name())
  })
  
  # Update input values when returning to the project setup page
  observe({
    if (current_page() == "project_setup" && !is.null(params$c_team)) {
      updateNumericInput(session, "c_team", value = params$c_team)
      updateNumericInput(session, "b_subteam", value = params$b_subteam)
      updateNumericInput(session, "x_topic_teams", value = params$x_topic_teams)
    }
  })
}