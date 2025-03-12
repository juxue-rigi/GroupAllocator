source("../../R/optimization_model.R")
source("../../R/import_data.R")

server <- function(input, output, session) {
  
  # Track which page is displayed
  current_page <- reactiveVal("login")
  
  # Store username for display
  user_name <- reactiveVal("")
  
  # Reactive values for data
  params <- reactiveValues(
    c_team = NULL,
    b_subteam = NULL,
    x_topic_teams = NULL,
    survey_uploaded = FALSE,     # Track if user has uploaded CSV
    final_assignments = NULL     # Store final optimization result
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
  
  # --------------------------------------------------------------------------
  # Page Navigation
  # --------------------------------------------------------------------------
  
  # 1) Login -> Project Setup
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
    # Save numeric inputs to user_inputs.csv
    # so read_project_data() can read them
    data_to_save <- data.frame(
      c_team        = input$c_team,
      b_subteam     = input$b_subteam,
      x_topic_teams = input$x_topic_teams
    )
    write.table(
      data_to_save,
      file = "shiny_app/user_inputs.csv",
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists("shiny_app/user_inputs.csv"),
      append = FALSE
    )
    
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
  # CSV Upload Logic
  # --------------------------------------------------------------------------
  
  # Upload CSV -> Save to "shiny_app/survey_data.csv"
  observeEvent(input$upload_csv, {
    req(input$survey_csv)
    
    # Copy the uploaded file to a known location
    dest_path <- file.path("shiny_app", "survey_data.csv")
    file.copy(input$survey_csv$datapath, dest_path, overwrite = TRUE)
    
    params$survey_uploaded <- TRUE
    
    showModal(modalDialog(
      title = "Upload Successful",
      paste("CSV has been saved to", dest_path),
      easyClose = TRUE
    ))
  })
  
  # --------------------------------------------------------------------------
  # Generate Allocation -> Run Optimization -> Go to Result Page
  # --------------------------------------------------------------------------
  observeEvent(input$generate_allocation, {
    req(params$survey_uploaded)  # Ensure user uploaded CSV
    
    # Now run_optimization(), which calls read_data() with the path
    # read_data() -> read_project_data() + process_survey_data()
    # We'll pass the path "shiny_app/survey_data.csv"
    res <- run_optimization()  # If run_optimization() is hardcoded to read "path/to/uploaded_student_survey.csv"
    
    # Alternatively, if run_optimization() calls read_data("shiny_app/survey_data.csv"), 
    # you might need to update run_optimization() to point to the correct file, e.g.:
    # res <- run_optimization("shiny_app/survey_data.csv")
    # Then inside run_optimization() do read_data(survey_csv_path).
    
    params$final_assignments <- res$assignments
    
    current_page("result")
  })
  
  # --------------------------------------------------------------------------
  # Display the Allocation Table on the Result Page
  # --------------------------------------------------------------------------
  output$allocation_table <- renderTable({
    req(params$final_assignments)
    params$final_assignments
  })
  
  # --------------------------------------------------------------------------
  # Display text
  # --------------------------------------------------------------------------
  output$welcome_message <- renderText({
    req(user_name(), input$course)
    paste("Hello", user_name(), ", Welcome to the Project Set-up Page for Course", input$course)
  })
  
  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,", user_name())
  })
}