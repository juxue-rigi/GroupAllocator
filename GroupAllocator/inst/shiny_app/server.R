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
    error_message = NULL,        # Store any error messages
    preference_score = NULL,     # Store the current objective value
    previous_score = NULL,       # Store the previous objective value for comparison
    k_solutions = 3             # Number of solutions to find
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
    if (is.na(input$c_team) || is.na(input$b_subteam) || is.na(input$x_topic_teams) || is.na(input$k_solutions)) {
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
    params$c_team <- as.numeric(input$c_team)
  params$b_subteam <- as.numeric(input$b_subteam)
  params$x_topic_teams <- as.numeric(input$x_topic_teams)
  params$k_solutions <- as.numeric(input$k_solutions)

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
    form_url <- "https://forms.office.com/Pages/ResponsePage.aspx?id=DQSIkWdsW0yxEjajBLZtrQAAAAAAAAAAAANAATn8PsFUMjI0Q0JLOEgwRkwxVTdaNjlYNzJTVk1RUy4u"
    js <- sprintf("window.open('%s', '_blank');", form_url)
    session$sendCustomMessage(type = "jsCode", js)
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
      
      # Ensure parameters are numeric before calling optimization
      c_team <- as.numeric(params$c_team)
      b_subteam <- as.numeric(params$b_subteam)
      x_topic_teams <- as.numeric(params$x_topic_teams)
      k_solutions <- as.numeric(params$k_solutions)
      
      # Print debug info to console
      message("Optimization parameters:")
      message("c_team = ", c_team, " (", class(c_team), ")")
      message("b_subteam = ", b_subteam, " (", class(b_subteam), ")")
      message("x_topic_teams = ", x_topic_teams, " (", class(x_topic_teams), ")")
      message("k_solutions = ", k_solutions, " (", class(k_solutions), ")")

      
      # Run the optimization with error handling
      tryCatch({
        # Pass the path to the uploaded CSV file
        res <- run_multi_solution_optimization("survey_data.csv",
                                                params$k_solutions,
                                                5.0  # Default score gap of 5%
        )
        
        # Check if we have valid results
        if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
          # Store the results
          params$all_assignments <- res$assignments_list
          params$objective_values <- res$objective_values
          params$score_diffs <- res$score_diffs
          params$current_solution_idx <- res$best_solution_index
          params$final_assignments <- res$assignments_list[[res$best_solution_index]]
          params$preference_score <- res$objective_values[res$best_solution_index]
          params$previous_score <- NULL  # No previous score for first run
          params$error_message <- NULL
          
          # Go to results page
          current_page("result")
        } else {
          showModal(modalDialog(
            title = "Optimization Error",
            "The optimization completed but didn't produce valid assignments. Please check your data and parameters.",
            easyClose = TRUE
          ))
        }
      }, error = function(e) {
        params$error_message <- e$message
        message("Optimization error: ", e$message)  # Log for debugging
        showModal(modalDialog(
          title = "Optimization Error",
          paste("Failed to generate allocation:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })

  # --------------------------------------------------------------------------
  # Navigation between solutions
  # --------------------------------------------------------------------------
  
  observeEvent(input$next_solution, {
    req(params$all_assignments)
    
    # Calculate next index (with wrap-around)
    next_idx <- params$current_solution_idx %% length(params$all_assignments) + 1
    
    # Update displayed solution
    params$current_solution_idx <- next_idx
    params$final_assignments <- params$all_assignments[[next_idx]]
    params$preference_score <- params$objective_values[next_idx]
  })

  observeEvent(input$prev_solution, {
    req(params$all_assignments)
    
    # Calculate previous index (with wrap-around)
    prev_idx <- params$current_solution_idx - 1
    if (prev_idx < 1) prev_idx <- length(params$all_assignments)
    
    # Update displayed solution
    params$current_solution_idx <- prev_idx
    params$final_assignments <- params$all_assignments[[prev_idx]]
    params$preference_score <- params$objective_values[prev_idx]
  })

  # Add observer for solution card click
  observeEvent(input$select_solution, {
    req(params$all_assignments, input$select_solution)
    
    sol_idx <- input$select_solution
    if (sol_idx >= 1 && sol_idx <= length(params$all_assignments)) {
      params$current_solution_idx <- sol_idx
      params$final_assignments <- params$all_assignments[[sol_idx]]
      params$preference_score <- params$objective_values[sol_idx]
    }
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
    params$c_team <- as.numeric(input$new_c_team)
    params$b_subteam <- as.numeric(input$new_b_subteam)
    params$x_topic_teams <- as.numeric(input$new_x_topic_teams)
  
    # Log for debugging
    message("New parameters for re-run:")
    message("c_team = ", params$c_team, " (", class(params$c_team), ")")
    message("b_subteam = ", params$b_subteam, " (", class(params$b_subteam), ")")
    message("x_topic_teams = ", params$x_topic_teams, " (", class(params$x_topic_teams), ")")

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
    params$c_team <- as.numeric(input$new_c_team)
  params$b_subteam <- as.numeric(input$new_b_subteam)
  params$x_topic_teams <- as.numeric(input$new_x_topic_teams)
    
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

    # Store the current score as previous before running again
    params$previous_score <- params$preference_score
    
    # Show progress while running optimization
    withProgress(message = "Running optimization with new parameters...", {
      # Run the optimization with error handling
      tryCatch({
        # Pass the path to the uploaded CSV file
        res <- run_balanced_optimization(
        "survey_data.csv", 
        params$k_solutions,
        5.0  # Default score gap of 5%
      )
        
        # Check if we have valid results
      if (length(res$assignments_list) > 0 && !is.null(res$assignments_list[[res$best_solution_index]])) {
        # Store the results
        params$all_assignments <- res$assignments_list
        params$objective_values <- res$objective_values
        params$score_diffs <- res$score_diffs
        params$current_solution_idx <- res$best_solution_index
        params$final_assignments <- res$assignments_list[[res$best_solution_index]]
        params$preference_score <- res$objective_values[res$best_solution_index]
        params$error_message <- NULL
        
        # Hide the parameter panel
        shinyjs::hide("params_panel")
        
        # Show success message with score comparison
        score_diff <- params$preference_score - params$previous_score
        score_change_msg <- if(score_diff > 0) {
          paste("Preference score increased by", round(score_diff, 2), "points!")
        } else if(score_diff < 0) {
          paste("Preference score decreased by", round(abs(score_diff), 2), "points.")
        } else {
          "Preference score remained the same."
        }
        
        showModal(modalDialog(
          title = "Success",
          HTML(paste(
            "Model ran successfully with new parameters!<br><br>",
            "<strong>", score_change_msg, "</strong>"
          )),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else {
        showModal(modalDialog(
          title = "Optimization Error",
          "The optimization completed but didn't produce valid assignments. Please check your data and parameters.",
          easyClose = TRUE
        ))
      }
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
  # Display text (Output)
  # --------------------------------------------------------------------------
  output$welcome_message <- renderText({
    req(user_name(), course_name())
    paste("Hello", user_name(), ", Welcome to the Project Set-up Page for Course", course_name())
  })
  
  output$profile_name <- renderText({
    req(user_name())
    paste("Hello,", user_name())
  })

  output$course_display <- renderText({
    req(course_name())
    paste("Course:", course_name())
  })

  # Displaying the preference score and score change
  output$preference_score_text <- renderText({
    req(params$preference_score)
    paste("Total Preference Score:", round(params$preference_score, 2))
  })
  
  # Update input values when returning to the project setup page
  observe({
    if (current_page() == "project_setup" && !is.null(params$c_team)) {
      updateNumericInput(session, "c_team", value = params$c_team)
      updateNumericInput(session, "b_subteam", value = params$b_subteam)
      updateNumericInput(session, "x_topic_teams", value = params$x_topic_teams)
    }
  })
  
  # Displaying the preference score and score change
  output$preference_score_text <- renderText({
    req(params$preference_score)
    paste("Total Preference Score:", round(params$preference_score, 2))
  })
  
  # Render the score comparison text with icons
  output$score_comparison_text <- renderUI({
    # Only show if we have both a current and previous score
    req(params$preference_score, params$previous_score)
    
    score_diff <- params$preference_score - params$previous_score
    
    if(score_diff > 0) {
      # Positive change - upward arrow with green text
      HTML(paste(
        "<span style='color: var(--success); font-weight: 500;'>",
        "<i class='fa fa-arrow-up' style='margin-right: 5px;'></i>",
        "Increased by", round(score_diff, 2), "points",
        "</span>"
      ))
    } else if(score_diff < 0) {
      # Negative change - downward arrow with red text
      HTML(paste(
        "<span style='color: var(--danger); font-weight: 500;'>",
        "<i class='fa fa-arrow-down' style='margin-right: 5px;'></i>",
        "Decreased by", round(abs(score_diff), 2), "points",
        "</span>"
      ))
    } else {
      # No change - horizontal arrow with neutral color
      HTML(paste(
        "<span style='color: var(--dark); font-weight: 500;'>",
        "<i class='fa fa-arrow-right' style='margin-right: 5px;'></i>",
        "No change in score",
        "</span>"
      ))
    }
  })

  output$solution_nav_info <- renderText({
    req(params$all_assignments, params$current_solution_idx)
    paste("Solution", params$current_solution_idx, "of", length(params$all_assignments))
  })

  #  Output for solution comparison cards
  output$solution_comparison <- renderUI({
    req(params$all_assignments, params$objective_values, params$score_diffs)
    
    # Get the number of solutions
    n_solutions <- length(params$all_assignments)
    best_score <- max(params$objective_values)
    best_idx <- which.max(params$objective_values)
    
    # Create a card for each solution
    solution_cards <- lapply(1:n_solutions, function(idx) {
      score <- params$objective_values[idx]
      diff <- params$score_diffs[idx]
      is_current <- idx == params$current_solution_idx
      is_best <- idx == best_idx
      
      # Determine card styling based on current selection and whether it's the best solution
      card_style <- if (is_current) {
        "border: 2px solid var(--secondary); box-shadow: 0 6px 16px rgba(0, 0, 0, 0.15);"
      } else if (is_best) {
        "border: 2px solid var(--success);"
      } else {
        ""
      }
      
      # Create the card
      div(
        id = paste0("solution_card_", idx),
        class = "solution-card",
        style = paste(
          "background-color: white;",
          "border-radius: 8px;",
          "padding: 15px;",
          "margin-bottom: 15px;",
          "cursor: pointer;",
          card_style
        ),
        onclick = paste0("Shiny.setInputValue('select_solution', ", idx, ")"),
        
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              h4(paste("Solution", idx), style = if(is_best) "color: var(--success); margin: 0;" else "margin: 0;"),
              if (is_best) {
                tags$span(class = "badge", style = "background-color: var(--success); color: white; padding: 3px 8px; border-radius: 4px;", "Best")
              } else {
                if (is_current) {
                  tags$span(class = "badge", style = "background-color: var(--secondary); color: white; padding: 3px 8px; border-radius: 4px;", "Current")
                }
              }
            ),
            div(
              style = "text-align: right;",
              div(style = "font-weight: 600;", paste0("Score: ", round(score, 2))),
              if (diff > 0) {
                div(style = "color: var(--danger); font-size: 0.9em;", 
                    paste0("-", round(diff, 2), " points from best"))
              } else {
                div(style = "color: var(--success); font-size: 0.9em;", "Best score")
              }
            )
        )
      )
    })
    
    # Wrap cards in a container
    div(
      style = "margin-top: 15px;",
      solution_cards
    )
  })


} 