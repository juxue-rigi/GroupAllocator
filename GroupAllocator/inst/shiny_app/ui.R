#===========================================================================
# TABLE OF CONTENTS
#===========================================================================
# 1. THEME AND STYLING
#    - Global CSS and Custom Theme
#
# 2. PAGE DEFINITIONS
#    - Login Page UI
#    - Project Setup Page UI
#    - CSV Upload Page UI
#    - Results Page UI
#
# 3. MAIN UI ASSEMBLY
#    - Dynamic UI Output
#===========================================================================


#===========================================================================
# 1. THEME AND STYLING
#===========================================================================

# ------------------------------------------------------------------------------
# Global CSS and Custom Theme
# ------------------------------------------------------------------------------
custom_styles <- tags$head(
  tags$style(HTML("
    :root {
      --primary: #2c3e50;
      --secondary: #3498db;
      --accent: #1abc9c;
      --light: #ecf0f1;
      --dark: #34495e;
      --danger: #e74c3c;
      --warning: #f39c12;
      --success: #2ecc71;
    }
    
    body {
      font-family: 'Roboto', 'Helvetica Neue', sans-serif;
      background-color: #f5f7fa;
      color: var(--dark);
      line-height: 1.6;
    }
    
    .card {
      background: white;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
      padding: 2rem;
      margin-bottom: 2rem;
      border-top: 4px solid var(--accent);
    }
    
    .btn-custom-primary {
      background-color: var(--secondary);
      color: white;
      border: none;
      border-radius: 4px;
      padding: 8px 16px;
      font-weight: 500;
      transition: all 0.3s ease;
    }
    
    .btn-custom-primary:hover {
      background-color: #2980b9;
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    }
    
    .btn-custom-secondary {
      background-color: var(--light);
      color: var(--dark);
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 8px 16px;
      font-weight: 500;
      transition: all 0.3s ease;
    }
    
    .btn-custom-secondary:hover {
      background-color: #dfe6e9;
      transform: translateY(-2px);
    }
    
    .btn-custom-success {
      background-color: var(--success);
      color: white;
      border: none;
      border-radius: 4px;
      padding: 8px 16px;
      font-weight: 500;
      transition: all 0.3s ease;
    }
    
    .btn-custom-success:hover {
      background-color: #27ae60;
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    }
    
    .btn-custom-info {
      background-color: var(--primary);
      color: white;
      border: none;
      border-radius: 4px;
      padding: 8px 16px;
      font-weight: 500;
      transition: all 0.3s ease;
    }
    
    .btn-custom-info:hover {
      background-color: #243441;
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    }
    
    .button-group {
      display: flex;
      gap: 10px;
      margin-top: 20px;
      justify-content: center;
    }
    
    h1, h2, h3, h4 {
      color: var(--primary);
      font-weight: 600;
    }
    
    .page-title {
      color: var(--primary);
      text-align: center;
      margin-bottom: 2rem;
      font-weight: 700;
      border-bottom: 2px solid var(--accent);
      padding-bottom: 0.5rem;
      display: inline-block;
    }
    
    .center-container { 
      display: flex; 
      flex-direction: column; 
      justify-content: center; 
      align-items: center; 
      min-height: 90vh; 
      width: 100%; 
      padding: 2rem;
    }
    
    .profile-section {
      display: flex;
      justify-content: flex-end;
      align-items: center;
      gap: 10px;
      width: 100%;
      margin-bottom: 20px;
      background-color: var(--light);
      padding: 10px 20px;
      border-radius: 8px;
    }
    
    .form-control {
      border-radius: 4px;
      border: 1px solid #ddd;
      padding: 8px 12px;
      margin-bottom: 15px;
      transition: all 0.3s ease;
    }
    
    .form-control:focus {
      border-color: var(--secondary);
      box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.25);
    }
    
    label {
      font-weight: 500;
      margin-bottom: 5px;
      display: block;
      color: var(--dark);
    }
    
    .well {
      background-color: white;
      border-radius: 8px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
      padding: 15px;
      margin-bottom: 20px;
    }
    
    table {
      width: 100%;
      border-collapse: collapse;
      margin-bottom: 20px;
    }
    
    th {
      background-color: var(--primary);
      color: white;
      text-align: left;
      padding: 12px;
    }
    
    td {
      padding: 10px 12px;
      border-bottom: 1px solid #ddd;
    }
    
    tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    
    tr:hover {
      background-color: #f1f1f1;
    }
    
    .file-input-container {
      border: 2px dashed #ddd;
      border-radius: 8px;
      padding: 20px;
      text-align: center;
      transition: all 0.3s ease;
      margin-bottom: 20px;
    }
    
    .file-input-container:hover {
      border-color: var(--secondary);
    }
    
    .progress-container {
      margin: 20px 0;
      height: 8px;
    }
    
    .progress {
      height: 8px;
      border-radius: 4px;
      background-color: #eee;
    }
    
    .progress-bar {
      background-color: var(--accent);
      border-radius: 4px;
      height: 8px;
    }

    .dataTable select {
      width: 100%;
      padding: 5px;
      border: 1px solid #ddd;
      border-radius: 4px;
      background-color: white;
      font-family: inherit;
    }

    .dataTable select:focus {
      outline: none;
      border-color: var(--secondary);
      box-shadow: 0 0 0 2px rgba(52, 152, 219, 0.25);
    }

    /* Highlight changed rows */
    .row-modified {
      background-color: rgba(26, 188, 156, 0.15) !important;
    }

    /* Edit mode indicator */
    .edit-mode-indicator {
      background-color: var(--light);
      border-left: 4px solid var(--warning);
      padding: 10px 15px;
      margin-bottom: 15px;
      border-radius: 4px;
      display: flex;
      align-items: center;
    }

    .edit-mode-indicator i {
      font-size: 20px;
      color: var(--warning);
      margin-right: 10px;
    }

    .edit-mode-indicator .score-impact {
      margin-top: 5px;
      font-weight: 500;
    }

    .score-impact.positive {
      color: var(--success);
    }

    .score-impact.negative {
      color: var(--danger);
    }

    .score-impact.neutral {
      color: var(--dark);
    }

    /* Tooltip styling */
    .tooltip-inner {
      background-color: var(--dark);
      color: white;
      border-radius: 4px;
      padding: 8px 12px;
      max-width: 250px;
    }

    .tooltip.bs-tooltip-auto[x-placement^=top] .arrow::before, 
    .tooltip.bs-tooltip-top .arrow::before {
      border-top-color: var(--dark);
    }

    /* Styling for validation messages */
    .validation-message {
      margin-top: 10px;
      padding: 8px 12px;
      border-radius: 4px;
      color: white;
    }

    .validation-message.error {
      background-color: var(--danger);
    }

    .validation-message.warning {
      background-color: var(--warning);
    }

    .validation-message.success {
      background-color: var(--success);
    }
  "))
)

#===========================================================================
# 2. PAGE DEFINITIONS
#===========================================================================

#---------------------------------------------------------------------------
# Login Page UI
#---------------------------------------------------------------------------
login_ui <- fluidPage(
  custom_styles,
  div(class = "center-container",
      div(class = "card", style = "max-width: 500px; width: 100%;",
          div(style = "text-align: center; margin-bottom: 2rem;",
              h1("Team Formation Assistant", class = "page-title"),
              p("Please enter your details to get started")
          ),
          textInput("username", "Username", placeholder = "Enter your username"),
          textInput("course", "Course Name", placeholder = "Enter your course name"),
          div(style = "text-align: center; margin-top: 2rem;",
              actionButton("go", "Go to Project Set-up", class = "btn-custom-primary", 
                           style = "width: 100%;")
          )
      )
  )
)

#---------------------------------------------------------------------------
# Project Setup Page UI
#---------------------------------------------------------------------------
project_setup_ui <- fluidPage(
  custom_styles,
  div(class = "container", style = "max-width: 1200px; margin: 0 auto; padding: 2rem 1rem;",
      # Profile section
      div(class = "profile-section",
          tags$i(class = "fa fa-user-circle", style = "font-size: 24px; color: var(--primary);"),
          span(textOutput("profile_name"), style = "font-weight: 500;"),
          span(style = "flex-grow: 1;"),
          span(textOutput("course_display"), style = "color: var(--secondary); font-weight: 500;")
      ),
      
      # Title and welcome
      div(style = "text-align: center; margin-bottom: 3rem;",
          h1("Project Set-up", class = "page-title"),
          h3(textOutput("welcome_message"), style = "font-weight: 400; color: var(--dark);")
      ),
      
      # Main content
      div(class = "card", style = "max-width: 800px; margin: 0 auto;",
          h3("Please Enter Team Formation Parameters", style = "text-align: center; margin-bottom: 2rem;"),
          
          # New: Add instruction text
          div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent); margin-bottom: 2rem;",
              h4("Instructions:", style = "color: var(--primary); margin-top: 0;"),
              p(strong("Now you can create a Microsoft Form to gather student information and their preferences over topics and sub-teams."),
                strong("Follow the \"Student Survey Template (Microsoft Form)\"."),
                style = "margin-bottom: 10px;"),
              p(strong("Make sure:")),
              tags$ol(
                tags$li(strong("Leaving spaces for more than Subteam Size will cause an error in the optimization process."), 
                        " e.g. If you have entered \"Subteam Size = 4\", then you should give at most 4 spaces for students to enter their information. 
                        Giving 5 spaces for student information will cause error."),
                tags$li(strong("Make Sure Student Information starts with \"Student_ID\"."), " Do not change this."),
                tags$li(strong("Do not change the names of the preference columns."))
              ),
              p(strong("After you have collected your data, download the response in CSV format and make sure the csv is of the following format:"),
                br(),
                em("(you could have more student_ID spaces if you allow bigger sub-team size)"))
          ),
          
          # Example CSV format table
          div(style = "overflow-x: auto; margin-bottom: 2rem;",
              HTML('<table style="width:100%; border-collapse: collapse; text-align: center;">
                    <tr style="background-color: #2c3e50; color: white;">
                      <th style="padding: 8px; border: 1px solid #ddd;">Student_ID #1</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Student_ID #2</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Student_ID #3</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Student_ID #4</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">First Choice (Topic)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Second Choice (Topic)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Third Choice (Topic)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Fourth Choice (Topic)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">First Choice (Subteam)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Second Choice (Subteam)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Third Choice (Subteam)</th>
                      <th style="padding: 8px; border: 1px solid #ddd;">Fourth Choice (Subteam)</th>
                    </tr>
                    <tr style="background-color: #f9f9f9;">
                      <td style="padding: 8px; border: 1px solid #ddd;">S112345</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">S212345</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">S312345</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">S412345</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Topic A</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Topic B</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Topic C</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Topic D</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Role 1</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Role 2</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Role 3</td>
                      <td style="padding: 8px; border: 1px solid #ddd;">Role 4</td>
                    </tr>
                  </table>')
          ),
          
          # Parameter input form
          div(style = "max-width: 500px; margin: 0 auto;",
              div(class = "form-group",
                  numericInput("c_team", 
                              tags$span(
                                tags$i(class = "fa fa-users", style = "margin-right: 8px; color: var(--secondary);"), 
                                "Team Size"
                              ),
                              value = 8, min = 1, max = 20)
              ),
              
              div(class = "form-group",
                  numericInput("b_subteam", 
                              tags$span(
                                tags$i(class = "fa fa-user-friends", style = "margin-right: 8px; color: var(--secondary);"), 
                                "Sub-team Size"
                              ),
                              value = 4, min = 1, max = 10)
              ),
              
              div(class = "form-group",
                  numericInput("x_topic_teams", 
                              tags$span(
                                tags$i(class = "fa fa-project-diagram", style = "margin-right: 8px; color: var(--secondary);"), 
                                "Max Teams per Topic"
                              ),
                              value = 3, min = 1, max = 10)
              ),

              div(class = "form-group",
                  numericInput("k_solutions", 
                              tags$span(
                                tags$i(class = "fa fa-copy", style = "margin-right: 8px; color: var(--secondary);"), 
                                "Number of Solutions to Find"
                              ),
                              value = 3, min = 1, max = 10)
              ),
              
              # Button group
              div(class = "button-group", style = "flex-direction: column; align-items: stretch;",
                  actionButton("go_survey", 
                              tags$span(
                                tags$i(class = "fa fa-file-alt", style = "margin-right: 8px;"), 
                                "Open Student Survey Template (Microsoft Form)"
                              ), 
                              class = "btn-custom-primary", 
                              style = "width: 100%; margin-bottom: 15px;"),
                  
                  actionButton("next_step", 
                              tags$span(
                                tags$i(class = "fa fa-arrow-right", style = "margin-right: 8px;"), 
                                "Next Step"
                              ), 
                              class = "btn-custom-success", 
                              style = "width: 100%; margin-bottom: 15px;"),
                  
                  actionButton("back_to_login", 
                              tags$span(
                                tags$i(class = "fa fa-arrow-left", style = "margin-right: 8px;"), 
                                "Back"
                              ), 
                              class = "btn-custom-secondary", 
                              style = "width: 100%;")
              )
          )
      )
  )
)

#---------------------------------------------------------------------------
# CSV Upload Page UI
#---------------------------------------------------------------------------
csv_upload_ui <- fluidPage(
  custom_styles,

  div(class = "container", style = "max-width: 1200px; margin: 0 auto; padding: 2rem 1rem;",
      # Profile section
      div(class = "profile-section",
          tags$i(class = "fa fa-user-circle", style = "font-size: 24px; color: var(--primary);"),
          span(textOutput("profile_name"), style = "font-weight: 500;"),
          span(style = "flex-grow: 1;"),
          span(textOutput("course_display"), style = "color: var(--secondary); font-weight: 500;")
      ),
      
      # Title
      div(style = "text-align: center; margin-bottom: 3rem;",
          h1("Upload Survey Data", class = "page-title")
      ),
      
      # Main content
      div(class = "card",
          div(style = "text-align: center; margin-bottom: 2rem;",
              tags$i(class = "fa fa-file-csv", style = "font-size: 48px; color: var(--secondary); margin-bottom: 1rem;"),
              h3("Student Survey Data", style = "margin-bottom: 0.5rem;"),
              p("Upload the CSV file containing student preferences", style = "color: #666;")
          ),
          
          # File upload section
          div(style = "text-align: center; margin-bottom: 25px;",
              div(style = "display: inline-block; text-align: left; width: 100%; max-width: 500px;",
                  div(style = "font-weight: 500; margin-bottom: 10px;", "Choose CSV File"),
                  div(style = "border: 1px solid #ddd; border-radius: 4px; overflow: hidden;",
                      div(class = "shiny-input-container", style = "margin-bottom: 0;",
                          fileInput("survey_csv", NULL, 
                                   accept = ".csv",
                                   buttonLabel = tags$span(tags$i(class = "fa fa-upload"), "Browse")
                                   )
                      )
                  )
              )
          ),
          
          # Upload button
          div(style = "display: flex; justify-content: center; margin: 30px 0;",
              actionButton("upload_csv", 
                         tags$span(
                            tags$i(class = "fa fa-cloud-upload-alt", style = "margin-right: 8px;"), 
                            "Upload CSV"
                         ), 
                         class = "btn-custom-primary", 
                         style = "min-width: 180px;")
          ),
          
          hr(),
          
          # Info section
          div(style = "margin-top: 2rem;",
              h4("Required Data Format", style = "color: var(--primary); margin-bottom: 1rem;"),
              p("Please ensure your CSV file contains the following columns:"),
              
              div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent);",
                  tags$ul(
                    tags$li(strong("Student_ID"), " columns for each student in a group"),
                    tags$li(strong("Topic"), " columns for topic preferences"),
                    tags$li(strong("Subteam"), " columns for subteam preferences")
                  )
              ),
              
              # Button group for Back and Generate Allocation
              div(class = "button-group", style = "justify-content: space-between; margin-top: 2rem;",
                  actionButton("back_to_setup", 
                              tags$span(
                                tags$i(class = "fa fa-arrow-left", style = "margin-right: 8px;"), 
                                "Back"
                              ), 
                              class = "btn-custom-secondary"),
                  
                  actionButton("generate_allocation", 
                              tags$span(
                                tags$i(class = "fa fa-cogs", style = "margin-right: 8px;"), 
                                "Generate Allocation"
                              ), 
                              class = "btn-custom-success", 
                              disabled = TRUE)
              )
          )
      )
  )
)

#---------------------------------------------------------------------------
# Results Page UI
#---------------------------------------------------------------------------
result_ui <- fluidPage(
  custom_styles,
  div(class = "container", style = "max-width: 1200px; margin: 0 auto; padding: 2rem 1rem;",
      # Profile section
      div(class = "profile-section",
          tags$i(class = "fa fa-user-circle", style = "font-size: 24px; color: var(--primary);"),
          span(textOutput("profile_name"), style = "font-weight: 500;"),
          span(style = "flex-grow: 1;"),
          span(textOutput("course_display"), style = "color: var(--secondary); font-weight: 500;")
      ),
      
      # Title
      div(style = "text-align: center; margin-bottom: 2rem;",
          h1("Allocation Results", class = "page-title")
      ),
      
      # Parameters section
      div(class = "card",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                h4("Current Parameters", style = "margin: 0; color: var(--primary);"),
                textOutput("optimization_params")
              ),
              actionButton("change_params", 
                          tags$span(
                            tags$i(class = "fa fa-sliders-h", style = "margin-right: 8px;"), 
                            "Change Parameters"
                          ), 
                          class = "btn-custom-info")
          ),
          
          # Collapsible parameter settings panel (initially hidden)
          shinyjs::hidden(
            div(id = "params_panel", style = "margin-top: 20px; padding-top: 20px; border-top: 1px solid #eee;",
                h4("Update Parameters", style = "margin-bottom: 20px; color: var(--primary);"),
                
                div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px;",
                    div(
                      numericInput("new_c_team", 
                                  tags$span(
                                    tags$i(class = "fa fa-users", style = "margin-right: 8px; color: var(--secondary);"), 
                                    "Team Size"
                                  ),
                                  value = 8, min = 1, max = 20)
                    ),
                    div(
                      numericInput("new_b_subteam", 
                                  tags$span(
                                    tags$i(class = "fa fa-user-friends", style = "margin-right: 8px; color: var(--secondary);"), 
                                    "Subteam Size"
                                  ),
                                  value = 4, min = 1, max = 10)
                    ),
                    div(
                      numericInput("new_x_topic_teams", 
                                  tags$span(
                                    tags$i(class = "fa fa-project-diagram", style = "margin-right: 8px; color: var(--secondary);"), 
                                    "Max Teams per Topic"
                                  ),
                                  value = 3, min = 1, max = 10)
                    ),
                ),
                
                div(style = "text-align: right; margin-top: 20px;",
                    actionButton("run_again", 
                                tags$span(
                                  tags$i(class = "fa fa-sync", style = "margin-right: 8px;"), 
                                  "Run with New Parameters"
                                ), 
                                class = "btn-custom-success")
                )
            )
          )
      ),
      
      # Multiple Solutions Navigation Card
      div(class = "card", style = "margin-top: 2rem;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              h3("Alternative Solutions", style = "margin: 0;"),
              textOutput("solution_nav_info")
          ),
          
          # Solution comparison cards
          uiOutput("solution_comparison"),
          
          # Navigation controls
          div(style = "display: flex; justify-content: center; gap: 15px; margin-top: 20px;",
              actionButton("prev_solution", 
                          tags$span(
                            tags$i(class = "fa fa-chevron-left"), 
                            "Previous Solution"
                          ), 
                          class = "btn-custom-info"),
              actionButton("next_solution", 
                          tags$span(
                            "Next Solution",
                            tags$i(class = "fa fa-chevron-right", style = "margin-left: 8px;")
                          ), 
                          class = "btn-custom-info")
          )
      ),
      
      # Results section with current solution
      div(class = "card", style = "margin-top: 2rem;",
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
              h3("Current Solution", style = "margin: 0;"),
              downloadButton("download_csv", "Download as CSV", class = "btn-custom-primary")
          ),
          
          # New section to display preference score and comparison
          div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid var(--success); margin-bottom: 20px;",
              div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
                  div(
                    h4("Optimization Result", style = "margin: 0; color: var(--primary);"),
                    textOutput("preference_score_text", inline = TRUE)
                  ),
                  div(
                    style = "color: var(--success); font-size: 24px;",
                    tags$i(class = "fa fa-chart-line")
                  )
              ),
              # Conditionally show score comparison
              uiOutput("score_comparison_text")
          ),
          
          # Modified table container with editing capability
          div(style = "overflow-x: auto; margin-bottom: 20px;",
              # Add a toggle button for manual edit mode
              div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  actionButton("toggle_edit_mode", 
                              tags$span(
                                tags$i(class = "fa fa-edit", style = "margin-right: 8px;"), 
                                "Toggle Manual Edit Mode"
                              ), 
                              class = "btn-custom-info")
              ),
              # Conditional UI display based on edit mode
              conditionalPanel(
                condition = "input.toggle_edit_mode % 2 == 1", # Odd clicks = edit mode
                div(style = "min-width: 80%; max-width: 100%; display: flex; justify-content: center;",
                    DT::dataTableOutput("editable_allocation_table")
                ),
                div(style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 8px; border-left: 4px solid var(--warning);",
                    div(style = "display: flex; align-items: center;",
                        tags$i(class = "fa fa-info-circle", style = "color: var(--warning); font-size: 20px; margin-right: 10px;"),
                        div(
                          h5("Manual Edit Mode", style = "margin: 0; color: var(--dark);"),
                          p("You can manually edit student assignments by selecting a new project team or subteam from the dropdown.", 
                            style = "margin: 0; color: var(--dark);")
                        )
                    ),
                    div(style = "margin-top: 10px;",
                        textOutput("manual_edit_score_impact")
                    )
                )
              )
          ),
          
          div(style = "text-align: center;",
              actionButton("back_to_upload", 
                          tags$span(
                            tags$i(class = "fa fa-arrow-left", style = "margin-right: 8px;"), 
                            "Back to Upload"
                          ), 
                          class = "btn-custom-secondary")
          )
      )
  )
)

#===========================================================================
# 3. MAIN UI ASSEMBLY
#===========================================================================

#---------------------------------------------------------------------------
# Define the dynamic UI output
#---------------------------------------------------------------------------
ui <- fluidPage(
  # Include required libraries
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('jsCode', function(code) {
        eval(code);
      });
      $(document).ready(function(){
        $(document).on('click', '#profile_pic', function(event){
          event.stopPropagation();
          $('#sidebar').toggle();
        });
        $(document).on('click', function(event){
          if (!$(event.target).closest('#sidebar').length && !$(event.target).is('#profile_pic')) {
            $('#sidebar').hide();
          }
        });
      });
    "))
  ),
  uiOutput("main_ui")
)

