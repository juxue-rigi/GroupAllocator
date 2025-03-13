# ------------------------------------------------------------------------------
# Define UI for Login Page
# ------------------------------------------------------------------------------
login_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center-container { 
        display: flex; 
        flex-direction: column; 
        justify-content: center; 
        align-items: center; 
        height: 100vh; 
        width: 100%; 
      }
      .form-container {
        max-width: 1000px; 
        width: 100%; 
        text-align: center;
      }
    "))
  ),
  div(class = "center-container",
      titlePanel("Login Page"),
      div(class = "form-container",
          wellPanel(
            textInput("username", "Enter your username:"),
            textInput("course", "Enter your course name:"),
            actionButton("go", "Go to Project Set-up", class = "btn-primary")
          )
      )
  )
)

# ------------------------------------------------------------------------------
# Define UI for Project Setup Page
# ------------------------------------------------------------------------------
project_setup_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .project-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        min-height: 100vh;
        width: 100%;
        max-width: 1400px;
        margin: 0 auto;
        padding: 20px;
        box-sizing: border-box;
      }
      .profile-section {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        gap: 10px;
        width: 100%;
        margin-bottom: 20px;
      }
      .main-content {
        width: 100%;
      }
      .button-group {
        margin-top: 20px;
      }
    "))
  ),
  div(class = "project-container",
      titlePanel(textOutput("welcome_message")),
      
      # Profile section - use a default icon instead of relying on profile.png
      div(class = "profile-section",
          tags$i(class = "fa fa-user-circle", style = "font-size: 30px; cursor: pointer;", id = "profile_pic"),
          textOutput("profile_name")
      ),
      
      # Main content: centered column for numeric inputs and buttons
      div(class = "main-content",
          fluidRow(
            column(width = 4, offset = 4,
                   numericInput("c_team", "What is the size of each topic group?",
                                value = 8, min = 1, max = 20),
                   numericInput("b_subteam", "What is the size of the sub-team under each topic groups?",
                                value = 4, min = 1, max = 10),
                   numericInput("x_topic_teams", "For each topic, set a threshold for the number of teams under each topic:",
                                value = 3, min = 1, max = 10),
                   div(class = "button-group",
                       actionButton("go_survey", "Open Microsoft Form Template", class = "btn-primary"),
                       br(), br(),
                       actionButton("next_step", "Next Step", class = "btn-primary"),
                       br(), br(),
                       actionButton("back_to_login", "Back", class = "btn-secondary")
                   )
            )
          )
      )
  )
)

# ------------------------------------------------------------------------------
# Define UI for CSV Upload Page (Student Survey Data)
# ------------------------------------------------------------------------------
csv_upload_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .upload-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        padding: 20px;
      }
      .instruction-section {
        max-width: 800px;
        margin-bottom: 20px;
      }
      .button-group {
        margin-top: 20px;
      }
    "))
  ),
  titlePanel("Upload Student Survey Data"),
  div(class = "upload-container",
      div(class = "instruction-section",
          h3("Instructions"),
          p("Please upload the CSV file containing the student survey responses. The file should include the required columns:"),
          tags$ul(
            tags$li("Student_ID columns for each student in a group"),
            tags$li("Topic columns for topic preferences"),
            tags$li("Subteam columns for subteam preferences")
          ),
          p("Ensure that the CSV is formatted correctly. Once uploaded, the data will be processed and converted into the inputs needed for the optimization model.")
      ),
      fileInput("survey_csv", "Choose CSV File", accept = ".csv"),
      actionButton("upload_csv", "Upload CSV", class = "btn-primary"),
      div(class = "button-group",
          br(),
          actionButton("generate_allocation", "Generate Allocation", class = "btn-success", disabled = TRUE),
          br(), br(),
          actionButton("back_to_setup", "Back", class = "btn-secondary")
      )
  )
)

# ------------------------------------------------------------------------------
# Define UI for the Result Page
# ------------------------------------------------------------------------------

result_ui <- fluidPage(
  titlePanel("Allocation Results"),
  fluidRow(
    column(width = 12,
           # Display current optimization parameters
           wellPanel(
             textOutput("optimization_params"),
             actionButton("change_params", "Change Parameter Settings", class = "btn-info")
           ),
           
           # Collapsible parameter settings panel (initially hidden)
           shinyjs::hidden(
             div(id = "params_panel", class = "well",
                 h4("Update Parameters"),
                 fluidRow(
                   column(width = 4, 
                          numericInput("new_c_team", "Team Size:", 
                                      value = 8, min = 1, max = 20)
                   ),
                   column(width = 4, 
                          numericInput("new_b_subteam", "Subteam Size:", 
                                      value = 4, min = 1, max = 10)
                   ),
                   column(width = 4, 
                          numericInput("new_x_topic_teams", "Max Teams per Topic:", 
                                      value = 3, min = 1, max = 10)
                   )
                 ),
                 actionButton("run_again", "Run Model with New Parameters", class = "btn-success")
             )
           ),
           
           # Results section
           h3("Student Assignments"),
           p("The table below shows the final assignments of students to project teams and subteams."),
           div(style = "overflow-x: auto;", # Make table scrollable horizontally
               tableOutput("allocation_table")
           ),
           br(),
           downloadButton("download_csv", "Download Assignments as CSV"),
           br(), br(),
           actionButton("back_to_upload", "Back", class = "btn-secondary")
    )
  )
)

# ------------------------------------------------------------------------------
# Define the dynamic UI output
# ------------------------------------------------------------------------------
ui <- fluidPage(
  # Include required libraries
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
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