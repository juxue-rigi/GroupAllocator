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
      
      # Profile section
      div(class = "profile-section",
          tags$img(src = "profile.png", height = "40px", width = "40px",
                   style = "border-radius: 50%; cursor: pointer;", id = "profile_pic"),
          textOutput("profile_name")
      ),
      
      # Main content: centered column for numeric inputs and buttons
      div(class = "main-content",
          fluidRow(
            column(width = 4, offset = 4,
                   numericInput("c_team", "What is the size of each topic group?",
                                value = 8, min = 1, max = 5),
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
          p("Please upload the CSV file containing the student survey responses. The file should include the required columns (e.g., student ID, group assignment, preferences, etc.). Ensure that the CSV is formatted correctly. Once uploaded, the data will be processed and converted into the inputs needed for the optimization model.")
      ),
      fileInput("survey_csv", "Choose CSV File", accept = ".csv"),
      actionButton("upload_csv", "Upload CSV", class = "btn-primary"),
      div(class = "button-group",
          br(),
          actionButton("generate_allocation", "Generate Allocation", class = "btn-success"),
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
      tableOutput("allocation_table")
    )
  ),
  br(),
  actionButton("back_to_upload", "Back", class = "btn-secondary")
)

# ------------------------------------------------------------------------------
# Define the dynamic UI output
# ------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
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
