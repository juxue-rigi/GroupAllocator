library(shiny)

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
    titlePanel("Login Page"),  # Ensure Title is centered
    div(class = "form-container",
      wellPanel(  # Adds padding and a slight border
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
        min-height: 100vh;  /* Changed to min-height */
        width: 100%; 
      }
      .sidebar {
        display: none;
        position: fixed;
        top: 0;
        right: 0;
        width: 300px;
        height: 100%;
        background: white;
        border-left: 1px solid #ddd;
        padding: 20px;
        box-shadow: -2px 0 5px rgba(0,0,0,0.2);
        z-index: 100;
      }
      .profile-section {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        gap: 10px;
        padding: 10px;
        width: 100%;  /* Changed from 90% */
        max-width: 1400px;
      }
      .wide-input {
        width: 100%;
      }
    "))
  ),
  div(class = "project-container",
    titlePanel(textOutput("welcome_message")),
    div(class = "profile-section",
      tags$img(src = "profile.png", height = "40px", width = "40px",
               style = "border-radius: 50%; cursor: pointer;",
               id = "profile_pic"),
      textOutput("profile_name")
    ),
    div(id = "sidebar", class = "sidebar",
      actionButton("logout",
                   "Log Out",
                   style = "width: 100%; background-color: red; color: white;")
    ),
    div(style = "
        flex-grow: 1; 
        display: flex; 
        justify-content: center; 
        align-items: center; 
        width: 100%;
        ",
      div(style = "width: 100%; max-width: 1400px; margin: auto;",
        sidebarLayout(
          sidebarPanel(width = 5,
            textInput("project_name", "Enter project name:"),
            actionButton("add_project", "Add Project"),
            uiOutput("project_list"),
            numericInput("sub_group_size", "Max students per self-formed group:", value = 3, min = 1, max = 5), # nolint: line_length_linter.
            numericInput("project_group_size", "Total project group size:", value = 4, min = 1, max = 10), # nolint: line_length_linter.
            textInput("subgroup_name", "Enter sub-group function:"),
            actionButton("add_subgroup", "Add Sub-group"),
            uiOutput("subgroup_list"),
            textInput("skill_name", "Enter skill:"),
            actionButton("add_skill", "Add Skill"),
            uiOutput("skill_list"),
            actionButton("generate_survey", "Generate Student Survey", class = "btn-primary") # nolint: line_length_linter.
          ),
          mainPanel(width = 7,
            div(style = "text-align: center;",
              textOutput("confirmation")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Define the dynamic UI output
# ------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$script(HTML("$(document).ready(function(){
      $(document).on('click', '#profile_pic', function(event){
        event.stopPropagation();
        $('#sidebar').toggle();
      });
      $(document).on('click', function(event){
        if (!$(event.target).closest('#sidebar').length && !$(event.target).is('#profile_pic')) { # nolint: line_length_linter.
          $('#sidebar').hide();
        }
      });
    });"))
  ),
  uiOutput("main_ui")
)

# ------------------------------------------------------------------------------
# Survey Interface UI
# ------------------------------------------------------------------------------
survey_ui <- fluidPage(
  titlePanel("Student Survey"),
  fluidRow(
    column(4,
      h3("Student Information"),
      uiOutput("student_inputs")  # name & ID fields according to sub-group size
    ),
    column(8,
      h3("Rank your project and sub-group preference (1st is most preferred)"),
      uiOutput("project_ranking"),
      uiOutput("subgroup_selection"),
      h3("Rate your skills (1 = Beginner, 5 = Expert)"),
      uiOutput("skill_ratings"),
      actionButton("submit_survey", "Submit Survey", class = "btn-primary"),
      textOutput("survey_confirmation")
    )
  )
)
