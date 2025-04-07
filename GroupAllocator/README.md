GroupAllocator
├── .github/
│   └── ...
├── data/
│   └── ...
├── docker/
│   └── ...
├── inst/
│   ├── extdata/
│   │   └── ... (additional CSV or data files, if any)
│   └── shiny_app/
│       ├── RData/           # Potentially cached or example data
│       ├── .Rhistory
│       ├── app.R            # Main Shiny app file
│       ├── install_packages.R
│       ├── server.R         # Shiny server logic
│       ├── survey_data.csv  # Example input data for the Shiny app
│       ├── ui.R             # Shiny UI definitions
│       └── user_inputs.csv  # Example user inputs or parameter file
├── R/
│   ├── import_data.R        # Functions to read/process survey data
│   ├── optimization_model.R  # Core optimization routines
│   ├── api.R                # (Optional) Additional API functions
│   ├── export_results.R     # Scripts for exporting solver results
│   ├── ...                  # Additional R scripts (subteam, diversity, etc.)
├── vignettes/
│   ├── model_subteam_skills.Rmd   # RMarkdown describing subteam + skills model
│   ├── model_test.Rmd             # Testing approach or example usage
│   ├── model_v1.Rmd               # Documentation for Model v1
│   ├── model_v2.Rmd               # Documentation for Model v2
│   └── structure_tracker.Rmd      # Additional model or data structure explanations
├── .gitignore
├── DESCRIPTION
├── NAMESPACE
├── README.md            # This file
├── final_output.csv     # Example final assignment output
├── config.yml           # Configuration file for certain environment parameters
├── GroupAllocator.Rproj # RStudio project file
└── ...
