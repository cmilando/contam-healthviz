# =============================================================================
# Author: CWM
# Purpose: Shiny version for runtime tool
# =============================================================================
library(shiny)
library(shinyvalidate)
library(shinyjs)
library(shinythemes)
library(jsonlite)
library(waiter)
library(jsonlite)

# =============================================================================
ui <- fluidPage(
  
  # theme
  theme = shinytheme("flatly"),
  
  # multi-column checkbox html
  tags$head(
    tags$style(HTML("

     .multicol {
       -webkit-column-count: 3; /* Chrome, Safari, Opera */
       -moz-column-count: 3; /* Firefox */
       column-count: 3;
     }

   "))
  ),
  
  # waiter
  use_waiter(),
  
  # shiny js
  shinyjs::useShinyjs(),
  
  # max width
  style = "max-width: 700px;",
  
  # title
  h2("CONTAM health vizualizer tool", align = "center"),
  helpText(
    "This facilitates vizualizing post-processed CONTAM output.",
    "Windows only.", "See code and objects at",
    a("Github", href = "https://github.com/cmilando/contam-healthviz")
  ),
  br(),
  
  # file upload and output
  textInput("out_dir",
            "File path for outputs (e.g., C:\\tmp\\contam)",
            width = "100%",
            value = "D:\\BU_backup\\contam_test\\ASTHMA_database\\0_final"
  ),
  
  helpText("By default, json objs are looked for in the `01_torun` folder"),
  
  selectInput("json_obj", selectize = T,
              "Available run.JSON objects:", 
              choices = c()),
  
  # Run files
  actionButton("run_JSON", "Run prjs",
               style = paste(
                 "color: #fff;",
                 "background-color: #337ab7;",
                 "border-color: #2e6da4"
               )
  ),
  
  # copyright
  hr(),
  tags$p(
    HTML("&copy; 2021 Chad W. Milando")
  )
)

# =============================================================================
server <- function(input, output, session) {
  
  # waiting button
  w <- Waiter$new(id = "run_JSON")
  
  
  # ---------------------------------------
  # validators
  iv <- InputValidator$new()
  
  # valid out_dir
  iv$add_rule(
    "out_dir",
    sv_required(
      message = "Valid directory required",
      test = function(val) {
        val != "" & dir.exists(val)
      }
    )
  )
  
  #
  iv$enable()
  
  # --------------------------------------
  # make a new JSON if a file is uploaded
  # >> Update this each time you add a new tabset
  observe({
    
    updateSelectInput(session, 
                      "json_obj", 
                      choices = list.files(
                        path = file.path(input$out_dir, "01_torun"),
                        pattern = "_run.JSON"))
  })
  
  # --------------------------------------
  # What happens when you want to create new PRJs
  observeEvent(input$run_JSON, {
    # show the spinner
    w$show() 
    
    
    # hide the spinner
    w$hide()
  })
}
# =============================================================================
shinyApp(ui, server)