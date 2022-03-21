# =============================================================================
# Author: CWM
# Purpose: Shiny version vizualizer
# =============================================================================
library(shiny)
library(tidyverse)
library(shinyvalidate)
library(shinyjs)
library(bslib)
library(jsonlite)
library(waiter)
library(jsonlite)
library(DT)
library(spsComps)

# =============================================================================
ui <- fluidPage(
  
  # theme
  theme = bs_theme(bootswatch = "flatly"),
  
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
            value = "K:\\CONTAM_database"
  ),
  
  helpText("By default, json objs are looked for in the `01_torun` folder"),
  
  selectInput("json_obj", selectize = T,
              "Available run.JSON objects:", 
              choices = c()),
  hr(),
  
  # the payload
  tabsetPanel(
    type = "tabs",
    
    # Select via the data table
    tabPanel(
      "Select files", 
      DT::dataTableOutput('rj_table')
    ),
    
    # Show box-plots arranged in facet grid
    tabPanel(
      "Boxplot",
      helpText("Shows daily averages"),
      plotOutput("boxplot")
    ),
    
    # Show hourlys
    tabPanel(
      "Diurnal",
      helpText("Shows diurnal patterns"),
      plotOutput("diurnal")
    ),
    
    # Show summary stats
    tabPanel(
      "Summary stats",
      helpText("Shows summary stats -- takes a minute to load"),
      DT::dataTableOutput('summary_stats')
    ),
    
    # Show summary stats
    tabPanel(
      "Aggregate",
      helpText("Table and functions to aggregate various RDS files into one (daily etc)")
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
  
  # waiting buttons
  w_box <- Waiter$new(id = "boxplot")
  w_diurnal <- Waiter$new(id = "diurnal")
  w_stats <- Waiter$new(id = "summary_stats")
  
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
  observe({
    
    updateSelectInput(session, 
                      "json_obj", 
                      choices = list.files(
                        path = file.path(input$out_dir, "01_torun"),
                        pattern = "_run.JSON"))
  })
  
  # --------------------------------------
  # f_name data_frame
  all_opts <- reactive({
  
    # require validation before conintuing
    req(iv$is_valid())
  
    shinyCatch({
      # new json object? kick it off
      out_dir <- input$out_dir
      rj <- input$json_obj
      if(rj == "") return()
      
      # 1. load rj
      files_to_run <- read_json(file.path(out_dir, "01_torun", rj))
      for(i in seq(files_to_run)) {
        files_to_run[[i]]$f_name = names(files_to_run)[[i]]
      }
      
      # 2. iterate through, and make the table, include a unique identifier, 
      #    or just hide the column that is the super long row name
      #
      #    This probably involves an Rbind of a `long` format of the `opts` object, 
      #    followed by a pivot_wider to make the table, and fill with "NA" or similar
      #    so that different levels of `opts` are allowed
      #
      do.call(rbind, lapply(files_to_run, function(x) {
        x_df <- data.frame(x) %>% 
          select(starts_with("opts.")) %>%
          pivot_longer(cols = everything(), names_to = "opt")
        x_df$name <- x$f_name
        x_df$opt <- gsub("opts.", "", x_df$opt, fixed = T, perl = F)
        x_df
      })) %>% pivot_wider(id_cols = name, names_from = opt)
    })
  })
  
  # render the f_name table
  output$rj_table <- DT::renderDataTable(
    all_opts(), 
    options = list(scrollX = T,
                   columnDefs = list(list(visible = F, targets = 1)),
                   pageLength = 10)
  )
  
  # ------------------------------------
  # all db
  # load in based on
  contam_data <- reactive({
    shinyCatch({
      df <- all_opts()
      
      timestamp(suffix = "> get data")
      
      s <- input$rj_table_rows_selected
      
      xx <- lapply(s, function(i) {
        
        x <- df$name[i]
        
        x2 <- readRDS(file.path(input$out_dir, "02_outputs", x, 
                                paste0(x, ".RDS")))
        x2$sim_name <- x
        x2
      })
      
      do.call(rbind, xx)
    })
  })
  
  # --------------------------------------
  # for the boxplot
  output$boxplot <- renderPlot({
    
    shinyCatch({
      
      # show the spinner
      w_box$show()

      timestamp(suffix = "> make boxplot")
      
        # finalize
      contam_data_x <- contam_data()
      
      # hide the spinner
      w_box$hide()
      
      # the plot
      contam_data_x %>%
        group_by(Date, Season, ctm, unit, generic_name, name, sim_name) %>%
        summarize(mean_conc = mean(value_converted),
                  .groups = "keep") %>%
        ggplot(.) +
        geom_boxplot(aes(x = unit, 
                         y = mean_conc, 
                         fill = sim_name), outlier.size = 0.25) +
        theme_bw() +
        #facet_grid(. ~ unit) +
        theme(legend.position = "bottom",
              legend.direction = "vertical")
    })
  })
  
  # --------------------------------------
  # for the diurnal
  output$diurnal <- renderPlot({
    
    shinyCatch({
    
      # show the spinner
      w_diurnal$show()
      
      timestamp(suffix = "> make diurnal")
      
      # finalize
      contam_data_x <- contam_data()
      
      # hide the spinner
      w_diurnal$hide()
      
      # the plot
      contam_data_x %>%
        group_by(Hour, ctm, unit, generic_name, name, sim_name) %>%
        summarize(mean_conc = mean(value_converted),
                  q25 = quantile(value_converted, probs = .25),
                  q75 = quantile(value_converted, probs = .75),
                  .groups = "keep") %>%
        ggplot(., aes(x = Hour, y = mean_conc, color = sim_name, fill = sim_name,
                      ymin = q25, ymax = q75)) +
        geom_ribbon(color = NA, alpha = 0.2) +
        geom_line() +
        geom_point() +
        theme_bw() +
        facet_grid(. ~ unit) +
        theme(legend.position = "bottom",
              legend.direction = "vertical")
    
    })
    
  })
  
  # --------------------------------------
  # for the summary stats
  # render the f_name table
  contam_summary_table <- reactive({
    
    # show the spinner
    w_stats$show()
    
    timestamp(suffix = "> get summary stats")
    
    # get filtered table 1
    df <- all_opts()
    s <- input$rj_table_rows_selected
    df_sub <- df[s, ]
    
    # get data
    contam_data_x <- contam_data()

    sum1 <- contam_data_x %>%
      group_by(sim_name, name) %>%
      summarize(
        .groups = 'keep',
        mean_conc = signif(mean(value_converted), 2),
        sd_conc = signif(sd(value_converted), 2)
      ) %>%
      rename(zone_name = name)
    
    # hide spinner
    w_stats$hide()
    
    # left_join with df_sum
    merge(df_sub, sum1, by.x = "name", by.y = "sim_name")
    
  })
  
  output$summary_stats <- DT::renderDataTable(
    contam_summary_table(), 
    options = list(scrollX = T,
                   columnDefs = list(list(visible = F, targets = 1)),
                   pageLength = 10)
  )
  
}
# =============================================================================
shinyApp(ui, server)