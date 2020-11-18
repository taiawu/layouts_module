# 2_add_layouts/app.R
library(shinyBS) # drop-down panels
library(tidyverse) #  handling data structures and plotting
library(shinyalert) # pop-up error messages
library(shinycssloaders) # spinning plot loading icon
library(rhandsontable) # user-interactive tables 
library(shiny) # for shiny web-apps 

#  df_sample <- read.csv("sample_data_file.csv")

rhandsonUI <- function(id) {
    tagList(
        rHandsontableOutput(NS(id, "r_table")),
        actionButton(NS(id,"submit_handson_names"), p("Update names from manual table (above)", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),  width = '100%')
    )
}

rhandsonServer <- function(id, layout_raw, hide_cols = c("row", "column", "condition")) {
    values <- reactiveValues()
    values$df <- isolate(layout_raw())
    
    moduleServer(id, function(input, output, session) {
        output$r_table <- renderRHandsontable({
            req(layout_raw)
            rhandsontable(layout_raw() %>% 
                              select(-hide_cols), 
                          height = 200, useTypes = TRUE, stretch = "all") %>%
                hot_col(c("well"), readOnly = TRUE)
        })
        
        observeEvent(input$submit_handson_names, { # when r_table is updated
            req(input$r_table)
            
            values$df <- hot_to_r(input$r_table) %>% # update the layout
                as_tibble() %>%
                mutate(row = str_remove(well,  "[0-9]+"),
                       column = str_match_all(well, "[0-9]+")) %>%
                unite(condition, -c(row, column, well), sep = "__", remove = FALSE)
            
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
        
        reactive(values$df)
    }) 
}

# example implementation
ui <- navbarPage(useShinyalert(),
                 sidebarLayout( # Sidebar layout with input and output definitions
                     sidebarPanel( 
                      
                     ),
                     mainPanel( 
                         rhandsonUI("edit_layout"),
                         tableOutput("updated_table")
                     ) # end main panel
                 )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    #values <- reactiveValues()
    layout_raw <- reactive(readRDS("sample_layout_file.rds") %>% head())
    layout_update <- rhandsonServer("edit_layout", layout_raw, hide_cols = c("row", "column", "condition"))
    output$updated_table <- renderTable(layout_update())
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
