# A util shiny model
# adapted from https://mastering-shiny.org/scaling-modules.html #  19.4.3 Dynamic UI
library(shinycssloaders)
library(shinyalert)
library(platetools)
library(varhandle) # has check.numeric, used in the plate plots
library(tidyverse)
library(shiny)
source("modules.R")

layoutApp <- function() {
    ui <- fluidPage(useShinyalert(),
                    sidebarLayout(
                        sidebarPanel(
                            #### upload layout ####
                            uploadLayoutUI("data")[[1]], # upload panel,
                            
                            #### filter layout ####
                            uiOutput("update_button"),
                            textOutput("n"), # number of remaining conditions
                            
                            ## put input filters in a well panel
                            wellPanel(style = "border:grey; overflow-y:scroll; max-height: 600px",
                                      filterUi("filter") # dynamic selectInputs
                                      ) 

                        ),

                        mainPanel(
                            #### display filtered layout ####
                            selectLayoutColorUI("color_var")[[1]], # select a color_by variable 
                            selectLayoutColorUI("color_var")[[2]] # plate plot

                        )
                    )
    )

    server <- function(input, output, session) {
        #### upload layout ####
        layout_raw <- uploadLayoutServer("data") # upload the data
        layout <- reactive(layout_raw())
        
        #### filter layout ####
        
        # observer to create and update the selectInputs
        observe({
            # update options when
            input$reset_layout # user clicks reset button
            layout <- reactive(layout_raw()) # a (new) layout is uploaded
            
            # filtering
            filter <- filterServer_select("filter", layout)
            layout_filtered <- reactive(layout()[filter(), , drop = FALSE])
            output$table <- renderTable(layout_filtered())
            output$n <- renderText(paste0(sum(filter()), " conditions"))
            layout_plot <- selectLayoutColorServer("color_var",
                                                   data = layout_filtered %>% debounce(1000))
            
            #### layout_filtered() is likely the main downstream-relevant output here
        })

        # button to reset the selections
        output$update_button <- renderUI({ # for layout
            req(layout())
            actionButton("reset_layout",
                         button_text("Reset"),
                         width = '100%',
                         style="font-size: 14px; color: #00000; background-color: lightgrey; border-color: lightgrey")

        })

    }

    shinyApp(ui, server)
}

layoutApp()

