library(varhandle) # has check.numeric function
library(shinyalert) # pop-up error messages
library(platetools) # plate-view plot tools, only lightly used
library(tidyverse)
library(shiny)

get_simplest_var <-  function(layout) {
    layout %>%
        select(-c(row, column, well, condition))  %>%
        mutate_all(as.character) %>%
        pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
        group_by(variable) %>%
        summarise(count = n_distinct(value)) %>%
        ungroup() %>%
        filter(count == min(count)) %>%
        pull(variable) %>%
        unique() %>%
        as.character()
}

make_platemap_plot <- function( data, plot_title = "", fill_var, alpha_var = NULL ) {
    data <- data %>%
        mutate("-" = rep("", nrow(.)))

    fill_var <- enquo(fill_var)
    alpha_var <- enquo(alpha_var)


    p <- platetools::plate_map(data = data$well, well = data$well ) %>%
        mutate(well_f = well,
               well = as.character(well)) %>%
        select(-values) %>%

        left_join(data, by = "well") %>%
        mutate(well = well_f) %>%
        filter(!!fill_var != "Empty") %>%


        ggplot( . , aes_string(x = "Column", y = "Row")) +
        geom_point(data = expand.grid(seq(1, 24), seq(1, 16)),
                   aes_string(x = "Var1", y = "Var2"),
                   color = "grey90", fill = "white", shape = 22, size = 5-2, alpha = 0.1) +
        coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5)) +
        scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
        scale_x_continuous(position = "top", breaks = seq(1, 24)) +
        xlab("") +
        ylab("") +
        theme_dark() +

        # geom_point(aes(fill = !!fill_var, alpha = !!alpha_var), colour = "gray20", shape = 22, size = 5)
        geom_point(aes(fill = !!fill_var), colour = "gray20", shape = 22, size = 5) +
        labs(title = plot_title)

    if(all(check.numeric(data %>% select(!!fill_var) %>% as_vector()))){
        p <- p +
            scale_fill_viridis_c(begin = 0.8, end = 0)
    } else {

        p <- p +
            scale_fill_viridis_d(begin = 0.8, end = 0)
    }

    p

}

upload_layout <- function(filepath){
    # read file based on it's type
    ext <- tools::file_ext(filepath)
    raw <- switch(ext,
                  csv = read_csv(filepath, col_names = FALSE),
                  txt = read_tsv(filepath, col_names = FALSE),
                  xlsx = readxl::read_excel(filepath, col_names = FALSE),
                  xls = readxl::read_excel(filepath, col_names = FALSE),
                  validate("Invalid file type"))

    # handle files with or without the "Type" header
    first_cell <- raw[1,1][[1]]
    out <- switch(first_cell,
                  Type = raw[-1,],
                  raw)

    # convert into layout form
    out %>%
        set_names( c("variable", "row", .[1,][-c(1,2)])) %>%
        filter(row %in% LETTERS[1:16]) %>%
        discard(~all(is.na(.x)))  %>% # drop columns if everything is NA
        filter_all(any_vars(!is.na(.))) %>% # drop empty rows
        mutate_all(as.character) %>% # make all character, to prevent issues in pivot
        pivot_longer(-c(variable, row), names_to = "column", values_to = "value") %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        mutate(well = paste0(row, column)) %>% # make well column
        unite(condition, -c(row, column, well), sep = "__", remove = FALSE) %>% # make condition column
        filter(!across(-c(well, row, column, condition)) == "Empty") %>% # if all variables are "Empty",   wells
        filter(!is.na(across(-c(well, row, column, condition))) == TRUE) %>% # if all variables are NA,   wells
        mutate_all(parse_guess) # convert likely numeric variables to numeric
}

layoutUI <- function(id) {
    tagList(
        
        fileInput( NS(id, "file"), 
                   label = "",
                   placeholder = "Upload raw RFU data" ,
                   multiple = FALSE,
                   accept = c(".csv", ".tsv", ".xls", ".xlsx")),
        
        uiOutput(NS(id, "color_by")),
        
        plotOutput(NS(id, "plot")),
        tableOutput(NS(id,"layout_table"))
    )
}

layoutServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        layout_raw <- reactive({
            req(input$file)
            tryCatch({ upload_layout(input$file$datapath)
            }, error  = function(e) {
                shinyalert("Layout can't be read", "Please check layout format.")
            })
        })
        
        output$layout_table <- renderTable({ layout_raw()  })
        
        # layout plots
        output$color_by <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
            req(layout_raw())
            
            varSelectInput("color_by", label = "Color plate plot by",  # as select input
                           data = layout_raw() %>% select(-c(row, column, well, condition)),
                           selected = "conc") #get_simplest_var(layout_raw()))
            
        }) 
        
        output$plot <- renderPlot({
            req(layout_raw())
           # req(input$color_by)  # wait until input$color_by is created
            
            make_platemap_plot( data = layout_raw(),
                                fill_var = conc, #!!input$color_by,
                                plot_title = "Plate Map",
                                alpha_var = NULL)
        })
        
        #reactive(layout_raw()) # this the assignable output from this module
    })
}


# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
#     layoutUI("layout1")
#     # Sidebar with a slider input for number of bins
#     # sidebarLayout(
#     #     sidebarPanel(
#     #         layoutUI("layout1")[[1]]
#     #     ),
#     # 
#     #     # Show a plot of the generated distribution
#     #     mainPanel(
#     #         layoutUI("layout1")[[2]],
#     #         layoutUI("layout1")[3],
#     #         layoutUI("layout1")[[4]]
#     #     )
#     # )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#   layout_raw <-  layoutServer("layout1")
# 
# }
# 
# shinyApp(ui = ui, server = server)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput( "file", label = "",
                      placeholder = "Upload raw RFU data" ,
                      multiple = FALSE,
                      accept = c(".csv", ".tsv", ".xls", ".xlsx")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("color_by"),
            plotOutput("plot"),
            tableOutput("layout_table")
        )
    )
)

# stopifnot(is.reactive(data))
# stopifnot(!is.reactive(filter))

# Define server logic required to draw a histogram
server <- function(input, output) {

    layout_raw <- reactive({
        req(input$file)
        tryCatch({ upload_layout(input$file$datapath)
        }, error  = function(e) {
            shinyalert("Layout can't be read", "Please check layout format.")
        })
    })

    output$layout_table <- renderTable({ layout_raw()  })

    # layout plots
    output$color_by <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
        req(layout_raw())

        varSelectInput("color_by", label = "Color plate plot by",  # as select input
                       data = layout_raw() %>% select(-c(row, column, well, condition)),
                       selected = get_simplest_var(layout_raw()))

    })

    output$plot <- renderPlot({
            req(layout_raw())
            req(input$color_by)  # wait until input$color_by is created

            make_platemap_plot( data = layout_raw(),
                                         fill_var = !!input$color_by,
                                         plot_title = "Plate Map",
                                         alpha_var = NULL)
    })

}

# Run the application
shinyApp(ui = ui, server = server)


# histogramServer <- function(id) {
#     moduleServer(id, function(input, output, session) {
#         layout_raw <- reactive({
#             req(input$file)
#             tryCatch({ upload_layout(input$file$datapath)
#             }, error  = function(e) {
#                 shinyalert("Layout can't be read", "Please check layout format.")
#             })
#         })
#         
#         output$layout_table <- renderTable({ layout_raw()  })
#         
#         # layout plots
#         output$color_by <- renderUI({ # this is reactive by nature of being a render call? it can accept, therefore, rt(), which is a reactive expression. Can we
#             req(layout_raw())
#             
#             varSelectInput("color_by", label = "Color plate plot by",  # as select input
#                            data = layout_raw() %>% select(-c(row, column, well, condition)),
#                            selected = "conc") #get_simplest_var(layout_raw()))
#             
#         }) 
#         
#         output$plot <- renderPlot({
#             req(layout_raw())
#             # req(input$color_by)  # wait until input$color_by is created
#             
#             make_platemap_plot( data = layout_raw(),
#                                 fill_var = conc, #!!input$color_by,
#                                 plot_title = "Plate Map",
#                                 alpha_var = NULL)
#         })
#     })
# }

