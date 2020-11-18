
#### --- make text styles ---- ####
text_style <- function( font = "'Avenir Next'", 
                        size = 14,
                        color = "'black'",
                        bold = TRUE,
                        align_default = "center") {
  
  style <- str_c("font-family:", font,
                 "; color:",  color,
                 "; align: ", align_default,
                 "; font-size:", size, "px")
  
  # create a p() function
  function(message_text, align = align_default) {
    message <-  p(message_text,
                  style = style,
                  align = align)
    
    if (bold == TRUE) {
      message %>% strong()
    } else {
      message
    }
  }
}

header_text <- text_style(bold = TRUE, align = "center", size = 14) # text used for headers
panel_text <- text_style(bold = FALSE, align = "center", size = 14) # text used for headers
small_text <- text_style(bold = FALSE, align = "center", size = 10) # small text used for explanations
button_text <- text_style(bold = TRUE, align = "center", size = 14)
# example:
# replaces: p("header message", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
# with header_text("header message")
# option to overwrite default alignment: header_text("header message", align = "left")

upload_layout <- function(filepath){
  # read file based on it's type
  ext <- tools::file_ext(filepath)
  raw <- switch(ext,
                csv = read_csv(filepath, col_names = FALSE),
                txt = read_tsv(filepath, col_names = FALSE),
                xlsx = readxl::read_excel(filepath, col_names = FALSE),
                xls = readxl::read_excel(filepath, col_names = FALSE)
                # ,
                # validate("Invalid file type")
  )
  
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

get_var_order <-  function(layout) {
  layout %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(count = n_distinct(value), .groups = 'drop') %>% # .groups is experimental, so keep an eye on this line as dplyr advances see: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
    arrange(count) %>% 
    pull(variable) %>%
    unique() %>%
    as.character()
}

sort_layout_vars <- function( vars ) {
  plate_vars <- c("condition", "well", "row", "column")
  end_vars <- plate_vars[plate_vars %in% vars] # these should always be here, but just in case 
  user_vars <- vars[!vars %in% end_vars ]
  c(user_vars, end_vars)
} 

make_platemap_plot <- function( data,  plot_title, fill_var ) {
  
  fill_var <- enquo(fill_var)
  
  df <-  platetools::plate_map(data = data$well, well = data$well ) %>%
    mutate(well_f = well,
           well = as.character(well)) %>%
    select(-values) %>%
    left_join(data, by = "well") %>%
    mutate(well = well_f) %>%
    filter(!!fill_var != "Empty") %>%
    select(well, Column, Row, !!fill_var) %>%
    set_names(c("well", "Column", "Row", "var"))
  
  
  
  p <- df %>%
    ggplot( . , aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, 24), seq(1, 16)),
               aes_string(x = "Var1", y = "Var2"),
               color = "grey90", fill = "white", shape = 22, size = 5-2, alpha = 0.1) +
    coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5)) +
    scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
    scale_x_continuous(position = "top", breaks = seq(1, 24)) +
    xlab("") +
    ylab("") +
    theme_dark() +
    
    geom_point(aes(fill = var), colour = "gray20", shape = 22, size = 5) +
    labs(title = plot_title, fill = fill_var) 
  
  if(all(check.numeric(df  %>% select(var) %>% as_vector()))){
    p <- p +
      scale_fill_viridis_c(begin = 0.8, end = 0) ### this is evaluating wrong---why?
    
  } else {
    
    p <- p +
      scale_fill_viridis_d(begin = 0.8, end = 0)  ### this is evaluating wrong---why?
  }
  
  p
  
}

try_catch_popup <- function( execute_this, 
                             error_title = "Error!", 
                             error_subtitle = "Try, try again",
                             warning_title = "Warning!",
                             warning_subtitle = "You've been warned") {
  tryCatch( {
    execute_this
  }, warning = function(w) {
    shinyalert(warning_title, warning_subtitle)
  },  error = function(e) {
    shinyalert(error_title, error_subtitle)
  })
}


filterUi <- function(id) {
  uiOutput(NS(id, "controls"))
}


make_select_ui <- function(x, id, var) {
  if (is.numeric(x)) {
    levs <- x %>% as.character() %>% unique() %>% sort()
    selectInput(id, var, choices = levs %>% sort(), selected = levs, multiple = TRUE) 
    # selectInput(id, var, choices = nums %>% sort(), selected = nums, multiple = TRUE)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(id, var, choices = levs %>% sort(), selected = levs, multiple = TRUE)
  }else if (is.character(x)) { 
    levs <- x %>% as.character() %>% unique() %>% sort()
    selectInput(id, var, choices = levs %>% sort(), selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

make_ui <- function(x, id, var) {
  if (is.numeric(x)) {
    nums <- x %>% unique() %>% sort()
    rng <- range(x, na.rm = TRUE)
    sliderInput(id, var, min = rng[1], max = rng[2], value = rng)
    # selectInput(id, var, choices = nums %>% sort(), selected = nums, multiple = TRUE)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(id, var, choices = levs %>% sort(), selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

filter_select_var <- function(x, val) { # makes the bool
  as.character(x) %in% val
}

filterServer_select <- function(id, df) {
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    vars <- reactive( df() %>% get_var_order(.) %>% sort_layout_vars(.) ) 
    output$controls <- renderUI({
      map(vars(), function(var) make_select_ui(df()[[var]], NS(id, var), var))
    })
    
    reactive({
      each_var <- map(vars(), function(var) filter_select_var(df()[[var]], input[[var]]))
      reduce(each_var, `&`)
    })
  })
}

##### ---- working layout reading module
uploadLayoutUI <- function(id) {
  tagList(
    fileInput( NS(id, "file"), 
               label = "",
               placeholder = "Upload layout file" ,
               multiple = FALSE,
               accept = c(".csv", ".tsv", ".xls", ".xlsx")),
    tableOutput(NS(id,"layout_table"))
  )
}


uploadLayoutServer <- function(id) {
  moduleServer(id, function(input, output, session) { # data path comes from "input"
    
    layout_raw <- reactive({
      req(input$file)
      
      try_catch_popup(execute_this = upload_layout(input$file$datapath) ,
                      error_title = "Layout can't be read",
                      error_subtitle = "Please check layout format.")
      
    })
    
    output$layout_table <- renderTable({ layout_raw()  })
    reactive({layout_raw()}) # return this as the value for uploadLayoutServer
  })
  
}

#### select var 
selectLayoutColorUI <- function(id) {
  tagList(
    selectInput(NS(id, "color_by"), 
                label = "Color plate plot by",  # as select input
                choices = NULL),
    plotOutput(NS(id, "plot"))  %>% withSpinner(color="#525252"), style = "overflow-x: scroll;overflow-y: scroll;height:580px"
  )
  
}

#  Server
# helper function for the server
find_vars <- function(data, hide_vars) {
  data %>% 
    select(-all_of(hide_vars)) %>% # this could replace
    get_var_order(.)
}


selectLayoutColorServer <- function(id, data, 
                                    hide_vars = c("row", "column", "well")) { # filter arg added here
  
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), { # ifever the dataset is changed
      updateSelectInput(session, "color_by",
                        choices = find_vars(data(), hide_vars)) # update the choices
    })
    
    
    output$plot <- renderPlot({
      req(input$color_by)  # wait until input$color_by is created
      
      make_platemap_plot( data = data(),
                          fill_var = !!input$color_by,
                          plot_title = paste0("Plate layout: ", input$color_by))
    })
    
    reactive( make_platemap_plot( data = data(),
                                  fill_var = !!input$color_by,
                                  plot_title = paste0("Plate layout: ", input$color_by))
    )
    
  })
}





layout_instructionsUI <- function(id) {
  instruction_text <- text_style(bold = FALSE, align = "center", size = 12) # text used for headers
  tagList(
    instruction_text("Recommended approach to DSF data analysis."),
    instruction_text("Use the template below to create a layout file for your experiment. Each plate in the layout file defines a new experimental variable (e.g. compound, pH, concentration), with the varible name provided in the first column of the layout file. You can define any number of variables by adding additional plates to the layout file. Using this method, data can be visualized by user-defined variables (e.g. color by concentration)."),
    instruction_text("Layouts are connected to data by well name, so your data must have a 'well' column to use this feature."),
    instruction_text("For more template exmples information, see the instructions tab."),
    downloadButton(NS(id,"sample_layout_file"), p("Download example layout", style = "font-family: 'Avenir Next'; font-size: 14px; color: black"), width = '100%', style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
  )
}



download_example_layoutServer <- function(id) {
  moduleServer(id, function(input, output, session) { # data path comes from "input"
    
    output$sample_layout_file <- downloadHandler(
      filename = function() {
        paste('dsfworld_example_layout.csv', sep='')
      },
      content = function(file) {
        read_csv("dsfworld_example_layout.csv")
        write.csv(read_csv("dsfworld_example_layout.csv"), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
  
}