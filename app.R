#### Libraries ####

library(shiny)
library(plotly)
library(dplyr)
library(DT)

#### UI ####
ui <- fluidPage(
  #titlePanel(uiOutput("appTitle")),
  
  # Language toggle
  radioButtons(
    inputId = "lang",
    label = NULL,
    choices = c("Español" = "ES", "English" = "EN"),
    selected = "ES",
    inline = TRUE
  ),
  
  # Year selector
  uiOutput("yearSelectorUI"),
  
  #uiOutput("sankeyInstructions"),
  
  # Legend Section
  uiOutput("sankeyLegend"),
  

  fluidRow(
    column(
      width = 12,
      
      # Full-width sankey
      plotlyOutput("sankeyPlot", width = "100%", height = "800px"),
      
      br(),
      h4(textOutput("videoTableTitle")),
      
      # Full-width video table
      DTOutput("videoTable"),
      
      tags$hr(),
      
      # Footer
      uiOutput("footerUI")
    )
  )
)

#### Server ####
server <- function(input, output, session) {
  
  # ----- Dynamic app title -----
  # output$appTitle <- renderUI({
  #   if (input$lang == "EN") {
  #     HTML(paste("Synapse AlumnUSB",
  #                "Dynamic Flow Explorer",
  #                sep = "<br />"))
  #   } else {
  #     HTML(paste("SINAPSIS AlumnUSB",
  #           "Sistema Integrado de Análisis para el Seguimiento Interactivo de Solicitudes de AlumnUSB",
  #           sep = "<br />"))
  #   }
  # })
  
  # ----- Dynamic year selector -----
  output$yearSelectorUI <- renderUI({
    label_text <- if (input$lang == "EN") "Select Year:" else "Seleccionar Año:"
    
    selectInput(
      inputId = "year",
      label = label_text,
      choices = c(2015:2025, if (input$lang == "EN") "Total" else "Total"),
      selected = if (!is.null(input$year)) input$year else "Total"
    )
  })
  
  # ----- Instructions -----
  # output$sankeyInstructions <- renderUI({
  #   
  #   if (input$lang == "EN") {
  #     
  #     # English Text
  #     instructions <- c(
  #       "The horizontal axis indicates the journey of the money through each stage.",
  #       "The vertical axis shows the categories through which the funds pass, as well as the magnitude of the amounts: the wider the bar, the greater the resource in that phase.",
  #       "By hovering the cursor over the bars, you can see the exact amounts and the percentage they represent at each stage.",
  #       "At the top, you can change the year you wish to view or see the accumulated total for the organization's first decade.",
  #       "Below the graph, you will find a table with videos of each donation to the university, showing the real impact your support makes possible."
  #     )
  #     title_text <- "The Sankey diagram shows how your contributions flow from initial collection, pass through our internal funds, and reach their final use (expenses for the year or savings for the next)."
  #     
  #   } else {
  #     
  #     # Spanish Text
  #     instructions <- c(
  #       "El eje horizontal indica el recorrido del dinero a través de cada etapa.",
  #       "El eje vertical muestra las categorías por donde pasan los fondos y también la magnitud de los montos: mientras más ancha la barra, mayor es el recurso en esa fase.",
  #       "Al poner el cursor sobre las barras, puedes ver los montos exactos y el porcentaje que representan en cada etapa.",
  #       "En la parte superior puedes cambiar el año que deseas visualizar o ver el acumulado de la primera década de la organización.",
  #       "Debajo del gráfico encontrarás una tabla con videos de cada donación a la universidad, mostrando el impacto real que tu apoyo hace posible."
  #     )
  #     title_text <-  "El diagrama de Sankey muestra cómo tus aportes fluyen desde la recaudación inicial, pasan por nuestros fondos internos y llegan a su uso final (gastos del año o ahorros para el siguiente)."
  #     
  #   }
  #   
  #   # Generate the HTML output
  #   # Wrap the entire thing in a fluidRow/wellPanel for a nice visual separation
  #   fluidRow(
  #     column(12,
  #            wellPanel(
  #              p(title_text),
  #              tags$ul(
  #                lapply(instructions, tags$li) # Convert each string into an HTML list item (<li>)
  #              )
  #            )
  #     )
  #   )
  # })
  
  # ----- Sankey Diagram Legend (Image) -----
  output$sankeyLegend <- renderUI({
    
    # Determine the file name based on the language input
    image_file <- if (input$lang == "EN") {
      "legend_EN.png"
    } else {
      "legend_ES.png"
    }
    
    # Construct the full path to the image, assuming the 'www' folder is in the app directory
    image_path <- file.path("www", image_file)
    
    # Use tags$img to display the image. We wrap it in a div/fluidRow for layout.
    fluidRow(
      column(12,
             # You can adjust the width/height/style as needed
             tags$img(src = image_file, style = "display: block; margin-left: auto; margin-right: auto; max-width: 70%;")
      )
    )
  })
  
  # ----- Video table title -----
  output$videoTableTitle <- renderText({
    if (input$lang == "EN") {
      "Available Testimonials:"
    } else {
      "Testimonios disponibles:"
    }
  })
  
  # -----------------------------
  # Load nodes (static)
  # -----------------------------
  nodes <- read.csv("nodes.csv", header = TRUE, encoding = "UTF-8")
  
  # Load percentages
  percentages <- read.csv("percentages.csv", header = TRUE, encoding = "UTF-8")
  
  # Create a color palette by group
  groups <- unique(nodes$group)
  pal <- RColorBrewer::brewer.pal(n = max(3, length(groups)), "Set2")
  group_colors <- setNames(pal[1:length(groups)], groups)
  
  # Assign each node and link a color
  node_colors <- group_colors[nodes$group]
  
  # Use pos column for horizontal location (must be 0–1)
  node_x <- nodes$pos
  #node_x <- (node_x - min(node_x)) / (max(node_x) - min(node_x))
  node_y <- seq(0, 1, length.out = nrow(nodes))  # vertical spacing
  
  # Helper function to convert hex color to rgba string
  hex_to_rgba <- function(hex_colors, alpha = 0.4) {
    # Remove the '#' from the start of the hex color
    hex_colors <- sub("^#", "", hex_colors)
    
    # Convert 2-digit hex values to R, G, B decimal (0-255)
    r <- strtoi(substr(hex_colors, 1, 2), 16)
    g <- strtoi(substr(hex_colors, 3, 4), 16)
    b <- strtoi(substr(hex_colors, 5, 6), 16)
    
    # Construct the rgba string
    paste0("rgba(", r, ", ", g, ", ", b, ", ", alpha, ")")
  }
  
  # -----------------------------
  # Plot (reactive)
  # -----------------------------
  output$sankeyPlot <- renderPlotly({
    
    req(input$year)  # Ensure year is selected
    
    # Load links fresh (changes every year)
    links <- read.csv("links.csv", header = TRUE, encoding = "UTF-8")
    
    # Ensure amount column is numeric
    links$amount <- as.numeric(links$amount)
    
    # Handle "Total" selection
    if (input$year == "Total") {
      links_year <- links %>% filter(year == "Total")
      title_year <- if (input$lang == "EN") "Total (All Years)" else "Total (Todos los Años)"
      selected_year <- NULL
    } else {
      links_year <- links %>% filter(year == as.numeric(input$year))
      title_year <- if (input$lang == "EN") {
        paste("Year", input$year)
      } else {
        paste("Año", input$year)
      }
      selected_year <- as.numeric(input$year)
    }
    
    #link_colors <- node_colors[links_year$source + 1]
    
    # ---- Compute totals and percentages ----
    total_flow <- sum(links_year$amount)

    # Select language for node names
    node_names <- if (input$lang == "EN") nodes$name_EN else nodes$name_ES
    
    # Get percentages from percentages file
    if (input$year == "Total") {
      perc_filtered <- percentages %>% filter(year == "Total")
      year_label <- "Total"
    } else {
      perc_filtered <- percentages %>% filter(year == as.numeric(input$year))
      year_label <- as.character(input$year)
    }
    
    # Create a lookup for percentages by node name
    perc_lookup <- if (input$lang == "EN") {
      setNames(perc_filtered$Percentage, perc_filtered$name_EN)
    } else {
      setNames(perc_filtered$Percentage, perc_filtered$name_ES)
    }
    
    # Get percentage for each node
    node_percentages <- sapply(node_names, function(name) {
      perc <- perc_lookup[name]
      if (is.na(perc)) return(0)
      return(round(perc * 100, 1))
    })
    
    # Create a lookup for amount by node name
    amount_lookup <- if (input$lang == "EN") {
      setNames(perc_filtered$amount, perc_filtered$name_EN)
    } else {
      setNames(perc_filtered$amount, perc_filtered$name_ES)
    }
    
    flow_per_node <- sapply(node_names, function(name) {
      amount <- amount_lookup[name]
      if (is.na(amount)) return(0)
      return(amount)
    })
    
    # Create customdata with language-specific labels
    node_customdata <- if (input$lang == "EN") {
      paste0(
        "Amount: USD ", format(flow_per_node, big.mark = ","), "<br>",
        "Percentage: ", node_percentages, "%"
      )
    } else {
      paste0(
        "Cantidad: USD ", format(flow_per_node, big.mark = ","), "<br>",
        "Porcentaje: ", node_percentages, "%"
      )
    }
    
    link_customdata <- if (input$lang == "EN") {
      paste0("Amount: USD ", format(links_year$amount, big.mark = ","))
    } else {
      paste0("Cantidad: USD ", format(links_year$amount, big.mark = ","))
    }
    
    source_groups <- nodes$group[as.numeric(links_year$source) + 1]
    
    # Map the group name to the SOLID hex color
    solid_link_colors <- group_colors[source_groups]
    
    # Convert hex to RGBA with alpha (e.g., 40% transparency)
    link_colors_with_alpha <- hex_to_rgba(solid_link_colors, alpha = 0.7)
    
    # ---------------------------------------------
    # Sankey
    # ---------------------------------------------
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      
      node = list(
        label = node_names,
        color = node_colors,
        pad = 40,           # Increased spacing between nodes (default is 15)
        thickness = 25,     # Increased node width (default is 20)
        x = node_x,
        #y = node_y,
        customdata = node_customdata,
        hovertemplate = if (input$lang == "EN") {
          "Node: %{label}<br>%{customdata}<extra></extra>"
        } else {
          "Nodo: %{label}<br>%{customdata}<extra></extra>"
        }
      ),
      
      link = list(
        source = links_year$source,
        target = links_year$target,
        value = links_year$amount,
        customdata = link_customdata,
        color = link_colors_with_alpha,
        hovertemplate = "%{source.label} → %{target.label}<br>%{customdata}<extra></extra>"
      )
    ) %>%
      layout(
        font = list(size = 12)
      )
    
  })
  
  # -----------------------------
  # Video Table (reactive)
  # -----------------------------
  output$videoTable <- DT::renderDT({
    
    req(input$year)  # Ensure year is selected
    
    urls_data <- read.csv("urls.csv", header = TRUE, encoding = "UTF-8")
    
    if (input$year != "Total") {
      urls_filtered <- urls_data %>% 
        filter(year == as.numeric(input$year))
    } else {
      urls_filtered <- urls_data
    }
    
    urls_filtered <- urls_filtered %>% 
      filter(!is.na(URL) & URL != "")
    
    if (nrow(urls_filtered) > 0) {
      
      # Select the appropriate name column based on language
      name_col <- if (input$lang == "EN") "name_EN" else "name_ES"
      
      # Create link text based on language
      link_text <- if (input$lang == "EN") "Watch testimonial" else "Ver testimonio"
      
      table_data <- urls_filtered %>%
        mutate(Link = paste0('<a href="', URL, '" target="_blank">', link_text, '</a>')) %>%
        select(!!sym(name_col), Link)
      
      # Rename columns based on language
      if (input$lang == "EN") {
        table_data <- table_data %>% rename("Activity" = name_EN, "Testimonial" = Link)
      } else {
        table_data <- table_data %>% rename("Actividad" = name_ES, "Testimonio" = Link)
      }
      
      # Create language-specific options
      language_options <- if (input$lang == "EN") {
        list(
          search = "Search testimonials:",
          lengthMenu = "Show _MENU_ testimonials per page",
          info = "Showing _START_ to _END_ of _TOTAL_ testimonials",
          infoEmpty = "No testimonials available",
          zeroRecords = "No matching testimonials found",
          paginate = list(
            first = "First",
            last = "Last",
            `next` = "Next",
            previous = "Previous"
          )
        )
      } else {
        list(
          search = "Buscar testimonios:",
          lengthMenu = "Mostrar _MENU_ testimonios por página",
          info = "Mostrando _START_ a _END_ de _TOTAL_ testimonios",
          infoEmpty = "No hay testimonios disponibles",
          zeroRecords = "No se encontraron testimonios",
          paginate = list(
            first = "Primero",
            last = "Último",
            `next` = "Siguiente",
            previous = "Anterior"
          )
        )
      }
      
      datatable(
        table_data,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25),
          language = language_options,
          dom = 'frtip'
        ),
        class = 'cell-border stripe hover'
      )
    } else {
      # No videos message
      no_videos_msg <- if (input$lang == "EN") {
        "No testimonials available for this year"
      } else {
        "No hay testimonios disponibles para este año"
      }
      
      datatable(
        data.frame(Message = no_videos_msg),
        options = list(dom = 't', ordering = FALSE, searching = FALSE),
        colnames = ""
      )
    }
  })
  
  output$footerUI <- renderUI({
    
    if (input$lang == "ES") {
      footer_text <- HTML(
        "Creado por Pablo Hernández Borges · 
       <a href='http://pablohernandezb.dev' target='_blank'>
         pablohernandezb.dev
       </a>"
      )
    } else {  # English
      footer_text <- HTML(
        "Created by Pablo Hernández Borges · 
       <a href='http://pablohernandezb.dev' target='_blank'>
         pablohernandezb.dev
       </a>"
      )
    }
    
    tags$div(
      footer_text,
      style = "
      width:100%; 
      text-align:center; 
      margin-top:10px; 
      font-size:12px; 
      color:#666;
    "
    )
  })
  
}

#### Run App ####
shinyApp(ui = ui, server = server)


#### Deploy online ####
# rsconnect::deployApp()
