library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(shinydashboard)
library(MetBrewer)
library(shinythemes)
library(fresh)
library(taylor)
library(ggcorrplot)
library(plotly)
library(ggrepel)
library(DT)

taylor_album_songs <- taylor_album_songs |> 
    select(album_name, track_name,danceability,energy,loudness,speechiness,acousticness,liveness,valence,tempo) |> 
    na.omit()


# Custom CSS
custom_css <- "
  .skin-purple .main-header .logo {
    background-color: #64a6bd;
    color: black;
  }
  .skin-purple .main-header .navbar {
    background-color: #64a6bd;
  }
  .skin-purple .main-sidebar {
    background-color: #d7b9d5;
color: black;
  }
  .content-wrapper, .right-side {
    background-color: #FFFFFF;
    color: black;
  }
  .box.box-solid.box-primary>.box-header {
    background-color: #d7b9d5;
    color: black;
  }
  .box {
    border-top-color: #64a6bd;
  }
  .nav-tabs-custom>.nav-tabs>li.active {
    border-top-color: #B19CD9;
  }
  /* New styles for sidebar labels */
  .skin-purple .sidebar-menu>li>a {
    color: #000000;
  }
  .skin-purple .sidebar .sidebar-menu .treeview-menu>li>a {
    color: #000000;
  }
  /* Style for select inputs in sidebar */
  .sidebar .shiny-input-container label {
    color: #000000;
  }
  .sidebar .selectize-input {
    color: #000000;
  }
"

# UI
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(titleWidth = 250, title = span(tags$img(src = 'taylor-swift.jpg', height = '40px'), "Taylor Swift Songs")),
    dashboardSidebar(
        tags$head(tags$style(HTML(custom_css))),
        selectInput("album_name", "Album Name:",
                    choices = c("All", unique(taylor_album_songs$album_name))),
        selectInput("featureX", "X Axis:",
                    choices = names(taylor_album_songs)[3:ncol(taylor_album_songs)]),
        selectInput("featureY", "Y Axis:",
                    choices = names(taylor_album_songs)[3:ncol(taylor_album_songs)])
    ),
    dashboardBody(
        tags$head(tags$style(HTML(custom_css))),
        fluidRow(
            valueBoxOutput("albums"),
            valueBoxOutput("songs"), 
            valueBoxOutput("energy"),
        ),
        fluidRow(
            box(title = "Feature Correlation Matrix", status = "primary", solidHeader = TRUE, plotlyOutput("plot1")),
            box(title = "Feature Cross Analysis", status = "primary", solidHeader = TRUE, plotlyOutput("plot2"))
        ),
        fluidRow(
            box(title = "Track Features", status = "primary", solidHeader = TRUE, DTOutput("table"), width = 12)
        )
    )
)

# Server logic
server <- function(input, output) {
    
    col_pal <- c("Taylor Swift" = "#1BAEC6",
                 "Fearless (Taylor's Version)" = "#A47F45",
                 "Speak Now (Taylor's Version)" = "#4a2454",
                 "Red (Taylor's Version)" = "#731803",
                 "1989 (Taylor's Version)" = "#487398",
                 "reputation" = "#515151",
                 "Lover" = "#EBBED3",
                 "folklore" = "#3E3E3E",
                 "evermore" = "#421E18",
                 "Midnights" = "#6F86A2",
                 "THE TORTURED POETS DEPARTMENT" = "#3F3824")
    
    filteredData <- reactive({
        data <- taylor_album_songs
        if(input$album_name != "All") {
            data <- data[data$album_name == input$album_name, ]
        }
        data
    })
    
    customValueBox <- function(value, title, icon) {
        div(class = "col-lg-10 col-md-12 col-sm-6",
            div(class = "small-box", style = "background-color: #d7b9d5; color: black;",
                div(class = "inner",
                    h3(value),
                    p(title)
                ),
                div(class = "icon", icon(icon))
            )
        )
    }
    
    output$albums <- renderUI({
        customValueBox("11", "Albums", "film")
    })
    
    output$songs <- renderUI({
        customValueBox("237", "Songs", "music")
    })
    
    output$energy <- renderUI({
        customValueBox("0.57", "Avg Energy", "bolt")
    })
    
    output$plot1 <- renderPlotly({
        data <- filteredData() %>%
            select(danceability, energy, loudness, speechiness, acousticness, liveness, valence, tempo) %>%
            na.omit()
        
        corr_matrix <- cor(data)
        
        p <- ggcorrplot(corr_matrix,
                        lab = TRUE, lab_size = 2) +
            scale_fill_gradient(low = "#f4cae0", high = "#64a6bd") +
            theme_minimal() +
            theme(
                legend.position = "top", 
                axis.text.x = element_text(angle = 45),
                axis.title.x = element_blank(),
                plot.background = element_rect(fill = "#FFFFFF"),
                panel.background = element_rect(fill = "#FFFFFF")
            )
        
        ggplotly(p, tooltip = "text")
    })
    
    output$plot2 <- renderPlotly({
        data <- filteredData()
        
        if(input$featureX %in% names(data) && input$featureY %in% names(data)) {
            p <- ggplot(data, aes_string(x = input$featureX, y = input$featureY)) +
                geom_point(aes(text = paste("Track:", track_name,
                                            "<br>", input$featureX, ":", !!sym(input$featureX),
                                            "<br>", input$featureY, ":", !!sym(input$featureY))),
                           color = "white", size = 2.5) +
                geom_point(aes(color = album_name), size = 2) +
                scale_color_manual(values = col_pal) +
                theme_minimal() +
                theme(
                    plot.background = element_rect(fill = "#FFFFFF"),
                    panel.background = element_rect(fill = "#FFFFFF"),
                    axis.text = element_text(color = "#4A4A4A"),
                    axis.title = element_text(color = "#4A4A4A"),
                    legend.position = "none"
                )
            
            ggplotly(p, tooltip = "text")
        }
    })
    
    output$table <- renderDT({
        data <- filteredData() %>% select(track_name, danceability, energy, loudness, speechiness, acousticness, liveness, valence, tempo)
        
        datatable(data, options = list(pageLength = 5)) %>%
            formatStyle(
                columns = c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'liveness', 'valence','tempo'),
                backgroundColor = styleInterval(cuts = c(0.2, 0.4, 0.6, 0.8), 
                                                values = c("#f4cae0", "#d7b9d5", "#ada7c9", "#90a8c3", "#64a6bd"))
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)