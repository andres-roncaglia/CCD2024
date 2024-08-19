
# Carga de librerias ---------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
library(tidyr)
library(dplyr)

# Carga de datos -------------
Ciencia <- read_xlsx("Datos/Ciencia y cambios tecnologicos.xlsx")
Vida <- read_xlsx("Datos/Condiciones de vida.xlsx")
Economia <- read_xlsx("Datos/Desarrollo economico.xlsx")
Educacion <- read_xlsx("Datos/Educacion.xlsx")
Poblacion <- read_xlsx("Datos/Poblacion y cambios demograficos.xlsx")
Pobreza <- read_xlsx("Datos/Pobreza.xlsx")
Salud <- read_xlsx("Datos/Salud.xlsx")
Trabajo <- read_xlsx("Datos/Trabajo.xlsx")

# Preparación de bases de datos --------------
prep <- function(x) {
  x |> 
    pivot_longer(
      cols = !1:3,
      names_to = "Año",
      values_to = "Valor"
    ) |> 
    `colnames<-`(c("Pais", "Codigo", "Indicador", "Año", "Valor")) |> 
    mutate(
      Pais = case_when(
        Pais == "Bolivia (Plurinational State of)" ~ "Bolivia",
        Pais == "Venezuela (Bolivarian Republic of)" ~ "Venezuela", 
        T ~ Pais))
}

Ciencia <- prep(Ciencia)
Vida <- prep(Vida)
Economia <- prep(Economia)
Educacion <- prep(Educacion)
Poblacion <- prep(Poblacion)
Pobreza <- prep(Pobreza)
Salud <- prep(Salud)
Trabajo <- prep(Trabajo)

Ciencia |> 
  filter(Indicador == "Access to internet, percent of population") |> 
  ggplot(aes(x = Año, y = Valor, color = Pais, group = Pais)) +
  geom_point() +
  geom_line()


# Página principal
general_view <- function() {
  fluidPage(
    h2("Welcome to the App"),
    p("Click the button to go to the detailed view."),
    actionButton("go_to_details", "Go to Detailed Page")
  )
}


# Interfaz ------------


ui <- fluidPage(
  
  uiOutput("main_ui")

  
)


# Servidor ----------------
server <- function(input, output, session) {
  
  ## Menu principal -------------------
  output$main_ui <- renderUI({
    general_view()
  })
  
  observeEvent(input$go_to_details, {
    
    ## Pagina 1 -----------------
    output$main_ui <- renderUI({
      sidebarLayout(
        sidebarPanel(
          h3("Detailed Sidebar"),
          # Add more sidebar content here
          p("This is a detailed page with a sidebar."),
          actionButton("go_back", "Back to Main Page")
        ),
        mainPanel(
          h2("Detailed Content"),
          p("Here is where you put more detailed content."),
          # Add your detailed UI elements here
        )
      )
    })
  })
  
  
  observeEvent(input$go_back, {
    output$main_ui <- renderUI({
      general_view()
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
