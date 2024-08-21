
# Carga de librerias ---------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
library(RColorBrewer)

# Carga de datos https://data.undp.org/access-all-data-------------
Ciencia <- read_xlsx("Datos/Ciencia y cambios tecnologicos.xlsx")
Vida <- read_xlsx("Datos/Condiciones de vida.xlsx")
Economia <- read_xlsx("Datos/Desarrollo economico.xlsx")
Educacion <- read_xlsx("Datos/Educacion.xlsx")
Poblacion <- read_xlsx("Datos/Poblacion y cambios demograficos.xlsx")
Pobreza <- read_xlsx("Datos/Pobreza.xlsx")
Salud <- read_xlsx("Datos/Salud.xlsx")
Trabajo <- read_xlsx("Datos/Trabajo.xlsx")


# Carga de datos https://ourworldindata.org/ ---------------

transporte_publico <- read.csv("Datos/sub - acceso transporte publico.csv") |> 
  rename(Valor = Proportion.of.population.that.has.convenient.access.to.public.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje de la poblacion con acceso conveniente al transporte público")

emisiones <- read.csv("Datos/sub - emisiones de co2.csv")|> 
  rename(Valor = Per.capita.carbon.dioxide.emissions.from.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Emisiones de dioxido de carbono por el transporte per capita")

gasto_investigacion <- read.csv("Datos/sub - gasto en investigacion.csv") |> 
  rename(Valor = Research.and.development.expenditure....of.GDP.,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje del GDP invertido en investigación y desarrollo")


# Carga de datos https://github.com/argendatafundar/data?tab=readme-ov-file ------------------





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
        T ~ Pais),
      Año = as.numeric(Año))
}

Ciencia <- prep(Ciencia) 
Ciencia <- Ciencia |> 
  bind_rows(
    filter(transporte_publico, Codigo %in% unique(Ciencia$Codigo)),
    filter(emisiones, Codigo %in% unique(Ciencia$Codigo)),
    filter(gasto_investigacion, Codigo %in% unique(Ciencia$Codigo))) |> 
  arrange(Pais) |> 
  filter(!(Indicador %in% c("Percentage of primary schools with access to the internet", "Percentage of secondary schools with access to the internet")))

Vida <- prep(Vida) |> arrange(Pais)
Economia <- prep(Economia) |> arrange(Pais)
Educacion <- prep(Educacion) |> arrange(Pais)
Poblacion <- prep(Poblacion) |> arrange(Pais)
Pobreza <- prep(Pobreza) |> arrange(Pais)
Salud <- prep(Salud) |> arrange(Pais)
Trabajo <- prep(Trabajo) |> arrange(Pais)

library(FactoMineR)

datos_acp <- bind_rows(Ciencia, Vida, Economia, Educacion, Poblacion, Pobreza, Salud, Trabajo) |> 
  filter(Año == 2022) |> 
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE) |> # Elimina las filas duplicadas
  pivot_wider(names_from = Indicador,
              values_from = Valor,
              values_fill = list(Valor = NA)) |> 
  select(!Año) |> 
  select(where(~ sum(is.na(.)) < 3)) |> 
  select(where(~ !all(is.na(.)))) # Elimino las columnas con NA

acp <- PCA(X = select_if(datos_acp, is.numeric) , scale.unit = T, graph = F, ncp = Inf)

jugadores <- acp$ind$coord %>% 
  bind_cols(datos_acp) %>%
  select(Pais, Dim.1, Dim.2) %>% 
  ggplot() +
  aes(x = Dim.1, y = Dim.2, label = Pais, color = Pais) +
  geom_hline(yintercept = 0, linewidth= 0.1) +
  geom_vline(xintercept = 0, linewidth= 0.1) +
  geom_point(alpha = 0.80, size = 3) +
  theme_bw()

ggplotly(jugadores)

acp$eig |> ggplot(aes(x = 1:nrow(acp$eig), y = `percentage of variance`)) +
  geom_point() +
  geom_line() +
  xlab("Componente principal") +
  ylab("Variancia explicada por la componente (%)") +
  scale_x_continuous(breaks = 1:nrow(acp$eig))

# Mapa de sudamerica ----------------
library(leaflet)
library(spData)
library(sf)

data(world)

datos_mapa <- filter(world, continent == "South America") |> 
  arrange(name_long) |> 
  filter(iso_a2 != "FK")

mapa <- datos_mapa |> 
  leaflet() |> 
  addTiles() |> 
  setMaxBounds(lng1 = -22, lng2 = -98, lat1 = -61, lat2 = 20) |> 
  setView(lng = -58, lat = -23, zoom = 2.6)

# Interfaz ------------


ui <- dashboardPage(
  
  
  dashboardHeader(titleWidth = "0%"),
  
  ## Barra de opciones ----------------
  dashboardSidebar(collapsed = T,
                   sidebarMenu(
                     id = "sidebar",
                     menuItem("Página principal", tabName = "pag_principal", icon = icon("home")),
                     menuItem("Ciencia", tabName = "ciencia", icon = icon("microscope")),
                     menuItem("Vida", tabName = "vida", icon = icon("hand-holding-heart")),
                     menuItem("Economia", tabName = "economia", icon = icon("money-bill-wave")),
                     menuItem("Educacion", tabName = "educacion", icon = icon("user-graduate")),
                     menuItem("Poblacion", tabName = "poblacion", icon = icon("users")),
                     menuItem("Pobreza", tabName = "pobreza", icon = icon("person-shelter")),
                     menuItem("Salud", tabName = "salud", icon = icon("heart-pulse")),
                     menuItem("Trabajo", tabName = "trabajo", icon = icon("briefcase"))
                     )
                   ),
  
  
  dashboardBody(
    
    tabItems(
      
      # Página principal ------------------
      tabItem(
        tabName = "pag_principal",
        
        h2("Contar con datos 2024", style = "text-align: center;"),
        p("Click the button to go to the detailed view."),
        
        ## Primera hilera de botones ---------------------
        fluidRow(
          column(3,
                 actionBttn("ciencia", "Ciencia", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("vida", "Vida", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("economia", "Economia", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("educacion", "Educacion", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          )
        ),
        
        ## Segunda hilera de botones ---------------------
        fluidRow(
          column(3,
                 actionBttn("poblacion", "Poblacion", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("pobreza", "Pobreza", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("salud", "Salud", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("trabajo", "Trabajo", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          )
        ),
        
        ## Tercera hilera de botones ---------------------
        fluidRow(
          column(3,
                 actionBttn("go_to_details", "Go to Detailed Page", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("go_to_details", "Go to Detailed Page", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("go_to_details", "Go to Detailed Page", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("github", "Github", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", icon = icon("github"))
          )
        )
        
        
      ),
      
      
      # Pagina Ciencia -------------
      
      tabItem(
        tabName = "ciencia",
        
        h2("La ciencia en Latinoamerica"),
        
        fluidRow(column(7, 
                        pickerInput(inputId = "indicador",
                                    label = "Variable",
                                    unique(Ciencia$Indicador))),
                 column(5,
                        sliderTextInput(inputId = "mapa_anio",
                                    label = "Año",
                                    grid = T,
                                    choices = sort(unique(Ciencia$Año)),
                                    selected = max(Ciencia$Año)
                                        ))),
        
        fluidRow(
          column(7,
                 plotlyOutput("plot_evo_ciencia")
                 ),
          
          column(5,
                 leafletOutput("mapa_ciencia", width = "100%", height = "400px")
                 )
        )
        
        
        
        )
    )
  )
)



# Servidor ----------------
server <- function(input, output, session) {
  
  ## Botones a cada una de las secciones ----------------------
  observeEvent(input$ciencia, {
    updateTabItems(session, "sidebar", selected = "ciencia")
  })
  
  observeEvent(input$vida, {
    updateTabItems(session, "sidebar", selected = "vida")
  })
  
  observeEvent(input$economia, {
    updateTabItems(session, "sidebar", selected = "economia")
  })
  
  observeEvent(input$educacion, {
    updateTabItems(session, "sidebar", selected = "educacion")
  })
  
  observeEvent(input$poblacion, {
    updateTabItems(session, "sidebar", selected = "poblacion")
  })
  
  observeEvent(input$pobreza, {
    updateTabItems(session, "sidebar", selected = "pobreza")
  })
  
  observeEvent(input$salud, {
    updateTabItems(session, "sidebar", selected = "salud")
  })
  
  observeEvent(input$trabajo, {
    updateTabItems(session, "sidebar", selected = "trabajo")
  })
  
  observeEvent(input$github, {
    browseURL("https://github.com/andres-roncaglia/CCD2024")
  })
  
  ## Ciencia -------------------------------
  
  ### Grafico evolutivo ----------------
  
  output$plot_evo_ciencia <- renderPlotly({
  
    graf <- Ciencia |> 
      filter(Indicador == input$indicador) |> 
      ggplot(aes(x = Año, y = Valor, color = Pais, group = Pais)) +
      geom_point() +
      geom_line() +
      ylab(label = input$indicador) |> 
      scale_x_continuous(breaks = floor(seq(min(filter(Ciencia, Indicador == input$indicador)$Año), max(filter(Ciencia, Indicador == input$indicador)$Año), length.out = 6)))
    
    ggplotly(graf)
  })

  
  ### Grafico correlaciones ----------------
  
  output$plot_corr <- renderPlotly({
    
    if (input$var_corr_2 %in% Ciencia$Indicador) {
      datos_corr = filter(Ciencia, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Vida$Indicador) {
      datos_corr = filter(Vida, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Economia$Indicador) {
      datos_corr = filter(Economia, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Educacion$Indicador) {
      datos_corr = filter(Educacion, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Poblacion$Indicador) {
      datos_corr = filter(Poblacion, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Pobreza$Indicador) {
      datos_corr = filter(Pobreza, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Salud$Indicador) {
      datos_corr = filter(Salud, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Trabajo$Indicador) {
      datos_corr = filter(Trabajo, Indicador == input$var_corr_2)
    }
    
    datos_corr <- Ciencia |> 
      filter(Año == input$anio_corr, Indicador == input$indicador) |> 
      inner_join(datos_corr, unmatched = "drop", by = c("Pais","Codigo", "Año")) |> 
      drop_na()

      graf <- 
        datos_corr |> ggplot(aes(x = Valor.x, y = Valor.y, color = Pais), color = "dodgerblue") +
        geom_point() +
        xlab(label = input$indicador) +
        ylab(label = input$var_corr_2) +
        annotate(geom = "text", 
                 label = ifelse(nrow(datos_corr) == 0, "No hay datos que coincidan con los filtros aplicados", paste("Correlación:", cor(datos_corr$Valor.x, datos_corr$Valor.y))),
                 x = ifelse(nrow(datos_corr) == 0, 1, mean(datos_corr$Valor.x)),
                 y = ifelse(nrow(datos_corr) == 0, 1, max(datos_corr$Valor.y)*0.7)
        )
        
    
    ggplotly(graf)
  })
  
  ### Mapa ciencia -----------------------
  
  # Actualizador de la barra para seleccionar año
  observeEvent(input$indicador, {
    
    updateSliderTextInput(
      session = session,
      inputId = "mapa_anio",
      choices = sort(unique(filter(Ciencia, Indicador == input$indicador)$Año)),
      selected = max(unique(filter(Ciencia, Indicador == input$indicador)$Año))
    )
  })
  
  # Maapa
  output$mapa_ciencia <- renderLeaflet({
    
    datos <- left_join(datos_mapa, filter(Ciencia, Indicador == input$indicador, Año == input$mapa_anio), by = c("name_long" = "Pais")) |> 
      rename(Pais = name_long)

    
    pal <- colorNumeric(
      palette = "RdYlGn",
      domain = filter(Ciencia, Indicador == input$indicador)$Valor
    )
    
    Labels <- sprintf(
      paste("<strong>%s</strong><br/>",input$indicador, ": ", "%g"),
      datos$Pais, 
      datos$Valor
    ) %>% lapply(htmltools::HTML)
    
    mapa |> 
      addPolygons(fillColor = ~pal(datos$Valor),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = Labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) |> 
      addLegend(pal = pal, values = ~datos$Valor, opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



# Ideas ------------------

# Analisis de componentes principales y luego analisis cluster para agrupar paises, hacerlo fijo para un solo año

# En la misma pagina meter el grafico de correlaciones

# Arreglar valores NA grafico mapa

# 