
# Carga de librerias ---------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
library(RColorBrewer)
library(DT)

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
  mutate(Indicador = "Porcentaje de la poblacion con acceso conveniente al transporte público",
         Trad = "Porcentaje de la poblacion con acceso conveniente al transporte público",
         Abrev = "Acceso al transporte público (%)",
         Ayuda = "Porcentaje de la poblacion con acceso conveniente al transporte público")

emisiones <- read.csv("Datos/sub - emisiones de co2.csv")|> 
  rename(Valor = Per.capita.carbon.dioxide.emissions.from.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Emisiones CO2 por el transporte (per capita)",
         Trad = "Emisiones CO2 por el transporte (per capita)",
         Abrev = "Emisiones CO2 por el transporte (per capita)",
         Ayuda = "Emisiones de Dioxido de carbono per capita emitidas por el transporte")

gasto_investigacion <- read.csv("Datos/sub - gasto en investigacion.csv") |> 
  rename(Valor = Research.and.development.expenditure....of.GDP.,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje del GDP invertido en investigación y desarrollo",
         Trad = "Porcentaje del GDP invertido en investigación y desarrollo",
         Abrev = "GDP invertido (%)",
         Ayuda = "Porcentaje del GDP invertido en investigación y desarrollo")


# Carga de datos https://github.com/argendatafundar/data?tab=readme-ov-file ------------------


# Carga de traducciones ----------

Traducciones <- read_xlsx("Datos/Traducciones variables.xlsx")



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
      Año = as.numeric(Año)) |> 
    left_join(Traducciones, by = "Indicador")
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

vida_poblacion <- rbind(Vida,Poblacion)

# Analisis de componentes principales -----------------------

library(FactoMineR)

## Preparacion de los datos para ACP -------------------
datos_acp <- bind_rows(Ciencia, Vida, Economia, Educacion, Poblacion, Pobreza, Salud, Trabajo) |> 
  filter(Año == 2022) |> 
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE) |> # Elimina las filas duplicadas
  select(!c(Año, Abrev, Ayuda, Indicador)) |> 
  pivot_wider(names_from = Trad,
              values_from = Valor,
              values_fill = list(Valor = NA)) |> 
  select(where(~ sum(is.na(.)) < 3)) # Excluyo las variables con pocos datos


# Analisis de componentes principales
acp <- PCA(X = select_if(datos_acp, is.numeric) , scale.unit = T, graph = F, ncp = Inf)


## Grafico aporte de las componentes principales ----------------------
graf_aporte_cp <- as.data.frame(acp$eig) |>
  mutate_if(is.numeric,round,2) |> 
  mutate(color = case_when(`cumulative percentage of variance` < 85 ~ "Menor al 85%",
                           T ~ "Mayor al 85%"),
         text = paste("Porcentaje acumulado de la variancia:",`cumulative percentage of variance`)) |> 
  rename("Porcentaje de la variancia" = `percentage of variance`, "Porcentaje acumulado de la variancia" = `cumulative percentage of variance`) |> 
  ggplot(aes(x = 1:nrow(acp$eig), y = `Porcentaje de la variancia`, color = color)) +
  geom_line(color = "black") +
  geom_point(aes(text = text),size = 3) +
  xlab("Componente principal") +
  ylab("Variancia explicada por la componente (%)") +
  scale_x_continuous(breaks = 1:nrow(acp$eig)) +
  scale_color_manual(values = c("Menor al 85%" = "forestgreen", "Mayor al 85%" = "firebrick3"),
                     breaks = c("Menor al 85%", "Mayor al 85%")) +
  labs(color = "Porcentaje acumulado de la variancia") +
  theme(legend.position = "bottom")


# Analisis Cluster con las componentes principales ---------------------------

# Cantidad de componentes con las que trabajar

num_cp <- length(which(acp$eig[,"cumulative percentage of variance"] < 85))

variables_componentes <- acp$ind$coord[,1:num_cp]

datos_cluster <- datos_acp |> 
  select(Pais, Codigo) |> 
  bind_cols(variables_componentes) |> 
  mutate_if(is.numeric, scale)


# Matriz de distancias
d <- datos_cluster |> 
  select_if(is.numeric) |> 
  dist(method = "euclidean")

## Analisis cluster ---------
fit <- hclust(d, method="ward.D")

# Cantidad de grupos
groups <- cutree(fit, k=4)

datos_cluster <- datos_cluster |> 
  mutate(Grupo = as.factor(groups))



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

# Graficos --------------------

graf_evolutivo = function(base_datos, indicador) {
  base_datos |> 
    filter(Trad == indicador) |> 
    ggplot(aes(x = Año, y = Valor, color = Pais, group = Pais)) +
    geom_point() +
    geom_line() +
    ylab(label = indicador) +
    xlab(label = "Año") +
    scale_x_continuous(breaks = floor(seq(min(filter(base_datos, Trad == indicador)$Año), max(filter(base_datos, Trad == indicador)$Año), length.out = 6)))
}

graf_mapa = function(base_datos, indicador, anio) {
  
  datos <- left_join(datos_mapa, filter(base_datos, Trad == indicador, Año == anio), by = c("name_long" = "Pais")) |> 
    rename(Pais = name_long)
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = filter(base_datos, Trad == indicador)$Valor
  )
  
  Labels <- sprintf(
    paste("<strong>%s</strong><br/>%g"),
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
  
}


graf_torta_vida <- vida_poblacion |> 
  filter(Pais == "Argentina",Trad == "Población rural, total" | Trad == "Población Urbana, total", Año == 2020) |>
  plot_ly(labels = ~Trad, values = ~Valor, type = 'pie',
          textposition = 'inside',
          
          textinfo = 'label+percent',
          
          insidetextfont = list(color = '#FFFFFF'),
          
          
          marker = list(colors = colors,
                        
                        line = list(color = '#FFFFFF', width = 1)),
          
          #The 'pull' attribute can also be used to create space between the sectors
          
          showlegend = FALSE)

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
                     menuItem("Trabajo", tabName = "trabajo", icon = icon("briefcase")),
                     menuItem("Análisis de datos", tabName = "analisis", icon = icon("magnifying-glass-chart")),
                     menuItem("Bases de datos", tabName = "datos", icon = icon("database"))
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
                 actionBttn("analisis", "Análisis de datos", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("datos", "Bases de datos", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
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
                        pickerInput(inputId = "indicador_ciencia",
                                    label = "Variable",
                                    unique(Ciencia$Trad))),
                 column(5,
                        sliderTextInput(inputId = "mapa_anio",
                                    label = "Año",
                                    grid = T,
                                    choices = sort(unique(Ciencia$Año)),
                                    selected = max(Ciencia$Año)
                                        ))),
        
        fluidRow(
          column(7,
                 plotlyOutput("plot_evo_ciencia")),
          
          column(5,
                 leafletOutput("mapa_ciencia", width = "100%", height = "400px")
                 )
        )
        
        
        
        ),
      
      # Pagina Vida y poblacion --------------------------
      
      tabItem(
        tabName = "vida",
        h2("Población y vida"),
        
        fluidRow(
          column(7,
                 h4("Población a lo largo de los años")
                 ),
          column(5,
                 pickerInput(inputId = "indicador_vida",
                             label = "Variable",
                             unique(vida_poblacion$Trad))
                 )
        ),
        
        fluidRow(
          column(7,
                 plotlyOutput("plot_evo_vida")),
          
          column(5,
                 leafletOutput("mapa_vida", width = "100%", height = "400px")
          )
        ),
        
        fluidRow(column(4,
                        pickerInput(inputId = "indicador_vida_pais",
                                    label = "País",
                                    unique(vida_poblacion$Pais))),
                 column(3),
                 column(4,
                        sliderTextInput(inputId = "mapa_anio",
                                        label = "Año",
                                        grid = T,
                                        choices = sort(unique(vida_poblacion$Año)),
                                        selected = max(vida_poblacion$Año)
                        )
                        )),
        
        fluidRow(column(4
                        
                        ),
                 column(4
                        ),
                 column(4
                        ))
      ),
      
      # Pagina Analisis de datos ------------------
      
      tabItem(
        tabName = "analisis",
        h2("Analisis de datos"), br(),
        h4("Componentes principales y clustering"), br(),
        
        fluidRow(
          column(7,
                 plotlyOutput("plot_corr")),
          column(5,
                 fluidRow(
                   pickerInput(inputId = "var_corr_1", label = "Variable eje x", choices = list(
                     Ciencia = unique(Ciencia$Indicador), 
                     Vida = unique(Vida$Indicador), 
                     Economia = unique(Economia$Indicador), 
                     Educacion = unique(Educacion$Indicador), 
                     Poblacion = unique(Poblacion$Indicador), 
                     Pobreza = unique(Pobreza$Indicador), 
                     Salud = unique(Salud$Indicador), 
                     Trabajo = unique(Trabajo$Indicador)
                   ), selected = "Access to internet, percent of population",
                   options = list(
                     `live-search` = TRUE,
                     size = 7))
                 ),
                 fluidRow(
                   pickerInput(inputId = "var_corr_2", label = "Variable eje y", choices = list(
                     Ciencia = unique(Ciencia$Indicador),
                     Vida = unique(Vida$Indicador),
                     Economia = unique(Economia$Indicador),
                     Educacion = unique(Educacion$Indicador),
                     Poblacion = unique(Poblacion$Indicador),
                     Pobreza = unique(Pobreza$Indicador),
                     Salud = unique(Salud$Indicador),
                     Trabajo = unique(Trabajo$Indicador)
                   ), selected = "Economic Inequality Score",
                   options = list(
                     `live-search` = TRUE,
                     size = 7))
                 ),
                 fluidRow(
                   sliderInput(inputId = "anio_corr",
                               label = "Año",
                               min = 1960,
                               max = 2022,
                               value = 2022
                   )
                 ),
                 fluidRow(
                   valueBoxOutput("caja_correlaciones", width = 11)
                 )
                 )
        ),
        br(),
        fluidRow(
          column(7,
                 plotlyOutput("graf_aporte_cp")),
          
          column(5,
                 DTOutput("tabla_cp")
                 )
        ),
        br(),
        fluidRow(
          column(9,
                 plotlyOutput("graf_acp")),
          
          column(3, 
                 fluidRow(
                   pickerInput(inputId = "componentex", label = "Componente en el eje x", choices = colnames(datos_cluster)[3:7], selected = colnames(datos_cluster)[3])
                   ),
                 fluidRow(
                   pickerInput(inputId = "componentey", label = "Componente en el eje y", choices = colnames(datos_cluster)[3:7], selected = colnames(datos_cluster)[4])
                 ))
        )
      ),
      
      # Pagina Bases de datos ---------------------
      
      tabItem(
        tabName = "datos",
        
        h2("Bases de datos"),
        
        fluidRow(
          column(1),
          
          column(8,
                 pickerInput(
                   inputId = "base_datos",
                   label = "Seleccionar Base de datos", 
                   choices = c("Ciencia", "Vida y población")
                   )
                 ),
          column(2,
                 downloadButton("datos_descarga", "Descargar")
                 )
          
          
        ),
        
        fluidRow(
          column(1),
          column(10,
            DTOutput("tabla_datos")
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
  
  observeEvent(input$analisis, {
    updateTabItems(session, "sidebar", selected = "analisis")
  })
  
  observeEvent(input$datos, {
    updateTabItems(session, "sidebar", selected = "datos")
  })
  
  observeEvent(input$github, {
    browseURL("https://github.com/andres-roncaglia/CCD2024")
  })
  
  ## Configuraciones Mapa -----------------------
  
  # Seleccion del conjunto de datos
  
  base_datos <- reactive({
    if (length(input$tabs) == 0) {Ciencia} else{
      if (input$tabs == "ciencia") {
        Ciencia
        
      } else if (input$tabs == "vida") {
        Vida
        
      } else if  (input$tabs == "economia") {
        Economia
      } else if (input$tabs == "educacion") {
        Educacion
      } else if  (input$tabs == "poblacion") {
        Poblacion
      } else if (input$tabs == "pobreza") {
        Pobreza
      } else if  (input$tabs == "salud") {
        Salud
      } else if (input$tabs == "trabajo") {
        Trabajo}
    }
    
  })
  
  
  # Actualizador de la barra para seleccionar año
  observeEvent(input$indicador, {
    base_datos <- base_datos()
    
    updateSliderTextInput(
      session = session,
      inputId = "mapa_anio",
      choices = sort(unique(filter(base_datos, Trad == input$indicador)$Año)),
      selected = max(unique(filter(base_datos, Trad == input$indicador)$Año))
    )
  })
  
  
  ## Ciencia -------------------------------
  
  ### Grafico evolutivo ----------------
  
  output$plot_evo_ciencia <- renderPlotly({
    
    graf = graf_evolutivo(Ciencia, input$indicador_ciencia)
    
    ggplotly(graf, tooltip = c("Año", "Valor", "Pais"))
  })
  
  ### Mapa ---------------------
  
  output$mapa_ciencia <- renderLeaflet({
   
    graf_mapa(Ciencia, input$indicador_ciencia, input$mapa_anio)
    
  })
  
  ## Vida y poblacion -------------------------------
  
  ### Grafico evolutivo ----------------
  
  output$plot_evo_vida <- renderPlotly({
    
    base_datos = vida_poblacion %>%
      mutate(Valor = round(Valor/1000000, 2)) |>
      filter(!is.na(Valor))
    
    graf = graf_evolutivo(base_datos,"Población, total") +
      ylab(label = "Población total (millones)")
    
    ggplotly(graf, tooltip = c("Año", "Valor", "Pais"))
    
  })
  
  ### Mapa ---------------------
  
  output$mapa_vida <- renderLeaflet({
    graf_mapa(vida_poblacion, input$indicador_vida, input$mapa_anio)
    
  })
  
  ## Analisis -------------------------
  
  ### Grafico correlaciones ----------------
  
  datos_corr <- reactive({
    
    if (input$var_corr_1 %in% Ciencia$Indicador) {
      datos_corr_1 = filter(Ciencia, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Vida$Indicador) {
      datos_corr_1 = filter(Vida, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Economia$Indicador) {
      datos_corr_1 = filter(Economia, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Educacion$Indicador) {
      datos_corr_1 = filter(Educacion, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Poblacion$Indicador) {
      datos_corr_1 = filter(Poblacion, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Pobreza$Indicador) {
      datos_corr_1 = filter(Pobreza, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Salud$Indicador) {
      datos_corr_1 = filter(Salud, Indicador == input$var_corr_1)
    } else if (input$var_corr_1 %in% Trabajo$Indicador) {
      datos_corr_1 = filter(Trabajo, Indicador == input$var_corr_1)
    }
    
    
    if (input$var_corr_2 %in% Ciencia$Indicador) {
      datos_corr_2 = filter(Ciencia, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Vida$Indicador) {
      datos_corr_2 = filter(Vida, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Economia$Indicador) {
      datos_corr_2 = filter(Economia, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Educacion$Indicador) {
      datos_corr_2 = filter(Educacion, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Poblacion$Indicador) {
      datos_corr_2 = filter(Poblacion, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Pobreza$Indicador) {
      datos_corr_2 = filter(Pobreza, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Salud$Indicador) {
      datos_corr_2 = filter(Salud, Indicador == input$var_corr_2)
    } else if (input$var_corr_2 %in% Trabajo$Indicador) {
      datos_corr_2 = filter(Trabajo, Indicador == input$var_corr_2)
    }
    
    
    datos_corr_1 |> 
      filter(Año == input$anio_corr, Indicador == input$var_corr_1) |> 
      inner_join(datos_corr_2, unmatched = "drop", by = c("Pais","Codigo", "Año")) |> 
      drop_na()
  })
  
  
  output$plot_corr <- renderPlotly({
    
    datos_corr <- datos_corr()
    
    graf <- 
      datos_corr |> ggplot(aes(x = Valor.x, y = Valor.y, color = Pais), color = "dodgerblue") +
      geom_point() +
      xlab(label = input$var_corr_1) +
      ylab(label = input$var_corr_2)
    
    if (nrow(datos_corr) < 1) {
      graf <- graf +
        annotate(geom = "text", 
                 label = "Pocos datos que coincidan con los filtros aplicados",
                 x = 1,
                 y = 1
        )
    }
    
    
    ggplotly(graf)
  })
  
  ### Caja correlaciones ---------------
  
  output$caja_correlaciones <- renderValueBox({
    datos_corr <- datos_corr()
    
    
    if (nrow(datos_corr) < 4) {
      
      fuerza_corr <- "Datos insuficientes"
    } else if (cor(datos_corr$Valor.x, datos_corr$Valor.y) < -0.7) {
      
      fuerza_corr <- "Correlación negativa fuerte"
    } else if (cor(datos_corr$Valor.x, datos_corr$Valor.y) < -0.3) {
      
      fuerza_corr <- "Correlación negativa leve"
    } else if (cor(datos_corr$Valor.x, datos_corr$Valor.y) < 0.3) {
      
      fuerza_corr <- "Incorrelacionadas"
    } else if (cor(datos_corr$Valor.x, datos_corr$Valor.y) < 0.7) {
      
      fuerza_corr <- "Correlación positiva leve"
    }else if (cor(datos_corr$Valor.x, datos_corr$Valor.y) < 1) {
      
      fuerza_corr <- "Correlación positiva fuerte"
    }
    
    
    valueBox(
      subtitle = fuerza_corr,
      value = ifelse(nrow(datos_corr) < 4, "-", round(cor(datos_corr$Valor.x, datos_corr$Valor.y), 3)),
      width = 3,
      color = "aqua"
    )
  })
  
  ### Grafico aporte componentes principales --------------------
  
  output$graf_aporte_cp <- renderPlotly({
    ggplotly(graf_aporte_cp, tooltip = c("y", "text")) |> 
      layout(legend = list(orientation = "h",  
                           x = 0.5,              
                           xanchor = "center",  
                           y = -0.2))
  })
  
  
  ### Tabla de componentes principales -----------------
  
  output$tabla_cp <- renderDT({
    acp$var$contrib |>
      as.data.frame() |> 
      mutate(Variable = rownames(acp$var$contrib)) |>
      `rownames<-`(NULL) |> 
      mutate_if(is.numeric, round, 4) |> 
      relocate(Variable, .before = colnames(acp$var$contrib)[1]) |> 
      datatable(selection = "none",escape = F, options = list(pageLength = 5,
                                                              scrollX = T,
                                                              scrollY = "300px",
                                                              paging = F,
                                                              scrollCollapse = T,
                                                              lengthMenu = c(3,5,10, 15, nrow(acp$var$contrib)),
                                                              select = list(style = 'none'),
                                                              columnDefs = list(
                                                                list(
                                                                  targets = 0,  # First column (index starts at 0)
                                                                  width = '600px'  # Set width for the first column
                                                                ))
                                                              ))
  })
  
  ### Grafico clusters ----------------
  
  output$graf_acp <- renderPlotly({
    
    componentex <- sym(input$componentex)
    componentey <- sym(input$componentey)
    
    graf_acp <- acp$ind$coord |> 
      bind_cols(datos_acp, Grupo = datos_cluster$Grupo) |> 
      select(Pais, !!componentex, !!componentey, Grupo) |> 
      ggplot() +
      aes(x = !!componentex, y = !!componentey, label = Pais, color = Grupo) +  # Usa color según el grupo
      geom_hline(yintercept = 0, linewidth= 0.1) +
      geom_vline(xintercept = 0, linewidth= 0.1) +
      geom_point(alpha = 0.80, size = 3) +
      labs(color = "Grupo", fill = "Grupo")
    
    graf_acp <- ggplotly(graf_acp, tooltip = c("Pais", "Grupo", "x", "y"))
  })
  
  ## Bases de datos -------------
  
  ### Boton descarga --------------------
  
  # Seleccion del conjunto de datos
  
  base_datos <- reactive({
    if (length(input$base_datos) == 0) {Ciencia} else{
      if (input$base_datos == "Ciencia") {
        Ciencia
        
      } else if (input$base_datos == "Vida y población") {
        vida_poblacion
        
      } else if  (input$base_datos == "economia") {
        Economia
      } else if (input$base_datos == "educacion") {
        Educacion
      } else if  (input$base_datos == "poblacion") {
        Poblacion
      } else if (input$base_datos == "pobreza") {
        Pobreza
      } else if  (input$base_datos == "salud") {
        Salud
      } else if (input$base_datos == "trabajo") {
        Trabajo} else {Ciencia}
    } 
    
  })
  
  
  output$datos_descarga <- downloadHandler(
    
    filename = function() {
      paste0(input$base_datos, ".csv")
    },
    content = function(file) {
      write.csv(base_datos(), file)
    }
  )
  
  ### Tabla ------------
  
  output$tabla_datos = renderDT({
    
    if (input$base_datos == "Ciencia") {datos = Ciencia
    } else if (input$base_datos == "Vida y población") {datos = vida_poblacion}
    
    datos %>% 
      datatable(selection = "single",escape = F, options = list(scrollX = T,
                                                              scrollY = "300px",
                                                              paging = F,
                                                              scrollCollapse = T,
                                                              lengthMenu = c(10, 15, 30, nrow(datos))
                                                              ))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



# Ideas ------------------

# Analisis de componentes principales y luego analisis cluster para agrupar paises, hacerlo fijo para un solo año

# En la misma pagina meter el grafico de correlaciones

# Arreglar valores NA grafico mapa

# 