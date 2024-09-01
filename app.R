
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
library(stringr)
library(zoo)
library(shinydashboardPlus)

# Para el carrusel si mantengo el mouse
jscode <-"
$(document).ready(function(){
            $('#mycarousel').carousel( { interval:  false } );
});"

# Carga de datos https://data.undp.org/access-all-data-------------
Ciencia <- read_xlsx("Datos/Ciencia y cambios tecnologicos.xlsx")
Vida <- read_xlsx("Datos/Condiciones de vida.xlsx")
Economia <- read_xlsx("Datos/Desarrollo economico.xlsx")
Educacion <- read_xlsx("Datos/Educacion.xlsx")
Poblacion <- read_xlsx("Datos/Poblacion y cambios demograficos.xlsx")
Pobreza <- read_xlsx("Datos/Pobreza.xlsx")
Trabajo <- read_xlsx("Datos/Trabajo.xlsx")


# Carga de datos https://ourworldindata.org/ ---------------

transporte_publico <- read.csv("Datos/sub - acceso transporte publico.csv") |> 
  rename(Valor = Proportion.of.population.that.has.convenient.access.to.public.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje de la poblacion con acceso conveniente al transporte público",
         Descripción = "Porcentaje de la poblacion con acceso conveniente al transporte público")

emisiones <- read.csv("Datos/sub - emisiones de co2.csv")|> 
  rename(Valor = Per.capita.carbon.dioxide.emissions.from.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Emisiones CO2 por el transporte (per capita)",
         Descripción = "Emisiones de Dioxido de carbono per capita emitidas por el transporte")

gasto_investigacion <- read.csv("Datos/sub - gasto en investigacion.csv") |> 
  rename(Valor = Research.and.development.expenditure....of.GDP.,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje del GDP invertido en investigación y desarrollo",
         Descripción = "Porcentaje del GDP invertido en investigación y desarrollo")


# Carga de datos https://github.com/argendatafundar/data?tab=readme-ov-file ------------------


# Carga de traducciones ----------

Traducciones <- read_xlsx("Datos/Traducciones variables.xlsx")



# Preparación de bases de datos --------------
prep <- function(x) {
  x <- x |> 
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
    left_join(Traducciones, by = "Indicador") |> 
    mutate(Indicador = Trad) |> 
    select(-c("Trad")) |> 
    filter(!is.na(Valor)) # Saco las filas sin datos
  
  # Dejo solo los indicadores que tengan al menos la misma cantidad de datos que de paises 
  for (i in unique(x$Indicador)) {
    data <- filter(x, Indicador == i, !is.na(Valor))
    
    if (nrow(data) < length(unique(x$Pais))) {
      x <- filter(x, Indicador != i)
    }
  }
  
  return(x)
}

Ciencia <- prep(Ciencia) 
Ciencia <- Ciencia |> 
  bind_rows(
    filter(transporte_publico, Codigo %in% unique(Ciencia$Codigo)),
    filter(emisiones, Codigo %in% unique(Ciencia$Codigo)),
    filter(gasto_investigacion, Codigo %in% unique(Ciencia$Codigo)))

Vida <- prep(Vida) |> arrange(Pais)
Economia <- prep(Economia) |> arrange(Pais)
Educacion <- prep(Educacion) |> arrange(Pais)
Poblacion <- prep(Poblacion) |> arrange(Pais)
Pobreza <- prep(Pobreza) |> arrange(Pais)
Trabajo <- prep(Trabajo) |> arrange(Pais)

vida_poblacion <- rbind(Vida,Poblacion)
ciencia_educacion <- rbind(Ciencia,Educacion)


# Tema de los graficos -------------

tema_graficos <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#272c30", colour = NA),
    panel.background = element_rect(fill = "#272c30"),
    panel.grid.major = element_line(color = "grey5"),
    legend.background = element_rect(fill = "grey15", colour = NA),
    axis.text = element_text(color = "grey70", size = 8),
    axis.title = element_text(color = "#bcc3c7", size = 11),
    legend.text = element_text(color = "#bcc3c7", size = 9),
    legend.title = element_text(color = "#bcc3c7", size = 11)
  )

theme_set(tema_graficos)


# Tema de las tablas --------------

estilotablas <- function() {
  tags$head(
    tags$style(HTML("
      table.dataTable {
        background-color: #272c30; /* Cambia el color de fondo de las tablas */
      }
      table.dataTable th {
        background-color: #262626; /* Cambia el fondo de los encabezados */
        color: #bcc3c7; /* Cambia el color del texto de los encabezados */
      }
      table.dataTable td {
        color: #bcc3c7;                /* Color del texto en las celdas */
      }
      .dataTables_filter input {
        background-color: #262626; /* Fondo de la caja de búsqueda */
        color: #bcc3c7;               /* Color del texto en la caja de búsqueda */
      }
      .dataTables_filter label {
        color: #bcc3c7; /* Cambia a cualquier color que prefieras */
      }
      
    "))
  )
} 

# Analisis de componentes principales -----------------------

library(FactoMineR)

## Preparacion de los datos para ACP -------------------
datos_acp <- bind_rows(Ciencia, Vida, Economia, Educacion, Poblacion, Pobreza, Trabajo) |> 
  filter(Año == 2022) |> 
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE) |> # Elimina las filas duplicadas
  select(!c(Año, Descripción)) |> 
  pivot_wider(names_from = Indicador,
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
  setView(lng = -58, lat = -23, zoom = 2.6) |> 
  addProviderTiles("Stadia.AlidadeSmoothDark")

# Graficos --------------------

graf_evolutivo = function(base_datos, indicador) {
  base_datos |> 
    filter(Indicador == indicador) |> 
    complete(Año = full_seq(Año, 1), Pais) |> 
    group_by(Pais) |> 
    mutate(Linea = ifelse((row_number() >= which.min(is.na(Valor))) & (row_number() <= max(which(!is.na(Valor)))), na.approx(Valor, rule = 2), Valor)) |> # interpola los datos entre medio que faltan
    ungroup() |> 
    ggplot(aes(x = Año, y = Valor, color = Pais, group = Pais)) +
    geom_point() +
    geom_line(aes(x = Año, y = Linea, color = Pais, group = Pais), na.rm = TRUE) +
    ylab(label = indicador) +
    xlab(label = "Año") +
    scale_x_continuous(breaks = floor(seq(min(filter(base_datos, Indicador == indicador)$Año), max(filter(base_datos, Indicador == indicador)$Año), length.out = 6)))
}

graf_mapa = function(base_datos, indicador, anio) {
  
  datos <- left_join(datos_mapa, filter(base_datos, Indicador == indicador, Año == anio), by = c("name_long" = "Pais")) |> 
    rename(Pais = name_long)
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = filter(base_datos, Indicador == indicador)$Valor
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

## Graf torta ---------

graf_torta <- function(Indicador_torta, Año_torta, Pais_torta) {
  datos_torta <- NULL
  datos_torta<- vida_poblacion |> 
    mutate(Valor = Valor/100 * filter(vida_poblacion, Pais == Pais_torta,Indicador == "Población, total", Año == Año_torta)$Valor) |> 
    filter(str_detect(Indicador, "%") | str_detect(Indicador, "porcentaje") | str_detect(Indicador, "Porcentaje"),
           Pais == Pais_torta, Año == Año_torta)

  datos_torta <- datos_torta |> mutate(Par = 1:nrow(datos_torta))
  
  datos_torta_complemento <- datos_torta |> 
    mutate(Valor = abs(Valor - filter(vida_poblacion, Indicador == "Población, total", Pais == Pais_torta, Año == Año_torta)$Valor),
           Indicador = paste("No", Indicador))

  datos_torta <- bind_rows(datos_torta,datos_torta_complemento) |> 
    filter(Par == filter(datos_torta, Indicador == Indicador_torta)$Par[1])

  if (str_detect(Indicador_torta, "Población rural/urbana")) {
    datos_torta <- vida_poblacion |>
      filter(Pais == Pais_torta,Indicador == "Población rural, total" | Indicador == "Población Urbana, total", Año == Año_torta)
  } else if (str_detect(Indicador_torta, "Población inmigrante")) {
    datos_torta <- vida_poblacion |>
      filter(Pais == Pais_torta,Indicador == "Stock de migrantes internacionales a mitad de año, total", Año == Año_torta) |> 
      mutate(Indicador = "Migrantes")

    datos_torta_complemento <- datos_torta |>
      mutate(Valor = abs(Valor - filter(vida_poblacion, Indicador == "Población, total", Pais == Pais_torta, Año == Año_torta)$Valor),
             Indicador = "Población oriunda")

    datos_torta <- bind_rows(datos_torta, datos_torta_complemento)

  }
  print(datos_torta)
  datos_torta |> 
    plot_ly(labels = ~Indicador, values = ~Valor, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE)
}

## Graf semi torta -------------

graf_dona <- function(base_datos, Pais_dona, Anio_dona, indicador_dona, maximo = "proporcion") {
  
  if (nrow(filter(base_datos, Pais == Pais_dona, Indicador == indicador_dona)) <1) {
    graf <- ggplot(base_datos) +
      aes(x = 1, y = 1) +
      annotate(geom = "text", label = "Sin información", x = 0, y = 0, size = 10) +
      scale_x_continuous(limits = c(-3,3)) +
      scale_y_continuous(limits = c(-3,3)) +
      theme(axis.line = element_blank(),
            panel.grid.major = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    return(ggplotly(graf))
  }
  
  base_datos <- filter(base_datos, Pais == Pais_dona, Indicador == indicador_dona) |> 
    complete(Año = full_seq(Año, 1), Pais) |>  
    fill(Valor, .direction = "down") # Si el año no tiene medicion toma la del ultimo año que tenga
  
  if (maximo == "proporcion") {
    maximo <- 100
  } else {
    maximo <- max(base_datos$Valor)
  }
  
  plot_ly(
    
    domain = list(x = c(0, 1), y = c(0, 1)),
    
    value = filter(base_datos, Año == Anio_dona)$Valor,
    
    type = "indicator",
    
    mode = "gauge+number+delta",
    
    delta = list(reference = ifelse(nrow(filter(base_datos, Año == Anio_dona-1)) > 0, 
                                    filter(base_datos, Año == Anio_dona-1)$Valor,
                                    filter(base_datos, Año == Anio_dona)$Valor)),
    gauge = list(
      
      bar = list(color = "#41cadb"),
      
      axis =list(range = list(NULL, maximo), tickcolor = "#bcc3c7"),
      
      steps = list(
        
        list(range = c(0, max(base_datos$Valor)), color = "#1B2845"))
      )) |> 
    config(responsive = F) |> 
    layout(
      
      margin = list(l=30,r=40),
      
      paper_bgcolor = "#272c30",
      
      font = list(color = "#bcc3c7", family = "Arial"))
  
    
  
}

carrusel_item <- function(indicador, num) {
    carouselItem(
      
      h4(indicador), br(),
      
      column(width = 8, offset = 2,
             sliderTextInput(inputId = paste0("carrusel_educacion_anio_", num),
                             label = "Año",
                             grid = T,
                             choices = sort(unique(filter(Educacion, Indicador == indicador)$Año)),
                             selected = max(filter(Educacion, Indicador == indicador)$Año),
                             animate = T)),
      br(),
      
      flipBox(
        id = paste0("carrusel",num),
        width = 12,
        front = div(
          class = "d-flex justify-content-center",
          height = "300px",
          width = "100%",
          plotlyOutput(paste0("dona_carrusel_",num))),
        back = box(width = NULL,title = indicador, height = 500, 
                   style = "height: 500px; width: 100%; padding: 0; margin: 0;",
                   p(filter(Educacion, Indicador == indicador)$Descripción[1])))
        # div(
        #   class = "text-center",
        #   height = "99999999px",
        #   width = "100%",
        #   style = "background-color: #272c30; padding: 0; margin: 0;",
        #   h1(indicador),
        #   p(filter(Educacion, Indicador == indicador)$Descripción[1])
        # ))
    )
  }



# Datos historicos ----------------

datos_historicos <- data.frame(
  nom_periodo = c("El comienzo en el internet"),
  
  
  
  des_periodo = c("Se conforman las primeras conexiones por internet en la región. Brasil fue el primer país sudamericano en hacerlo mediante la creación de la Fundación de Amparo a la Investigación del Estado de São Paulo (FAPESP), que creó una red de internet con el objetivo inicial de conectar a los investigadores brasileños entre sí. Chile tuvo su primera conexión a internet en el año 1992 en el Centro de Computación de la Facultad de Ciencias Físicas y Matemáticas de la Universidad de Chile (CEC) mientras que Argentina logró conectar los sectores académicos a principios de 1994. <br> <br> Fuentes: <br> https://www.rnp.br/es/noticias/evolucion-de-la-internet-en-brasil <br> https://namidia.fapesp.br/cuando-llego-brasil-a-internet/482045 <br> https://www.latercera.com/que-pasa/noticia/transmitiendo-el-primer-paquete-ip-del-internet-chileno-a-30-anos-de-la-primera-conexion-en-la-web-del-pais/NGBLH3IE3RDFHKVCMNYRWI25KI/ <br> https://agencia.unq.edu.ar/?p=9623")
)



# Interfaz ------------


ui <- dashboardPage(
  
  
  dashboardHeader(),
  skin = "midnight",
  
  ## Barra de opciones ----------------
  dashboardSidebar(collapsed = T,
                   sidebarMenu(
                     id = "sidebar",
                     menuItem("Página principal", tabName = "pag_principal", icon = icon("home")),
                     menuItem("Desarrollo", tabName = "ciencia", icon = icon("microscope")),
                     menuItem("Vida y Población", tabName = "vida", icon = icon("hand-holding-heart")),
                     menuItem("Economia", tabName = "economia", icon = icon("money-bill-wave")),
                     menuItem("Pobreza", tabName = "pobreza", icon = icon("person-shelter")),
                     menuItem("Trabajo", tabName = "trabajo", icon = icon("briefcase")),
                     menuItem("Análisis de datos", tabName = "analisis", icon = icon("magnifying-glass-chart")),
                     menuItem("Bases de datos", tabName = "datos", icon = icon("database"))
                     )
                   ),
  
  
  dashboardBody(
    estilotablas(),
    
    tabItems(
      
      # Página principal ------------------
      tabItem(
        tabName = "pag_principal",
        
        h2("Que los datos te cuenten la historia", style = "text-align: center;"),
        p("Los países del sur de América fueron siempre afectados por las políticas de países exteriores a la zona"),
        
        ## Primera hilera de botones ---------------------
        fluidRow(
          column(3,
                 actionBttn("ciencia", "Desarrollo", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("vida", "Vida y Población", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("economia", "Economia", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("educacion", "Pobreza", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          )
        ),
        
        ## Segunda hilera de botones ---------------------
        fluidRow(
          column(3,
                 actionBttn("trabajo", "Trabajo", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("analisis", "Análisis de datos", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("datos", "Datos y Glosario", block = T, style = "fill", size = "lg", no_outline = F, color = "royal"),
          ),
          column(3,
                 actionBttn("github", "Github", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", icon = icon("github"))
          )
        ),
        
      ),
      
      
      # Pagina Desarrollo -------------
      
      tabItem(
        tabName = "ciencia",
        
        h2("Avances tecnológicos"),
        
        fluidRow(
          box(width = 12,
              fluidRow(column(7, 
                              pickerInput(inputId = "indicador_ciencia",
                                          label = "Variable",
                                          choices = unique(Ciencia$Indicador),
                                          selected = unique(Ciencia$Indicador)[1])),
                       column(5,
                              sliderTextInput(inputId = "mapa_ciencia_anio",
                                              label = "Año",
                                              grid = T,
                                              choices = sort(unique(Ciencia$Año)),
                                              selected = max(filter(Ciencia, Indicador == unique(Ciencia$Indicador)[1])$Año)
                              ))),
              
              fluidRow(
                column(7,
                       plotlyOutput("plot_evo_ciencia")),
                
                column(5,
                       leafletOutput("mapa_ciencia", width = "100%", height = "400px")
                )
              ),
              
              accordion(
                id = "acordion_ciencia",
                accordionItem(
                  title = "Descripción del indicador",
                  collapsed = F,
                  textOutput("descripcion_indicador_ciencia")
                ),
                accordionItem(
                  title = "¡Que los datos te cuenten la historia!",
                  collapsed = F,
                  uiOutput("historia_ciencia")
                )
              )
              
              )
          ),
        
        ## Educacion -------------------
        h2("Educación"),
        box(width = 12,
            column(4, 
                   pickerInput(inputId = "indicador_educacion",
                               label = "Variable",
                               choices = c(
                                 "Años promedio de escolaridad",
                                 "Años esperados de escolarización",
                                 "Índice de Desarrollo Humano",
                                 "Tasa de alfabetización",
                                 "Población con al menos educación secundaria")
                   )),
            column(3,
                   pickerInput(inputId = "pais_educacion",
                               label = "País",
                               choices = sort(unique(Educacion$Pais))
                   )), 
          column(7,
                 plotlyOutput("plot_evo_educacion"),
                 accordion(
                   id = "acordion_educacion",
                   accordionItem(
                     title = "Descripción del indicador",
                     collapsed = F,
                     textOutput("descripcion_indicador_educacion")
                   ),
                   accordionItem(
                     title = "¡Que los datos te cuenten la historia!",
                     collapsed = F,
                     textOutput("historia_educacion")
                   )
                 )
                 ),
          
          column(5,
                 carousel(
                   width = 12,
                   id = "carrusel_educacion",
                   
                   carrusel_item(indicador = "Coeficiente de desigualdad humana", num = 1),
                   carrusel_item(indicador = "Proporción de la fuerza laboral con educación avanzada", num = 2),
                   carrusel_item(indicador = "Índice de Inclusión Social y Equidad", num = 3),
                   carrusel_item(indicador = "Gasto en educación terciaria (% del gasto gubernamental en educación)", num = 4),
                   carrusel_item(indicador = "Índice de Desarrollo de Género", num = 5),
                   carrusel_item(indicador = "Desigualdad en educación", num = 6),
                   carrusel_item(indicador = "Índice de Desarrollo Humano Ajustado por Desigualdad", num = 7),
                   carrusel_item(indicador = "Índice de Desarrollo Humano Ajustado por Desigualdad, diferencia con el valor del IDH no ajustado", num = 8),
                   carrusel_item(indicador = "Índice de Desarrollo Humano ajustado por presiones planetarias", num = 9),
                   carrusel_item(indicador = "Índice de Desarrollo Humano ajustado por presiones planetarias, diferencia con el IDH no ajustado", num = 10),
                   carrusel_item(indicador = "Inscripción escolar, terciaria (% bruto)", num = 11)
                 )
          )
        ),
        

        ),
      
      # Pagina Vida y poblacion --------------------------
      
      tabItem(
        tabName = "vida",
        h2("Población y vida"),
        
        fluidRow(
          column(10,
                 pickerInput(inputId = "indicador_vida",
                             label = "Variable",
                             choices = c("Población",                                                                                                                              
                                        "Índice de pobreza multidimensional",
                                        "Población de refugiados por país o territorio de asilo"))
                 )
        ),
        
        fluidRow(
          column(width = 10, offset = 1, plotlyOutput("plot_evo_vida"))
          
        ),
        br(),
        fluidRow(column(4,
                        pickerInput(inputId = "vida_pais",
                                    label = "País",
                                    unique(vida_poblacion$Pais))),
                 ),
        
        fluidRow(
          column(4, box(width = NULL,
                        pickerInput(inputId = "indicador_vida_total",
                                    label = "Población total",
                                    choices = c("Población rural/urbana" ,unique(filter(vida_poblacion, 
                                                                                        str_detect(Indicador, "\\(% de la población\\)") | str_detect(Indicador, " total, porcentaje") | str_detect(Indicador, "% de la población total"),
                                    )$Indicador),
                                    "Población inmigrante")), br(),
                        sliderTextInput(inputId = "dona_vida_anio_1",
                                        label = "Año",
                                        grid = T,
                                        choices = sort(unique(filter(vida_poblacion, Indicador =="Población Urbana, total")$Año)),
                                        selected = max(unique(filter(vida_poblacion, Indicador =="Población Urbana, total")$Año))
                        ),
                        plotlyOutput("graf_torta_total")
                        )),
          column(4, box(width = NULL,
                        pickerInput(inputId = "indicador_vida_urbano",
                                    label = "Población Urbana",
                                    choices = c(unique(filter(vida_poblacion, 
                                                              str_detect(Indicador, ", urbano, porcentaje") | str_detect(Indicador, "\\(% de la población urbana\\)") | str_detect(Indicador, "% de la población urbana"),
                                    )$Indicador))), br(),
                        sliderTextInput(inputId = "dona_vida_anio_2",
                                        label = "Año",
                                        grid = T,
                                        choices = sort(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, urbano (% de la población urbana)")$Año)),
                                        selected = max(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, urbano (% de la población urbana)")$Año))
                        ),
                        plotlyOutput("graf_torta_urbano")
                        )),
          column(4, box(width = NULL,
                        pickerInput(inputId = "indicador_vida_rural",
                                    label = "Población Rural",
                                    choices = c(unique(filter(vida_poblacion, 
                                                              str_detect(Indicador, ", rural, porcentaje") | str_detect(Indicador, "\\(% de la población rural\\)") | str_detect(Indicador, "% de la población rural"),
                                    )$Indicador))), br(),
                        sliderTextInput(inputId = "dona_vida_anio_3",
                                        label = "Año",
                                        grid = T,
                                        choices = sort(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, rural (% de la población rural)")$Año)),
                                        selected = max(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, rural (% de la población rural)")$Año))
                        ),
                        plotlyOutput("graf_torta_rural")
                        ))
        ),
        
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
                     Desarrollo = unique(ciencia_educacion$Indicador), 
                     "Vida Y Población" = unique(vida_poblacion$Indicador), 
                     Economia = unique(Economia$Indicador), 
                     Poblacion = unique(Poblacion$Indicador), 
                     Pobreza = unique(Pobreza$Indicador), 
                     Trabajo = unique(Trabajo$Indicador)
                   ), selected = "Access to internet, percent of population",
                   options = list(
                     `live-search` = TRUE,
                     size = 7))
                 ),
                 fluidRow(
                   pickerInput(inputId = "var_corr_2", label = "Variable eje y", choices = list(
                     Desarrollo = unique(ciencia_educacion$Indicador), 
                     "Vida Y Población" = unique(vida_poblacion$Indicador), 
                     Economia = unique(Economia$Indicador), 
                     Poblacion = unique(Poblacion$Indicador), 
                     Pobreza = unique(Pobreza$Indicador), 
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
        
        box(width = 12,
            fluidRow(
              column(1),
              
              column(8,
                     pickerInput(
                       inputId = "base_datos",
                       label = "Seleccionar Base de datos", 
                       choices = c("Desarrollo", "Vida y población")
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
  
  observeEvent(input$pobreza, {
    updateTabItems(session, "sidebar", selected = "pobreza")
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
  
  
  ## Desarrollo -------------------------------
  
  ### Grafico evolutivo ----------------
  
  output$plot_evo_ciencia <- renderPlotly({
    
    graf = graf_evolutivo(Ciencia, input$indicador_ciencia)
    
    #### Internet Que los datos te cuenten la historia ----------------------
    if (str_detect(input$indicador_ciencia, "internet") | str_detect(input$indicador_ciencia, "Internet")) {
      Periodos <- data.frame(
        Comienzo = c(1990:1994, 1998:1999),
        Periodo = c(rep("El comienzo en el internet", 5), rep("Evolucion", 2)))
      
      maximo <- max(filter(Ciencia, Indicador == input$indicador_ciencia)$Valor)
      
      graf <- graf +
        geom_bar(data = Periodos, inherit.aes = F,
                 aes(x= Comienzo, y = maximo, customdata = Periodo),
                 stat = "identity", position = "stack", width = 1,
                 fill = "cyan3", alpha = 0.3,
                 just = 0) +
        scale_x_continuous(limits = c(min(filter(Ciencia, Indicador == input$indicador_ciencia)$Año),
                                      max(filter(Ciencia, Indicador == input$indicador_ciencia)$Año)))
    }
    
    ggplotly(graf, tooltip = c("Año", "Valor", "color", "customdata"), source = "plot_evo_ciencia_click") |> 
      event_register("plotly_click")
    
  })
  
  ### Mapa ---------------------
  
  # Actualizador de la barra para seleccionar año
  observeEvent(input$indicador_ciencia, {
    base_datos <- Ciencia

    updateSliderTextInput(
      session = session,
      inputId = "mapa_ciencia_anio",
      choices = sort(unique(filter(base_datos, Indicador == input$indicador_ciencia)$Año)),
      selected = max(unique(filter(base_datos, Indicador == input$indicador_ciencia)$Año))
    )
  })
  
  output$mapa_ciencia <- renderLeaflet({
   
    graf_mapa(ciencia_educacion, input$indicador_ciencia, input$mapa_ciencia_anio)
    
  })
  
  ### Descripcion acordion -----------------
  
  output$descripcion_indicador_ciencia <- renderText({
    filter(Ciencia, Indicador == input$indicador_ciencia)$Descripción[1]
  })
  
  ### Historia acordion -----------------
  
  output$historia_ciencia <- renderUI({
    
    click_data <- event_data("plotly_click", source = "plot_evo_ciencia_click")
    
    if (is.null(click_data)) {
      HTML("Clickea alguna zona marcada para conocer la historia.")
    } else if (click_data$customdata %in% datos_historicos$nom_periodo) {
      HTML(filter(datos_historicos, nom_periodo == click_data$customdata)$des_periodo) 
    } else {
      HTML("Clickea alguna zona marcada para conocer la historia.")
    }
    

  })
  
  ###Funcion carrusel items -------------
  
  observeEvent(
    input$pais_educacion, {
      carrusel_item <- function(indicador, num) {
      carouselItem(
        
        h4(indicador), br(),
        
        column(width = 8, offset = 2,
               sliderTextInput(inputId = paste0("carrusel_educacion_anio_", num),
                               label = "Año",
                               grid = T,
                               choices = sort(unique(filter(Educacion, Pais == input$pais_educacion, Indicador == indicador)$Año)),
                               selected = max(filter(Educacion, Pais == input$pais_educacion, Indicador == indicador)$Año),
                               animate = T)),
        br(),
        
        flipBox(
          id = paste0("carrusel",num),
          width = 12,
          front = div(
            class = "d-flex justify-content-center",
            height = "300px",
            width = "100%",
            plotlyOutput(paste0("dona_carrusel_",num))),
          back = box(width = NULL,title = indicador, height = 500, 
                     style = "height: 500px; width: 100%; padding: 0; margin: 0;",
                     p(filter(Educacion, Indicador == indicador)$Descripción[1]))
          )
      )
    }
  })
  
  
  ### Graf evo Educacion -----------------
  
  output$plot_evo_educacion <- renderPlotly({
    base_datos <- Educacion |> 
      filter(Pais == input$pais_educacion, str_detect(Indicador, input$indicador_educacion) & 
               Indicador != "Índice de Desarrollo Humano ajustado por presiones planetarias" & Indicador != "Índice de Desarrollo Humano ajustado por presiones planetarias, diferencia con el IDH no ajustado" &
               Indicador != "Índice de Desarrollo Humano Ajustado por Desigualdad" & Indicador != "Índice de Desarrollo Humano Ajustado por Desigualdad, diferencia con el valor del IDH no ajustado") |> 
      mutate(Sexo = case_when(
        str_detect(Indicador, "femenin") ~ "Mujeres",
        str_detect(Indicador, "masculin") ~ "Varones",
        TRUE ~ "Ambos sexos"))
    
    graf <- base_datos |>
      ggplot(aes(x = Año, y = Valor, color = Sexo, group = Sexo)) +
      geom_point() +
      geom_line() +
      ylab(label = input$indicador_educacion) +
      xlab(label = "Año") +
      scale_x_continuous(breaks = floor(seq(min(base_datos$Año), max(base_datos$Año), length.out = 6))) +
      scale_color_manual(values = c("Mujeres" = "pink", "Varones" = "dodgerblue3", "Ambos sexos" = "#FEFADC"))
    
    ggplotly(graf, tooltip = c("Año", "Valor", "color"), source = "plot_evo_educacion") |> 
      layout(legend = list(orientation = "h",   # Horizontal
                           x = 0.5,             # Centrado horizontalmente
                           y = -0.2,            # Posición vertical (abajo del gráfico)
                           xanchor = "center",  # Ancla en el centro
                           yanchor = "top"))

  })
  
  output$info_extra_educacion <- renderPrint({
    event_data(event = "plotly_click", source = "plot_evo_educacion")
  })
  
  ### Descripcion acordion Educacion -----------------
  
  output$descripcion_indicador_educacion <- renderText({
    filter(Educacion, str_detect(Indicador, input$indicador_educacion))$Descripción[1]
  })
  
  ### Historia acordion Educacion -----------------
  
  output$historia_educacion <- renderText({
    
    if (T) {
      "Clickea alguna zona marcada para conocer la historia."
    }
    
  })
  
  ### Carrusel Educacion ----------------
  
  
  output$dona_carrusel_1 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_1, indicador_dona = "Coeficiente de desigualdad humana",
              maximo = "")
  })
  
  output$dona_carrusel_2 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_2, indicador_dona = "Proporción de la fuerza laboral con educación avanzada",
              maximo = "proporcion")
  })
  output$dona_carrusel_3 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_3, indicador_dona = "Índice de Inclusión Social y Equidad",
              maximo = "")
  })
  output$dona_carrusel_4 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_4, indicador_dona = "Gasto en educación terciaria (% del gasto gubernamental en educación)",
              maximo = "proporcion")
  })
  output$dona_carrusel_5 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_5, indicador_dona = "Índice de Desarrollo de Género",
              maximo = "")
  })
  output$dona_carrusel_6 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_6, indicador_dona = "Desigualdad en educación",
              maximo = "")
  })
  output$dona_carrusel_7 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_7, indicador_dona = "Índice de Desarrollo Humano Ajustado por Desigualdad",
              maximo = "")
  })
  output$dona_carrusel_8 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_8, indicador_dona = "Índice de Desarrollo Humano Ajustado por Desigualdad, diferencia con el valor del IDH no ajustado",
              maximo = "")
  })
  output$dona_carrusel_9 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_9, indicador_dona = "Índice de Desarrollo Humano ajustado por presiones planetarias",
              maximo = "")
  })
  output$dona_carrusel_10 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_10, indicador_dona = "Índice de Desarrollo Humano ajustado por presiones planetarias, diferencia con el IDH no ajustado",
              maximo = "")
  })
  output$dona_carrusel_11 <- renderPlotly({
    graf_dona(base_datos = Educacion, Pais_dona = input$pais_educacion, 
              Anio_dona = input$carrusel_educacion_anio_11, indicador_dona = "Inscripción escolar, terciaria (% bruto)",
              maximo = "proporcion")
  })
  
  ## Vida y poblacion -------------------------------
  
  ### Grafico evolutivo ----------------
  
  output$plot_evo_vida <- renderPlotly({
    
    if (input$indicador_vida == "Población") {
      base_datos = vida_poblacion %>%
        mutate(Valor = round(Valor/1000000, 2)) |>
        filter(!is.na(Valor))
      
      graf = graf_evolutivo(base_datos,"Población, total") +
        ylab(label = "Población total (millones)")
      
      ggplotly(graf, tooltip = c("Año", "Valor", "color"))
    } else {
      graf = graf_evolutivo(vida_poblacion, input$indicador_vida)
      
      ggplotly(graf, tooltip = c("Año", "Valor", "color"))
    }
    
    
    
  })
  
  
  ### Graficos dona poblacion ------------------------
  
  # Actualizador de la barra para seleccionar año
  observeEvent(input$indicador_vida_total, {
    
    base_datos <- vida_poblacion
    
    updateSliderTextInput(
      session = session,
      inputId = "dona_vida_anio_1",
      choices = sort(unique(filter(base_datos, Pais == input$vida_pais, Indicador == input$indicador_vida_total)$Año)),
      selected = max(unique(filter(base_datos, Pais == input$vida_pais, Indicador == input$indicador_vida_total)$Año))
    )
    
    if (input$indicador_vida_total == "Población rural/urbana") {
      updateSliderTextInput(
        session = session,
        inputId = "dona_vida_anio_1",
        choices = sort(unique(filter(base_datos, Pais == input$vida_pais, Indicador == "Población Urbana, total")$Año)),
        selected = max(unique(filter(base_datos, Pais == input$vida_pais, Indicador == "Población Urbana, total")$Año))
      )
    } else if (input$indicador_vida_total == "Población inmigrante") {
      updateSliderTextInput(
        session = session,
        inputId = "dona_vida_anio_1",
        choices = sort(unique(filter(base_datos, Pais == input$vida_pais, Indicador == "Stock de migrantes internacionales a mitad de año, total")$Año)),
        selected = max(unique(filter(base_datos, Pais == input$vida_pais, Indicador == "Stock de migrantes internacionales a mitad de año, total")$Año))
      )
    }
      
    
  })
  observeEvent(input$indicador_vida_urbano, {
    
    base_datos <- vida_poblacion
    
    updateSliderTextInput(
      session = session,
      inputId = "dona_vida_anio_2",
      choices = sort(unique(filter(base_datos, Pais == input$vida_pais, Indicador == input$indicador_vida_urbano)$Año)),
      selected = max(unique(filter(base_datos, Pais == input$vida_pais, Indicador == input$indicador_vida_urbano)$Año))
    )
  })
  observeEvent(input$indicador_vida_rural, {
    
    base_datos <- vida_poblacion
    
    updateSliderTextInput(
      session = session,
      inputId = "dona_vida_anio_3",
      choices = sort(unique(filter(base_datos, Pais == input$vida_pais, Indicador == input$indicador_vida_rural)$Año)),
      selected = max(unique(filter(base_datos, Pais == input$vida_pais, Indicador == input$indicador_vida_rural)$Año))
    )
  })
  
  # Donas
  
  output$graf_torta_total <- renderPlotly({
    
    graf_dona(base_datos = vida_poblacion, Pais_dona = input$vida_pais, 
              Anio_dona = input$dona_vida_anio_1, indicador_dona = input$indicador_vida_total,
              maximo = "proporcion")
    
    
    
    if (input$indicador_vida_total == "Población rural/urbana") {
      
      base_datos <- filter(vida_poblacion, Pais == input$vida_pais, Indicador == "Población rural, total" | Indicador == "Población Urbana, total") |> 
        complete(Año = full_seq(Año, 1), Pais) |>  
        fill(Valor, .direction = "down")
      
      
      if (nrow(base_datos) <1) {
        graf <- ggplot(base_datos) +
          aes(x = 1, y = 1) +
          annotate(geom = "text", label = "Sin información", x = 0, y = 0, size = 10) +
          scale_x_continuous(limits = c(-3,3)) +
          scale_y_continuous(limits = c(-3,3)) +
          theme(axis.line = element_blank(),
                panel.grid.major = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank())
        
        return(ggplotly(graf))
      }
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = filter(base_datos, Año == input$dona_vida_anio_1, Indicador == "Población Urbana, total")$Valor,
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = ifelse(nrow(filter(base_datos, Indicador == "Población Urbana, total", Año == input$dona_vida_anio_1-1)) > 0, 
                                        filter(base_datos, Indicador == "Población Urbana, total", Año == input$dona_vida_anio_1-1)$Valor,
                                        filter(base_datos, Indicador == "Población Urbana, total", Año == input$dona_vida_anio_1)$Valor)),
        gauge = list(
          bar = list(color = "#41cadb"),
          axis =list(range = list(NULL, sum(filter(base_datos, Año == input$dona_vida_anio_1)$Valor)), tickcolor = "#bcc3c7"),
          steps = list(
            list(range = c(filter(base_datos, Año == input$dona_vida_anio_1, Indicador == "Población Urbana, total")$Valor ,sum(filter(base_datos, Año == input$dona_vida_anio_1)$Valor)),
                 color = "#ACFF47"),
            list(range = c(0, filter(base_datos, Año == input$dona_vida_anio_1, Indicador == "Población Urbana, total")$Valor),
                 color = "#41cadb"))
          )
        ) |> 
        config(responsive = F) |> 
        layout(
          margin = list(l=30,r=40),
          paper_bgcolor = "#272c30",
          font = list(color = "#bcc3c7", family = "Arial"))
      
      
      
    } else if (input$indicador_vida_total == "Población inmigrante") {
      
      graf_dona(base_datos = vida_poblacion, Pais_dona = input$vida_pais, 
                Anio_dona = input$dona_vida_anio_1, indicador_dona = "Stock de migrantes internacionales a mitad de año, total",
                maximo = "")

    }
    
    
  })
  
  output$graf_torta_urbano <- renderPlotly({
    graf_dona(base_datos = vida_poblacion, Pais_dona = input$vida_pais, 
              Anio_dona = input$dona_vida_anio_2, indicador_dona = input$indicador_vida_urbano,
              maximo = "proporcion")
  })
  
  output$graf_torta_rural <- renderPlotly({
    graf_dona(base_datos = vida_poblacion, Pais_dona = input$vida_pais, 
              Anio_dona = input$dona_vida_anio_3, indicador_dona = input$indicador_vida_rural,
              maximo = "proporcion")
  })
  
  ## Analisis -------------------------
  
  ### Grafico correlaciones ----------------
  
  datos_corr <- reactive({
    
    if (input$var_corr_1 %in% ciencia_educacion$Indicador) {
      datos_corr_1 = filter(ciencia_educacion, Indicador == input$var_corr_1)
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
    } else if (input$var_corr_1 %in% Trabajo$Indicador) {
      datos_corr_1 = filter(Trabajo, Indicador == input$var_corr_1)
    }
    
    
    if (input$var_corr_2 %in% ciencia_educacion$Indicador) {
      datos_corr_2 = filter(ciencia_educacion, Indicador == input$var_corr_2)
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
    }else if (cor(datos_corr$Valor.x, datos_corr$Valor.y) <= 1) {
      
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
    if (length(input$base_datos) == 0) {ciencia_educacion} else{
      if (input$base_datos == "Ciencia") {
        ciencia_educacion
        
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
      } else if (input$base_datos == "trabajo") {
        Trabajo} else {ciencia_educacion}
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
    
    if (input$base_datos == "Desarrollo") {datos = ciencia_educacion
    } else if (input$base_datos == "Vida y población") {datos = vida_poblacion}
    
    datos %>% 
      datatable(selection = "single",escape = F, options = list(scrollX = T,
                                                              scrollY = "450px",
                                                              paging = F,
                                                              scrollCollapse = T,
                                                              lengthMenu = c(10, 15, 30, nrow(datos))
                                                              ))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



# Ideas ------------------

# https://worldmigrationreport.iom.int/msite/wmr-2024-interactive/?lang=ES

# https://es.statista.com/temas/9257/el-uso-de-internet-en-america-latina/#topFacts

# Unir bases de datos: Poblacion-Vida, Ciencia-Educacion, Trabajo, pobreza (Salud no)

# Dim1: Desarrollo, Educacion, Desigualdad

# Dim 2: Economía, educación, Acceso al agua potable

#  Dim 3: Poblacion, 

#  Dim 4: Inflación, Igualdad de genero+


# Agregar tasas de crecimiento decrecimiento 

# Meter todos los plotlys en cajas? agregar anajo de cada caja la descripcion del indicador?

# Agregar regiones de tiempo de eventos, clickear la region abre mas abajo una descripcion

# Series de tiempo para educacion hacer 

# Arreglar-------------

# Data en hover me tira la info de la recta tmb, evitando que pueda comparar mas paises

# en el carrusel los sliders de año tienen años en los que muchos paises no tienen datos

# Colocalr todas las tablas en cajas, cambiar el tema de los graficos

# Modificar el boton de play de los slider input con años

# Arreglar grafico de correlaciones

# Cambiar donde diga Variable a Indicador