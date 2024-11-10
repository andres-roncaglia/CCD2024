# Para el futuro usar:

# bslib https://www.youtube.com/watch?v=O6WLERr5bKU <- alternativa de dashboardpage
# echarts4r <- alternativa plotly
# reactable <- alternativa DT
# Cargar las librerias necesarias de tidy


# Carga de librerias ---------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(DT)
library(stringr)
library(zoo)
library(htmlwidgets)
library(waiter)

# Para el carrusel si mantengo el mouse
jscode <-"
$(document).ready(function(){
            $('#mycarousel').carousel( { interval:  false } );
});"

# Carga de datos https://data.undp.org/access-all-data-------------
Ciencia <- read_xlsx("www/Datos/Ciencia y cambios tecnologicos.xlsx")
Vida <- read_xlsx("www/Datos/Condiciones de vida.xlsx")
Economia <- read_xlsx("www/Datos/Desarrollo economico.xlsx")
Educacion <- read_xlsx("www/Datos/Educacion.xlsx")
Poblacion <- read_xlsx("www/Datos/Poblacion y cambios demograficos.xlsx")
Pobreza <- read_xlsx("www/Datos/Pobreza.xlsx")
Trabajo <- read_xlsx("www/Datos/Trabajo.xlsx")


# Carga de datos https://ourworldindata.org/ ---------------

transporte_publico <- read.csv("www/Datos/Acceso transporte publico.csv") |> 
  rename(Valor = Proportion.of.population.that.has.convenient.access.to.public.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje de la poblacion con acceso conveniente al transporte público",
         Descripción = "Porcentaje de la poblacion con acceso conveniente al transporte público",
         Pais = case_when(Pais == "Brazil" ~ "Brasil",
                          Pais == "Peru" ~ "Perú",
                               T ~ Pais))

emisiones <- read.csv("www/Datos/Emisiones de co2.csv")|> 
  rename(Valor = Per.capita.carbon.dioxide.emissions.from.transport,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Emisiones CO2 por el transporte (per capita)",
         Descripción = "Emisiones de Dioxido de carbono per capita emitidas por el transporte",
         Pais = case_when(Pais == "Brazil" ~ "Brasil",
                          Pais == "Peru" ~ "Perú",
                          T ~ Pais))

gasto_investigacion <- read.csv("www/Datos/Gasto en investigacion.csv") |> 
  rename(Valor = Research.and.development.expenditure....of.GDP.,
         Año = Year,
         Codigo = Code,
         Pais = Entity) |> 
  mutate(Indicador = "Porcentaje del GDP invertido en investigación y desarrollo",
         Descripción = "Porcentaje del GDP invertido en investigación y desarrollo",
         Pais = case_when(Pais == "Brazil" ~ "Brasil",
                          Pais == "Peru" ~ "Perú",
                          T ~ Pais))



# Carga de datos https://statistics.cepal.org/portal/databank ------------------

mod_cepal <- function(data, seleccion = T) {
  data <- data |> 
    rename(Indicador = indicator, Pais = País__ESTANDAR,Año = Años__ESTANDAR, Valor = value) |> 
    mutate(Pais = case_when(str_detect(Pais, "Venezuela") ~ "Venezuela",
                            str_detect(Pais, "Bolivia") ~ "Bolivia",
                            T ~ Pais),
           Codigo = str_to_upper(str_sub(Pais, start = 1, end = 3)),
           Año = as.numeric(Año),
           Indicator = Indicador) 
  
  if (seleccion) {
    data <- data |> 
      select(Indicador, Pais, Año, Valor, Codigo, Indicator)
    return(data)
  } else {
    return(data)
  }
  
}

poblacion_edad <- read_xlsx("www/Datos/Poblacion edad.xlsx") |> 
  mod_cepal(seleccion = F) |> 
  mutate(Descripción = "Número de habitantes por grupos quinquenales de edad que viven efectivamente dentro de los límites fronterizos de un país, territorio o área determinada.")

esperanza_vida <- read_xlsx("www/Datos/Esperanza de vida.xlsx") |> 
  filter(Sexo == "Ambos sexos") |> 
  mod_cepal() |> 
  mutate(Descripción = "Representa la duración media de la vida de los individuos, que integran una cohorte hipotética de nacimientos, sometidos en todas las edades a los riesgos de mortalidad del período en estudio")

natalidad <- read_xlsx("www/Datos/Natalidad.xlsx") |> 
  mod_cepal() |> 
  mutate(Descripción = "Mide la frecuencia de los nacimientos ocurridos en un período en relación a la población total. Es el cociente entre el número de nacimientos ocurridos durante un período dado y la población media de ese período")

tramites <- read_xlsx("www/Datos/Tramites gub.xlsx") |> 
  mod_cepal() |> 
  mutate(Descripción = "Porcentaje de digitalización de trámites gubernamentales. La metodología empleada para este indicador implica la búsqueda manual en línea de trámites ciudadanos recurrentes ofrecidos por diversas entidades gubernamentales. Estos trámites se clasifican en 16 categorías según su naturaleza. Si al menos un trámite de una categoría se encuentra parcial o totalmente digitalizado en un país, se considera que esa categoría está digitalizada. El porcentaje se calcula como el número de categorías digitalizadas sobre el total de categorías analizadas para cada país.")

empresas_unicornio <- read_xlsx("www/Datos/Empresas unicornio.xlsx") |> 
  mod_cepal() |> 
  mutate(Descripción = "Número de empresas unicornio por país de la región. Una empresa unicornio es una startup privada valorada sobre el billón de dólares. Desde CB Insights se obtiene un listado de todas las empresas unicornio y se obtienen las distintas estadísticas descriptivas.")

empresas_ia <- read_xlsx("www/Datos/Empresas IA.xlsx") |> 
  mod_cepal() |> 
  mutate(Descripción = "Número de empresas de inteligencia artificial por país de la región. Se recopilaron las primeras 10.000 empresas que operan en la industria de la 'inteligencia artificial', según la definición de Crunchbase, clasificadas por el índice CB en orden descendente. Posteriormente, se generan estadísticas descriptivas a partir de estos datos.")

canasta_digital <- read_xlsx("www/Datos/Canasta digital.xlsx") |> 
  mod_cepal() |> 
  mutate(Descripción = "La canasta básica digital está compuesta por los servicios de banda ancha fija y móvil y un smartphone, un computador y una tablet. Para el cálculo, se usa la tarifa mensual de los servicios de banda ancha, el prorrateo mensual del costo de los dispositivos (suponiendo una vida útil de 3 años) y el ingreso mensual promedio de los hogares.")

pobreza_categorizada <- read_excel("www/Datos/Pobreza_categorizada.xlsx") |> 
  mod_cepal(seleccion = F) |> 
  mutate(Descripción = "Porcentaje de personas de 25 y más años de edad cuyo ingreso per cápita medio está por debajo de las líneas de pobreza y pobreza extrema respectivamente, según  su nivel educativo máximo alcanzado por sexo.")

valores_pobreza <- read_excel("www/Datos/Valores de pobreza.xlsx")|> 
  mod_cepal(seleccion = F) |> 
  mutate(Descripción = "La línea de pobreza extrema (indigencia) representa el monto mensual que requiere una persona para adquirir una canasta básica de alimentos que satisfaga sus requerimientos nutricionales. La línea de pobreza representa el monto mensual que una persona requiere para adquirir sus necesidades esenciales.")

trabajadores_pobreza <- read_excel("www/Datos/Trabajadores_pobreza.xlsx")|> 
  mod_cepal(seleccion = F) |> 
  mutate(Descripción = "Porcentaje del total de la población ocupada cuyo ingreso per cápita medio está por debajo de la línea de pobreza e indigencia (extrema pobreza).")

pobreza_cepal <- bind_rows(pobreza_categorizada, valores_pobreza, trabajadores_pobreza)

# Carga de traducciones ----------

Traducciones <- read_xlsx("www/Datos/Traducciones variables.xlsx")



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
        Pais == "Brazil" ~ "Brasil", 
        Pais == "Peru" ~ "Perú", 
        T ~ Pais),
      Año = as.numeric(Año)) |> 
    left_join(Traducciones, by = "Indicador") |> 
    mutate(Indicator = Indicador,
           Indicador = Trad) |> 
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
    filter(gasto_investigacion, Codigo %in% unique(Ciencia$Codigo)),
    tramites,
    empresas_ia,
    empresas_unicornio,
    canasta_digital)

Vida <- prep(Vida)
Economia <- prep(Economia)
Educacion <- prep(Educacion)
Poblacion <- prep(Poblacion)
Pobreza <- prep(Pobreza)
Trabajo <- prep(Trabajo)

vida_poblacion <- rbind(Vida, Poblacion, Pobreza,
                        esperanza_vida,
                        natalidad) |> 
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE)

ciencia_educacion <- rbind(Ciencia,Educacion) |> 
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE)

economia_trabajo <- rbind(Economia, Trabajo)|>
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE)


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


# CSS Configuracion --------------

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
        color: #bcc3c7; 
      }
      
      
      /* Configuraciones botones ActionBttn */
      
      .my-custom-btn {
        background-color: #272c30 !important;
        color: #bcc3c7 !important;
        border-color: #272c30 !important;
      }
      .my-custom-btn:hover {
        background-color: #272c30 !important;
        color: black !important;
        border-color: #272c30 !important;
      }
      
      /* Boton play */
      
      .play-bttn {
      
        background: #0F0E0E;
        color: #bcc3c7;
      
      }
      
      .play-bttn:hover {
        color = #0F0E0E;
        background: #bcc3c7;
      }
      
      /* Fix the sidebar to be always visible */
      .main-sidebar {
        position: fixed;
      }
      
    "))
  )
} 

# Analisis de componentes principales -----------------------

library(FactoMineR)

## Preparacion de los datos para ACP -------------------
datos_acp <- bind_rows(ciencia_educacion, Economia, vida_poblacion) |> 
  filter(Año == 2022) |> 
  distinct(Pais, Codigo, Año, Indicador, .keep_all = TRUE) |> # Elimina las filas duplicadas
  select(!c(Año, Descripción, Indicator, Codigo)) |> 
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
  geom_point(aes(text = text),size = 2) +
  xlab("Componente principal") +
  ylab("Variancia explicada por la componente (%)") +
  scale_x_continuous(breaks = 1:nrow(acp$eig)) +
  scale_color_manual(values = c("Menor al 85%" = "#41cadb", "Mayor al 85%" = "firebrick2"),
                     breaks = c("Menor al 85%", "Mayor al 85%")) +
  labs(color = "Porcentaje acumulado de la variancia") +
  theme(legend.position = "bottom")


# Analisis Cluster con las componentes principales ---------------------------

# Cantidad de componentes con las que trabajar

num_cp <- length(which(acp$eig[,"cumulative percentage of variance"] < 85))

variables_componentes <- acp$ind$coord[,1:num_cp]

datos_cluster <- datos_acp |> 
  select(Pais) |> 
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
  mutate(name_long = case_when(name_long == "Brazil" ~ "Brasil",
                               name_long == "Peru" ~ "Perú",
                               T ~ name_long)) |> 
  arrange(name_long) |> 
  filter(iso_a2 != "FK") 

mapa <- datos_mapa |> 
  leaflet() |> 
  addTiles() |> 
  setMaxBounds(lng1 = -22, lng2 = -98, lat1 = -61, lat2 = 20) |> 
  setView(lng = -58, lat = -23, zoom = 2.6) |> 
  addProviderTiles("Stadia.AlidadeSmoothDark", options = providerTileOptions(apiKey = "d1d22c35-1739-4e51-b74f-03743b030beb"))

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
    complete(Año = full_seq(Año, 1)) |>  
    group_by(Pais) |> 
    fill(Valor, .direction = "down") |> # Si el año no tiene medicion toma la del ultimo año que tenga
    ungroup()
  
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
      
      axis =list(range = list(NULL, maximo), tickcolor = "#bcc3c7")
      
      )) |> 
    config(responsive = F) |> 
    layout(
      
      margin = list(l=30,r=40),
      
      paper_bgcolor = "#272c30",
      
      font = list(color = "#bcc3c7", family = "Arial"))
  
    
  
}

## Graf areas --------------
#### Areas empleo tipo empresa ---------------

datos_graf_areas <- filter(Economia, str_detect(Indicador, "Empleo en "), Indicador != "Empleo en MIPYMES, % del total", Pais != "Paraguay")

i <- 1

for (pais in unique(datos_graf_areas$Pais)) {
  datos_graf_areas_pais <- filter(datos_graf_areas, Pais == pais)
  
  graf_areas <- plot_ly(x = unique(datos_graf_areas_pais$Año), y = filter(datos_graf_areas_pais, Indicador == "Empleo en grandes empresas, % del total")$Valor, name = 'Empleo en grandes empresas', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#ACFF47') |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Empleo en microempresas, % del total")$Valor, name = 'Empleo en microempresas', fillcolor = '#6D64B9', opacity = 0.3) |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Empleo en PYMEs, % del total")$Valor, name = 'Empleo en PYMEs', fillcolor = '#41cadb', opacity = 0.3) |> 
    layout(
      font = list(color = "#bcc3c7", family = "Arial"),
      margin = list(l = 0, r = 0),
      paper_bgcolor = "#272c30",
      plot_bgcolor ="#272c30",
      showlegend = F,
      xaxis = list(showgrid = FALSE,
                   fixedrange = T,
                   range = list(min(unique(datos_graf_areas_pais$Año)), max(unique(datos_graf_areas_pais$Año)))),
      yaxis = list(
        range = list(0,102),
        fixedrange = T,
        title = NULL,
        zeroline = FALSE,      # Remove the zero line
        showline = FALSE,      # Remove the axis line
        showgrid = FALSE,      # Remove grid lines
        ticks = "",            # Remove ticks
        showticklabels = FALSE, # Already removed the tick labels
        automargin = T,
        ticksuffix = '%'    # Adjust margins automatically
      ),
      shapes = list(
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 20, y1 = 20, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 40, y1 = 40, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 60, y1 = 60, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 80, y1 = 80, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 50, y1 = 50, 
             line = list(color = "#272c30"))
      ),
      annotations = list(
        list(x = median(datos_graf_areas_pais$Año), y = 102, text = pais, showarrow = FALSE, xanchor = 'middle', font = list(size = 14,color = "#bcc3c7")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 20+2, text = "20%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 40+2, text = "40%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 60+2, text = "60%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 80+2, text = "80%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30"))
        ),
      hoverlabel = list(font = list(size = 15, color = "#bcc3c7"),
                        namelength = -1),
      hovermode = "x unified"
      )
  assign(paste0("plot_areas_",i), graf_areas)
  i = i+1
}

graf_areas <- subplot(plot_areas_1,plot_areas_2, plot_areas_3, plot_areas_4, plot_areas_5, plot_areas_6,
        nrows = 1)

#### Areas tipo empresas ---------------

datos_graf_areas <- filter(Economia, str_detect(Indicador, "% del total de empresas"), Indicador != "MIPYMES, % del total de empresas",Indicador != "PYMEs, % del total de empresas", Pais != "Paraguay", Pais != "Venezuela")

i <- 1

for (pais in unique(datos_graf_areas$Pais)) {
  datos_graf_areas_pais <- filter(datos_graf_areas, Pais == pais)
  
  graf_areas_2 <- plot_ly(x = unique(datos_graf_areas_pais$Año), y = filter(datos_graf_areas_pais, Indicador == "Grandes empresas, % del total de empresas")$Valor, name = 'Grandes empresas', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#ACFF47') |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Empresas medianas, % del total de empresas")$Valor, name = 'Empresas medianas', fillcolor = '#6D64B9', opacity = 0.3) |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Pequeñas empresas, % del total de empresas")$Valor, name = 'Pequeñas empresas', fillcolor = '#41cadb', opacity = 0.3) |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Microempresas, % del total de empresas")$Valor, name = 'Microempresas', fillcolor = '#F1C8DB', opacity = 0.3) |> 
    layout(
      font = list(color = "#bcc3c7", family = "Arial"),
      margin = list(l = 0, r = 0),
      paper_bgcolor = "#272c30",
      plot_bgcolor ="#272c30",
      showlegend = F,
      xaxis = list(showgrid = FALSE,
                   fixedrange = T,
                   range = list(min(unique(datos_graf_areas_pais$Año)), max(unique(datos_graf_areas_pais$Año)))),
      yaxis = list(
        range = list(0,102),
        fixedrange = T,
        title = NULL,
        zeroline = FALSE,      # Remove the zero line
        showline = FALSE,      # Remove the axis line
        showgrid = FALSE,      # Remove grid lines
        ticks = "",            # Remove ticks
        showticklabels = FALSE, # Already removed the tick labels
        automargin = T,
        ticksuffix = '%'    # Adjust margins automatically
      ),
      shapes = list(
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 20, y1 = 20, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 40, y1 = 40, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 60, y1 = 60, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 80, y1 = 80, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 50, y1 = 50, 
             line = list(color = "#272c30"))
      ),
      annotations = list(
        list(x = median(datos_graf_areas_pais$Año), y = 102, text = pais, showarrow = FALSE, xanchor = 'middle', font = list(size = 14,color = "#bcc3c7")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 20+2, text = "20%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 40+2, text = "40%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 60+2, text = "60%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 80+2, text = "80%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30"))
      ),
      hoverlabel = list(font = list(size = 15, color = "#bcc3c7"),
                        namelength = -1),
      hovermode = "x unified"
    )
  assign(paste0("plot_areas_",i), graf_areas_2)
  i = i+1
}

graf_areas_2 <- subplot(plot_areas_1,plot_areas_2, plot_areas_3, plot_areas_4, plot_areas_5, plot_areas_6,plot_areas_7,
                      nrows = 1)


#### Areas empleo sector ---------------


datos_graf_areas <- filter(Trabajo, str_detect(Indicador, "% del empleo total"))

i <- 1

for (pais in unique(datos_graf_areas$Pais)) {
  datos_graf_areas_pais <- filter(datos_graf_areas, Pais == pais)
  
  graf_areas_3 <- plot_ly(x = unique(datos_graf_areas_pais$Año), y = filter(datos_graf_areas_pais, Indicador == "Empleo en agricultura, % del empleo total")$Valor, name = 'Empleo en agricultura', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#ACFF47') |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Empleo en industria, % del empleo total")$Valor, name = 'Empleo en industrias', fillcolor = '#6D64B9', opacity = 0.3) |> 
    add_trace(y = filter(datos_graf_areas_pais, Indicador == "Empleo en servicios, % del empleo total")$Valor, name = 'Empleo en servicios', fillcolor = '#41cadb', opacity = 0.3) |> 
    layout(
      font = list(color = "#bcc3c7", family = "Arial"),
      margin = list(l = 0, r = 0),
      paper_bgcolor = "#272c30",
      plot_bgcolor ="#272c30",
      showlegend = F,
      xaxis = list(showgrid = FALSE,
                   fixedrange = T,
                   range = list(min(unique(datos_graf_areas_pais$Año)), max(unique(datos_graf_areas_pais$Año)))),
      yaxis = list(
        range = list(0,102),
        fixedrange = T,
        title = NULL,
        zeroline = FALSE,      # Remove the zero line
        showline = FALSE,      # Remove the axis line
        showgrid = FALSE,      # Remove grid lines
        ticks = "",            # Remove ticks
        showticklabels = FALSE, # Already removed the tick labels
        automargin = T,
        ticksuffix = '%'    # Adjust margins automatically
      ),
      shapes = list(
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 20, y1 = 20, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 40, y1 = 40, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 60, y1 = 60, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 80, y1 = 80, 
             line = list(dash = "dot", color = "#272c30")),
        list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 50, y1 = 50, 
             line = list(color = "#272c30"))
      ),
      annotations = list(
        list(x = median(datos_graf_areas_pais$Año), y = 102, text = pais, showarrow = FALSE, xanchor = 'middle', font = list(size = 14,color = "#bcc3c7")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 20+2, text = "20%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 40+2, text = "40%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 60+2, text = "60%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
        list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 80+2, text = "80%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30"))
      ),
      hoverlabel = list(font = list(size = 15, color = "#bcc3c7"),
                        namelength = -1),
      hovermode = "x unified"
    )
  assign(paste0("plot_areas_",i), graf_areas_3)
  i = i+1
}

graf_areas_3 <- subplot(plot_areas_1,plot_areas_2, plot_areas_3, plot_areas_4, plot_areas_5, plot_areas_6,plot_areas_7,plot_areas_8,plot_areas_9,plot_areas_10,plot_areas_11,plot_areas_12,
                        nrows = 2)

## Bump chart ----------------------------

graf_bump <- function(dataset, indicador) {
  data_bump <- filter(dataset, Indicador == indicador) |>
    complete(Año = full_seq(Año, 1)) |>  
    group_by(Pais) |> 
    fill(Valor, .direction = "down") |> 
    ungroup() |> 
    group_by(Año) |> 
    mutate(pos = rank(Valor),
           Posicion = abs(length(unique(Pais))-pos+1))
  
  plot_bump <- ggplot(data_bump) + 
    aes(x = Año, y = pos, color = Pais, ind = Valor, imp = Posicion) + 
    geom_line(linewidth = 1.5, lineend = "round", linejoin = "round")+
    geom_point(size = 6) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
  
  anotaciones_plotly <- list()
  j <- 1
  for(i in 1:length(unique(data_bump$Pais))) {
    anotaciones_plotly[[j]] <- list(
      x = min(data_bump$Año)-1,
      y = filter(data_bump, Año == min(data_bump$Año))$pos[i],
      text = filter(data_bump, Año == min(data_bump$Año))$Pais[i],
      showarrow = FALSE,
      xanchor = 'right',
      font = list(color = "white")
    )
    anotaciones_plotly[[j+1]] <- list(
      x = max(data_bump$Año)+1,
      y = filter(data_bump, Año == max(data_bump$Año))$pos[i],
      text = filter(data_bump, Año == max(data_bump$Año))$Pais[i],
      showarrow = FALSE,
      xanchor = 'left',
      font = list(color = "#bcc3c7")
    )
    j <- j+2
  }
  
  
  ggplotly(plot_bump, tooltip = c("Pais", "Año", "Valor", "Posicion")) |> 
    layout(showlegend = F,
           xaxis = list(fixedrange = T,
                        range = list(min(data_bump$Año)-4, max(data_bump$Año)+4)),
           yaxis = list(fixedrange = T),
           font = list (color = "#bcc3c7"),
           annotations = anotaciones_plotly) |> 
    style(opacity = 1) |>  # Establecer opacidad baja por defecto
    onRender("
    function(el, x) {
      el.on('plotly_hover', function(d) {
        var country = d.points[0].data.name;
        var traces = el.data;
        traces.forEach(function(trace, i) {
          if (trace.name === country) {
            Plotly.restyle(el, {'opacity': 1}, [i]);  // Resaltar la línea del país seleccionado
          } else {
            Plotly.restyle(el, {'opacity': 0.4}, [i]); // Hacer el resto de las líneas más transparentes
          }
        });
      });

      el.on('plotly_unhover', function(d) {
        Plotly.restyle(el, {'opacity': 1});  // Restaurar la opacidad de todas las líneas cuando no se está en hover
      });
    }
  ")
}


## Lollipop comparativo -----------------------

graf_lollipop <- function(dataset, indicador, orden, anio) {
  
  data_lollipop <- filter(dataset, str_detect(Indicador, indicador), Año == anio)
  
  if (orden == "Máximo valor") {
    level <- unique(arrange(data_lollipop, desc(Valor))$Pais)
  } else if (orden == "Máxima diferencia") {
    level <- data_lollipop |> 
      group_by(Pais) |> 
      summarise(n = abs(diff(Valor))) |> 
      arrange(desc(n))
    level <- unique(level$Pais)
  } else {
    level <- unique(arrange(data_lollipop, Pais)$Pais)
  }
  
  maximo <- max(filter(dataset, str_detect(Indicador, indicador))$Valor)
  minimo <- min(filter(dataset, str_detect(Indicador, indicador))$Valor)
  
  graf_lollipop <- data_lollipop |> 
    mutate(Pais = factor(Pais, levels = rev(level))) |> 
    ggplot() +
    aes(group = Pais, x = Valor, y = Pais, color = Indicador) + 
    geom_line(color = "#bcc3c7")  +
    geom_point(size = 3) +
    scale_color_manual(values = set_names(c("#F1C8DB", "#41cadb"), c(filter(data_lollipop, str_detect(Indicador, "femeni"))$Indicador[1],
                                                                     filter(data_lollipop, str_detect(Indicador, "mascu"))$Indicador[1]))) +
    xlab(label = indicador) +
    scale_x_continuous(limits = c(minimo, maximo)) +
    theme(legend.position = "none")
  
  
  ggplotly(graf_lollipop, tooltip = c("y", "Valor", "Indicador"))
}

## Piramide poblacional -----------------

graf_piramide <- function(pais, anio) {
  
  level <- unique(poblacion_edad$`Grupos quinquenales de edad`)
  
  maximo <- max(filter(poblacion_edad, Pais == pais, Sexo != "Ambos sexos")$Valor)
  
  data <- poblacion_edad |> 
    mutate(Edad = factor(`Grupos quinquenales de edad`, levels = level),
           Valor = ifelse(Sexo == "Hombres", -Valor, Valor)) |> 
    filter(Pais == pais, Año == anio, Sexo != "Ambos sexos")
  
  graf <- ggplot(data, aes(x = Edad, y = Valor, fill = Sexo)) +
    geom_bar(stat = "identity", position = "identity", width = 1) +
    scale_y_continuous(labels = abs, limits = c((-1)*maximo, maximo), breaks = c(floor(seq((-1)*maximo, 0, length.out = 5))+1, floor(seq(0, maximo, length.out = 5)))) +  # Mostrar números positivos en el eje y
    labs(x = "Edad", y = "Población (miles)") +
    scale_fill_manual(values = c("Mujeres" = "#F1C8DB", "Hombres" = "#41CADB")) +
    coord_flip()
  
  ggplotly(graf) |> 
    layout(hovermode = "y unified",
           legend = list(
             orientation = "h",  
             x = 0.5,  
             y = 1.1,  
             xanchor = "center",  
             yanchor = "bottom"  
           ))
  
}

## Grafico Barplot -----------------

graf_bar <- function(dataset, indicador, anio) {
  datos_barplot <- filter(dataset, Indicador == indicador) |> 
    complete(Año = full_seq(Año, 1), Pais) |>  
    group_by(Pais) |>
    fill(Valor, .direction = "down")
  
  datos_barplot$Pais <- factor(datos_barplot$Pais, levels = unique(datos_barplot$Pais))
  
  maximo <- max(datos_barplot$Valor, na.rm = TRUE)
  
  graf <- datos_barplot |> 
    filter(Año == anio) |>
    ggplot() +
    aes(y = Valor, x = Pais) +
    geom_col(fill = "#41cadb") +
    scale_y_continuous(limits = c(0, maximo)) +
    ylab(label = ifelse(str_detect(indicador, "[%]") | str_detect(indicador, "porcentaje") | str_detect(indicador, "Porcentaje"), "Porcentaje", indicador))
  
  ggplotly(graf)
}

## Barplot agrupado ------------------

graf_bar_agrup <- function(dataset, anio, pais, indicador) {
  
  
  datos_barras <- dataset |> filter(Pais == pais, Año == anio, 
                                    str_detect(Indicador, indicador))
  
  
  txt <- strsplit(datos_barras$Indicador, ",")
  for (i in 1:nrow(datos_barras)) {
    
    segments <- trimws(txt[[i]])
    
    datos_barras$Indicador[i] <- segments[1]
    datos_barras$Sexo[i] <- segments[2]
    datos_barras$Tipo1[i] <- segments[3]
    datos_barras$Tipo[i] <- paste0(segments[2: length(segments)], collapse = ", ")
    
  }
  
  graf <- datos_barras |>
    arrange(Sexo) |>  
    ggplot() +
    geom_col(aes(x = Tipo1, y = Valor, fill = Sexo), position = "dodge") +
    ylab(label = ifelse(str_detect(datos_barras$Indicador[1], "Propor"), "Porcentaje", datos_barras$Indicador[1])) +
    xlab("Tipo") +
    scale_fill_manual(values = c("#F1C8DB", "#41CADB", "#ACFF47")) +
    scale_y_continuous(limits = c(0,100))
  
  ggplotly(graf)
  
}

# Items del carrusel ------------

carrusel_item <- function(indicador, num) {
    carouselItem(
      
      h4(indicador), br(),
      
      column(width = 8, offset = 2,
             sliderTextInput(inputId = paste0("carrusel_educacion_anio_", num),
                             label = "Año",
                             grid = T,
                             choices = sort(unique(filter(Educacion, Indicador == indicador)$Año)),
                             selected = max(filter(Educacion, Indicador == indicador)$Año),
                             animate = animationOptions(
                               interval = 600,
                               loop = FALSE,
                               playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                               pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                             )),
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
                     menuItem("Desarrollo y Educación", tabName = "ciencia", icon = icon("microscope")),
                     menuItem("Vida y Población", tabName = "vida", icon = icon("hand-holding-heart")),
                     menuItem("Economia y Trabajo", tabName = "economia", icon = icon("money-bill-wave")),
                     menuItem("Análisis de datos", tabName = "analisis", icon = icon("magnifying-glass-chart")),
                     menuItem("Bases de datos", tabName = "datos", icon = icon("database"))
                     )
                   ),
  
  
  dashboardBody(
    useWaiter(), 
    waiterPreloader(html = spin_1(), color = "#272c30"),
    
    estilotablas(),
    
    tabItems(
      
      # Página principal ------------------
      tabItem(
        tabName = "pag_principal",
        
        
        h1("Que los datos te cuenten la historia", style = "text-align: center;"),
        h5("Explora, compara y analiza de manera fácil y clara los avances de los paises sudamericanos en diversos tópicos a través del tiempo.", style = "text-align: center;"),
        
        # Video Youtube
        div(
          style = "display: flex; justify-content: center;",
          HTML('<iframe width="60%" height="300" src="https://www.youtube.com/embed/SvXGah3Xqjw?si=VASaOoFk1v8-eZCH" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        ),
        br(),
        ## Primera hilera de botones ---------------------
        fluidRow(
          column(4,
                 actionBttn("ciencia", "Desarrollo y Educación", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", class = "my-custom-btn", class = "my-custom-btn"),
          ),
          column(4,
                 actionBttn("vida", "Vida y Población", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", class = "my-custom-btn"),
          ),
          column(4,
                 actionBttn("economia", "Economía y Trabajo", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", class = "my-custom-btn"),
          )
        ), br(),
        
        ## Segunda hilera de botones ---------------------
        fluidRow(
          column(4,
                 actionBttn("analisis", "Análisis de datos", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", class = "my-custom-btn"),
          ),
          column(4,
                 actionBttn("datos", "Bases de datos", block = T, style = "fill", size = "lg", no_outline = F, color = "royal", class = "my-custom-btn"),
          ),
          column(4,
                 tags$a(href = "https://github.com/andres-roncaglia/CCD2024", 
                        target = "_blank",
                        actionBttn(
                          inputId = "github",
                          label = "Github",
                          block = TRUE,
                          style = "fill",
                          size = "lg",
                          no_outline = FALSE,
                          color = "royal",
                          icon = icon("github"), 
                          class = "my-custom-btn"
                        )
                 )
          )
        ), br(),
        h2("Aclaraciones"),
        p("Al estar en inglés, y por cuestiones de tiempo, ciertos indicadores y sus descripciones fueron traducidas con la ayuda de inteligencia artificial, por lo que puede que las traducciones no sean coherentes, no estén del todo claras o no sean precisas. Para acceder a los indicadores en idioma original referirse a la pestaña de bases de datos y si lo desea buscarlo en la página de donde se extrajeron estos.")
        
      ),
      
      
      # Pagina Desarrollo -------------
      
      tabItem(
        tabName = "ciencia",
        
        h2("Avances tecnológicos"),
        
        fluidRow(
          box(width = 12,
              fluidRow(column(7, 
                              pickerInput(inputId = "indicador_ciencia",
                                          label = "Indicador",
                                          choices = unique(filter(Ciencia, !(Indicador %in% c("Proporción de la fuerza laboral con educación avanzada", "Puntaje de Fuga de Cerebros y Éxodo Humano", "Porcentaje de la poblacion con acceso conveniente al transporte público", "Asequibilidad de la canasta básica digital.", "Empresas de IA , países América Latina y el Caribe, 2023", "Empresas unicornio, países América Latina y el Caribe, 2023", "Trámites gubernamentales disponibles en línea por país, países seleccionados América Latina y el Caribe, 2023")))$Indicador),
                                          selected = unique(Ciencia$Indicador)[1])),
                       column(5,
                              pickerInput(inputId = "indicador_mapa_ciencia",
                                          label = "Indicador",
                                          choices = c("Puntaje de Fuga de Cerebros y Éxodo Humano", "Porcentaje de la poblacion con acceso conveniente al transporte público", "Asequibilidad de la canasta básica digital.", "Empresas de IA", "Empresas unicornio", "Trámites gubernamentales disponibles en línea por país"),
                                          selected = unique(Ciencia$Indicador)[1]))),
              
              fluidRow(
                column(7,
                       plotlyOutput("plot_evo_ciencia")),
                
                column(5,
                       leafletOutput("mapa_ciencia", width = "100%", height = "400px")
                )
              ),
              
              fluidRow(
                column(7,
                       
                       accordion(
                         id = "acordion_ciencia",
                         accordionItem(
                           title = "Descripción del indicador",
                           collapsed = F,
                           textOutput("descripcion_indicador_ciencia")
                         )
                       )
                       
                       ),
                
                column(5,
                       
                       accordion(
                         id = "acordion_mapa_ciencia",
                         accordionItem(
                           title = "Descripción del indicador",
                           collapsed = F,
                           textOutput("descripcion_indicador_mapa_ciencia")
                         )
                       )
                       
                )
              )
              
              )
          ),
        
        ## Educacion -------------------
        h2("Educación"),
        box(width = 12,
            column(4, 
                   pickerInput(inputId = "indicador_educacion",
                               label = "Indicador",
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
        h2("Población"),
        
        box(width = 12,
          fluidRow(
          column(8, offset = 2,
                 pickerInput(inputId = "indicador_vida",
                             label = "Indicador",
                             choices = c("Población, total",
                                         "Esperanza de vida al nacer",
                                         "Tasa bruta de natalidad",
                                        "Índice de pobreza multidimensional",
                                        "Población de refugiados por país o territorio de asilo"))
                 )
          ),
        
        fluidRow(
          column(width = 10, offset = 1, plotlyOutput("plot_evo_vida"))
          ),
        accordion(
          id = "acordion_vida",
          accordionItem(
            title = "Descripción del indicador",
            collapsed = F,
            textOutput("descripcion_indicador_vida")
          )
          )
        ),
        br(),
        box(width = 12,
            fluidRow(
              column(4, offset = 1, 
                     sliderTextInput(inputId = "piramide_anio",
                                     label = "Año",
                                     grid = T,
                                     choices = sort(unique(filter(poblacion_edad, Pais =="Argentina")$Año)),
                                     selected = max(unique(filter(poblacion_edad, Pais =="Argentina")$Año)),
                                     animate = animationOptions(
                                       interval = 600,
                                       loop = FALSE,
                                       playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                       pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                     )),
              column(width = 2,
                     pickerInput(inputId = "piramide_pais",
                                 label = "País",
                                 unique(poblacion_edad$Pais))
              )
              ),
            fluidRow(
              column(6,
                     plotlyOutput("piramide_vida")
              ),
              
              column(6,
                     plotlyOutput("area_poblacion"))
            ),
            
            fluidRow(
              column(6,
                     accordion(
                       id = "acordion_piramide",
                       accordionItem(
                         title = "Descripción del indicador",
                         collapsed = F,
                         p("Número de habitantes por grupos quinquenales de edad que viven efectivamente dentro de los límites fronterizos de un país, territorio o área determinada.")
                         )
                       )
                     
                     ),
              column(6,
                     accordion(
                       id = "acordion_area_pob",
                       accordionItem(
                         title = "Descripción del indicador",
                         collapsed = F,
                         p(paste(filter(vida_poblacion, Indicador == "Población Urbana, total")$Descripción[1],
                                 "
                                     ",
                                 filter(vida_poblacion, Indicador == "Población rural, total")$Descripción[1])))
                     ))
            )
            
            ),
        ## Calidad de vida -------------------
        br(),
        h2("Calidad de vida"),
        br(),
        box(width = 12,
            
            fluidRow(column(4,
                            pickerInput(inputId = "vida_pais",
                                        label = "País",
                                        unique(vida_poblacion$Pais)))
            ),
            
            fluidRow(
              column(4,
                     pickerInput(inputId = "indicador_vida_total",
                                 label = "Población total",
                                 choices = unique(filter(vida_poblacion, Indicador != "Población de 65 años y más, % de la población total",
                                                           str_detect(Indicador, "\\(% de la población\\)") | str_detect(Indicador, " total, porcentaje") | str_detect(Indicador, "% de la población total"),
                                 )$Indicador)), br(),
                     sliderTextInput(inputId = "dona_vida_anio_1",
                                     label = "Año",
                                     grid = T,
                                     choices = sort(unique(filter(vida_poblacion, Indicador =="Población Urbana, total")$Año)),
                                     selected = max(unique(filter(vida_poblacion, Indicador =="Población Urbana, total")$Año)),
                                     animate = animationOptions(
                                       interval = 600,
                                       loop = FALSE,
                                       playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                       pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                     ),
                     flipBox(
                       id = "dona_vida_1",
                       width = 12,
                       front = div(
                         class = "d-flex justify-content-center",
                         height = "300px",
                         width = "100%",
                         plotlyOutput("graf_torta_total")),
                       back = uiOutput("des_dona_1"))
                            
              ),
              column(4, 
                     pickerInput(inputId = "indicador_vida_urbano",
                                 label = "Población Urbana",
                                 choices = c(unique(filter(vida_poblacion, 
                                                           str_detect(Indicador, ", urbano, porcentaje") | str_detect(Indicador, "\\(% de la población urbana\\)") | str_detect(Indicador, "% de la población urbana"),
                                 )$Indicador))), br(),
                     sliderTextInput(inputId = "dona_vida_anio_2",
                                     label = "Año",
                                     grid = T,
                                     choices = sort(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, urbano (% de la población urbana)")$Año)),
                                     selected = max(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, urbano (% de la población urbana)")$Año)),
                                     animate = animationOptions(
                                       interval = 600,
                                       loop = FALSE,
                                       playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                       pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                     ),
                     flipBox(
                       id = "dona_vida_2",
                       width = 12,
                       front = div(
                         class = "d-flex justify-content-center",
                         height = "300px",
                         width = "100%",
                         plotlyOutput("graf_torta_urbano")),
                       back = uiOutput("des_dona_2"))
                     
              ),
              column(4, 
                     pickerInput(inputId = "indicador_vida_rural",
                                 label = "Población Rural",
                                 choices = c(unique(filter(vida_poblacion, 
                                                           str_detect(Indicador, ", rural, porcentaje") | str_detect(Indicador, "\\(% de la población rural\\)") | str_detect(Indicador, "% de la población rural"),
                                 )$Indicador))), br(),
                     sliderTextInput(inputId = "dona_vida_anio_3",
                                     label = "Año",
                                     grid = T,
                                     choices = sort(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, rural (% de la población rural)")$Año)),
                                     selected = max(unique(filter(vida_poblacion, Indicador =="Acceso a electricidad, rural (% de la población rural)")$Año)),
                                     animate = animationOptions(
                                       interval = 600,
                                       loop = FALSE,
                                       playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                       pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                     ),
                     flipBox(
                       id = "dona_vida_3",
                       width = 12,
                       front = div(
                         class = "d-flex justify-content-center",
                         height = "300px",
                         width = "100%",
                         plotlyOutput("graf_torta_rural")),
                       back = uiOutput("des_dona_3")
                       )
                       
                     )
              )
            
            ),
        
        br(),
        ## Pobreza --------------------------
        h2("Pobreza y ayudas sociales"),
        
        br(),
        box(width = 6,
            column(12,
                   pickerInput(inputId = "indicador_barras_ayudas",
                               label = "Indicador",
                               choices = sort(c(
                                 "Gasto promedio per cápita en salud de bolsillo por hogar ($ 2011 PPP)",
                                 "Proporción de la población mayor de edad para pensiones que recibe una pensión, total",
                                 "Población cubierta por al menos un beneficio de protección social, total",
                                 "Personas Vulnerables Cubiertas por Asistencia Social, porcentaje",
                                 "Personas pobres cubiertas por sistemas de protección social, porcentaje",
                                 "Personas con discapacidades severas recibiendo beneficios de protección social por discapacidad",
                                 "Niños/Hogares que reciben beneficios en efectivo para niños/familias, porcentaje",
                                 "Madres con recién nacidos recibiendo beneficios de maternidad, Femeninas",
                                 "Adecuación de los programas de protección social (porcentaje del bienestar total de los hogares beneficiarios)"
                               ))),
                   
                   sliderTextInput(inputId = "anio_ayudas_barra",
                                   label = "Año",
                                   grid = T,
                                   choices = sort(unique(filter(Pobreza, Indicador =="Población cubierta por al menos un beneficio de protección social, total")$Año)),
                                   selected = max(unique(filter(Pobreza, Indicador =="Población cubierta por al menos un beneficio de protección social, total")$Año)),
                                   animate = animationOptions(
                                     interval = 600,
                                     loop = FALSE,
                                     playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                     pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                   ),
                   
                   plotlyOutput("barras_ayudas"),
                   
                   accordion(
                     id = "acordion_barras_ayudas",
                     accordionItem(
                       title = "Descripción del indicador",
                       collapsed = F,
                       textOutput("des_barras_ayudas")
                       ))
                   
                   )
            
            
            ),
        
        box(width = 6,
            
            column(8,
                   pickerInput(inputId = "indicador_barras_pobreza",
                               label = "Indicador",
                               choices = sort(c(
                                 "Población en situación de pobreza extrema y pobreza, por nivel educativo máximo alcanzado y sexo",
                                 "Población ocupada en situación de pobreza extrema y pobreza"
                               )))),
            column(4,
                   pickerInput(inputId = "pais_pobreza_barra",
                               label = "Indicador",
                               choices = unique(pobreza_categorizada$Pais)
                                 )
                   ),
                   
                   sliderTextInput(inputId = "anio_pobreza_barra",
                                   label = "Año",
                                   grid = T,
                                   choices = sort(unique(pobreza_categorizada$Año)),
                                   selected = max(unique(pobreza_categorizada$Año)),
                                   animate = animationOptions(
                                     interval = 600,
                                     loop = FALSE,
                                     playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                     pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                   ),
            
            plotlyOutput("barras_pobreza"),
            
            accordion(
              id = "acordion_barras_pobreza",
              accordionItem(
                title = "Descripción del indicador",
                collapsed = F,
                uiOutput("des_barras_pobreza")
              ))
                   
                   )
        
      ),
      
      # Pagina Economia -------------------
      
      tabItem(
        tabName = "economia",
        h2("Economía"), 
        # Grafico bump
        box(width = 12,
            pickerInput(inputId = "indicador_economia_bump",
                        label = "Indicador",
                        choices = c(
                          "Ingreso Nacional Bruto per cápita (PPP$ 2017)",
                          "Desigualdad en ingresos",
                          "Índice de estricta respuesta gubernamental",
                          "Deuda neta como porcentaje del PIB",
                          "Estimaciones de Producción Informal, porcentaje del PIB oficial",
                          "Inflación, cambio porcentual",
                          "Préstamos/endeudamiento neto del gobierno como porcentaje del PIB",
                          "Índice de Gini"
                        )), br(),
            plotlyOutput("graf_bump_economia"),
            accordion(
              id = "acordion_economia_bump",
              accordionItem(
                title = "Descripción del indicador",
                collapsed = F,
                textOutput("descripcion_indicador_economia_bump")
              ))
            ),
        br(),
        ## Trabajo ----------------
        
        h2("Trabajo"),
        br(),
        # Grafico de areas
        box(width = 12,
            pickerInput(inputId = "indicador_economia_areas",
                        label = "Indicador",
                        choices = c(
                          "Distribución del empleo por tipo de empresas",
                          "Distribución del empleo por sector",
                          "Distribución de tamaño de empresas"
                        )), br(),
            plotlyOutput("graf_areas", height = "600px")
            ),
        
        # Grafico lollipop
        box(width = 6,
            column(8,pickerInput(inputId = "indicador_economia_lollipop",
                        label = "Indicador",
                        choices = c(
                          "Tasa de participación en la fuerza laboral",
                          "Ingreso Nacional Bruto per cápita"
                        ))),
            column(4,
                   pickerInput(inputId = "orden_economia_lollipop",
                               label = "Orden",
                               choices = c(
                                 "Máximo valor",
                                 "Máxima diferencia",
                                 "País"
                               ))),
            sliderTextInput(inputId = "anio_economia_lollipop",
                            label = "Año",
                            grid = T,
                            choices = sort(unique(filter(Economia, str_detect(Indicador,"Tasa de participación en la fuerza laboral"))$Año)),
                            selected = max(unique(filter(Economia, str_detect(Indicador,"Tasa de participación en la fuerza laboral"))$Año)),
                            animate = animationOptions(
                              interval = 600,
                              loop = FALSE,
                              playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                              pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                            ),
            
            plotlyOutput("lollipop_economia"),
            accordion(
              id = "acordion_economia_lollipop",
              accordionItem(
                title = "Descripción del indicador",
                collapsed = F,
                textOutput("des_lollipop_economia")
              ))
            ),
        
        # Grafico barras agrupadas
        box(width = 6,
            column(8,pickerInput(inputId = "indicador_economia_barras",
                                 label = "Indicador",
                                 choices = c(
                                   "Proporción de empleo informal por sexo y actividad económica",
                                   "Proporción de empleo informal por sexo y edad",
                                   "Proporción de empleo informal por sexo y estado en el empleo"
                                 ))),
            column(4,
                   pickerInput(inputId = "pais_economia_barras",
                               label = "Pais",
                               choices = unique(filter(Trabajo, Pais != "Venezuela", Pais != "Suriname",)$Pais))),
            sliderTextInput(inputId = "anio_economia_barras",
                            label = "Año",
                            grid = T,
                            choices = sort(unique(filter(Trabajo, str_detect(Indicador,"Proporción de empleo informal por sexo y actividad económica"))$Año)),
                            selected = max(unique(filter(Trabajo, str_detect(Indicador,"Proporción de empleo informal por sexo y actividad económica"))$Año)),
                            animate = animationOptions(
                              interval = 600,
                              loop = FALSE,
                              playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                              pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon")))
                            ),
            
            plotlyOutput("barras_economia"),
            accordion(
              id = "acordion_economia_barras",
              accordionItem(
                title = "Descripción del indicador",
                collapsed = F,
                textOutput("des_barras_economia")
              ))
        )
        
        
      ),
      
      # Pagina Analisis de datos ------------------
      
      tabItem(
        tabName = "analisis",
        h2("Análisis de datos"), br(),
        h4("Análisis de correlaciones"), br(),
        
        p("Ver la correlación entre dos variables es útil para medir la relación o el grado de asociación entre ellas. La correlación puede proporcionar información clave sobre cómo un indicador se comporta en relación con otro, lo que puede ayudar a descubrir patrones, dependencias, o incluso factores causales. Que la correlación sea positiva significa que ambos indicadores aumentan o disminuyen juntos, que se negativa significaría que cuando uno aumenta el otro disminuye, mientras que una correlación nula indicaría que los indicadores no tienen relación entre sí."),
        br(),
        box(width = 12,
          fluidRow(
          column(7,
                 plotlyOutput("plot_corr")),
          column(4,
                 fluidRow(
                   pickerInput(inputId = "var_corr_1", label = "Indicador eje x", choices = list(
                     "Desarrollo y educación" = setNames(sort(unique(ciencia_educacion$Indicador)), ifelse(str_count(sort(unique(ciencia_educacion$Indicador))) > 80, paste0(str_sub(sort(unique(ciencia_educacion$Indicador)), start = 1, end = 80), "..."), sort(unique(ciencia_educacion$Indicador)))), 
                     "Vida y población" = setNames(sort(unique(vida_poblacion$Indicador)), ifelse(str_count(sort(unique(vida_poblacion$Indicador))) > 80, paste0(str_sub(sort(unique(vida_poblacion$Indicador)), start = 1, end = 80), "..."), sort(unique(vida_poblacion$Indicador)))), 
                     "Economia y trabajo" = setNames(sort(unique(economia_trabajo$Indicador)), ifelse(str_count(sort(unique(economia_trabajo$Indicador))) > 80, paste0(str_sub(sort(unique(economia_trabajo$Indicador)), start = 1, end = 80), "..."), sort(unique(economia_trabajo$Indicador))))
                     ), selected = "Empleo fuera del sector formal por sexo (miles), Total",
                   options = list(
                     `live-search` = TRUE,
                     size = 7))
                 ),
                 fluidRow(
                   pickerInput(inputId = "var_corr_2", label = "Indicador eje y", choices = list(
                     "Desarrollo y educación" = setNames(sort(unique(ciencia_educacion$Indicador)), ifelse(str_count(sort(unique(ciencia_educacion$Indicador))) > 80, paste0(str_sub(sort(unique(ciencia_educacion$Indicador)), start = 1, end = 80), "..."), sort(unique(ciencia_educacion$Indicador)))), 
                     "Vida y población" = setNames(sort(unique(vida_poblacion$Indicador)), ifelse(str_count(sort(unique(vida_poblacion$Indicador))) > 80, paste0(str_sub(sort(unique(vida_poblacion$Indicador)), start = 1, end = 80), "..."), sort(unique(vida_poblacion$Indicador)))), 
                     "Economia y trabajo" = setNames(sort(unique(economia_trabajo$Indicador)), ifelse(str_count(sort(unique(economia_trabajo$Indicador))) > 80, paste0(str_sub(sort(unique(economia_trabajo$Indicador)), start = 1, end = 80), "..."), sort(unique(economia_trabajo$Indicador))))
                   ), selected = "Tasa de desempleo",
                   options = list(
                     `live-search` = TRUE,
                     size = 7))
                 ),
                 fluidRow(
                   sliderTextInput(inputId = "anio_corr",
                               label = "Año",
                               choices = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
                               selected = 2022,
                               animate = animationOptions(
                                 interval = 600,
                                 loop = FALSE,
                                 playButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-play", lib = "glyphicon")),
                                 pauseButton = tags$button(class = "play-bttn", icon("glyphicon glyphicon-pause", lib = "glyphicon"))))
                 ),
                 fluidRow(
                   valueBoxOutput("caja_correlaciones", width = 11)
                 )
                 )
        )),
        br(),
        h4("Análisis de componentes principales (Datos del 2022)"), br(),
        p("El análisis de componentes principales es una técnica estadística que se utiliza para reducir la dimensionalidad de un conjunto de datos, reteniendo la mayor cantidad de información posible. En otras palabras, se usa para reducir el número de variables (en este caso indicadores) sin perder demasiada información importante. Se transforman las variables originales en un nuevo conjunto de variables no correlacionadas llamadas componentes principales, donde cada componente principal es una combinación lineal de las variables originales."),
        br(),
        box(width = 12,
            fluidRow(
          column(7,
                 plotlyOutput("graf_aporte_cp")),
          
          column(5,
                 DTOutput("tabla_cp")
                 )
        )),
        br(),
        p("En el gráfico de la izquierda se pueden observar en cúantas variables se pueden resumir todos los indicadores sin perder demasiada información. La tabla de la derecha muestra el aporte de cada indicador a las nuevas variables, cuanto mayor sea el valor, mayor es la influencia del indicador en la variable."),
        br(),
        p("A partir de las componentes principales obtenidas se podrían agrupar a los países según estas nuevas variables."),
        br(),
        h4("Agrupamiento por cluster"),
        br(),
        p("El análisis de clústeres permite encontrar patrones en los datos al agrupar países que sean similares entre sí según ciertas características. El análisis de clústeres identifica grupos 'naturales' que no son evidentes a simple vista."),
        br(),
        box(width = 12,
            fluidRow(
              column(3,
                     pickerInput(inputId = "componentex", label = "Componente en el eje x", choices = colnames(datos_cluster)[2:6], selected = colnames(datos_cluster)[2])
                     ),
              column(3,
                     pickerInput(inputId = "componentey", label = "Componente en el eje y", choices = colnames(datos_cluster)[2:6], selected = colnames(datos_cluster)[3])
                     ),
              
              
              column(3,
                     pickerInput(inputId = "pais_radar_1", label = "País", choices = unique(datos_cluster$Pais), selected = unique(datos_cluster$Pais)[1])
                     ),
              column(3,
                     pickerInput(inputId = "pais_radar_2", label = "País", choices = unique(datos_cluster$Pais), selected = unique(datos_cluster$Pais)[2])
                     )
            ),
            
            fluidRow(
          column(7,
                 plotlyOutput("graf_acp")),

          
          column(5,
                 
                 plotlyOutput("graf_radar")
                 
                 )
        ))
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
                       choices = c("Desarrollo y educación", "Vida y población", "Economía y trabajo", "Población por cuantiles", "Pobreza")
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
            ),
        br(),
        h2("Fuentes"),
        br(),
        
        HTML("
             
              <ul>
                <li>
              CEPAL - Comisión Económica para América Latina y el Caribe: CELADE - División de Población de la CEPAL y Naciones Unidas, Departamento de Asuntos Económicos y Sociales, División de Población. (2024). <i> World Population Prospects, 2024, edición online </i>. 
<a href='https://population.un.org/wpp/' target='_blank'> https://population.un.org/wpp/</a>
                </li>
                <li>
                    Observatorio de Desarrollo Digital (ODD) sobre la base de prestadores del servicio de Internet seleccionados en cada país para las tarifas del los servicios de banda ancha, marketplaces seleccionados en cada país para el costo de los dispositivos y el Banco de Encuestas de Hogares (BADEHOG) para ingresos, CEPAL. (2022). <i> Asequibilidad de la canasta básica digital, trámites gubernamentales disponibles en línea por país, empresas de IA y empresas unicornio, países América Latina y el Caribe, 2023. </i> <a href='http://www.eclac.org/' target='_blank'> http://www.eclac.org/</a>
                </li>  
                <li>
                    CEPAL - Comisión Económica para América Latina y el Caribe. (2023). <i> Valor de las líneas de pobreza extrema y pobreza; población ocupada en situación de pobreza extrema y pobreza, por área; población en situación de pobreza extrema y pobreza, por nivel educativo máximo alcanzado, sexo y área. </i> <a href='https://www.cepal.org/' target='_blank'> https://www.cepal.org/</a>
                </li>  

                <li>
             Programa de las Naciones Unidas para el Desarrollo. (s.f.). <i> América Latina y el Caribe: Data Futures Platform. Programa de las Naciones Unidas para el Desarrollo. </i>. <a href='https://data.undp.org/regions/latin-america-and-the-caribbean' target='_blank'> https://data.undp.org/regions/latin-america-and-the-caribbean</a>
                </li>
                  <li>
                  Hannah Ritchie and Edouard Mathieu and Max Roser. (2023). <i> Research and Development </i>. <a href='https://ourworldindata.org/research-and-development' target='_blank'> https://ourworldindata.org/research-and-development</a>
                </li>
              </ul> 
             
             ")
        
        
        
      )
      
    )
  )
)



# Servidor ----------------
server <- function(input, output, session) {
  Sys.sleep(1)
  waiter_hide()
  
  
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
  
  # observeEvent(input$github, {
  #   runjs('window.open("https://github.com/andres-roncaglia/CCD2024", "_blank");')
  # })
  
  
  ## Desarrollo -------------------------------
  
  ### Grafico evolutivo ----------------
  
  output$plot_evo_ciencia <- renderPlotly({
    
    graf = graf_evolutivo(Ciencia, input$indicador_ciencia)
    
    #### Internet Que los datos te cuenten la historia ----------------------
    # if (str_detect(input$indicador_ciencia, "internet") | str_detect(input$indicador_ciencia, "Internet")) {
    #   Periodos <- data.frame(
    #     Comienzo = c(1990:1994, 1998:1999),
    #     Periodo = c(rep("El comienzo en el internet", 5), rep("Evolucion", 2)))
    #   
    #   maximo <- max(filter(Ciencia, Indicador == input$indicador_ciencia)$Valor)
    #   
    #   graf <- graf +
    #     geom_bar(data = Periodos, inherit.aes = F,
    #              aes(x= Comienzo, y = maximo, customdata = Periodo),
    #              stat = "identity", position = "stack", width = 1,
    #              fill = "cyan3", alpha = 0.3,
    #              just = 0) +
    #     scale_x_continuous(limits = c(min(filter(Ciencia, Indicador == input$indicador_ciencia)$Año),
    #                                   max(filter(Ciencia, Indicador == input$indicador_ciencia)$Año)))
    # }
    # 
    ggplotly(graf, tooltip = c("Año", "Valor", "color", "customdata"), source = "plot_evo_ciencia_click") |> 
      event_register("plotly_click")
    
  })
  
  ### Mapa ---------------------
  
  
  output$mapa_ciencia <- renderLeaflet({
   
    if (input$indicador_mapa_ciencia == "Puntaje de Fuga de Cerebros y Éxodo Humano") {
      anio <- 2022
    } else if (input$indicador_mapa_ciencia =="Porcentaje de la poblacion con acceso conveniente al transporte público") {
      anio <- 2020
    } else {
      anio <- 2023
      }
    
    if (anio == 2023) {
      indicador <- filter(ciencia_educacion, str_detect(Indicador, input$indicador_mapa_ciencia))$Indicador[1]
      graf_mapa(ciencia_educacion, indicador, anio)
    } else {
      graf_mapa(ciencia_educacion, input$indicador_mapa_ciencia, anio)
    }
    
    
  })
  
  ### Descripcion acordion -----------------
  
  output$descripcion_indicador_ciencia <- renderText({
    filter(Ciencia, Indicador == input$indicador_ciencia)$Descripción[1]
  })
  
  output$descripcion_indicador_mapa_ciencia <- renderText({
    filter(ciencia_educacion, str_detect(Indicador, input$indicador_mapa_ciencia))$Descripción[1]
  })
  
  ### Historia acordion -----------------
  
  # output$historia_ciencia <- renderUI({
  #   
  #   click_data <- event_data("plotly_click", source = "plot_evo_ciencia_click")
  #   
  #   if (is.null(click_data)) {
  #     HTML("Clickea alguna zona marcada para conocer la historia.")
  #   } else if (click_data$customdata %in% datos_historicos$nom_periodo) {
  #     HTML(filter(datos_historicos, nom_periodo == click_data$customdata)$des_periodo) 
  #   } else {
  #     HTML("Clickea alguna zona marcada para conocer la historia.")
  #   }
  #   
  # 
  # })
  
  ###Funcion carrusel items -------------
  
  observeEvent(
    input$pais_educacion, {
      
      indicador <- c("Coeficiente de desigualdad humana", "Proporción de la fuerza laboral con educación avanzada", "Índice de Inclusión Social y Equidad", "Gasto en educación terciaria (% del gasto gubernamental en educación)", "Índice de Desarrollo de Género", "Desigualdad en educación", "Índice de Desarrollo Humano Ajustado por Desigualdad", "Índice de Desarrollo Humano Ajustado por Desigualdad, diferencia con el valor del IDH no ajustado", "Índice de Desarrollo Humano ajustado por presiones planetarias", "Índice de Desarrollo Humano ajustado por presiones planetarias, diferencia con el IDH no ajustado", "Inscripción escolar, terciaria (% bruto)")
      
      for (num in 1:11) {
        
        if (length(sort(unique(filter(Educacion, Pais == input$pais_educacion, Indicador == indicador[num])$Año))) < 1) {
          updateSliderTextInput(
            session = session,
            inputId = paste0("carrusel_educacion_anio_", num),
            label = "Año",
            choices = c(2020,2021),
            selected = 2020
          )
          next
          
        } else {
          updateSliderTextInput(
            session = session,
            inputId = paste0("carrusel_educacion_anio_", num),
            label = "Año",
            choices = sort(unique(filter(Educacion, Pais == input$pais_educacion, Indicador == indicador[num])$Año)),
            selected = max(filter(Educacion, Pais == input$pais_educacion, Indicador == indicador[num])$Año)
          )
        }
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
  
  # output$historia_educacion <- renderText({
  #   
  #   if (T) {
  #     "Clickea alguna zona marcada para conocer la historia."
  #   }
  #   
  # })
  
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
    
    if (input$indicador_vida == "Población, total") {
      base_datos = vida_poblacion %>%
        mutate(Valor = round(Valor/1000000, 2)) |>
        filter(!is.na(Valor))
      
      graf = graf_evolutivo(base_datos,"Población, total") +
        ylab(label = "Población total (millones)")
      
      ggplotly(graf, tooltip = c("Año", "Valor", "color"))
    } else if (input$indicador_vida == "Esperanza de vida al nacer") {
      
      graf = graf_evolutivo(vida_poblacion,"Esperanza de vida al nacer, según sexo")+
        ylab(label = "Esperanza de vida al nacer")
      
      ggplotly(graf, tooltip = c("Año", "Valor", "color"))
      
    } else {
      graf = graf_evolutivo(vida_poblacion, input$indicador_vida)
      
      ggplotly(graf, tooltip = c("Año", "Valor", "color"))
    }
    
    
    
  })
  
  ### Descripcion acordion -----------------
  
  output$descripcion_indicador_vida <- renderText({
    if(input$indicador_vida == "Esperanza de vida al nacer") {
      filter(vida_poblacion, Indicador == "Esperanza de vida al nacer, según sexo")$Descripción[1]
    } else {
      filter(vida_poblacion, Indicador == input$indicador_vida)$Descripción[1]
    }
    
  })
  
  ### Piramide poblacional ---------------------
  
  observeEvent(input$ppiramide_anio, {
    updateSliderTextInput(session = session,
                          inputId = "piramide_anio",
                          label = "Año",
                          choices = sort(unique(filter(poblacion_edad, Pais ==input$piramide_pais)$Año)),
                          selected = max(unique(filter(poblacion_edad, Pais ==input$piramide_pais)$Año)))
  })
  
  output$piramide_vida <- renderPlotly({
    
    graf_piramide(pais = input$piramide_pais, anio = input$piramide_anio)
    
  })
  
  ### Areas poblacionales ------------------
  
  output$area_poblacion <- renderPlotly({
    datos_graf_areas_pais <- filter(Poblacion, Indicador == "Población rural, total" | Indicador == "Población Urbana, total", Pais == input$piramide_pais)
    
    rural_total <- filter(datos_graf_areas_pais, Indicador == "Población rural, total")$Valor
    urbana_total <- filter(datos_graf_areas_pais, Indicador == "Población Urbana, total")$Valor
    
    hover_rural <- paste("Población rural, total:", rural_total, "<br>% del total:", round(rural_total / (rural_total + urbana_total) * 100, 1), "%")
    hover_urbana <- paste("Población Urbana, total:", urbana_total, "<br>% del total:", round(urbana_total / (rural_total + urbana_total) * 100, 1), "%")
    
    
    plot_ly(x = unique(datos_graf_areas_pais$Año), y = filter(datos_graf_areas_pais, Indicador == "Población rural, total")$Valor, name = "Población rural, total", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#ACFF47', text = hover_rural, hoverinfo = "text") |> 
      add_trace(y = filter(datos_graf_areas_pais, Indicador == "Población Urbana, total")$Valor, name = "Población Urbana, total", fillcolor = '#41cadb', opacity = 0.3, text = hover_urbana, hoverinfo = "text") |> 
      layout(
        title = "Relación población rural/urbana",
        font = list(color = "#bcc3c7", family = "Arial"),
        margin = list(l = 20, r = 20, t = 50),
        paper_bgcolor = "#272c30",
        plot_bgcolor ="#272c30",
        showlegend = F,
        xaxis = list(title= "Año",
                     showgrid = FALSE,
                     fixedrange = T),
        yaxis = list(
          ticks = "",            # Remove ticks
          showticklabels = FALSE, # Already removed the tick labels
          automargin = T,
          ticksuffix = '%'    # Adjust margins automatically
        ),
        shapes = list(
          list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 20, y1 = 20, 
               line = list(dash = "dot", color = "#272c30")),
          list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 40, y1 = 40, 
               line = list(dash = "dot", color = "#272c30")),
          list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 60, y1 = 60, 
               line = list(dash = "dot", color = "#272c30")),
          list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 80, y1 = 80, 
               line = list(dash = "dot", color = "#272c30")),
          list(type = "line", x0 = min(datos_graf_areas_pais$Año), x1 = max(datos_graf_areas_pais$Año), y0 = 50, y1 = 50, 
               line = list(color = "#272c30"))
        ),
        annotations = list(
          list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 20+2, text = "20%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
          list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 40+2, text = "40%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
          list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 60+2, text = "60%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30")),
          list(x = min(datos_graf_areas_pais$Año) + 0.1, y = 80+2, text = "80%", showarrow = FALSE, xanchor = 'left', font = list(color = "#272c30"))
        ),
        hoverlabel = list(font = list(size = 15, color = "#bcc3c7"),
                          namelength = -1),
        hovermode = "x unified"
      )
    
  })
  
  ### Graficos dona poblacion ------------------------
  
  # Descripcion indicador 
  
  output$des_dona_1 <- renderUI({
  
    
    if (input$indicador_vida_total == "Población rural/urbana") {
      descripcion_indicador <- paste(filter(vida_poblacion, Indicador == "Población Urbana, total")$Descripción[1],
                                     "
                                     ",
                                     filter(vida_poblacion, Indicador == "Población rural, total")$Descripción[1])
    } else if (input$indicador_vida_total == "Población inmigrante") {
      descripcion_indicador <- filter(vida_poblacion, Indicador == "Stock de migrantes internacionales a mitad de año, total")$Descripción[1]
    } else {
      descripcion_indicador <- filter(vida_poblacion, Indicador == input$indicador_vida_total)$Descripción[1]
    }
    
    box(width = NULL,title = input$indicador_vida_total, height = "1000px", 
        style = "height: 360px; width: 100%; padding: 0; margin: 0;",
        p(descripcion_indicador))
    
  })
  
  output$des_dona_2 <- renderUI({
    
    box(width = NULL,title = input$indicador_vida_urbano, height = "1000px", 
        style = "height: 360px; width: 100%; padding: 0; margin: 0;",
        p(filter(vida_poblacion, Indicador == input$indicador_vida_urbano)$Descripción[1]))
    
  })
  
  output$des_dona_3 <- renderUI({
    
    box(width = NULL,title = input$indicador_vida_rural, height = "1000px", 
        style = "height: 360px; width: 100%; padding: 0; margin: 0;",
        p(filter(vida_poblacion, Indicador == input$indicador_vida_rural)$Descripción[1]))
    
  })
  
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

    } else {
      graf_dona(base_datos = vida_poblacion, Pais_dona = input$vida_pais, 
                Anio_dona = input$dona_vida_anio_1, indicador_dona = input$indicador_vida_total,
                maximo = "proporcion")
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
  
  ### Barras Ayudas ---------------------
  
  output$des_barras_ayudas <- renderText({
    
    filter(Pobreza, Indicador == input$indicador_barras_ayudas)$Descripción[1]
    
  })
  
  observeEvent(input$indicador_barras_ayudas, {
    
    base_datos <- Pobreza
    
    updateSliderTextInput(
      session = session,
      inputId = "anio_ayudas_barra",
      choices = sort(unique(filter(Pobreza, Indicador == input$indicador_barras_ayudas)$Año)),
      selected = max(unique(filter(Pobreza, Indicador == input$indicador_barras_ayudas)$Año))
    )
    
  })
  
  
  
  output$barras_ayudas <- renderPlotly({
    
    graf_bar(dataset = Pobreza,indicador = input$indicador_barras_ayudas,anio = input$anio_ayudas_barra)
    
  })
  
  ### Barras Pobreza ---------------------
  
  output$des_barras_pobreza <- renderUI({
    
    if (input$indicador_barras_pobreza== "Población en situación de pobreza extrema y pobreza, por nivel educativo máximo alcanzado y sexo") {
      HTML(paste(pobreza_categorizada$Descripción[1], "<br> <br>", valores_pobreza$Descripción[1]))
    } else {
      HTML(paste(trabajadores_pobreza$Descripción[1], "<br> <br>", valores_pobreza$Descripción[1]))
    }
    
  })
  
  observeEvent(list(input$indicador_barras_pobreza, input$pais_pobreza_barra), {
    
    if (input$indicador_barras_pobreza== "Población en situación de pobreza extrema y pobreza, por nivel educativo máximo alcanzado y sexo") {
      updateSliderTextInput(
        session = session,
        inputId = "anio_pobreza_barra",
        choices = sort(unique(filter(pobreza_categorizada, Pais == input$pais_pobreza_barra)$Año)),
        selected = max(unique(filter(pobreza_categorizada, Pais == input$pais_pobreza_barra)$Año))
      )
    } else {
      updateSliderTextInput(
        session = session,
        inputId = "anio_pobreza_barra",
        choices = sort(unique(filter(trabajadores_pobreza)$Año)),
        selected = max(unique(filter(trabajadores_pobreza)$Año))
      )
    }
    
    
    
  }, ignoreNULL = FALSE)
  
  
  
  output$barras_pobreza <- renderPlotly({
    
    if (input$indicador_barras_pobreza == "Población en situación de pobreza extrema y pobreza, por nivel educativo máximo alcanzado y sexo") {
      
      graf <- pobreza_categorizada |> 
        filter(Año == input$anio_pobreza_barra, Pais == input$pais_pobreza_barra, Sexo__ESTANDAR != "Ambos sexos", `Nivel educativo` != "Total nivel educativo", `Área geográfica` == "Urbana") |> 
        mutate(`Nivel educativo` = factor(`Nivel educativo`, levels = c("Primaria incompleta", "Primaria completa", "Secundaria incompleta", "Secundaria completa", "Terciaria incompleta (incluye técnica terciaria)", "Terciaria completa")),
               valor1 = case_when(`Pobreza extrema y pobreza` == "Pobreza" ~ Valor,
                                  T ~ NA),
               valor2 = case_when(`Pobreza extrema y pobreza` == "Pobreza extrema" ~ Valor,
                                  T ~ NA),
               "Sexo y condición económica" = case_when(Sexo__ESTANDAR == "Hombres" & `Pobreza extrema y pobreza` == "Pobreza" ~ "Hombres pobres",
                                                        Sexo__ESTANDAR == "Hombres" & `Pobreza extrema y pobreza` == "Pobreza extrema" ~ "Hombres indigentes",
                                                        Sexo__ESTANDAR == "Mujeres" & `Pobreza extrema y pobreza` == "Pobreza" ~ "Mujeres pobres",
                                                        Sexo__ESTANDAR == "Mujeres" & `Pobreza extrema y pobreza` == "Pobreza extrema" ~ "Mujeres indigentes",
                                                        T ~ "Error")
        ) |> 
        ggplot() +
        aes(x = `Nivel educativo`, fill = `Sexo y condición económica`, group = Sexo__ESTANDAR, Porcentaje = Valor) +
        geom_col(position = "dodge", aes(y = valor1)) +
        geom_col(position = "dodge", aes(y = valor2)) +
        scale_fill_manual(values = c("Hombres pobres" = "#41cadb", "Hombres indigentes" = "#219BAB", "Mujeres pobres" = "#F1C8DB", "Mujeres indigentes" = "#E28DB5")) +
        ylab(label = "Población en situación de pobreza (%)") +
        scale_y_continuous(limits = c(0,max(filter(pobreza_categorizada, Pais == input$pais_pobreza_barra, Sexo__ESTANDAR != "Ambos sexos", `Nivel educativo` != "Total nivel educativo", `Área geográfica` == "Urbana")$Valor))) +
        guides(fill=guide_legend(ncol=2)) +
        labs(fill = "") +
        scale_x_discrete(labels = function(x) sub(" ", "\n", sub(" ", "\n", x))) +
        theme(legend.position = "bottom")
      
      ggplotly(graf, tooltip = c("x", "fill", "Valor")) |> 
        layout(hovermode = "x unified",
               legend = list(
                 orientation = 'h',   # Set legend to be horizontal
                 x = 0.5,             # Center the legend horizontally
                 xanchor = 'center',
                 y = -0.3,            # Position the legend slightly below the plot
                 yanchor = 'top'
               ),
               
               annotations = list(
                 list(
                   x = 1,  # Posiciona en el extremo derecho del gráfico
                   y = 0.95,  # Posiciona en la parte superior del gráfico
                   xref = 'paper',  # Coordenadas relativas al gráfico
                   yref = 'paper',
                   text = paste0("Línea de indigencia: ", round(filter(valores_pobreza, Año == input$anio_pobreza_barra, Pais == input$pais_pobreza_barra, `Área geográfica` == "Urbana", `Tipo de línea de pobreza extrema y pobreza` == "Línea de pobreza extrema")$Valor, 2), " Dólares", "\n",
                                 "Línea de pobreza: ", round(filter(valores_pobreza, Año == input$anio_pobreza_barra, Pais == input$pais_pobreza_barra, `Área geográfica` == "Urbana", `Tipo de línea de pobreza extrema y pobreza` == "Línea de pobreza")$Valor, 2), " Dólares"),
                   showarrow = FALSE,
                   xanchor = 'right',
                   yanchor = 'top',
                   align = "left",  # Evitar que el texto salga del cuadro
                   font = list(size = 14, color = "#bcc3c7"),
                   bgcolor = "#272c30",  # Fondo blanco con transparencia
                   bordercolor = "#000000",
                   borderwidth = 1
                 )
               )
               )
      
    } else {
      
      valores <- filter(valores_pobreza,`Área geográfica` == "Urbana") |>  
        select(Pais, Año, `Tipo de línea de pobreza extrema y pobreza`, Valor) |> 
        pivot_wider(id_cols = c(Pais, Año),names_from = `Tipo de línea de pobreza extrema y pobreza`, values_from = Valor) |> 
        mutate("Linea indigencia/pobreza" = paste0(round(`Línea de pobreza extrema`, 2), " / ", round(`Línea de pobreza`, 2), " Dólares")) |> 
        select(Pais, Año, `Linea indigencia/pobreza`)
      
      
      graf <- trabajadores_pobreza |> 
        filter(Sexo__ESTANDAR != "Ambos sexos", `Área geográfica` == "Urbana") |> 
        left_join(valores, by = c("Pais", "Año")) |> 
        complete(Año = full_seq(Año, 1), Pais, Sexo__ESTANDAR, `Pobreza extrema y pobreza`) |>  
        group_by(Pais, Sexo__ESTANDAR, `Pobreza extrema y pobreza`) |>
        fill(Valor, `Linea indigencia/pobreza`, .direction = "down") |> 
        ungroup() |> 
        filter(Año == input$anio_pobreza_barra) |> 
        mutate(valor1 = case_when(`Pobreza extrema y pobreza` == "Pobreza" ~ Valor,
                                  T ~ NA),
               valor2 = case_when(`Pobreza extrema y pobreza` == "Pobreza extrema" ~ Valor,
                                  T ~ NA),
               "Sexo y condición económica" = case_when(Sexo__ESTANDAR == "Hombres" & `Pobreza extrema y pobreza` == "Pobreza" ~ "Hombres pobres",
                                                        Sexo__ESTANDAR == "Hombres" & `Pobreza extrema y pobreza` == "Pobreza extrema" ~ "Hombres indigentes",
                                                        Sexo__ESTANDAR == "Mujeres" & `Pobreza extrema y pobreza` == "Pobreza" ~ "Mujeres pobres",
                                                        Sexo__ESTANDAR == "Mujeres" & `Pobreza extrema y pobreza` == "Pobreza extrema" ~ "Mujeres indigentes",
                                                        T ~ "Error")
        ) |> 
        ggplot() +
        aes(x = Pais, fill = `Sexo y condición económica`, group = Sexo__ESTANDAR, Porcentaje = Valor) +
        geom_point(aes(x = Pais, y = 0, text1 = Pais, text2 = `Linea indigencia/pobreza`), alpha = 0, inherit.aes = F) +
        geom_col(position = "dodge", aes(y = valor1)) +
        geom_col(position = "dodge", aes(y = valor2)) +
        scale_fill_manual(values = c("Hombres pobres" = "#41cadb", "Hombres indigentes" = "#219BAB", "Mujeres pobres" = "#F1C8DB", "Mujeres indigentes" = "#E28DB5")) +
        ylab(label = "Población en situación de pobreza (%)") +
        scale_y_continuous(limits = c(0,max(filter(trabajadores_pobreza, Sexo__ESTANDAR != "Ambos sexos",`Área geográfica` == "Urbana")$Valor))) +
        guides(fill=guide_legend(ncol=2)) +
        labs(fill = "") +
        theme(legend.position = "bottom")
      
      ggplotly(graf, tooltip = c("fill", "Valor", "text1", "text2")) |> 
        layout(hovermode = "x unified",
               legend = list(
                 orientation = 'h',   # Set legend to be horizontal
                 x = 0.5,             # Center the legend horizontally
                 xanchor = 'center',
                 y = -0.3,            # Position the legend slightly below the plot
                 yanchor = 'top'
               ))
    }
    
    
  })
  
  ## Economia -------------------------
  
  ### Bump plot --------------
  
  output$graf_bump_economia <- renderPlotly({
    
    if (input$indicador_economia_bump %in% c("Deuda neta como porcentaje del PIB",
                "Estimaciones de Producción Informal, porcentaje del PIB oficial",
                "Inflación, cambio porcentual",
                "Préstamos/endeudamiento neto del gobierno como porcentaje del PIB",
                "Índice de Gini")) {
      graf_evolutivo(Economia, input$indicador_economia_bump)
    } else {
      graf_bump(dataset = Economia, indicador = input$indicador_economia_bump)
    }
    
    
  })
  
  ### Grafico de areas --------------
  
  output$graf_areas <- renderPlotly({
    if (input$indicador_economia_areas == "Distribución del empleo por tipo de empresas"){
      graf_areas
    } else if (input$indicador_economia_areas == "Distribución del empleo por sector"){
      graf_areas_3
    } else {
      graf_areas_2
    }
    
  })
  
  ### Lolliepop Economia ------------
  
  output$lollipop_economia <- renderPlotly({
    
    graf_lollipop(dataset = economia_trabajo, indicador = input$indicador_economia_lollipop,
                  orden = input$orden_economia_lollipop, anio = input$anio_economia_lollipop)
    
  })
  
  ### Graf barras agrupadas Economia ----------------------
  
  
  observeEvent(list(input$indicador_economia_barras, input$pais_economia_barras), {
    
    updateSliderTextInput(
      session = session,
      inputId = "anio_economia_barras",
      choices = sort(unique(filter(Trabajo, Pais == input$pais_economia_barras, str_detect(Indicador, input$indicador_economia_barras))$Año)),
      selected = max(unique(filter(Trabajo, Pais == input$pais_economia_barras, str_detect(Indicador, input$indicador_economia_barras))$Año))
    )
    
  }, ignoreNULL = FALSE)
  
  output$barras_economia <- renderPlotly({
    
    graf_bar_agrup(dataset = Trabajo, anio = input$anio_economia_barras, pais = input$pais_economia_barras, indicador = input$indicador_economia_barras)
    
  })
  
  ### Descripcion acordion Economia -----------------
  
  output$descripcion_indicador_economia_bump <- renderText({
    filter(Economia, Indicador == input$indicador_economia_bump)$Descripción[1]
  })
  
  output$des_lollipop_economia <- renderText({
    
    if (input$indicador_economia_lollipop == "Tasa de participación en la fuerza laboral") {
      "Porcentaje en la participación laboral de cada sexo"
    } else {
      filter(Economia, str_detect(Indicador,input$indicador_economia_lollipop))$Descripción[1]
    }
  })
  
  output$des_barras_economia <- renderText({
    
    if (str_detect(input$indicador_economia_barras,"Proporción de empleo informal por sexo y actividad económica")) {
      "Porcentaje de personas trabajando de forma informal por sexo y actividad económica"
    } else if (str_detect(input$indicador_economia_barras,"Proporción de empleo informal por sexo y edad")) {
      "Porcentaje de personas trabajando de forma informal por sexo y edad"
    } else {
      "Porcentaje de personas trabajando de forma informal por sexo y estado en el empleo"
    }
    
  })
  
  ## Analisis -------------------------
  
  observeEvent(list(input$var_corr_1,input$var_corr_2), {
    anios <- rbind(ciencia_educacion,vida_poblacion,Pobreza, Trabajo, Economia) |> 
      filter(Indicador == input$var_corr_1 | Indicador == input$var_corr_2) |> 
      group_by(Año) |> 
      summarise(n = length(unique(Indicador))) |> 
      filter(n == 2)
    
    anios <- sort(unique(anios$Año))
    
    updateSliderTextInput(
      session = session,
      inputId = "anio_corr",
      label = "Año",
      choices = anios,
      selected = max(anios)
    )
    
  }, ignoreNULL = FALSE)
  
  
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
                 color = "white",
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
      mutate(Indicador = rownames(acp$var$contrib)) |>
      `rownames<-`(NULL) |> 
      mutate_if(is.numeric, round, 4) |> 
      relocate(Indicador, .before = colnames(acp$var$contrib)[1]) |> 
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
  
  ### Grafico Radar -------------------
  
  output$graf_radar <- renderPlotly({
    
    datos_radar <- acp$ind$coord[,1:num_cp] |> 
      bind_cols(Pais = datos_cluster$Pais)
    
    datos_pais_1 <- filter(datos_radar, Pais == input$pais_radar_1)
    datos_pais_2 <- filter(datos_radar, Pais == input$pais_radar_2)

    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = "markers") |>
      add_trace(
        r = rep(1000, times = 5),
        theta = colnames(datos_pais_1)[1:5],
        name = "COLOR_FONDO",
        fillcolor = "#272c30",
        line = list(color = "transparent"),
        showlegend = FALSE,
        hoverinfo = "none",
        opacity = 0.85) |> 
      add_trace(
        r = c(datos_pais_1$Dim.1,datos_pais_1$Dim.2,datos_pais_1$Dim.3,datos_pais_1$Dim.4,datos_pais_1$Dim.5),
        theta = colnames(datos_pais_1)[1:num_cp],
        name = datos_pais_1$Pais,
        fillcolor = "#41cadb",
        opacity = 0.5,
        marker = list(color = "#207AAB")) |> 
      add_trace(
        r = c(datos_pais_2$Dim.1,datos_pais_2$Dim.2,datos_pais_2$Dim.3,datos_pais_2$Dim.4,datos_pais_2$Dim.5),
        theta = colnames(datos_pais_2)[1:num_cp],
        name = datos_pais_2$Pais,
        fillcolor = "#ACFF47",
        opacity = 0.5,
        marker = list(color = "#31E000")) |> 
      layout(
        polar = list(
          bg_color = "#272c30",
          radialaxis = list(
            visible = T,
            range = c(min(datos_radar[1:num_cp]), max(datos_radar[1:num_cp])),
            color = "white",
            gridcolor = "black"
          ),
          angularaxis = list(
            linecolor = "#bcc3c7"
          )
          
        ),
        hovermode = "x unified",
        font = list(color = "#bcc3c7", family = "Arial"),
        paper_bgcolor = "#272c30",
        plot_bgcolor ="#272c30"
      )
    
    
  })
  
  ## Bases de datos -------------
  
  ### Boton descarga --------------------
  
  # Seleccion del conjunto de datos
  
  base_datos <- reactive({
    if (length(input$base_datos) == 0) {ciencia_educacion} else{
      if (input$base_datos == "Desarrollo y educación") {
        ciencia_educacion
        
      } else if (input$base_datos == "Vida y población") {
        vida_poblacion
        
      } else if  (input$base_datos == "Economía y trabajo") {
        economia_trabajo
      } else if  (input$base_datos == "Población por cuantiles") {
        poblacion_edad
      } else if  (input$base_datos == "Pobreza") {
        pobreza_cepal
      } else {ciencia_educacion}
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
    
    if (length(input$base_datos) == 0) {datos <- ciencia_educacion} else{
      if (input$base_datos == "Desarrollo y educación") {
        datos <- ciencia_educacion
        
      } else if (input$base_datos == "Vida y población") {
        datos <- vida_poblacion
        
      } else if  (input$base_datos == "Economía y trabajo") {
        datos <- economia_trabajo
      } else if  (input$base_datos == "Población por cuantiles") {
        datos <- poblacion_edad |> select(-unit, -notes_ids, -source_id) |> select(Pais, Codigo, Indicador, Año, Valor, Descripción, everything())
      } else if  (input$base_datos == "Pobreza") {
        datos <- pobreza_cepal |> filter(`Área geográfica` == "Urbana") |> select(-unit ,-source_id, -notes_ids, -`Área geográfica`)|> select(Pais, Codigo, Indicador, Año, Valor, Descripción, everything())
      } else {datos <- ciencia_educacion}
    }
    datos %>% 
      datatable(selection = "single",escape = F, options = list(scrollX = T,
                                                              scrollY = "450px",
                                                              paging = F,
                                                              scrollCollapse = T,
                                                              lengthMenu = c(10, 15, 30, nrow(datos)),
                                                              columnDefs = list(
                                                                list(width = '50px', targets = c(1,2,4)),
                                                                list(width = '20px', targets = 0),
                                                                list(width = '1800px', targets = c(3,6))
                                                              )
                                                              ))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



# Ideas ------------------

# Agregar regiones de tiempo de eventos, clickear la region abre mas abajo una descripcion

# Opcion de crear tu propio grafico

# Agregar indicadores?

# Agregar la esperanza de vida segun sexo

# Componentes principales opcion cambiar de año

# Agregar cambiar la velocidad con la que se cambia de año

# Arreglar-------------

# Data en hover me tira la info de la recta tmb, evitando que pueda comparar mas paises

# En los graficos de semidona el valor de referencia es el año anterior, estaria bueno que sea el ultimo valor anotado
