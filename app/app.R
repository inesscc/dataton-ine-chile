library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(shiny)
library(sf)
library(mapview)
library(plotly)

## estructura de datos esperada
# 1 región
# 2 comuna
# 3 manzana
# 4 RBD colegio mas cercano
# 5 distancia colegio (minima en metroz)
# 6 población
# 7 calidad de la institución
# 8 NSE (manzana)
# 9 geometria manzana
# 10 geometry point colegio
# _____________________________________________________________________

# 1. # creamos datos de juguete
# data = chilemapas::censo_2017_comunas %>%
#   group_by(codigo_comuna) %>%
#   summarise(poblacion = sum(poblacion)) %>%
#   left_join(chilemapas::codigos_territoriales %>% select(-codigo_provincia,-nombre_provincia)) %>%
#   right_join(chilemapas::mapa_comunas) %>%
#   mutate(dist = runif(n()),
#          qa = runif(n()),
#          nse = runif(n())) %>%
#    select(pop =poblacion, everything()) %>%
#    sf::st_as_sf()
# #
# 2. # guardamos en formato shape

n_reg <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16")

# 2.1 separamos el dato por regiones y lo guardamos
# purrr::map(n_reg,~dir.create(paste0("app/data/tabla_shiny/r",.)))
#
# fe = split(data,data$codigo_region)
#
# purrr::map2(fe,n_reg,~st_write(.x,paste0("app/data/tabla_shiny/r",.y,"/datos.shp")))

### función para lectura de datos
read_regional_data <- function(region){
  sf::read_sf(paste0("data/tabla_shiny/r",region,"/datos.shp"))
}



## 3. # construimos listas para inputs

## lista de variables

l_variables <- list("pop","dist","qa","nse")

names(l_variables) <- c("distancia","nse","calidad","poblacion")

## lista de regiones
l_region <- as.list(n_reg)

names(l_region) <- c("TARAPACÁ","ANTOFAGASTA","ATACAMA","COQUIMBO","VALPARAÍSO","O'HIGGINS","MAULE","BIOBÍO","LA ARAUCANÍA",
  "LOS LAGOS","AYSÉN","MAGALLANES","METROPOLITANA","LOS RÍOS","ARICA Y PARINACOTA","ÑUBLE")


## 4. Construimos shiny app

ui <- fluidPage(

  shiny::titlePanel("Detection of territorial educational exclusion"),
  sidebarLayout(
  # sidebar
    sidebarPanel(
      selectInput("input_region","Region selection",choices = l_region, selected = "13"),
      radioButtons("input_variable","Variable",choices = l_variables)


  ),
  # main panel
  mainPanel(

    ## opción con plotly
    #plotly::plotlyOutput("mapplot")

    ## opción con ggplot
    plotOutput("ggplot")

      )
   )
)

server <- function(input, output, session) {



 plot <-  reactive({

   print(input$input_variable)

  ## opción con ggplot
   ggplot(read_regional_data(input$input_region)) +
     geom_sf() +
     geom_sf(aes_string(fill = input$input_variable)) +
     scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
     theme_bw()


## opción con plotly
   # plotly::plot_ly(
   # read_regional_data(input$input_region),
   #    split = ~nmbr_cm,
   #    color = ~var_in,
   #    alpha = 1,
   #    showlegend = FALSE
   #  )
 })


output$ggplot <- renderPlot({
   plot()
 })

## opción con plotly

 # output$mapplot <- renderPlotly({
 #   plot()
 # })



}

shinyApp(ui, server)
