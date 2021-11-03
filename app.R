
library(shinyWidgets)
library(shiny)
library(echarts4r) 
library(globe4r)
library(dplyr)
library(COVID19)
library(countrycode)
library(bslib)



paises <- countrycode::codelist$country.name.en

shinyApp(ui= 
           fluidPage(
             
             column(width = 12, align = "center",
                    titlePanel(span("¿Cómo se comporta el COVID en tu país?", 
                                    style = "color: gray; font-size: 42px;
                                  font-family: 'times';"))
                    
             ),
             column(width = 12, align = "center",
                    span("Por", a("Jorge Hernández", href = "https://twitter.com/ElJorgeHdz",
                                  style = "color: #3770bf; font-size: 12px;
                                   font-family: 'times';"),
                         style = "color: #000; font-size: 12px;
                                   font-family: 'times';"
                    )
                    
                    
                    
             ),
             br(),
             column(width = 4),
             column(width = 4, align = "center",
                    p("Tras casi 2 años de pandemia, probablemente lo último que quisieras saber es que los casos de coronavirus están incrementando en tu región. Si bien hay mucha información al respecto sobre COVID-19 en internet, me he dado a la tarea de crear esta aplicación para que puedas revisar los casos de todo el mundo actualizados al día.",
                      style = "font-family: 'times'; font-si16pt")),
             column(width = 4),
             column(width = 12, align = "center",
                    div(style="display: inline-block;vertical-align:top; width: 300px;font-family: 'times';",
                        selectInput("var", "Escoge un país", 
                                    choices = paises)),
                    div(style="display: inline-block;vertical-align:top; width: 300px;font-family: 'times';",
                        prettyRadioButtons(
                          inputId = "tipograf",
                          label = "Tipo de gráfico:", 
                          choices = c("Casos nuevos" = "confirmed", "Vacunas diarias" = "vaccines")
                        )
                    ),
                    div(style="display: inline-block;vertical-align:top; width: 300px; horizontal-align: right;",
                        globeOutput("globe", height = 200, width = 200))
                    
             ),
             column(width = 12,
                    echarts4rOutput(
                      "grafico", width = 1380, height = 500
                    ))
           ),
         
         server = function(input, output){
           
           output$grafico <- renderEcharts4r({
             if (nrow(COVID19::covid19(country = input$var )) == 0){
               COVID19::covid19(country = input$var) |> 
                 e_charts(date) |> 
                 e_draft(text = paste0("No hay datos para ",noquote( input$var)), size = "50px")
             }else{
               
               fecha <- COVID19::covid19(country = input$var) |>
                 select(date) |> 
                 tail(1)
               
               
               COVID19::covid19(country = input$var) |> 
                 mutate(nuevos = .data[[input$tipograf]]  -lag(.data[[input$tipograf]])
                 ) |> 
                 filter(nuevos > 0) |> 
                 mutate(media = zoo::rollmean(nuevos, k = 50, fill = NA)) |> 
                 as.data.frame() |> 
                 e_charts(date, dispose = FALSE) |> 
                 e_bar(nuevos, name = "Casos nuevos diarios") |> 
                 e_title(paste0("Casos diarios confirmados para ", noquote(input$var)),
                         paste0("Fecha de actualización: ", fecha$date ), left = "center",
                         textStyle = list(
                           color = "gray",
                           size = 18
                         )
                 ) |> 
                 e_line(media, symbol ="none", name = "Media movil 7 días") |> 
                 e_tooltip(trigger = "axis") |> 
                 e_color(color = c("#91343c", "#344a91")) |> 
                 e_theme("auritus") |> 
                 e_y_axis(show = FALSE) |> 
                 e_legend(FALSE) |> 
                 e_draft(text = " ", size = "50px")
             }
             
           }) |> 
             bindCache(input$var)
           
           
           casos <- reactive({
             COVID19::covid19(country = input$var) 
           }) |> 
             bindCache(input$var)
           
           output$globe <- renderGlobe({
             create_globe() |> 
               globe_img_url(image_url("blue-marble")) |>  
               #globe_bars(coords(lat, long, color = mag), data = quakes) %>%
               #scale_bars_color() |> 
               globe_background(color = "#fff")
           })
           
           observeEvent(input$var, {
             globe_proxy("globe") |> 
               globe_pov(
                 altitude = 1.5,
                 casos()$latitude[1],
                 casos()$longitude[1]
               ) 
           })
           
           
         }
         
)
