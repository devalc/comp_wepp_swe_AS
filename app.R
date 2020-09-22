library(shiny)
library(leaflet)

df <- readRDS("data/final.rds")

qpal <- colorQuantile("YlOrRd", df$states , n = 12)

pal <- colorFactor(
  palette = 'viridis',
  domain = df$states
)

ui <- navbarPage(title = "WEPP-SNOTEL-comparison",
                             
                          id="nav",
                             
                             tabPanel("Interactive map",
                                      div(class="outer",
                                          
                                          tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              
                                          ),
                                          
                                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                          leafletOutput("map", width="100%", height="100%"),
                                          
                                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                        width = 330, height = "auto",
                                                        
                                                        h2("Options"),
                                                        
                                                        # selectInput("color", "Color", vars),
                                                        # selectInput("size", "Size", vars, selected = "adultpop"),
                                                        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                        #                  # Only prompt for threshold when coloring or sizing by superzip
                                                        #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                                        ),
                                                        
                                                        plotOutput("histCentile", height = 200),
                                                        plotOutput("scatterCollegeIncome", height = 250)
                                          )
                                          
                                         
                                      )
                             )
  


server <- function(input, output, session) {
    
    # data frame
    
    
    # static backround map
    
    output$map <- renderLeaflet({
        leaflet(df) %>%
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    
    leafletProxy("map", data = df) %>% 
        addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   popup = ~sntl_name, radius = ~40000*nse_bcqc, 
                   color = ~pal(state), fillOpacity = .1)
    
    # qpal <- colorQuantile("YlOrRd", df$state, n = 12)
    
    # reactive circles map
    
    # leafletProxy("map", data = filteredData()) %>%
    #     clearShapes() %>%
    #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
    #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
    #     )
    
    # observe({
    #     leafletProxy("map", data = df) %>%
    #         clearShapes() %>%
    #         addCircles(lng=~longitude,
    #                    lat=~latitude,
    #                    color = ~pal(state),
    #                    layerId = ~nse_bcqc) ### Assigning df id to layerid
    # })  
    
  
}

shinyApp(ui, server)