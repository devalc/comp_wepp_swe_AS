library(shiny)
library(leaflet)
library(viridis)

# data frame

df <- readRDS("data/final.rds")

# color palette

# qpal <- colorFactor("viridis", df$state, n = 12)

pal <- colorFactor(
  palette = 'viridis',
  domain = df$states
)


# Choices for drop-downs
vars <- c(
  "NSE" = "NSE",
  "PBIAS" = "PBIAS",
  "Peak_NSE" = "Peak_NSE",
  "Peak_PBIAS" = "Peak_PBIAS"
)


ui <- navbarPage(title = "WEPP Precipitation Performance Explorer",
                             
                          id="nav",
                             
                             tabPanel("map",
                                      div(class="outer",
                                          
                                          tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              # includeScript("gomaps.js"),
                                              
                                          ),
                                          
                                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                          leafletOutput("map", width="100%", height="100%"),
                                          
                                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                        width = 330, height = "auto",
                                                        
                                                        h2("Options"),
                                                        
                                                        # selectInput("color", "Color", vars),
                                                        selectInput("metric", "Performance Metric", vars, selected = "NSE")
                                                        
                                                        ),
                                                        
                                                        plotOutput("heatmap", height = 300),
                                                        # plotOutput("scatterCollegeIncome", height = 250)
                                          )
                                          
                                         
                                      )
                             )
  


server <- function(input, output, session) {
    
  in_bounding_box <- function(data, lat, long, bounds) {
    data %>%
      dplyr::filter(
        lat > bounds$south &
          lat < bounds$north &
          long < bounds$east & long > bounds$west 
      )
  }
    
    # static backround map
    
    output$map <- renderLeaflet({
        leaflet(df) %>%
        addTiles() %>%
            fitBounds(~min(df$longitude), ~min(df$latitude), ~max(df$longitude), ~max(df$latitude))%>%
        setView(mean(df$longitude), mean(df$latitude), zoom = 5)
    })
    
    
    

    observe({
      metricval <- input$metric 
      
      if (input$metric == "NSE") {
    
      leafletProxy("map",  data = df) %>% 
        addTiles() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls()%>%
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>"),
                 radius = ~30000*(nse_bcqc),
                 color = ~pal(state), fillOpacity = .1,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "BCQC") %>%
      
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>"),
                 radius = ~30000*(nse_daymet),
                 color = ~pal(state), fillOpacity = .1,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "DAYMET")%>%
      
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>"),
                 radius = ~30000*(nse_gridmet),
                 color = ~pal(state), fillOpacity = .1,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "GRIDMET") %>%
      
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>"),
                 radius = ~30000*(nse_raw),
                 color = ~pal(state), fillOpacity = .1,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "RAW") %>%
      
      addLegend(pal = pal, values = ~state,
                group = c("BCQC", "DAYMET", "GRIDMET","RAW"),
                position = "topleft" )%>%
      addLayersControl(overlayGroups = c("BCQC", "DAYMET", "GRIDMET","RAW"),
                       position = "topleft",
                       options = layersControlOptions(collapsed = FALSE))%>% hideGroup(c("DAYMET", "GRIDMET","RAW"))
      }else
        if (input$metric == "PBIAS") {
         
        leafletProxy("map",  data = df) %>%
          clearMarkers() %>%
          clearShapes() %>%
          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>"),
                     radius = ~500*(pbias_bcqc),
                     color = ~pal(state), fillOpacity = .1,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "BCQC") %>%
          
          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>"),
                     radius = ~500*(pbias_daymet),
                     color = ~pal(state), fillOpacity = .1,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "DAYMET")%>%
          
          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>"),
                     radius = ~500*(pbias_gridmet),
                     color = ~pal(state), fillOpacity = .1,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "GRIDMET") %>%
          
          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>"),
                     radius = ~500*(pbias_raw),
                     color = ~pal(state), fillOpacity = .1,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "RAW")
      }else
        if (input$metric == "Peak_NSE") {
          
          leafletProxy("map",  data = df) %>%
            clearMarkers() %>%
            clearShapes() %>%
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>"),
                       radius = ~30000*(peak_nse_bcqc),
                       color = ~pal(state), fillOpacity = .1,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "BCQC") %>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>"),
                       radius = ~30000*(peak_nse_daymet),
                       color = ~pal(state), fillOpacity = .1,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "DAYMET")%>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>"),
                       radius = ~30000*(peak_nse_gridmet),
                       color = ~pal(state), fillOpacity = .1,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "GRIDMET") %>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>"),
                       radius = ~30000*(peak_nse_raw),
                       color = ~pal(state), fillOpacity = .1,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "RAW")
          
        }else
          if (input$metric == "Peak_PBIAS") {
            
            leafletProxy("map",  data = df) %>%
              clearMarkers() %>%
              clearShapes() %>%
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>"),
                         radius = ~500*(peak_pbias_bcqc),
                         color = ~pal(state), fillOpacity = .1,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "BCQC") %>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>"),
                         radius = ~500*(peak_pbias_daymet),
                         color = ~pal(state), fillOpacity = .1,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "DAYMET")%>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>"),
                         radius = ~500*(peak_pbias_gridmet),
                         color = ~pal(state), fillOpacity = .1,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "GRIDMET") %>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>"),
                         radius = ~500*(peak_pbias_raw),
                         color = ~pal(state), fillOpacity = .1,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "RAW")
            
          }else{
            leafletProxy("map",  data = df) %>%
              clearMarkers() %>%
              clearShapes() 
          }
            
        })
    
    data_map <- reactive({
      if (is.null(input$map_bounds)) {
        df
      } else {
        bounds <- input$map_bounds
        in_bounding_box(df, latitude, longitude, bounds)
      }
    })
   
    # output$heatmap <- renderPlot(
    #   d<- data_map() %>% dplyr::select(snotel_id, startsWith("nse_")) %>%
    #     dplyr::mutate_if(artsWith("nse_"), scale)
    #   d.m <- reshape2::melt(d)
    #   
    #   a<-ggplot(d.m, aes(, variable,  fill= value)) +
    #     geom_tile(inherit.aes = TRUE)  +
    #     scale_fill_distiller(palette = "BrBG", direction = -1) +
    #     theme(
    #       axis.text.x = element_text(angle = 90,colour = "Black", size = 12, face = "bold"),
    #       axis.text.y = element_text(colour = "Black", size = 12, face = 'bold'),
    #       axis.title = element_blank()
    # )
    
}

shinyApp(ui, server)