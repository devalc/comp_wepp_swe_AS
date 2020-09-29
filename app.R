library(shiny)
library(leaflet)
library(viridis)
library(plotly)
library(tidyverse)
library(plotly)
library(reshape2)
library(dplyr)

# data frame

df <- readRDS("data/final.rds")

# colnames(df)[27] <- "lat"
# colnames(df)[28] <- "long"

# color palette

pal_nse <- colorBin("BuPu", df$nse_daymet, bins = c(-40,-20, -10,-5,0,.2,0.4,0.6,0.8,1.0), pretty = TRUE,
         na.color = "#808080", alpha = FALSE, reverse = FALSE,
         right = FALSE)


pal_pbias <- colorBin('inferno', df$pbias_daymet, bins = c(-100, -75, -50, -25,0, 25, 50, 75, 100,300,500,700), pretty = TRUE,
                    na.color = "#808080", alpha = FALSE, reverse = TRUE,
                    right = FALSE)


# Choices for drop-downs
vars <- c(
  "NSE" = "NSE",
  "PBIAS" = "PBIAS",
  "Peak_NSE" = "Peak_NSE",
  "Peak_PBIAS" = "Peak_PBIAS"
)



ui <- navbarPage(title = "WEPP Performance Explorer",
                             
                          id="nav",
                             
                             tabPanel("Comparison With SNOTEL Sites",
                                      div(class="outer",
                                          
                                          tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              # includeScript("gomaps.js"),
                                              
                                          ),
                                          
                                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                          leafletOutput("map", width="100%", height="100%"),
                                          
                                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                                          absolutePanel(id = "controls", 
                                                        class = "panel panel-default",
                                                        fixed = TRUE,
                                                        draggable = TRUE, top = 60, left = "auto",
                                                        right = 20, bottom = "auto",
                                                        width = 330, height = "auto", 
                                                        
                                                        h2(" "),
                                                        
                                                        # selectInput("color", "Color", vars),
                                                        selectInput("metric", "Performance Metric", 
                                                                    vars, selected = "NSE"),
                                                        h4(" "),
                                                        plotOutput("box", height = 300),
                                                        
                                                        # h4(" "),
                                                        # plotOutput("heat", height = 300)
                                                        
                                                        ),
                                                        
                                          
                                          #plotlyOutput("heatmap", height = 300),
                                                        # plotOutput("scatterCollegeIncome", height = 250)
                                          )
                                          
                                         
                                      )
                             )
  


server <- function(input, output, session) {
    
    output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(df) %>%
      leaflet::addTiles(attribution = paste('Analyses by <a href="https://scholar.google.com/citations?user=ePWCTKoAAAAJ&hl=en">Anurag Srivastava</a>',
                                            'Tool developed by <a href="https://chinmaydeval.netlify.app/">Chinmay Deval</a> |',sep = "|"),
                                            
                        ) %>%
      leaflet::addMarkers()
  })
    

  data_map <- reactive({
    req(input$metric)
    req(input$map_bounds)
    if (input$metric == "NSE") {
    if (is.null(input$map_bounds)){
      df
    } else {
      bounds <- input$map_bounds
      df %>%
        dplyr::filter(latitude > bounds$south & latitude < bounds$north & longitude < bounds$east & longitude > bounds$west) %>%
        dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism")))%>%
        dplyr::select(sntl_station, state, starts_with(c("nse_"))) %>%
        reshape2::melt()  
    }
    }else
      if (input$metric == "PBIAS") {
        if (is.null(input$map_bounds)){
          df
        } else {
          bounds <- input$map_bounds
          df %>%
            dplyr::filter(latitude > bounds$south & latitude < bounds$north & longitude < bounds$east & longitude > bounds$west) %>%
            dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism")))%>%
            dplyr::select(sntl_station, state, starts_with(c("peak_pbias_"))) %>%
            reshape2::melt()  
        }
      }else
        if (input$metric == "Peak_NSE") {
          if (is.null(input$map_bounds)){
            df
          } else {
            bounds <- input$map_bounds
            df %>%
              dplyr::filter(latitude > bounds$south & latitude < bounds$north & longitude < bounds$east & longitude > bounds$west) %>%
              dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism")))%>%
              dplyr::select(sntl_station, state, starts_with(c("peak_nse_"))) %>%
              reshape2::melt()  
          }
        }else
          if (input$metric == "Peak_PBIAS") {
            if (is.null(input$map_bounds)){
              df
            } else {
              bounds <- input$map_bounds
              df %>%
                dplyr::filter(latitude > bounds$south & latitude < bounds$north & longitude < bounds$east & longitude > bounds$west) %>%
                dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism")))%>%
                dplyr::select(sntl_station, state, starts_with(c("peak_pbias_"))) %>%
                reshape2::melt()  
            }
          }
    
  })
  
  
  
  output$box <- renderPlot({
   
    boxplot(value~variable,
            data=data_map(),
            main=" ",
            xlab=" ",
            ylab=" ",
            col="#69b3a2",
            border="black",
            las=2,
            names=c('DAYMET','GRIDMET','PRISM')
    )
  })
    

  
  # output$heat <- renderPlotly(
  #   
  #   ggplot(data_map(), aes(variable, sntl_station,  fill = value)) +
  #     geom_tile(inherit.aes = TRUE)  +
  #     scale_fill_distiller(palette =  "RdBu", direction = -1) +
  #     theme(
  #       axis.text.x = element_text(angle = 90, colour = "Black"),
  #       axis.text.y = element_text(colour = "Black"),
  #       axis.title = element_blank(),
  #       legend.position='right')
  #   # ggplot(data_map(), aes(x=data_map()$variable, y=data_map()$value)) + 
  #   #   geom_boxplot() + theme_bw() +
  #   #   theme(axis.title.x=element_blank(),
  #   #         axis.title.y=element_blank(),
  #   #         #axis.text.x=element_blank(),
  #   #         axis.ticks.x=element_blank()
  #   #   ) + coord_flip()
  # )
 

    observe({
      metricval <- input$metric 
      
      if (input$metric == "NSE") {
    
      leafletProxy("map",  data = df) %>% 
        addTiles() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls()%>%
      
      
      addCircles(lng = ~longitude, lat = ~latitude, weight = 0.5,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>",
                             "NSE:", round(df$nse_daymet,3), "<br>",
                             "Product:", "DAYMET", "<br>"),
                 radius = 6000,
                 color = ~pal_nse(nse_daymet), fillOpacity = 0.6,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "DAYMET")%>%
      
      addCircles(lng = ~longitude, lat = ~latitude, weight = 0.5,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>",
                             "NSE:", round(df$nse_gridmet,3), "<br>",
                             "Product:", "GRIDMET", "<br>"),
                 radius = 6000,
                 color = ~pal_nse(nse_gridmet), fillOpacity = 0.6,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "GRIDMET") %>%
          
          addCircles(lng = ~longitude, lat = ~latitude, weight = 0.5,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>",
                                 "NSE:", round(df$nse_prism,3), "<br>",
                                 "Product:", "PRISM", "<br>"),
                     radius = 6000,
                     color = ~pal_nse(nse_prism), fillOpacity = 0.6,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "PRISM") %>%
      
      addLegend(pal = pal_nse, values = ~nse_daymet,title = "",
                group = c("DAYMET", "GRIDMET","PRISM"),
                position = "topleft" )%>%
      addLayersControl(overlayGroups = c("DAYMET", "GRIDMET", "PRISM"),
                       position = "topleft",
                       options = layersControlOptions(collapsed = FALSE))%>% hideGroup(c("PRISM", "GRIDMET"))
      }else
        if (input$metric == "PBIAS") {

        leafletProxy("map",  data = df) %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%

          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>",
                                 "PBIAS:", round(df$pbias_daymet,3), "<br>",
                                 "Product:", "DAYMET", "<br>"),
                     radius = 6000,
                     color = ~pal_pbias(pbias_daymet), fillOpacity = 0.6,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "DAYMET")%>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "PBIAS:", round(df$pbias_gridmet,3), "<br>",
                                   "Product:", "GRIDMET", "<br>"),
                       radius = 6000,
                       color = ~pal_pbias(pbias_gridmet), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "GRIDMET")%>%

            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "PBIAS:", round(df$pbias_prism,3), "<br>",
                                   "Product:", "PRISM", "<br>"),
                       radius = 6000,
                       color = ~pal_pbias(pbias_prism), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "PRISM") %>%

            addLegend(pal = pal_pbias, values = ~pbias_daymet,title = "",
                      group = c("DAYMET", "GRIDMET","PRISM"),
                      position = "topleft" )
      }else
        if (input$metric == "Peak_NSE") {
          
          leafletProxy("map",  data = df) %>%
            clearMarkers() %>%
            clearShapes() %>%
            clearControls() %>%

            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "NSE:", round(df$peak_nse_daymet,3), "<br>",
                                   "Product:", "DAYMET", "<br>"),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_daymet), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "DAYMET")%>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "NSE:", round(df$peak_nse_gridmet,3), "<br>",
                                   "Product:", "GRIDMET", "<br>"),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_gridmet), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "GRIDMET")%>%

            addCircles(lng = ~longitude, lat = ~latitude, weight = 0.6,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "NSE:", round(df$peak_nse_prism,3), "<br>",
                                   "Product:", "PRISM", "<br>"),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_prism), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "PRISM") %>%
            
            addLegend(pal = pal_nse, values = ~nse_daymet,title = "",
                      group = c("DAYMET", "GRIDMET","PRISM"),
                      position = "topleft" )
          # 
        }else
          if (input$metric == "Peak_PBIAS") {
            
            leafletProxy("map",  data = df) %>%
              clearMarkers() %>%
              clearShapes() %>%
              clearControls() %>%

              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>",
                                     "Peak PBIAS:", round(df$peak_pbias_daymet,3), "<br>",
                                     "Product:", "DAYMET", "<br>"),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_daymet), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "DAYMET")%>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>",
                                     "Peak PBIAS:", round(df$peak_pbias_gridmet,3), "<br>",
                                     "Product:", "GRIDMET", "<br>"),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_gridmet), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "GRIDMET")%>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>",
                                     "Peak PBIAS:", round(df$peak_pbias_prism,3), "<br>",
                                     "Product:", "PRISM", "<br>"),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_prism), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "PRISM") %>%
              
              addLegend(pal = pal_pbias, values = ~pbias_daymet,title = "",
                        group = c("DAYMET", "GRIDMET","PRISM"),
                        position = "topleft" )
            
          }else{
            leafletProxy("map",  data = df) %>%
              clearMarkers() %>%
              clearShapes() 
          }
            
        })
   
    
   
    
    
    
}

shinyApp(ui, server)