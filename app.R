library(shiny)
library(leaflet)
library(viridis)
library(plotly)
library(tidyverse)
library(reshape2)
library(shinyWidgets)
library(shinycustomloader)
# data frame

df <- readRDS("data/final_with_temp.RDS")

# df[ , 32:46] <- lapply(df[ , 32:46], as.Date,format = "%m/%d/%Y")
# 
# df$T1 <- tidyr::replace_na(df$T1, " ")

lst <- list.files("data/WEPP_WITH_DAYMET/", pattern = ".csv")
nos <-sapply( str_split(lst, "_"), `[`, 1) %>%
  as.numeric() %>% sort() 


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

prods <- c(
  "Select Option" = "Select Option",
  "DAYMET_" = "DAYMET",
  "GRIDMET_" = "GRIDMET",
  "PRISM_" = "PRISM",
  "RAW_" = "RAW"
)


sntlno <- c( prepend(nos, "Select Option"))


## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

## ----------------------------------define UI------------------------------------------##
# 

ui <- navbarPage(title = "WEPP Performance Explorer",
                             
                          id="nav",
                 theme = "mytheme.css",
                  
                             
                             tabPanel("Spatial Map",
                                      
                                      div(class="outer",
                                          
                                          tags$head(
                                              # Include our custom CSS
                                              includeCSS("styles.css"),
                                              includeScript("gtag.js"
                                              )
                                              ),
                                          
                                          
                                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                          leafletOutput("map", width="100%", height="100%"),
                                          
                                          
                                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                                          absolutePanel(id = "controls", 
                                                        class = "panel panel-default",
                                                        fixed = TRUE,
                                                        draggable = TRUE, top = 140, left = 180,
                                                        right = "auto", bottom = "auto",
                                                        width = 330, height = "auto", 
                                                        
                                                        pickerInput("metric", "Performance Metric", 
                                                                    vars, selected = "NSE",),
                                                        
                                                       
                                                        
                                                        plotOutput("box", height = 350)
                                                        
                                                        ),
                                         )
                                          
                                         
                                      ),
                 tabPanel("SNOTEL Specific Time Series",
                          
                          tags$head(
                            includeScript("gtag.js"
                            )
                          ),
                          
                          sidebarPanel(width = 3,
                                       
                                       pickerInput("product", "Precipitation Product", 
                                                   prods, selected = NULL),
                                       
                                       pickerInput("snotel", "SNOTEL ID", 
                                                   sntlno, selected = NULL),
                                       ),
                          mainPanel(width = 9,
                                    
                                    fluidRow(
                                      column(
                                        12,
                                        offset = 0,
                                        plotlyOutput("ts", height = 600) %>% 
                                          withLoader(type = "text",
                                                     loader = list(
                                                       marquee("Please select the precipitaiton product and SNOTEL ID to visualize",
                                                               style = "font-size:25px", scrollamount = 5,behavior = "alternate")))
                                        
                                      ))
                                    
                                            
                                    
                                    )
                 )
                 )
                             
  
## ----------------------------------define server logic------------------------------------------##

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
        dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism", "raw")))%>%
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
            dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism", "raw")))%>%
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
              dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism", "raw")))%>%
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
                dplyr::select(sntl_station, state, contains(c("daymet", "gridmet", "prism", "raw")))%>%
                dplyr::select(sntl_station, state, starts_with(c("peak_pbias_"))) %>%
                reshape2::melt()  
            }
          }
    
  })
  
  
  
  output$box <- renderPlot({
    par(mar=c(1,3,0,0))
    boxplot(value~variable,
            data=data_map(),
            main=" ",
            xlab=" ",
            ylab=" ",
            col="#69b3a2",
            border="black",
            las=1,
            names=c('DAYMET','GRIDMET','PRISM', 'RAW')
    )
  })
    

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
                             "Product:", "DAYMET", "<br>",
                             "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                      df$T4," ", df$T5," ", df$T6," ",
                                                      df$T7," ", df$T8," ", df$T9," ",
                                                      df$T10," ", df$T11," ", df$T12," ",
                                                      df$T13," ", df$T14," ", df$T15),"<br>" ),
                 radius = 6000,
                 color = ~pal_nse(nse_daymet), fillOpacity = 0.6,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "DAYMET",
                 layerId = ~paste0("D_", df$sntl_station))%>%
      
      addCircles(lng = ~longitude, lat = ~latitude, weight = 0.5,
                 popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                             "Elevation (ft):", df$snotel_elev_ft, "<br>",
                             "Observation Years:", df$period_yrs, "<br>",
                             "NSE:", round(df$nse_gridmet,3), "<br>",
                             "Product:", "GRIDMET", "<br>",
                             "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                      df$T4," ", df$T5," ", df$T6," ",
                                                      df$T7," ", df$T8," ", df$T9," ",
                                                      df$T10," ", df$T11," ", df$T12," ",
                                                      df$T13," ", df$T14," ", df$T15),"<br>" ),
                 radius = 6000,
                 color = ~pal_nse(nse_gridmet), fillOpacity = 0.6,
                 highlightOptions = highlightOptions(weight = 10),
                 label = ~sntl_name,
                 group = "GRIDMET",
                 layerId = ~paste0("G_", df$sntl_station)) %>%
          
          addCircles(lng = ~longitude, lat = ~latitude, weight = 0.5,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>",
                                 "NSE:", round(df$nse_prism,3), "<br>",
                                 "Product:", "PRISM", "<br>",
                                 "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                          df$T4," ", df$T5," ", df$T6," ",
                                                          df$T7," ", df$T8," ", df$T9," ",
                                                          df$T10," ", df$T11," ", df$T12," ",
                                                          df$T13," ", df$T14," ", df$T15),"<br>" ),
                     radius = 6000,
                     color = ~pal_nse(nse_prism), fillOpacity = 0.6,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "PRISM",
                     layerId = ~paste0("P_", df$sntl_station)) %>%
          
          addCircles(lng = ~longitude, lat = ~latitude, weight = 0.5,
                     popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", 
                                 df$sntl_id, "<br>",
                                 "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                 "Observation Years:", df$period_yrs, "<br>",
                                 "NSE:", round(df$nse_raw,3), "<br>",
                                 "Product:", "RAW", "<br>",
                                 "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                          df$T4," ", df$T5," ", df$T6," ",
                                                          df$T7," ", df$T8," ", df$T9," ",
                                                          df$T10," ", df$T11," ", df$T12," ",
                                                          df$T13," ", df$T14," ", df$T15),"<br>" ),
                     radius = 6000,
                     color = ~pal_nse(nse_raw), fillOpacity = 0.6,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "RAW",
                     layerId = ~paste0("R_", df$sntl_station))%>%
      
      addLegend(pal = pal_nse, values = ~nse_daymet,title = "",
                group = c("DAYMET", "GRIDMET","PRISM", "RAW"),
                position = "topleft" )%>%
      addLayersControl(overlayGroups = c("DAYMET", "GRIDMET", "PRISM", "RAW"),
                       position = "topleft",
                       options = layersControlOptions(collapsed = FALSE))%>% 
          hideGroup(c("PRISM", "GRIDMET", "RAW"))
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
                                 "Product:", "DAYMET", "<br>",
                                 "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                          df$T4," ", df$T5," ", df$T6," ",
                                                          df$T7," ", df$T8," ", df$T9," ",
                                                          df$T10," ", df$T11," ", df$T12," ",
                                                          df$T13," ", df$T14," ", df$T15),"<br>" ),
                     radius = 6000,
                     color = ~pal_pbias(pbias_daymet), fillOpacity = 0.6,
                     highlightOptions = highlightOptions(weight = 10),
                     label = ~sntl_name,
                     group = "DAYMET",
                     layerId = ~paste0("D_", df$sntl_station))%>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "PBIAS:", round(df$pbias_gridmet,3), "<br>",
                                   "Product:", "GRIDMET", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_pbias(pbias_gridmet), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "GRIDMET",
                       layerId = ~paste0("G_", df$sntl_station))%>%

            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "PBIAS:", round(df$pbias_prism,3), "<br>",
                                   "Product:", "PRISM", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_pbias(pbias_prism), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "PRISM",
                       layerId = ~paste0("P_", df$sntl_station)) %>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:",
                                   df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "PBIAS:", round(df$pbias_raw,3), "<br>",
                                   "Product:", "RAW", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_pbias(pbias_raw), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "RAW",
                       layerId = ~paste0("R_", df$sntl_station))%>%
            

            addLegend(pal = pal_pbias, values = ~pbias_daymet,title = "",
                      group = c("DAYMET", "GRIDMET","PRISM", "RAW"),
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
                                   "Product:", "DAYMET", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_daymet), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "DAYMET",
                       layerId = ~paste0("D_", df$sntl_station))%>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "NSE:", round(df$peak_nse_gridmet,3), "<br>",
                                   "Product:", "GRIDMET", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_gridmet), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "GRIDMET",
                       layerId = ~paste0("G_", df$sntl_station))%>%

            addCircles(lng = ~longitude, lat = ~latitude, weight = 0.6,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "NSE:", round(df$peak_nse_prism,3), "<br>",
                                   "Product:", "PRISM", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_prism), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "PRISM",
                       layerId = ~paste0("P_", df$sntl_station)) %>%
            
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                   "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                   "Observation Years:", df$period_yrs, "<br>",
                                   "NSE:", round(df$peak_nse_raw,3), "<br>",
                                   "Product:", "RAW", "<br>",
                                   "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                            df$T4," ", df$T5," ", df$T6," ",
                                                            df$T7," ", df$T8," ", df$T9," ",
                                                            df$T10," ", df$T11," ", df$T12," ",
                                                            df$T13," ", df$T14," ", df$T15),"<br>" ),
                       radius = 6000,
                       color = ~pal_nse(peak_nse_raw), fillOpacity = 0.6,
                       highlightOptions = highlightOptions(weight = 10),
                       label = ~sntl_name,
                       group = "RAW",
                       layerId = ~paste0("R_", df$sntl_station))%>%
            
            addLegend(pal = pal_nse, values = ~nse_daymet,title = "",
                      group = c("DAYMET", "GRIDMET","PRISM", "RAW"),
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
                                     "Product:", "DAYMET", "<br>",
                                     "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                              df$T4," ", df$T5," ", df$T6," ",
                                                              df$T7," ", df$T8," ", df$T9," ",
                                                              df$T10," ", df$T11," ", df$T12," ",
                                                              df$T13," ", df$T14," ", df$T15),"<br>" ),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_daymet), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "DAYMET",
                         layerId = ~paste0("D_", df$sntl_station))%>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>",
                                     "Peak PBIAS:", round(df$peak_pbias_gridmet,3), "<br>",
                                     "Product:", "GRIDMET", "<br>",
                                     "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                              df$T4," ", df$T5," ", df$T6," ",
                                                              df$T7," ", df$T8," ", df$T9," ",
                                                              df$T10," ", df$T11," ", df$T12," ",
                                                              df$T13," ", df$T14," ", df$T15),"<br>" ),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_gridmet), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "GRIDMET",
                         layerId = ~paste0("G_", df$sntl_station))%>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>",
                                     "Peak PBIAS:", round(df$peak_pbias_prism,3), "<br>",
                                     "Product:", "PRISM", "<br>",
                                     "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                              df$T4," ", df$T5," ", df$T6," ",
                                                              df$T7," ", df$T8," ", df$T9," ",
                                                              df$T10," ", df$T11," ", df$T12," ",
                                                              df$T13," ", df$T14," ", df$T15),"<br>" ),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_prism), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "PRISM",
                         layerId = ~paste0("P_", df$sntl_station)) %>%
              
              addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                         popup=paste("SNOTEL name:", df$sntl_name, "<br>", "SNOTEL ID:", df$sntl_id, "<br>",
                                     "Elevation (ft):", df$snotel_elev_ft, "<br>",
                                     "Observation Years:", df$period_yrs, "<br>",
                                     "Peak PBIAS:", round(df$peak_pbias_raw,3), "<br>",
                                     "Product:", "RAW", "<br>",
                                     "Sensor Change:", paste0(df$T1, " ", df$T2," ",df$T3," ",
                                                              df$T4," ", df$T5," ", df$T6," ",
                                                              df$T7," ", df$T8," ", df$T9," ",
                                                              df$T10," ", df$T11," ", df$T12," ",
                                                              df$T13," ", df$T14," ", df$T15),"<br>" ),
                         radius = 6000,
                         color = ~pal_pbias(peak_pbias_raw), fillOpacity = 0.6,
                         highlightOptions = highlightOptions(weight = 10),
                         label = ~sntl_name,
                         group = "RAW",
                         layerId = ~paste0("R_", df$sntl_station))%>%
              
              addLegend(pal = pal_pbias, values = ~pbias_daymet,title = "",
                        group = c("DAYMET", "GRIDMET","PRISM", "RAW"),
                        position = "topleft" )
            
          }else{
            leafletProxy("map",  data = df) %>%
              clearMarkers() %>%
              clearShapes() 
          }
            
        })
 
  
  observe({
    pprod <- input$product
    sntlno <- input$snotel

    if (pprod == "DAYMET") {
      path <- paste0("data/WEPP_WITH_",pprod,"/")
      # print(path)
      if(sntlno == "Select Option"){
        return()
      }else
      tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
      tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
      # print(str(tsf))
      output$ts <- renderPlotly({
        req(pprod)
        req(sntlno)
        ax <- list(
          title = " "
        )
        
        fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, 
                       type = 'scatter', mode = 'lines', name = "Observed SWE (mm)",
                       line = list(color = '#000000'))
        fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)', 
                                 mode = 'lines', line = list(color = '#FF0000')) 
        fig <- fig %>% layout(xaxis = ax, yaxis = ax, 
                              legend = list(orientation = "h",   # show entries horizontally
                                            xanchor = "center",  # use center of legend as anchor
                                            x = 0.5),
                              margin = list( pad = 0, autoexpand = TRUE))
        fig
      })
    }else{
      if (pprod == "GRIDMET") {
        path <- paste0("data/WEPP_WITH_",pprod,"/")
        # print(path)
        if(sntlno == "Select Option"){
          return()
        }else{
          tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
          tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
          # print(str(tsf))
          output$ts <- renderPlotly({
            req(pprod)
            req(sntlno)
            ax <- list(
              title = " "
            )
            
            fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, type = 'scatter', 
                           mode = 'lines', name = "Observed SWE (mm)",
                           line = list(color = '#000000'))
            fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)',
                                     mode = 'lines',
                                     line = list(color = '#FF0000')) 
            fig <- fig %>% layout(xaxis = ax, yaxis = ax, 
                                  legend = list(orientation = "h",   # show entries horizontally
                                                xanchor = "center",  # use center of legend as anchor
                                                x = 0.5))
            fig
            })
      }
      }else{
        if (pprod == "PRISM") {
          path <- paste0("data/WEPP_WITH_",pprod,"/")
          # print(path)
          if(sntlno == "Select Option"){
            return()
          }else{
            tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
            tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
            # print(str(tsf))
            output$ts <- renderPlotly({
              req(pprod)
              req(sntlno)
              ax <- list(
                title = " "
              )
              
              fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, type = 'scatter',
                             mode = 'lines', name = "Observed SWE (mm)",
                             line = list(color = '#000000'))
              fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)',
                                       mode = 'lines',
                                       line = list(color = '#FF0000')) 
              fig <- fig %>% layout(xaxis = ax, yaxis = ax, 
                                    legend = list(orientation = "h",   # show entries horizontally
                                                  xanchor = "center",  # use center of legend as anchor
                                                  x = 0.5))
              fig
            })
          }
        }else{
          if(pprod == "RAW"){
            path <- paste0("data/WEPP_WITH_",pprod,"/")
            # print(path)
            if(sntlno == "Select Option"){
              return()
            }else{
              tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
              tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
              # print(str(tsf))
              output$ts <- renderPlotly({
                req(pprod)
                req(sntlno)
                ax <- list(
                  title = " "
                )
                
                fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, type = 'scatter',
                               mode = 'lines', name = "Observed SWE (mm)",
                               line = list(color = '#000000'))
                fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)',
                                         mode = 'lines',
                                         line = list(color = '#FF0000')) 
                fig <- fig %>% layout(xaxis = ax, yaxis = ax, 
                                      legend = list(orientation = "h",   # show entries horizontally
                                                    xanchor = "center",  # use center of legend as anchor
                                                    x = 0.5))
                fig
              })
            }
            
          }
                      
                    }
      }
    }
       
  
    })

    
}

shinyApp(ui, server)