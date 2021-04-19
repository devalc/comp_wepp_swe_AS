## --------------------------------------------------------------------------------------##
##
## Script name: 
##
## Purpose of the script:
##
## Author: Chinmay Deval
##
## Created On: 2021-03-30
##
## Copyright (c) Chinmay Deval, 2021
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##  Notes: #TBD 1. before/after first sensor change stats for obs/sim swe
##  2. add cumulative precip plots for each product vs snotel.
##   
##
## --------------------------------------------------------------------------------------##



## ----------------------------------Load packages---------------------------------------##

library(shiny)
library(leaflet)
library(viridis)
library(plotly)
library(tidyverse)
library(reshape2)
library(shinyWidgets)
library(shinycustomloader)
library(qs)
library(hrbrthemes)
library(cowplot)
library(bslib)
# data frame

df <- readRDS("data/final_with_temp.RDS")
df1 <- df
df1[ , 32:46]<- lapply(df[ , 32:46], as.Date,format = "%m/%d/%Y")


lst <- list.files("data/WEPP_WITH_DAYMET/", pattern = ".csv")
nos <-sapply( str_split(lst, "_"), `[`, 1) %>%
  as.numeric() %>% sort() 


# color palette

pal_nse <- colorBin("BuPu", df$nse_daymet, bins = c(-40,-20, -10,-5,0,.2,0.4,0.6,0.8,1.0), 
                    pretty = TRUE,
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
  # "Select Option" = "Select Option",
  "DAYMET" = "DAYMET",
  "GRIDMET" = "GRIDMET",
  "PRISM" = "PRISM",
  "RAW" = "RAW"
)



# sntlno <- c( prepend(nos, "Select Option"))
sntlno <- nos

dfpath<- "data/Climate_merged_dfs_cd"

## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

## ----------------------------------define UI------------------------------------------##
# 

ui <- navbarPage(title = "WEPP Performance Explorer",
                             
                          id="nav",
                 # theme = "mytheme.css",
                 theme = bslib::bs_theme(primary = "#F5946D", secondary = "#25E7F3", 
                                         success = "#A8CEDD", bootswatch = "flatly"),
                  
                             
                             tabPanel("Spatial Map",
                                      
                                      div(class="outer",
                                          
                                          tags$head(
                                              # Include our custom CSS
                                              includeCSS("www/styles.css"),
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
                                                        
                                                       
                                                        
                                                        plotlyOutput("box", height = 350)
                                                        
                                                        ),
                                         )
                                          
                                         
                                      ),
                 tabPanel("SNOTEL Specific Time Series",
                          
                          tags$head(
                            includeScript("gtag.js"
                            )
                          ),
                          sidebarLayout(
                          sidebarPanel(width = 3,
                                       
                                       # pickerInput("product", "Precipitation Product", 
                                       #             prods, selected = "DAYMET"),
                                       
                                       pickerInput("snotel", "SNOTEL ID", 
                                                   sntlno, selected = 623),
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
                                    
                                            
                                    
                                    ))
                 ),
                 tabPanel("Climate Comparisons",
                          
                          tags$head(
                            includeScript("gtag.js"
                            )
                          ),
                          
                          sidebarLayout(
                          sidebarPanel(width = 3,
                                       
                                       # pickerInput("product", "Precipitation Product", 
                                       #             prods, selected = NULL),
                                       
                                       pickerInput("snotelcc", "SNOTEL ID", 
                                                   sntlno, selected = 623),
                                       
                                       pickerInput("clivar", "Climate variable",
                                                   choices = c("precip_mm",
                                                               "duration_hr",
                                                               "tp",
                                                               "ip",
                                                               "tmax",
                                                               "tmin",
                                                               "rad",
                                                               # "w-vl",
                                                               # "w-dir",
                                                               "tdew"),
                                                   selected = "tmin"),
                                       # actionButton("update", "Update"),
                          ),
                          mainPanel(width = 9,
                                    
                                    fluidRow(
                                      column(
                                        6,
                                        offset = 0,
                                        plotlyOutput("prod_scatter_prism") %>% shinycssloaders::withSpinner()
                                        ),
                                      column(
                                        6,
                                        offset = 0,
                                        plotlyOutput("prod_scatter_daymet")%>% shinycssloaders::withSpinner()
                                      )
                                      ),
                                    HTML("<br>"),
                                    fluidRow(
                                      column(
                                        6,
                                        offset = 0,
                                        plotlyOutput("prod_scatter_gridmet")%>% shinycssloaders::withSpinner()
                                      ),
                                      column(
                                        6,
                                        offset = 0,
                                        plotlyOutput("prod_scatter_bcqc")%>% shinycssloaders::withSpinner()
                                      )
                                    )
                                    # 
                                    
                                    
                          ))
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
  
  
  
  output$box <- renderPlotly({
    par(mar=c(1,3,0,0))
    p <- ggplot(data_map(), aes(x=variable, y=value, fill=variable)) + 
      geom_boxplot()+
      guides(fill=FALSE)+ theme(axis.text.x = element_text(angle = 90),
                                legend.position = "none",
                                axis.title = element_blank())
    
    fig <- ggplotly(p)
    
    fig
    
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
 
  # v <- reactiveValues(ts= NULL)
  
  # observeEvent(input$Update,{
    observe({
    sntlno <- input$snotel
    if(sntlno == "Select Option"){
      return()
    }else
    path1 <- paste0("data/WEPP_WITH_","DAYMET","/")
    path2 <- paste0("data/WEPP_WITH_","GRIDMET","/")
    path3 <- paste0("data/WEPP_WITH_","PRISM","/")
    path4 <- paste0("data/WEPP_WITH_","RAW","/")
    tsf_daymet <- read.csv(list.files(path1,pattern = input$snotel,full.names = T))
    tsf_daymet$Date <- as.Date(tsf_daymet$Date, "%Y-%m-%d")
    tsf_gridmet <- read.csv(list.files(path2,pattern = input$snotel,full.names = T))
    tsf_gridmet$Date <- as.Date(tsf_gridmet$Date, "%Y-%m-%d")
    tsf_prism <- read.csv(list.files(path3,pattern = input$snotel,full.names = T))
    tsf_prism$Date <- as.Date(tsf_prism$Date, "%Y-%m-%d")
    tsf_raw <- read.csv(list.files(path4,pattern = input$snotel,full.names = T))
    tsf_raw$Date <- as.Date(tsf_raw$Date, "%Y-%m-%d")
    df1 <- df1 %>% filter(sntl_id == input$snotel)
    schangevals <- as.vector(as.matrix(df1[,c("T1", "T2", "T3",
                                              "T4", "T5", "T6",
                                              "T7", "T8", "T9",
                                              "T10", "T11", "T12",
                                              "T13", "T14", "T5")]))
    
    output$ts <- renderPlotly({
      # req(pprod)
      req(sntlno)
      ax <- list(
        title = " "
      )
      
      fig <- plot_ly(tsf_daymet, x = ~Date, y = ~Observed.SWE, 
                     type = 'scatter', mode = 'lines', name = "Observed SWE (mm)",
                     line = list(color = '#003f5c'))
      fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'DAYMET Simulated SWE (mm)', 
                               mode = 'lines', line = list(color = '#58508d')) 
      fig <- fig %>% add_trace(data =tsf_gridmet, y = ~Simulated.SWE, name = 'GRIDMET Simulated SWE (mm)', 
                               mode = 'lines', line = list(color = '#bc5090'))
      fig <- fig %>% add_trace(data =tsf_prism, y = ~Simulated.SWE, name = 'PRISM Simulated SWE (mm)', 
                               mode = 'lines', line = list(color = '#ff6361'))
      fig <- fig %>% add_trace(data =tsf_raw, y = ~Simulated.SWE, name = 'RAW Simulated SWE (mm)', 
                               mode = 'lines', line = list(color = '#ffa600'))
      
      fig <- fig %>% 
        add_trace(x =schangevals[1],type = 'scatter', mode = 'lines', 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>%
        add_trace(x =schangevals[2],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[3],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[4],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[5],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[6],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[7],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[8],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[9],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[10],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[11],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[12],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[13],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[14],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
        add_trace(x =schangevals[15],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
                  line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change') %>%
        layout(xaxis = ax, yaxis = ax, 
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5),
               margin = list( pad = 0, autoexpand = TRUE))
      fig
    
    })
    # })
  })
  
  # observe({
  #   pprod <- input$product
  #   sntlno <- input$snotel
  # 
  #   if (pprod == "DAYMET") {
  #     path <- paste0("data/WEPP_WITH_",pprod,"/")
  #     # print(path)
  #     if(sntlno == "Select Option"){
  #       return()
  #     }else
  #     tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
  #     tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
  #     df1 <- df1 %>% filter(sntl_id == input$snotel)
  #     schangevals <- as.vector(as.matrix(df1[,c("T1", "T2", "T3",
  #                                                  "T4", "T5", "T6",
  #                                                  "T7", "T8", "T9",
  #                                                  "T10", "T11", "T12",
  #                                                  "T13", "T14", "T5")]))
  # 
  #     output$ts <- renderPlotly({
  #       req(pprod)
  #       req(sntlno)
  #       ax <- list(
  #         title = " "
  #       )
  #       
  #       fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, 
  #                      type = 'scatter', mode = 'lines', name = "Observed SWE (mm)",
  #                      line = list(color = '#000000'))
  #       fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)', 
  #                                mode = 'lines', line = list(color = '#FF0000')) 
  #       fig <- fig %>% 
  #         add_trace(x =schangevals[1],type = 'scatter', mode = 'lines', 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>%
  #         add_trace(x =schangevals[2],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[3],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[4],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[5],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[6],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[7],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[8],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[9],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[10],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[11],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[12],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[13],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[14],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #         add_trace(x =schangevals[15],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                   line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change') %>%
  #         layout(xaxis = ax, yaxis = ax, 
  #                             legend = list(orientation = "h",   # show entries horizontally
  #                                           xanchor = "center",  # use center of legend as anchor
  #                                           x = 0.5),
  #                             margin = list( pad = 0, autoexpand = TRUE))
  #       fig
  #       
  #     })
  #   }else{
  #     if (pprod == "GRIDMET") {
  #       path <- paste0("data/WEPP_WITH_",pprod,"/")
  #       # print(path)
  #       if(sntlno == "Select Option"){
  #         return()
  #       }else{
  #         tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
  #         tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
  #         # print(str(tsf))
  #         df1 <- df1 %>% filter(sntl_id == input$snotel)
  #         schangevals <- as.vector(as.matrix(df1[,c("T1", "T2", "T3",
  #                                                   "T4", "T5", "T6",
  #                                                   "T7", "T8", "T9",
  #                                                   "T10", "T11", "T12",
  #                                                   "T13", "T14", "T5")]))
  #         
  #         
  #         output$ts <- renderPlotly({
  #           req(pprod)
  #           req(sntlno)
  #           ax <- list(
  #             title = " "
  #           )
  #           
  #           fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, type = 'scatter', 
  #                          mode = 'lines', name = "Observed SWE (mm)",
  #                          line = list(color = '#000000'))
  #           fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)',
  #                                    mode = 'lines',
  #                                    line = list(color = '#FF0000')) 
  #           fig <- fig %>% 
  #             add_trace(x =schangevals[1],type = 'scatter', mode = 'lines', 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>%
  #             add_trace(x =schangevals[2],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[3],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[4],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[5],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[6],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[7],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[8],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[9],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[10],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[11],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[12],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[13],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[14],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #             add_trace(x =schangevals[15],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                       line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change') %>% layout(xaxis = ax, yaxis = ax, 
  #                                 legend = list(orientation = "h",   # show entries horizontally
  #                                               xanchor = "center",  # use center of legend as anchor
  #                                               x = 0.5))
  #           fig
  #           })
  #     }
  #     }else{
  #       if (pprod == "PRISM") {
  #         path <- paste0("data/WEPP_WITH_",pprod,"/")
  #         # print(path)
  #         if(sntlno == "Select Option"){
  #           return()
  #         }else{
  #           tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
  #           tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
  #           # print(str(tsf))
  #           df1 <- df1 %>% filter(sntl_id == input$snotel)
  #           schangevals <- as.vector(as.matrix(df1[,c("T1", "T2", "T3",
  #                                                     "T4", "T5", "T6",
  #                                                     "T7", "T8", "T9",
  #                                                     "T10", "T11", "T12",
  #                                                     "T13", "T14", "T5")]))
  #           
  #           
  #           output$ts <- renderPlotly({
  #             req(pprod)
  #             req(sntlno)
  #             ax <- list(
  #               title = " "
  #             )
  #             
  #             fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, type = 'scatter',
  #                            mode = 'lines', name = "Observed SWE (mm)",
  #                            line = list(color = '#000000'))
  #             fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)',
  #                                      mode = 'lines',
  #                                      line = list(color = '#FF0000')) 
  #             fig <- fig %>% 
  #               add_trace(x =schangevals[1],type = 'scatter', mode = 'lines', 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>%
  #               add_trace(x =schangevals[2],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[3],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[4],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[5],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[6],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[7],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[8],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[9],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[10],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[11],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[12],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[13],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[14],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #               add_trace(x =schangevals[15],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                         line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change') %>% layout(xaxis = ax, yaxis = ax, 
  #                                   legend = list(orientation = "h",   # show entries horizontally
  #                                                 xanchor = "center",  # use center of legend as anchor
  #                                                 x = 0.5))
  #             fig
  #           })
  #         }
  #       }else{
  #         if(pprod == "RAW"){
  #           path <- paste0("data/WEPP_WITH_",pprod,"/")
  #           # print(path)
  #           if(sntlno == "Select Option"){
  #             return()
  #           }else{
  #             tsf <- read.csv(list.files(path,pattern = input$snotel,full.names = T))
  #             tsf$Date <- as.Date(tsf$Date, "%Y-%m-%d")
  #             # print(str(tsf))
  #             df1 <- df1 %>% filter(sntl_id == input$snotel)
  #             schangevals <- as.vector(as.matrix(df1[,c("T1", "T2", "T3",
  #                                                       "T4", "T5", "T6",
  #                                                       "T7", "T8", "T9",
  #                                                       "T10", "T11", "T12",
  #                                                       "T13", "T14", "T5")]))
  #             
  #             
  #             output$ts <- renderPlotly({
  #               req(pprod)
  #               req(sntlno)
  #               ax <- list(
  #                 title = " "
  #               )
  #               
  #               fig <- plot_ly(tsf, x = ~Date, y = ~Observed.SWE, type = 'scatter',
  #                              mode = 'lines', name = "Observed SWE (mm)",
  #                              line = list(color = '#000000'))
  #               fig <- fig %>% add_trace(y = ~Simulated.SWE, name = 'Simulated SWE (mm)',
  #                                        mode = 'lines',
  #                                        line = list(color = '#FF0000')) 
  #               fig <- fig %>% 
  #                 add_trace(x =schangevals[1],type = 'scatter', mode = 'lines', 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>%
  #                 add_trace(x =schangevals[2],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[3],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[4],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[5],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[6],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[7],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[8],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[9],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[10],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[11],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[12],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[13],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[14],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change')%>% 
  #                 add_trace(x =schangevals[15],type = 'scatter', mode = 'lines',  showlegend = FALSE, 
  #                           line = list(color = 'Blue',dash = 'dash'),name = 'Sensor change') %>% layout(xaxis = ax, yaxis = ax, 
  #                                     legend = list(orientation = "h",   # show entries horizontally
  #                                                   xanchor = "center",  # use center of legend as anchor
  #                                                   x = 0.5))
  #               fig
  #             })
  #           }
  #           
  #         }
  #                     
  #                   }
  #     }
  #   }
  #      
  # 
  #   })
  
  
  
  observe({
    
    input$snotelcc
    input$clivar

  scatdf<- reactive({
    
    req(input$snotelcc)
    req(input$clivar)
    
    schange_date <- df1 %>% filter(sntl_id == input$snotelcc) %>% pull(T1)
    
   qs::qread(list.files(dfpath,
                                   pattern = input$snotelcc,
                                   full.names = T))  %>% 
      dplyr::select(Date, dplyr::contains(input$clivar)) %>% 
      na.omit() %>% dplyr::mutate(change = case_when(Date <= schange_date ~ "before", TRUE ~ "after"))
    
  })
  
 output$prod_scatter_prism <- renderPlotly({
      req(scatdf())
        
      if (input$clivar == "tmin") {
       scatdf() %>% ggplot(aes(x = tmin_raw_snotel, 
                     y = tmin_prism, color = change, shape = change)) +
                       geom_point() + theme_bw() + scale_colour_ipsum() + 
         theme(axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               title = element_text(hjust = 0.5),
               legend.position = "right"
         )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
          geom_abline(slope = 1, intercept = 0, color = "blue",
                      size = 1, linetype = "dashed",show.legend = TRUE)+
          geom_smooth(method=lm , se=F,
                      size = 1,show.legend = TRUE) 
      }else
        if (input$clivar == "tmax") {
          
          scatdf() %>% ggplot(aes(x = tmax_raw_snotel, 
                                  y = tmax_prism, color = change, shape = change)) +
            geom_point() + theme_bw() + scale_colour_ipsum() + 
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  title = element_text(hjust = 0.5)
            )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
            geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
            geom_smooth(method=lm , se=TRUE, size = 1) 
          
        }else
          if (input$clivar == "tdew") {
            
            scatdf() %>% ggplot(aes(x = tdew_raw_snotel, 
                                    y = tdew_prism, color = change, shape = change)) +
              geom_point() + theme_bw() + scale_colour_ipsum() + 
              theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    title = element_text(hjust = 0.5)
              )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
              geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
              geom_smooth(method=lm , se=TRUE, size = 1) 
            
          }else
            if (input$clivar == "rad") {
              
              scatdf() %>% ggplot(aes(x = rad_raw_snotel, 
                                      y = rad_prism, color = change, shape = change)) +
                geom_point() + theme_bw() + scale_colour_ipsum() + 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      title = element_text(hjust = 0.5)
                )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                geom_smooth(method=lm , se=TRUE, size = 1) 
              
            }else
              if (input$clivar == "w-vl") {
                
                scatdf() %>% ggplot(aes(x = w-vl_raw_snotel, 
                                        y = w-vl_prism, color = change, shape = change)) +
                  geom_point() + theme_bw() + scale_colour_ipsum() + 
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        title = element_text(hjust = 0.5)
                  )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                  geom_smooth(method=lm , se=TRUE, size = 1) 
                
              }else
                if (input$clivar == "w-dir") {
                  
                  scatdf() %>% ggplot(aes(x = w-dir_raw_snotel, 
                                          y = w-dir_prism, color = change, shape = change)) +
                    geom_point() + theme_bw() + scale_colour_ipsum() + 
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          title = element_text(hjust = 0.5)
                    )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                    geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                    geom_smooth(method=lm , se=TRUE, size = 1) 
                  
                }else
                  if (input$clivar == "tp") {
                    
                    scatdf() %>% ggplot(aes(x = tp_raw_snotel, 
                                            y = tp_prism, color = change, shape = change)) +
                      geom_point() + theme_bw() + scale_colour_ipsum() + 
                      theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            title = element_text(hjust = 0.5)
                      )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                      geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                      geom_smooth(method=lm , se=TRUE, size = 1) 
                    
                  }else
                    if (input$clivar == "ip") {
                      
                      scatdf() %>% ggplot(aes(x = ip_raw_snotel, 
                                              y = ip_prism, color = change, shape = change)) +
                        geom_point() + theme_bw() + scale_colour_ipsum() + 
                        theme(axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              title = element_text(hjust = 0.5)
                        )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                        geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                        geom_smooth(method=lm , se=TRUE, size = 1) 
                      
                    }else
                      if (input$clivar == "duration_hr") {
                        
                        scatdf() %>% ggplot(aes(x = duration_hr_raw_snotel, 
                                                y = duration_hr_prism, color = change, shape = change)) +
                          geom_point() + theme_bw() + scale_colour_ipsum() + 
                          theme(axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                title = element_text(hjust = 0.5)
                          )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                          geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                          geom_smooth(method=lm , se=TRUE, size = 1) 
                        
                      }else
                        if (input$clivar == "precip_mm") {
                          
                          scatdf() %>% ggplot(aes(x = precip_mm_raw_snotel, 
                                                  y = precip_mm_prism, color = change, shape = change)) +
                            geom_point() + theme_bw() + scale_colour_ipsum() + 
                            theme(axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  title = element_text(hjust = 0.5)
                            )+ggtitle("PRISM vs. SNOTEL (1:1 line - Dashed Blue)")+
                            geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                            geom_smooth(method=lm , se=TRUE, size = 1) 
                          
                        }
        
      })
      
      output$prod_scatter_daymet <- renderPlotly({
        req(scatdf())
        
        if (input$clivar == "tmin") {
        scatdf() %>% ggplot(aes(x = tmin_raw_snotel, 
                                y = tmin_daymet, color = change, shape = change)) +
          geom_point() + theme_bw()+ scale_colour_ipsum() + 
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                title = element_text(hjust = 0.5)
                )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
          geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
          geom_smooth(method=lm , se=TRUE, size=1) 
        }else
          if (input$clivar == "tmax") {
            
            scatdf() %>% ggplot(aes(x = tmax_raw_snotel, 
                                    y = tmax_daymet, color = change, shape = change)) +
              geom_point() + theme_bw() + scale_colour_ipsum() + 
              theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    title = element_text(hjust = 0.5)
              )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
              geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
              geom_smooth(method=lm , se=TRUE, size = 1) 
            
          }else
            if (input$clivar == "tdew") {
              
              scatdf() %>% ggplot(aes(x = tdew_raw_snotel, 
                                      y = tdew_daymet, color = change, shape = change)) +
                geom_point() + theme_bw() + scale_colour_ipsum() + 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      title = element_text(hjust = 0.5)
                )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                geom_smooth(method=lm ,  se=TRUE, size = 1) 
              
            }else
              if (input$clivar == "rad") {
                
                scatdf() %>% ggplot(aes(x = rad_raw_snotel, 
                                        y = rad_daymet, color = change, shape = change)) +
                  geom_point() + theme_bw()  + scale_colour_ipsum()+ 
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        title = element_text(hjust = 0.5)
                  )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                  geom_smooth(method=lm , se=TRUE, size = 1) 
                
              }else
                if (input$clivar == "w-vl") {
                  
                  scatdf() %>% ggplot(aes(x = w-vl_raw_snotel, 
                                          y = w-vl_daymet, color = change, shape = change)) +
                    geom_point() + theme_bw() + scale_colour_ipsum() + 
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          title = element_text(hjust = 0.5)
                    )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                    geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                    geom_smooth(method=lm ,  se=TRUE, size = 1) 
                  
                }else
                  if (input$clivar == "w-dir") {
                    
                    scatdf() %>% ggplot(aes(x = w-dir_raw_snotel, 
                                            y = w-dir_daymet, color = change, shape = change)) +
                      geom_point() + theme_bw()+ scale_colour_ipsum()  + 
                      theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            title = element_text(hjust = 0.5)
                      )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                      geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                      geom_smooth(method=lm ,  se=TRUE, size = 1) 
                    
                  }else
                    if (input$clivar == "tp") {
                      
                      scatdf() %>% ggplot(aes(x = tp_raw_snotel, 
                                              y = tp_daymet, color = change, shape = change)) +
                        geom_point() + theme_bw() + scale_colour_ipsum() + 
                        theme(axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              title = element_text(hjust = 0.5)
                        )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                        geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                        geom_smooth(method=lm , se=TRUE, size = 1) 
                      
                    }else
                      if (input$clivar == "ip") {
                        
                        scatdf() %>% ggplot(aes(x = ip_raw_snotel, 
                                                y = ip_daymet, color = change, shape = change)) +
                          geom_point() + theme_bw()  + scale_colour_ipsum()+ 
                          theme(axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                title = element_text(hjust = 0.5)
                          )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                          geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                          geom_smooth(method=lm ,  se=TRUE, size = 1) 
                        
                      }else
                        if (input$clivar == "duration_hr") {
                          
                          scatdf() %>% ggplot(aes(x = duration_hr_raw_snotel, 
                                                  y = duration_hr_daymet, color = change, shape = change)) +
                            geom_point() + theme_bw() + scale_colour_ipsum() + 
                            theme(axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  title = element_text(hjust = 0.5)
                            )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                            geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                            geom_smooth(method=lm ,  se=TRUE, size = 1) 
                          
                        }else
                          if (input$clivar == "precip_mm") {
                            
                            scatdf() %>% ggplot(aes(x = precip_mm_raw_snotel, 
                                                    y = precip_mm_daymet, color = change, shape = change)) +
                              geom_point() + theme_bw() + scale_colour_ipsum() + 
                              theme(axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    title = element_text(hjust = 0.5)
                              )+ggtitle("DAYMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                              geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                              geom_smooth(method=lm ,  se=TRUE, size = 1) 
                            
                          }
      })
      
      output$prod_scatter_gridmet <- renderPlotly({
        req(scatdf())
        
        if (input$clivar == "tmin") {
          
       
        scatdf() %>% ggplot(aes(x = tmin_raw_snotel, 
                                y = tmin_gridmet, color = change, shape = change)) +
          geom_point() + theme_bw() + scale_colour_ipsum() + 
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                title = element_text(hjust = 0.5)
          )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
          geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
          geom_smooth(method=lm , se=TRUE, size = 1) 
          
        }else
          if (input$clivar == "tmax") {
            
            scatdf() %>% ggplot(aes(x = tmax_raw_snotel, 
                                    y = tmax_gridmet, color = change, shape = change)) +
              geom_point() + theme_bw() + scale_colour_ipsum() + 
              theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    title = element_text(hjust = 0.5)
              )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
              geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
              geom_smooth(method=lm ,  se=TRUE, size = 1) 
            
          }else
            if (input$clivar == "tdew") {
              
              scatdf() %>% ggplot(aes(x = tdew_raw_snotel, 
                                      y = tdew_gridmet, color = change, shape = change)) +
                geom_point() + theme_bw() + scale_colour_ipsum() + 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      title = element_text(hjust = 0.5)
                )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                geom_smooth(method=lm ,  se=TRUE, size = 1) 
              
            }else
              if (input$clivar == "rad") {
                
                scatdf() %>% ggplot(aes(x = rad_raw_snotel, 
                                        y = rad_gridmet, color = change, shape = change)) +
                  geom_point() + theme_bw() + scale_colour_ipsum() + 
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        title = element_text(hjust = 0.5)
                  )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                  geom_smooth(method=lm ,se=TRUE, size = 1) 
                
              }else
                if (input$clivar == "w-vl") {
                  
                  scatdf() %>% ggplot(aes(x = w-vl_raw_snotel, 
                                          y = w-vl_gridmet, color = change, shape = change)) +
                    geom_point() + theme_bw() + scale_colour_ipsum() + 
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          title = element_text(hjust = 0.5)
                    )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                    geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                    geom_smooth(method=lm ,  se=TRUE, size = 1) 
                  
                }else
                  if (input$clivar == "w-dir") {
                    
                    scatdf() %>% ggplot(aes(x = w-dir_raw_snotel, 
                                            y = w-dir_gridmet, color = change, shape = change)) +
                      geom_point() + theme_bw() + scale_colour_ipsum() + 
                      theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            title = element_text(hjust = 0.5)
                      )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                      geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                      geom_smooth(method=lm ,  se=TRUE, size = 1) 
                    
                  }else
                    if (input$clivar == "tp") {
                      
                      scatdf() %>% ggplot(aes(x = tp_raw_snotel, 
                                              y = tp_gridmet, color = change, shape = change)) +
                        geom_point() + theme_bw()+ scale_colour_ipsum()  + 
                        theme(axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              title = element_text(hjust = 0.5)
                        )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                        geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                        geom_smooth(method=lm ,  se=TRUE, size = 1) 
                      
                    }else
                      if (input$clivar == "ip") {
                        
                        scatdf() %>% ggplot(aes(x = ip_raw_snotel, 
                                                y = ip_gridmet, color = change, shape = change)) +
                          geom_point() + theme_bw() + scale_colour_ipsum() + 
                          theme(axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                title = element_text(hjust = 0.5)
                          )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                          geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                          geom_smooth(method=lm ,  se=TRUE, size = 1) 
                        
                      }else
                        if (input$clivar == "duration_hr") {
                          
                          scatdf() %>% ggplot(aes(x = duration_hr_raw_snotel, 
                                                  y = duration_hr_gridmet, color = change, shape = change)) +
                            geom_point() + theme_bw() + scale_colour_ipsum() + 
                            theme(axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  title = element_text(hjust = 0.5)
                            )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                            geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                            geom_smooth(method=lm , se=TRUE, size = 1) 
                          
                        }else
                          if (input$clivar == "precip_mm") {
                            
                            scatdf() %>% ggplot(aes(x = precip_mm_raw_snotel, 
                                                    y = precip_mm_gridmet, color = change, shape = change)) +
                              geom_point() + theme_bw() + scale_colour_ipsum() + 
                              theme(axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    title = element_text(hjust = 0.5)
                              )+ggtitle("GRIDMET vs. SNOTEL (1:1 line - Dashed Blue)")+
                              geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                              geom_smooth(method=lm ,  se=TRUE, size = 1) 
                            
                          }
        
      })
      
      
      output$prod_scatter_bcqc <- renderPlotly({
        req(scatdf())
        
       if (input$clivar == "tmin") {
         
         scatdf() %>% ggplot(aes(x = tmin_raw_snotel, 
                                 y = tmin_bcqc, color = change, shape = change)) +
           geom_point() + theme_bw() + scale_colour_ipsum() + 
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 title = element_text(hjust = 0.5)
           )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
           geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
           geom_smooth(method=lm ,  se=TRUE, size = 1) 
         
       }else
         if (input$clivar == "tmax") {
           
           scatdf() %>% ggplot(aes(x = tmax_raw_snotel, 
                                   y = tmax_bcqc, color = change, shape = change)) +
             geom_point() + theme_bw()+ scale_colour_ipsum()  + 
             theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   title = element_text(hjust = 0.5)
             )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
             geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
             geom_smooth(method=lm , se=TRUE, size = 1) 
           
         }else
           if (input$clivar == "tdew") {
             
             scatdf() %>% ggplot(aes(x = tdew_raw_snotel, 
                                     y = tdew_bcqc, color = change, shape = change)) +
               geom_point() + theme_bw() + scale_colour_ipsum() + 
               theme(axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     title = element_text(hjust = 0.5)
               )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
               geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
               geom_smooth(method=lm ,se=TRUE, size = 1) 
             
           }else
             if (input$clivar == "rad") {
               
               scatdf() %>% ggplot(aes(x = rad_raw_snotel, 
                                       y = rad_bcqc, color = change, shape = change)) +
                 geom_point() + theme_bw() + scale_colour_ipsum() + 
                 theme(axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       title = element_text(hjust = 0.5)
                 )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
                 geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                 geom_smooth(method=lm , se=TRUE, size = 1) 
               
             }else
               if (input$clivar == "w-vl") {
                 
                 scatdf() %>% ggplot(aes(x = w-vl_raw_snotel, 
                                         y = w-vl_bcqc, color = change, shape = change)) +
                   geom_point() + theme_bw() + scale_colour_ipsum() + 
                   theme(axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         title = element_text(hjust = 0.5)
                   )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
                   geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                   geom_smooth(method=lm ,  se=TRUE, size = 1) 
                 
               }else
                 if (input$clivar == "w-dir") {
                   
                   scatdf() %>% ggplot(aes(x = w-dir_raw_snotel, 
                                           y = w-dir_bcqc, color = change, shape = change)) +
                     geom_point() + theme_bw() + scale_colour_ipsum() + 
                     theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           title = element_text(hjust = 0.5)
                     )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
                     geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                     geom_smooth(method=lm , se=TRUE, size = 1) 
                   
                 }else
                   if (input$clivar == "tp") {
                     
                     scatdf() %>% ggplot(aes(x = tp_raw_snotel, 
                                             y = tp_bcqc, color = change, shape = change)) +
                       geom_point() + theme_bw()+ scale_colour_ipsum()  + 
                       theme(axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             title = element_text(hjust = 0.5)
                       )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
                       geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                       geom_smooth(method=lm , se=TRUE, size = 1) 
                     
                   }else
                     if (input$clivar == "ip") {
                       
                       scatdf() %>% ggplot(aes(x = ip_raw_snotel, 
                                               y = ip_bcqc, color = change, shape = change)) +
                         geom_point() + theme_bw()+ scale_colour_ipsum()  + 
                         theme(axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               title = element_text(hjust = 0.5)
                         )+ggtitle("BCQC vs. SNOTEL (1:1 line - Dashed Blue)")+
                         geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                         geom_smooth(method=lm , se=TRUE, size = 1) 
                       
                     }else
                       if (input$clivar == "duration_hr (1:1 line - Dashed Blue)") {
                         
                         scatdf() %>% ggplot(aes(x = duration_hr_raw_snotel, 
                                                 y = duration_hr_bcqc, color = change, shape = change)) +
                           geom_point() + theme_bw() + scale_colour_ipsum() + 
                           theme(axis.title.x = element_blank(),
                                 axis.title.y = element_blank(),
                                 title = element_text(hjust = 0.5)
                           )+ggtitle("BCQC vs. SNOTEL")+
                           geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                           geom_smooth(method=lm ,  se=TRUE, size = 1) 
                         
                       }else
                         if (input$clivar == "precip_mm (1:1 line - Dashed Blue)") {
                           
                           scatdf() %>% ggplot(aes(x = precip_mm_raw_snotel, 
                                                   y = precip_mm_bcqc, color = change, shape = change)) +
                             geom_point() + theme_bw() + scale_colour_ipsum() + 
                             theme(axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
                                   title = element_text(hjust = 0.5)
                             )+ggtitle("BCQC vs. SNOTEL")+
                             geom_abline(slope = 1, intercept = 0, color = "blue", size = 1, linetype = "dashed")+
                             geom_smooth(method=lm ,  se=TRUE, size = 1) 
                           
                         }
        
      })
      
  })
    
}

# run_with_themer(shinyApp(ui, server))
shinyApp(ui, server)