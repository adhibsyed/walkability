library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(fontawesome)
library(shinythemes)
library(rsconnect)


ivw <- st_read("data/gdf_IVW.gpkg")
ivw <- st_as_sf(ivw)

boundary <- st_read("data/le_boundary/le_boundary.shp")
boundary <- st_as_sf(boundary)

h3_gdf <- st_read("data/h3_gdf_v2.gpkg")
h3_gdf <- st_as_sf(h3_gdf)

h3_gdf_sub_4 <- st_read("data/h3_sub_4_gdf.gpkg")
h3_gdf_sub_4 <- st_as_sf(h3_gdf_sub_4)

h3_gdf_sub_di_4 <- st_read("data/h3_sub_di_4_gdf.gpkg")
h3_gdf_sub_di_4 <- st_as_sf(h3_gdf_sub_di_4)



h3_gdf <- st_transform(h3_gdf, 4326)
h3_gdf_sub_4 <- st_transform(h3_gdf_sub_4, 4326)
h3_gdf_sub_di_4 <- st_transform(h3_gdf_sub_di_4, 4326)
ivw <- st_transform(ivw, 4326)
boundary <- st_transform(boundary, 4326)

ivw <- st_intersection(ivw, boundary)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"),
    tags$style(HTML('
      .sidebar {
        height: calc(100vh - 80px) !important;
        overflow-y: auto;
      }
      .github-link {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 100%;
      }
      .github-link a {
        color: #95a5a6;
        text-decoration: none;
      }
    '))
  ),
  titlePanel("Walkability Index / Classification of Leicester"),
  sidebarLayout(
    sidebarPanel(
      style = "height: calc(100vh - 80px); overflow-y: auto;",
      width = 3,
      br(),
      p("This dashboard illustrates the results obtained by applying computer vision and unsupervised learning techniques 
      to model the visual walkability of Leicester using Google Street View Images. The adapted Integrated Visual Walkability 
      (IVW) framework quantified walkability by extracting perceptual features through semantic segmentation of street-level 
      images using the SegFormer-B5 model. Additionally, the dashboard highlights the findings from cluster analysis, revealing 
      latent relationships and spatial groupings among various subindicators across the city."),
      br(),
      selectInput("first_dropdown", "Select an option:",
                  choices = c("Integrated Visual Walkability Index", "Walkability Index per Hexagon", "Walkability Clusters")),
      selectInput("second_dropdown", "Select a category:",
                  choices = c("Visual Walkability Index", "Greenery Index", "Crowdedness Index", "Enclosure Index", "Pavement Index")),
      br(),
      p("Click on a Point / Polygon for additional information. Descriptions of the clusters can be viewed below."),
      br(),
      actionButton("open_popup", "View Pen Portraits", style = "display: block; margin: 0 auto;"),
      br(),
      br(),
      div(class = "github-link",
          tags$a(href = "https://github.com/adhibsyed/walkability", target = "_blank",
                 tags$i(class = "fa fa-github", style = "font-size:24px;"))
      )
    ),
    mainPanel(
      width = 9,
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map", width = "100%")
    )
  )
)



server <- function(input, output, session) {
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -1.133333, lat = 52.633331, zoom = 12)
  })
  
  column_mapping <- list(
    "Visual Walkability Index" = "IVW",
    "Greenery Index" = "Gi",
    "Crowdedness Index" = "Ci",
    "Enclosure Index" = "Ei",
    "Pavement Index" = "Pi",
    "Visual Walkability Clusters (K-Means)" = "k4clst_sub",
    "Visual Walkability Clusters (Fuzzy C-Means)" = "c4clst_sub",
    "Filtered Visual Walkability Clusters (Fuzzy C-Means)" = "c4clst_sub",
    "Walkability Clusters with POI Diversity (K-Means)" = "k4clst_sub_di", 
    "Walkability Clusters with POI Diversity (Fuzzy C-Means)" = "c4clst_sub_di",
    "Filtered Walkability Clusters with POI Diversity (Fuzzy C-Means)" = "c4clst_sub_di",
    "Mean Integrated Visual Walkability Index" = "Mean_IVW",
    "Mean Greenery Index" = "Mean_Greenery",
    "Mean Crowdedness Index" = "Mean_Crowdedness",
    "Mean Enclosure Index" = "Mean_Enclosure",
    "Mean Pavement Index" = "Mean_Pavement"
  )
  
  ivw_choices <- c("Visual Walkability Index", "Greenery Index", "Crowdedness Index", "Enclosure Index", "Pavement Index")
  choices_cl <- c("Visual Walkability Clusters (K-Means)",
              "Visual Walkability Clusters (Fuzzy C-Means)",
              "Filtered Visual Walkability Clusters (Fuzzy C-Means)",
              "Walkability Clusters with POI Diversity (K-Means)", 
              "Walkability Clusters with POI Diversity (Fuzzy C-Means)",
              "Filtered Walkability Clusters with POI Diversity (Fuzzy C-Means)")
  choices_mean <- c("Mean Integrated Visual Walkability Index",
                    "Mean Greenery Index",
                    "Mean Crowdedness Index",
                    "Mean Enclosure Index",
                    "Mean Pavement Index")
  flipped <-  c("Mean Crowdedness Index",
                "Mean Enclosure Index")
  walk_clst <- c("Visual Walkability Clusters (K-Means)",
                "Visual Walkability Clusters (Fuzzy C-Means)",
                "Walkability Clusters with POI Diversity (K-Means)", 
                "Walkability Clusters with POI Diversity (Fuzzy C-Means)")
  filt_vis_walk <- c("Filtered Visual Walkability Clusters (Fuzzy C-Means)")
  filt_walk_di <- c("Filtered Walkability Clusters with POI Diversity (Fuzzy C-Means)")
  
  walk_clst_km <- c("Visual Walkability Clusters (K-Means)",
                    "Walkability Clusters with POI Diversity (K-Means)")
  walk_clst_fcm_sub <- c("Visual Walkability Clusters (Fuzzy C-Means)")
  walk_clst_fcm_sub_filt <- c("Filtered Visual Walkability Clusters (Fuzzy C-Means)")
  walk_clst_fcm_sub_di <- c("Walkability Clusters with POI Diversity (Fuzzy C-Means)")
  walk_clst_fcm_sub_di_filt <- c("Filtered Walkability Clusters with POI Diversity (Fuzzy C-Means)")
  
  color_palette_km <- c(A = "#8dd3c7", B = "#ffffb3", C = "#bebada", D = "#fb8072")
  color_palette_fcm_sub <- c(A = "#8dd3c7", B = "#fb8072", C = "#ffffb3", D = "#bebada")
  color_palette_fcm_sub_di <- c(A = "#8dd3c7", B = "#bebada", C = "#fb8072", D = "#ffffb3")
  
  paragraphs <- list(
    "Visual Walkability Clusters (K-Means)" = 
    "Cluster A:
Cluster A exhibits the lowest visual crowdedness and outdoor enclosure with the highest psychological 
greenery, but also the lowest visual pavement. It appears concentrated along major roads on the outskirts 
as well as some central areas near the University of Leicester incorrectly grouped due to model errors. 
This cluster likely represents major transportation corridors with ample vegetation but with insufficient
pedestrian infrastructure limiting accessibility.\n
Cluster B:
Cluster B displays the second highest visual crowdedness potentially due to dense on-street parking, 
along with the highest outdoor enclosure. It has the second lowest psychological greenery but has 
reasonably higher pavement to road ratios. This suburban cluster contains streets with decent 
walkability but lacking much visual appeal and green elements. Residential areas make up this cluster. \n
Cluster C:
With moderate means for all subindicators, Cluster C emerges as a relatively well-balanced cluster in 
terms of visual walkability. It does not stand out as exceptional in any one element but has reasonably 
positive conditions for all subindicators. Cluster C streets are dispersed across central neighbourhoods 
and outskirts, characterised by semi-high visual walkability. \n
Cluster D:
Cluster D contains the most visually crowded streets with low outdoor enclosure and minimal psychological 
greenery but ample pavement. Concentrated in the urban core and around the city centre, this cluster 
represents the busy commercial and residential areas that suffer from lack of vegetation but have 
well-developed pedestrian infrastructure. The high activity and lack of green elements hampers the 
visual walkability in these areas. 
",
    "Visual Walkability Clusters (Fuzzy C-Means)" = "Cluster A:
Cluster A exhibits the lowest visual crowdedness and outdoor enclosure with the highest psychological 
greenery, but also the lowest visual pavement. It appears concentrated along major outer roads and some 
central areas near University of Leicester that were likely incorrectly grouped due to model limitations. 
This cluster likely represents major transportation corridors with ample roadside vegetation, but with 
insufficient pedestrian infrastructure. It mirrors K-Means Cluster A.\n
Cluster B:
Cluster B contains the most crowded streets with medium outdoor enclosure, the lowest psychological 
greenery but the highest visual pavement. It represents the busy city centre and urban residential areas 
that suffer from lack of green elements but possess well-developed pedestrian infrastructure. The high 
visual crowdedness likely arises from the crowds within the city centre and from the dense on-street 
parking in the surrounding residential areas. \n
Cluster C:
Cluster C consists of moderately crowded streets with the highest outdoor enclosure, low psychological 
greenery but also relatively high visual pavement. It captures urban residential areas and neighbourhoods 
that, besides having good pedestrian infrastructure, lack visual appeal and vegetation. The crowdedness is 
less pronounced than Cluster B, potentially attributed to less on-street parking congestion.\n
Cluster D:
Cluster D displays moderately low visual crowdedness and outdoor enclosure with medium levels of 
psychological greenery and visual pavement. It emerges as a relatively balanced cluster characterised by 
semi-decent visual walkability that is dispersed throughout the city. 
",
    "Walkability Clusters with POI Diversity (K-Means)" = "Cluster A:
This cluster possesses the lowest POI diversity and visual crowdedness with high psychological greenery 
but insufficient visual pavement. Concentrated along major outer roads, it likely represents corridors with
vegetation but inadequate pedestrian infrastructure and amenity access.\n 
Cluster B:
This cluster exhibits the highest POI diversity index and visual crowdedness with low greenery but ample 
pavement. Capturing the city centre and commercial areas and also the urban residential areas around it, 
it comprises busy streets with many nearby amenities but lacking visual appeal and green space.\n
Cluster C:
Cluster C displays the second highest POI diversity and lowest visual crowdedness with moderately high 
greenery and pavement. Dispersed throughout central neighbourhoods, it encapsulates the most visually 
walkable areas with good amenity mix.\n
Cluster D:
This cluster possesses low POI diversity like Cluster A but with high enclosure, low greenery, and medium 
pavement. Occurring in suburban residential areas, it represents neighbourhoods with poor amenity access 
and walkability. 
",
    "Walkability Clusters with POI Diversity (Fuzzy C-Means)" = "Cluster A:
Cluster A possesses the second lowest POI diversity with low visual crowdedness, enclosure and pavement, 
but high greenery. Concentrated along major outer roads, it represents green yet inaccessible and 
pedestrian-unfriendly corridors.\n
Cluster B:
Cluster B exhibits the second highest POI diversity and lowest visual crowdedness and enclosure, with 
moderately high greenery and pavement. Occurring throughout central neighbourhoods and outskirts, it 
encapsulates the most visually walkable and vibrant areas.\n
Cluster C:
Cluster C displays low POI diversity with high visual crowdedness and enclosure, but lower greenery and 
pavement quality. Present in suburban residential areas, it comprises neighbourhoods with poor amenity 
access and walkability.\n
Cluster D:
Cluster D contains the highest POI diversity index with extreme visual crowdedness, medium enclosure, 
minimal greenery but ample pavement. As the urban city centre and residential neighbourhoods, it is walkable
in infrastructure but crowded and lacking green visual appeal.
"
  )
  
  
  observeEvent(input$open_popup, {
    showModal(
      modalDialog(
        title = "Pen Portraits",
        selectInput("portrait_dropdown", label = "Choose Clustering Technique / Dataset:",
                    choices = walk_clst),
        verbatimTextOutput("selected_text"),
        size = "l"
      )
    )
  })
  
  output$selected_text <- renderText({
    if (!is.null(input$portrait_dropdown)) {
      selected_option <- input$portrait_dropdown
      selected_text <- paragraphs[[selected_option]]
      return(selected_text)
    }
  })
  
  x <- reactive({
    input$first_dropdown
  })
  
  observe({
    if (x() == "Walkability Clusters") {
      updateSelectInput(
        session = session,
        inputId = "second_dropdown",
        choices = choices_cl
      )
    } else if (x() == "Walkability Index per Hexagon") {
      updateSelectInput(
        session = session,
        inputId = "second_dropdown",
        choices = choices_mean
      )
    } else {
      updateSelectInput(
        session = session,
        inputId = "second_dropdown",
        choices = ivw_choices
      )
    }
  })

  y <- reactive({
    input$second_dropdown
  })
  
  filteredData <- reactive({
    if (y() %in% ivw_choices) {
      return(ivw)
    } else if (y() %in% walk_clst | y() %in% choices_mean)  {
      return(h3_gdf)
    } else if (y() %in% filt_vis_walk) {
      return(h3_gdf_sub_4)
    } else {
      return(h3_gdf_sub_di_4)
    }
  })
  
  colorpal <- reactive({
    if (y() %in% ivw_choices) {
      colorNumeric("viridis", domain = ivw[[column_mapping[[input$second_dropdown]]]])
    } else if (y() %in% walk_clst_km) {
      colorFactor(
        palette = color_palette_km,
        domain = h3_gdf[[column_mapping[[input$second_dropdown]]]]
      )
    } else if (y() %in% walk_clst_fcm_sub) {
      colorFactor(
        palette = color_palette_fcm_sub,
        domain = h3_gdf[[column_mapping[[input$second_dropdown]]]]
      )
    } else if (y() %in% flipped) {
      colorNumeric("viridis", domain = h3_gdf[[column_mapping[[input$second_dropdown]]]], reverse = TRUE)
    }  else if (y() %in% walk_clst_fcm_sub_filt) {
      colorFactor(
        palette = color_palette_fcm_sub,
        domain = h3_gdf_sub_4[[column_mapping[[input$second_dropdown]]]]
      )
    } else if (y() %in% walk_clst_fcm_sub_di) {
      colorFactor(
        palette = color_palette_fcm_sub_di,
        domain = h3_gdf[[column_mapping[[input$second_dropdown]]]]
      )
    } else if (y() %in% walk_clst_fcm_sub_di_filt) {
      colorFactor(
        palette = color_palette_fcm_sub_di,
        domain = h3_gdf_sub_di_4[[column_mapping[[input$second_dropdown]]]]
      )
    } else {
      colorNumeric("viridis", domain = h3_gdf[[column_mapping[[input$second_dropdown]]]])
    }
  })
  

  
  observe({
    pal <- colorpal()

    if (y() %in% ivw_choices) {
      leafletProxy("map", data = filteredData()) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude,
                         fillColor = ~pal(ivw[[column_mapping[[input$second_dropdown]]]]),
                         radius = 1.5, stroke = FALSE,
                         fillOpacity = 1,
                         popup = paste0(
                           "<b>IVW: </b>",
                           ivw$IVW,
                           "<br>",
                           "<b>Gi: </b>",
                           ivw$Gi,
                           "<br>",
                           "<b>Ci: </b>",
                           ivw$Ci,
                           "<br>",
                           "<b>Ei: </b>",
                           ivw$Ei,
                           "<br>",
                           "<b>Pi: </b>",
                           ivw$Pi,
                           "<br>",
                           "<b>Latitude: </b>",
                           ivw$latitude,
                           "<br>",
                           "<b>Longitude: </b>",
                           ivw$longitude,
                           "<br>",
                           "<b>Panorama ID: </b>",
                           ivw$pano_id
                         )) %>% 
        addLegend(position = "topright",
                  title = "Index Score",
                  pal = pal, values = ~ivw[[column_mapping[[input$second_dropdown]]]])
    } else if (y() == "Visual Walkability Clusters (K-Means)") {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearMarkers() %>% 
        clearControls() %>%
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.4, weight = 0.4, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Cluster: </b>",
                      h3_gdf$k4clst_sub,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Cluster",
                  pal = pal, values = ~h3_gdf[[column_mapping[[input$second_dropdown]]]])
    } else if (y() == "Walkability Clusters with POI Diversity (K-Means)") {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearMarkers() %>% 
        clearControls() %>%
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.4, weight = 0.4, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Cluster: </b>",
                      h3_gdf$k4clst_sub_di,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Cluster",
                  pal = pal, values = ~h3_gdf[[column_mapping[[input$second_dropdown]]]])
    } else if (y() %in% walk_clst_fcm_sub) {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers() %>% 
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.4, weight = 0.4, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Cluster: </b>",
                      h3_gdf$c4clst_sub,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Cluster",
                  pal = pal, values = ~h3_gdf[[column_mapping[[input$second_dropdown]]]])
    } else if (y() %in% walk_clst_fcm_sub_filt) {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers() %>% 
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf_sub_4[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.4, weight = 0.4, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Cluster: </b>",
                      h3_gdf_sub_4$c4clst_sub,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Cluster",
                  pal = pal, values = ~h3_gdf_sub_4[[column_mapping[[input$second_dropdown]]]])
    } else if (y() %in% walk_clst_fcm_sub_di) {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearMarkers() %>% 
        clearControls() %>%
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.4, weight = 0.4, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Cluster: </b>",
                      h3_gdf$c4clst_sub_di,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Cluster",
                  pal = pal, values = ~h3_gdf[[column_mapping[[input$second_dropdown]]]])
    } else if (y() %in% walk_clst_fcm_sub_di_filt)  {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers() %>% 
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf_sub_di_4[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.4, weight = 0.4, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Cluster: </b>",
                      h3_gdf_sub_di_4$c4clst_sub_di,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Cluster",
                  pal = pal, values = ~h3_gdf_sub_di_4[[column_mapping[[input$second_dropdown]]]])
    } else if (y() %in% flipped)  {
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers() %>% 
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(rev(h3_gdf[[column_mapping[[input$second_dropdown]]]])), 
                    fillOpacity = 0.7, weight = 0.5, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Mean IVW: </b>",
                      h3_gdf$Mean_IVW,
                      "<br>",
                      "<b>Mean Gi: </b>",
                      h3_gdf$Mean_Greenery,
                      "<br>",
                      "<b>Mean Ci: </b>",
                      h3_gdf$Mean_Crowdedness,
                      "<br>",
                      "<b>Mean Ei: </b>",
                      h3_gdf$Mean_Enclosure,
                      "<br>",
                      "<b>Mean Pi: </b>",
                      h3_gdf$Mean_Pavement,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Mean Value",
                  pal = pal, values = ~h3_gdf[[column_mapping[[input$second_dropdown]]]])
    } else{
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers() %>% 
        addPolygons(data = filteredData(), 
                    fillColor = ~pal(h3_gdf[[column_mapping[[input$second_dropdown]]]]), 
                    fillOpacity = 0.7, weight = 0.5, color = "#A9A9A9",
                    popup = paste0(
                      "<b>Mean IVW: </b>",
                      h3_gdf$Mean_IVW,
                      "<br>",
                      "<b>Mean Gi: </b>",
                      h3_gdf$Mean_Greenery,
                      "<br>",
                      "<b>Mean Ci: </b>",
                      h3_gdf$Mean_Crowdedness,
                      "<br>",
                      "<b>Mean Ei: </b>",
                      h3_gdf$Mean_Enclosure,
                      "<br>",
                      "<b>Mean Pi: </b>",
                      h3_gdf$Mean_Pavement,
                      "<br>"
                    )) %>%
        addLegend(position = "topright",
                  title = "Mean Value",
                  pal = pal, values = ~h3_gdf[[column_mapping[[input$second_dropdown]]]])
    }
  })
  
}

shinyApp(ui, server)
