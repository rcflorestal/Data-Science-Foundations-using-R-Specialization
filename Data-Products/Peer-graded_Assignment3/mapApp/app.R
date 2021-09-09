#'@title 
#' mapApp
#'
#'@description 
#' This is a Shiny web application that allows you plot a global forest 
#' inventory. You can run the application by clicking the 'Run App' button above.
#'
#'@author 
#'Robson Cruz
#'
#'@details 
#'To run the APP is necessary load the following files:
#' the forest inventory in .csv file format which can be view in:
#' https://github.com/rcflorestal/Data-Science-Specialization/blob/main/Data-Products/Peer-graded_Assignment3/data/varDict.pdf
#' Shape file of the plots
#' 
 
## Load libraries
library(shiny)
library(miniUI)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmltools)

## Read the forest inventory
trees <- read.csv2(
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment3/data/arvs_upa2_umf2_cxn.csv'
)

## Set a data frame with only tress to cut down
cut <- trees %>%
        filter(Destination == 'Cut')

## Set a data frame with only tress to cut down
rem <- trees %>%
        filter(Destination == 'Remaining')

## Read the river influence area in .shp format file
ria <- rgdal::readOGR(
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment3/data/app.shp',
        verbose = FALSE, use_iconv = TRUE, encoding = 'UTF-8'
)

## Read the polygons of the plots in .shp file format
plot <- rgdal::readOGR(
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment3/data/ut_upa2_umf2_cxn.shp',
        verbose = FALSE, use_iconv = TRUE, encoding = 'UTF-8'
)

## Set the palette to the map
pal <- colorFactor(
        palette = c('red', 'green'),
        domain = trees$Destination
)

## Define UI for application that draws a map of trees from a global forest inventory
FI <- function(){
        ui <- miniPage(miniContentPanel(
                plotOutput("mapTrees", height = "100%", brush = "brush"))
        )
        server <- function(input, output){
                output$mapTrees <- renderPlot({
                        
                        # Show a plot of the
                        leaflet(trees) %>%
                                addTiles(group = 'OSM') %>%
                                addProviderTiles(
                                        'OpenTopoMap', group = 'Topo'
                                ) %>%
                                addProviderTiles(
                                        'Esri.WorldImagery', group = 'Satellite'
                                ) %>%
                                fitBounds(
                                        lat1 = -1.73007302, lng1 = -51.77978728,
                                        lat2 = -1.76975667, lng2 = -51.81636691
                                ) %>%
                                
                                # Load the plot polygons        
                                addPolygons(
                                        data = plot, stroke = TRUE, 
                                        group = 'plot',
                                        color = 'black',
                                        fillColor = 'transparent'
                                ) %>%
                                
                                # Load river influence area       
                                addPolygons(
                                        data = ria, group = 'River Area',
                                        color = 'green',
                                        fillColor = 'green',
                                        label = ~paste0(formatC(
                                                hectare, digits = 4), ' ha'
                                        )
                                ) %>%
                                
                                # Load de localization of the trees to cut down
                                addCircles(
                                        data = cut, group = 'Harvest',
                                        color = ~pal(Destination),
                                        radius = 2, weight = 4,
                                        popup = ~paste0(
                                                sep = ' ',
                                                '<b>Plot: </b>', UT,'<br>',
                                                '<b>Nº ', N, '<br>',
                                                '<b>Scientific_name: </b>', 
                                                '<i>',Scientific_Name,'</i>', 
                                                '<br>', '<b>DBH: </b>', formatC(
                                                        DBH, digits = 3),
                                                ' cm', '<br>',
                                                '<b>Height: ', H, ' m', '<br>',
                                                '<b>Vol: </b>', formatC(
                                                        vol, digits = 4),
                                                ' m³', '<br>',
                                                '<b>Destination: ', Destination
                                        )
                                ) %>%
                                
                                # Load de localization of the trees that 
                                # will not be cut down
                                addCircles(
                                        data = rem, group = 'Remaining',
                                        color = ~pal(Destination),
                                        radius = 2, weight = 4,
                                        popup = ~paste0(
                                                sep = ' ',
                                                '<b>Plot: </b>', UT,'<br>',
                                                '<b>Nº ', N, '<br>',
                                                '<b>Scientific_name: </b>', 
                                                '<i>',Scientific_Name,'</i>', 
                                                '<br>', '<b>DBH: </b>', formatC(
                                                        DBH, digits = 3),
                                                ' cm', '<br>',
                                                '<b>Height: ', H, ' m', '<br>',
                                                '<b>Vol: </b>', formatC(
                                                        vol, digits = 4),
                                                ' m³', '<br>',
                                                '<b>Destination: ', Destination
                                        )
                                ) %>%
                                
                                # Set a map localization
                                addMiniMap() %>%
                                
                                # Set legend of the trees
                                addLegend("bottomright", pal = pal, 
                                          values = ~Destination,
                                          title = 'Trees',
                                          opacity = 1
                                ) %>%
                                
                                # Set map providers and allow the user
                                # to select them
                                addLayersControl(
                                        baseGroups = c('OSM (default)', 'Topo',
                                                       'Satellite'), 
                                        overlayGroups = c('Harvest', 'Remaining', 
                                                          'plot', 'River Area'))
                })
        }
        
        # Run the application
        runGadget(ui, server)
}       

# Call the map application function
FI()
