## Libraries 
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(tidyverse)
library(htmltools)

## Read sahpefiles
defAmzState <- rgdal::readOGR(  ## States
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment2/data/Amz_States_def.shp',
        verbose = FALSE, use_iconv = TRUE, encoding = 'UTF-8'
)

defAmz1998_2007 <- rgdal::readOGR( ## Amazon Deforestation between 1988 and 2007
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment2/data/pol_deforestation_1988_2007.shp',
        verbose = FALSE
)

defAmz1998_2007 <- rgdal::readOGR( ## Amazon Deforestation between 2008 and 2020
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment2/data/pol_deforestation_2008_2020.shp',
        verbose = FALSE
)

## Read the Amazon deforestation data frame - 2008 to 2020
ds_amz <- read.csv2(
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment2/data/terrabrasilis_legal_amazon_1988_2020.csv'
) %>% mutate(year = as.character(year, format = '%Y'))

## Read the Amazon deforestation Data frame by State - 2008 to 2020        
ds_state <- read.csv2(
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment2/data/terrabrasilis_states_amazon_1988_2020.csv'
) %>% rename(State = uf) %>%
        mutate(year = as.character(year, format = '%Y'))

## Summarize the data by State
sum_State <- ds_state %>%
        group_by(State) %>%
        summarize(def = sum(area_km2)) %>%
        rename(nome = State) %>%
        mutate(nome = str_to_title(nome))

## Save the sum_state data frame as .csv file format
write.csv2(
        sum_State, 
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment2/output/def_State.csv',
        row.names = FALSE
)

## Bar plot Amazon deforestation - 1988-2020
amz_def <- ds_amz %>%
        ggplot(aes(x = year)) +
        geom_bar(aes(y = area_km2), stat = 'identity', fill = 'steelblue') +
        scale_y_continuous(
                breaks = seq(5000, 30000, by = 5000),
                labels = scales::label_number(big.mark = ',')
        ) +
        theme(
                axis.text.x = element_text(angle = 45)
        ) +
        labs(
                x = 'Year', y = 'Area  (km2)'
        )

ggplotly(amz_def)

## line plot Amazon deforestation by state - 1988-2020
amz_state <- ds_state %>%
        ggplot(aes(x = year, y = area_km2, color = State, group = State)) +
        geom_point() +
        geom_line() +
        scale_y_continuous(
                breaks = seq(1000, 12000, by = 1000),
                labels = scales::label_number(big.mark = ',')
        ) +
        theme(
                axis.text.x = element_text(angle = 45)
        ) +
        labs(
                x = 'Year', y = 'Area  (km2)'
        )

ggplotly(amz_state)

## Map of deforestation in Amazon
#
## Set color to legend
pal <- colorQuantile(palette = 'viridis', domain = defAmzState$def_, n = 9)

map <- leaflet() %>%
        addTiles() %>%
        addPolygons(data = defAmzState, color = 'red', fillColor = 'transparent') %>%
        addPolygons(
                data = defAmzState, 
                stroke = FALSE,
                color = ~pal(def_),
                fillOpacity = 0.85,
                popup = ~paste0(sep = ' ', 
                               '<b>State: </b>', as.character(nome), '<br>',
                               '<b>Deforestation: </b>', formatC(def_,
                                                                 digits = 2,
                                                                 format = 'f',
                                                                 big.mark = ',',
                                                                 decimal.mark = '.'),
                               ' km²'),
                label = ~paste0(sep = '', formatC(def_, big.mark = ',', 
                                                  format = 'f',
                                                  digits = 0), 
                                ' km²')
        ) %>%
        #addLegend(pal = pal, values = ~pal(def_), opacity = 0.7, position = 'bottomright') %>%
        addMiniMap(position = 'bottomleft')

map
