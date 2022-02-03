###########################
# ui.R for CWR Shiny App  #
###########################

# Written by Jens Ulrich and Erika Luna Perez

###########################
# LIBRARIES               #
###########################

library(shiny)
library(shinythemes)
library(sf) # the base package manipulating shapes
library(spdplyr) # the `dplyr` counterpart for shapes
library(dplyr) # data wrangling
library(tidyverse) # data wrangling
library(ggplot2) # plotting
library(tigris) # for joining spatial data with data frame classes
library(leaflet)
library(htmltools)
library(shinydashboard)
library(DT)
library(rgeos)
library(rgdal)
library(rmapshaper)
library(sp)
library(grid)

########################################
# DATA WRANGLING AND SUPPORT FUNCTIONS #
########################################

# Load required data and shapefiles for  
# building reactive maps and data tables 

# canada_ecoregions_geojson defines ecoregions in Canada, clipped to the national border of Canada
canada_ecoregions_geojson <- st_read("data/canada_ecoregions_clipped.geojson", quiet = TRUE)
# canada_provinces_geojson defines province and territory boundaries
canada_provinces_geojson <- st_read("data/canada_provinces.geojson", quiet = TRUE)

# province_gap_table includes all garden accessions from our surveyed gardens
# with lat/long when applicable (needs to be formatted here or before uploading)
# The table has a row for each native province that a species is native to with garden = NA
# along with a row for each garden accession from each native province.
# ecoregion_gap_table has similar setup
ecoregion_gap_table <- as_tibble(read.csv("data/ecoregion_gap_table_by_species.csv"))

# order gap tables so that user choices are alphabetically organized
ecoregion_gap_table <- ecoregion_gap_table[order(ecoregion_gap_table$PRIMARY_ASSOCIATED_CROP_COMMON_NAME),]


dbHeader <- dashboardHeader(title = "My Dashboard",
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.company.com',
                                      img(src = 'ubc.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

###########
# LOAD UI #
###########

# ui structure: one navbar page with 4 tab panels

ui <- fluidPage(
  
  includeCSS("www/style.css"),
  
  dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title = "Conservation of CWR in Canada", titleWidth = 500),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("What are CWR?", tabName = "about", icon = icon("seedling")),
        menuItem("Find Native CWR", tabName = "find", icon = icon("thumbtack")),
        menuItem("CWR Conservation", tabName = "explore", icon = icon("map marked alt")),
        menuItem("Acknowledgements", tabName = "aknow", icon = icon("tasks"))
        
      ) # end sidebarMenu
      
    ), # end dashboardSidebar
    
    dashboardBody(
      tabItems(
        # First tabItem element
        tabItem(tabName = "home",
                includeMarkdown("www/home.md")
        ), # end first tabItem element
        
        # Second tab element
        tabItem(tabName = "about",
                includeMarkdown("www/about.md")
        ) # end second tabItem element
      ) # end tabItems
    ) # end dashboardBody
  ) # end dashboardPage
) # ui

