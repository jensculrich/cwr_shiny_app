###############################
# server.R for CWR Shiny App  #
###############################

# Written by Jens Ulrich and Erika Luna Perez

########################################
# DATA WRANGLING AND SUPPORT FUNCTIONS #
########################################

# Load required data and shapefiles for building reactive maps and data tables
canada_ecoregions_geojson <- st_read("data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("data/canada_provinces.geojson", quiet = TRUE)
ecoregion_gap_table <- as_tibble(read.csv("data/ecoregion_gap_table_by_species.csv"))
ecoregion_gap_table_t <- as_tibble(read.csv("data/ecoregion_gap_table_by_taxon.csv"))
inventory <- as_tibble(read.csv("data/inventory.csv"))

# Define map projection
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Define the mapping theme -- remove axes, ticks, borders, legends, etc.
# Come back to this and add a legend
theme_map <- function(base_size=9, base_family="") { # 3
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          # legend.justification = c(0,0), # no longer using a legend
          legend.position = "none"
    )
}

# spatially project the garden data
ecoregion_gap_table_sf <- st_as_sf(ecoregion_gap_table, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326, 
                                  na.fail = FALSE)

################
# SERVER LOGIC #
################

# Server Logic is separated by tabs 
# 1. Find native CWR in a region
# 2. Conduct a CWR Gap Analysis

shinyServer(function(input, output, session){
  
  ####################
  #  1) NATIVE CWR   #  
  ####################
  
  # for native CWR tab,
  # user can input province or ecoregion consideration
  # and then generate a map and a table of native OR endemic CWRs by region focus
  # to do so, the server side needs to generate plot and table data
  # that's dependent on the users choices of geographic regions and variable of interest
  # TO ADD: user choice to select Crop Category and Crop (but not individual CWRs)
  # and then filter the datasets (use an observe function)
  
  filter_data <- reactive({
    x <- input$inSelectedGroup
    
    if(x == "Food crop relatives"){
      filtered_ecoregion_gap_table <- ecoregion_gap_table_t %>%
        filter(TIER == 1)
    } else if(x == "Forest resources"){
      filtered_ecoregion_gap_table <- ecoregion_gap_table_t %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(x == "Forage and feed crops"){
      filtered_ecoregion_gap_table <- ecoregion_gap_table_t %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_ecoregion_gap_table <- ecoregion_gap_table_t %>%
        filter(WUS == "Y")
    }
    
  })
  
  # allow user to click on a polygon (region) and filter the CWR table to that region
  observe({ 
    
    ## give the user the ability to choose by hovering on the map
    event <- input$choroplethPlot_shape_click
    updateSelectInput(session, inputId = "inRegion", selected = event$id)
    
  }) 
  
  # native range tab outputs: plot and table
  
  # get plot data
  plotDataNativeRanges <- reactive({
    
    filtered_data <- filter_data()
    
    native_occurrence_heatmap_ecoregion <- filtered_data %>%
      # group by ecoregion
      dplyr::group_by(ECO_NAME) %>%
      filter(FINEST_TAXON_RESOLUTION == "Y") %>%
      # distinct (since when there are >1 accessions for a species from the province the 
      # row gets expanded. We just want a count of one row per species found in the province)
      distinct(TAXON, .keep_all = TRUE) %>%
      # tally the number of species
      add_tally() %>%
      rename("variable" = "n") 
    
    native_occurrence_sf_ecoregions <- tigris::geo_join(canada_ecoregions_geojson, native_occurrence_heatmap_ecoregion, 
                                                        by_sp = "ECO_NAME", by_df = "ECO_NAME")
    
    native_occurrence_sf_ecoregions <- native_occurrence_sf_ecoregions %>%
      rename("region" = "ECO_NAME")
    
  }) # end get plot data
  
  # get table data
  tableDataNativeRanges <- reactive({
    
    filtered_data <- filter_data()
    
    native_occurrence_heatmap_ecoregion <- filtered_data %>%
      # filter the table to the selected region
      filter(ECO_NAME == input$inRegion) %>%
      # group by ecoregion
      dplyr::group_by(ECO_NAME) %>%
      filter(FINEST_TAXON_RESOLUTION == "Y") %>%
      # distinct (since when there are >1 accessions for a species from the province the 
      # row gets expanded. We just want a count of one row per species found in the province)
      distinct(TAXON, .keep_all = TRUE) %>%
      # tally the number of species
      add_tally() %>%
      rename("variable" = "n") %>%
      
      dplyr::select(ECO_NAME, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                    PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                    TAXON, NATIVE, ROUNDED_N_RANK) %>%
      
      relocate(ECO_NAME, PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
               TAXON, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
               NATIVE, ROUNDED_N_RANK) 
    
  }) # end get table data
  
  output$choroplethPlot <- renderLeaflet({
    
    # get data 
    mydat <- plotDataNativeRanges()    
    
    # Create a color palette for the map:
    mypalette <- colorNumeric( palette="YlOrBr", domain=mydat$variable, na.color="transparent")
    mypalette(c(45,43))
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Region: ", mydat$region,"<br/>", 
      "CWR: ", mydat$variable, "<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Basic choropleth with leaflet
    leaflet(plotDataNativeRanges()) %>% 
      addTiles()  %>% 
      setView( lat=60, lng=-98 , zoom=3) %>%
      addPolygons(fillOpacity = 0.5, 
                  smoothFactor = 0.5, 
                  color = ~colorNumeric("YlOrBr", variable)(variable),
                  label = mytext,
                  layerId = ~region) %>%
      addLegend( pal=mypalette, values=~variable, opacity=0.9, title = "CWR", position = "bottomleft" )
    
  }) # end renderPlot
  
  output$nativeRangeTable <- DT::renderDataTable({
    datatable(tableDataNativeRanges(), rownames = FALSE,
             colnames = c("Region", "Crop", "Taxon", 
                          "Category", "Native", 
                          "Conservation Status"))
  }) # end renderTable
  
  ##########################
  # Species level analysis #
  ##########################
  
  # filter the data set for a CWR of interest
  observe({ 
    xx <- input$inSelectedGroup2
    
    if(xx == "Food crop relatives"){
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(TIER == 1)
    } else if(xx == "Forest resources"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(xx == "Forage and feed crops"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_inventory <- ecoregion_gap_table %>%
        filter(WUS == "Y")
    }
    
    # order filtered table so that user choices for CWR are alphabetically organized
    # to facilitate user choice
    filtered_inventory <- filtered_inventory[order(filtered_inventory$PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1),]
    
    updateSelectInput(session, "inSelectedUse",
                      label = paste("Select a Crop"),
                      choices = filtered_inventory$PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1
    ) # updateSelectInput
    
  })
  
  observe({
    xx <- input$inSelectedGroup2
    
    if(xx == "Food crop relatives"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(TIER == 1)
    } else if(xx == "Forest resources"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(xx == "Forage and feed crops"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_inventory <- ecoregion_gap_table %>%
        filter(WUS == "Y")
    }
    
    # After user selects a group, user may select a crop from within that group
    y <- input$inSelectedUse
    
    inventory_filtered <- filter(filtered_inventory, 
                                 filtered_inventory$PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == y)
    
    inventory_filtered <- inventory_filtered[order(inventory_filtered$PRIMARY_ASSOCIATED_CROP_COMMON_NAME),]    
    # update select input so that CWRs choices are the subset related to the specified Crop
    updateSelectInput(session, "inSelectedCrop",
                      label = paste("Select a Crop Wild Relative"),
                      choices = inventory_filtered$PRIMARY_ASSOCIATED_CROP_COMMON_NAME
    ) # updateSelectInput
    
  })
  
  observe({
    xx <- input$inSelectedGroup2
    
    if(xx == "Food crop relatives"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(TIER == 1)
    } else if(xx == "Forest resources"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(xx == "Forage and feed crops"){
      filtered_inventory <- ecoregion_gap_table %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_inventory <- ecoregion_gap_table %>%
        filter(WUS == "Y")
    }
    
    # After user selects a group, user may select a crop from within that group
    z <- input$inSelectedCrop
    
    inventory_filtered <- filter(filtered_inventory, 
                                 filtered_inventory$PRIMARY_ASSOCIATED_CROP_COMMON_NAME == z)
    
    inventory_filtered <- inventory_filtered[order(inventory_filtered$PRIMARY_ASSOCIATED_CROP_COMMON_NAME),]    
    # update select input so that CWRs choices are the subset related to the specified Crop
    updateSelectInput(session, "inSelectedCWR",
                      label = paste("Select a CWR or WUS"),
                      choices = inventory_filtered$SPECIES
    ) # updateSelectInput
    
  })
  
  # plotData() is a reactive function that filters the gap table to provide 
  # necessary statistics for plotting (i.e. native range, coverage of native range, total accessions)
  # plotData() logically follows the user's choice of display:
  # IF user requests to display provinces, then generate province plot data
  # ELSE generate ecoregion plot data
  plotData <- reactive({ 
    # filter province_gap_table frame and calculate species specific stats
    ecoregionPlotData <- ecoregion_gap_table %>%
      # filter the table to the selected CWR
      filter(ecoregion_gap_table$SPECIES == input$inSelectedCWR) %>%
      
      # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
      group_by(ECO_NAME) %>%
      add_tally(!is.na(GARDEN_CODE)) %>%
      rename("accessions_in_ecoregion" = "n")  %>%
      ungroup() %>%
      
      # count the number of accessions w/ and w/out geographic data
      mutate(total_accessions_for_species = sum(!is.na(GARDEN_CODE))) %>%
      mutate(accessions_no_geo_data = sum(is.na(latitude))) %>%
      mutate(accessions_with_geo_data = sum(!is.na(latitude))) %>%
      
      # convert number of accessions to a binary "is there or is there not an accession from x region"
      group_by(ECO_NAME) %>%
      filter(row_number() == 1) %>%
      filter(!is.na(ECO_NAME)) %>%
      mutate(binary = ifelse(
        accessions_in_ecoregion > 0, 1, 0)) %>%
      ungroup() %>%
      
      # use the binary variable to determine the proportion of native regions with an accession
      mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
      mutate(num_covered_ecoregion = sum(binary)) %>%
      mutate(perc_ecoregion_range_covered = 
               num_covered_ecoregion / num_native_ecoregion) 
    
    # join plot data with the spatial data frame necessary for projecting the plot  
    tigris::geo_join(canada_ecoregions_geojson, ecoregionPlotData,  
                     by_sp = "ECO_NAME", by_df = "ECO_NAME")
    
  })
  
  tableData <- reactive({ 
    ecoregionTableData <- ecoregion_gap_table %>%
      # filter the table to the selected CWR
      filter(ecoregion_gap_table$SPECIES == input$inSelectedCWR) %>%
      
      # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
      dplyr::group_by(ECO_NAME) %>%
      add_tally(!is.na(GARDEN_CODE)) %>%
      rename("accessions_in_ecoregion" = "n")  %>%
      ungroup() %>%
      
      # convert number of accessions to a binary "is there or is there not an accession from x region"
      dplyr::group_by(ECO_NAME) %>%
      filter(row_number() == 1) %>%
      filter(!is.na(ECO_NAME)) %>%
      mutate(binary = ifelse(
        accessions_in_ecoregion > 0, 1, 0)) %>%
      ungroup() %>%
      
      # use the binary variable to determine the proportion of native regions with an accession
      mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
      mutate(num_covered_ecoregions = sum(binary)) %>%
      mutate(perc_ecoregion_range_covered = 
               num_covered_ecoregions / num_native_ecoregions) %>%
      
      # format the data for the summary table 
      filter(row_number() == 1) %>%
      dplyr::select(num_native_ecoregions, num_covered_ecoregions,
                    total_accessions_sp,
                    garden_accessions_w_finest_taxon_res,
                    genebank_accessions_w_finest_taxon_res,
                    ROUNDED_N_RANK,
                    NATIVE) %>%
      mutate(num_covered_ecoregions = as.integer(num_covered_ecoregions)) 
  
  })
  
  # add plot to the main panel using the reactive plotData() function
  #output$gapPlot <- renderPlot({
    
    # validate allows us to share a prompt (rather than an error message until a CWR is chosen)
   # shiny::validate(
    #  need(input$inSelectedCrop, "")
    #)
    
  #  subset_gap_table_sf <- ecoregion_gap_table_sf %>%
   #   filter(SPECIES == input$inSelectedCWR)
    
    # use ggplot to map the native range and conserved accessions  
    #ggplot(plotData()) +
     # geom_sf(aes(fill = as.factor(binary)),
      #        color = "gray60", size = 0.1) +
      #geom_sf(data = subset_gap_table_sf, color = 'skyblue', alpha = 0.8, size = 4) + 
      #coord_sf(crs = crs_string) +
      #scale_fill_manual(values = c("0" = "gray80", "1" = "gray18"), 
      #                  labels = c("No accessions with geographic data held in collection", 
      #                             "1 or more accession with geographic data held in collection", 
       #                            "Outside of native range")) +
      #theme_map() +
      #ggtitle("") +
      #theme(panel.grid.major = element_line(color = "white"),
      #      plot.title = element_text(color="black",
      #                                size=10, face="bold.italic", hjust = 0.5),
      #      legend.text = element_text(size=10))
    
  # }) # end renderPlot renderDataTable
  
  output$choroplethPlot2 <- renderLeaflet({
    
    # get data 
    mydat <- plotData()   
    
    mydat_filtered <- mydat %>%
      filter(!is.na(binary)) %>%
      mutate(label_text = ifelse(binary == 1, "Yes", "No")) %>%
      mutate(binary = as.factor(binary))
      
    # mypalette_discrete <- colorFactor(palette = c("gray80", "gray18"), domain=mydat_filtered$binary)
    # mypalette_discrete<- c("gray80", "gray18")
    mypalette_discrete <- colorFactor(c("gray80", "gray18"), c("0", "1"))
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Region: ", mydat_filtered$ECO_NAME,"<br/>", 
      "ex situ collections from region: ", mydat_filtered$label_text, "<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Basic choropleth with leaflet
    leaflet(mydat_filtered) %>% 
      addTiles()  %>% 
      setView(lat=60, lng=-98 , zoom=3)  %>%
      addPolygons(fillOpacity = 0.5, 
                  smoothFactor = 0.5,
                  weight = 0.5,
                  fillColor = ~mypalette_discrete(binary),
                  label = mytext,
                  layerId = ~ECO_NAME) # %>%
      
      # addLegend(pal=mypalette_discrete, values=~binary, opacity=0.9, title = "CWR", position = "bottomleft" )
    
  }) # end renderPlot
  
  observe({
    mydat_filtered <- ecoregion_gap_table %>%
      filter(SPECIES == input$inSelectedCWR)
    
    palette_for_points <- colorFactor(
      palette = c('goldenrod', 'magenta'),
      domain = mydat_filtered$INSTITUTION
    )
    
    leafletProxy("choroplethPlot2", data = mydat_filtered) %>%
      addCircles(lng = ~longitude, 
                 lat = ~latitude,
                 fillOpacity = 0.5,
                 weight = 1,
                 color = ~palette_for_points(INSTITUTION),
                 radius = 50000
      )
  })
  
  # add gap table to the main panel using the reactive tableData() function
  output$gapTable <- DT::renderDataTable({
    datatable(tableData(), rownames= FALSE,
              colnames = c("Native regions", 
                           "Regions represented by ex situ collections", 
                           "Total ex situ accessions", 
                           "Canadian, wild-origin accessions (BG)", 
                           "Canadian, wild-origin accessions (G)",
                           "Conservation Status",
                           "Native"))
  }) # end renderTable
  
}) # server