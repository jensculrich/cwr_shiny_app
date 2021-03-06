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
                    TAXON, NATIVE, ROUNDED_N_RANK, COSEWIC_DESC) %>%
      
      relocate(ECO_NAME, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
               PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
               TAXON, 
               NATIVE, ROUNDED_N_RANK,
               COSEWIC_DESC) 
    
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
             colnames = c("Region", "Category",
                          "Crop", "Taxon", 
                          "Native", 
                          "Conservation Status",
                          "COSEWIC Assessment"))
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
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(xx == "Forage and feed crops"){
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_inventory <- ecoregion_gap_table_t %>%
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
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(TIER == 1)
    } else if(xx == "Forest resources"){
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(xx == "Forage and feed crops"){
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_inventory <- ecoregion_gap_table_t %>%
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
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(TIER == 1)
    } else if(xx == "Forest resources"){
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 == "Forest Resources")
    } else if(xx == "Forage and feed crops"){
      filtered_inventory <- ecoregion_gap_table_t %>%
        filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == "Forage and Feed")
    } else {
      filtered_inventory <- ecoregion_gap_table_t %>%
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
    ecoregionTableData <- ecoregion_gap_table_t %>%
      # filter the table to the selected CWR
      filter(ecoregion_gap_table_t$SPECIES == input$inSelectedCWR) %>%
      
      # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
      dplyr::group_by(TAXON, ECO_NAME) %>%
      add_tally(!is.na(GARDEN_CODE)) %>%
      rename("accessions_in_ecoregion" = "n")  %>%
      ungroup() %>%
      
      # convert number of accessions to a binary "is there or is there not an accession from x region"
      dplyr::group_by(TAXON, ECO_NAME) %>%
      filter(row_number() == 1) %>%
      filter(!is.na(ECO_NAME)) %>%
      mutate(binary = ifelse(
        accessions_in_ecoregion > 0, 1, 0)) %>%
      ungroup() %>%
      
      #group_by(SPECIES) %>%
      #tidyr::fill(total_accessions_sp, .direction = "up") %>%
      #ungroup()
      
      # use the binary variable to determine the proportion of native regions with an accession
      group_by(TAXON) %>%
      mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
      mutate(num_covered_ecoregions = sum(binary)) %>%
      mutate(perc_ecoregion_range_covered = 
               num_covered_ecoregions / num_native_ecoregions) %>%
      
      # format the data for the summary table
      filter(row_number() == 1) %>%
      dplyr::select(total_accessions_sp,
                    garden_accessions_w_finest_taxon_res,
                    genebank_accessions_w_finest_taxon_res,
                    ROUNDED_N_RANK)
  
  })
  
  output$choroplethPlot2 <- renderLeaflet({
    
    # get data 
    mydat <- plotData()   
    
    mydat_filtered <- mydat %>%
      filter(!is.na(binary)) %>%
      mutate(label_text = ifelse(binary == 1, "Yes", "No")) %>%
      mutate(binary = as.factor(binary))
      
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
      filter(SPECIES == input$inSelectedCWR) %>%
      mutate(INSTITUTION = as.factor(INSTITUTION)) %>%
      filter(!is.na(INSTITUTION))
    
    palette_for_points <- colorFactor(
      palette = c('goldenrod', 'magenta'),
      c("BG", "G")
    )
    
    # levels(mydat_filtered$INSTITUTION)[levels(mydat_filtered$INSTITUTION) ==
        #               'accession held in botanical garden collections'] <- 'BG'
    
    leafletProxy("choroplethPlot2", data = mydat_filtered) %>%
      addCircles(lng = ~longitude, 
                 lat = ~latitude,
                 fillOpacity = 0.5,
                 weight = .1,
                 fillColor = ~palette_for_points(INSTITUTION),
                 radius = 50000
      ) %>% 
      addLegend(pal = palette_for_points, values = ~INSTITUTION,
                group = "circles", position = "bottomleft",
                title = "wild-populations represented in genebank (G) and 
                botanical garden (BG) ex situ conservation collections",
                na.label = FALSE
      )
  })
  
  # add gap table to the main panel using the reactive tableData() function
  output$gapTable <- DT::renderDataTable({
    datatable(tableData(), rownames= FALSE,
              colnames = c("Total accessions in ex situ collections (species level)",
                           "Canadian, wild-origin accessions (BG)", 
                           "Canadian, wild-origin accessions (G)",
                           "Conservation Status"))
  }) # end renderTable
  
}) # server