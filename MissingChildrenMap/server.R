
# read in the data
library(gsheet)
attempts = gsheet2tbl('https://docs.google.com/spreadsheets/d/1qQ-zqb077KbFeL53-j04aKmUac9We53V7jcz4kTJwKA/edit?usp=sharing')
missing = gsheet2tbl('https://docs.google.com/spreadsheets/d/1tRx2Hci88BxaGf_fsUl9aEQ3IM1eCwdH5uhaUQYdZIY/edit?usp=sharing')
latLonVals = gsheet2tbl('https://docs.google.com/spreadsheets/d/1ZpH4XuMxNzKU40KVUDpD3Cpw0Iekpdr1HLpXcXZTL14/edit?usp=sharing')

# merge lat/lon values into datasets by zip code
colnames(attempts)[which(names(attempts) == "Incident Zip")] <- "ZIP"
colnames(missing)[which(names(missing) == "Missing Zip")] <- "ZIP"
attempts = merge(attempts, latLonVals, by = "ZIP")
missing = merge(missing, latLonVals, by = "ZIP")

shinyServer(function(input, output, session) {
  
  output$mymap = renderLeaflet({
    
    attemptsPoints = attempts
    missingPoints = missing
    
    # check for case id
    if (!is.null(input$caseID)) {
      attemptsPoints = attemptsPoints[attemptsPoints$'Case Number' %in% input$caseID,]
      missingPoints = missingPoints[missingPoints$'Case Number' %in% input$caseID,]
    }
    
    # check for zip
    if (!is.null(input$zip)) {
      attemptsPoints = attemptsPoints[attemptsPoints$'ZIP' %in% input$zip,]
      missingPoints = missingPoints[missingPoints$'ZIP' %in% input$zip,]
    }
    
    # check for city
    if (!is.null(input$city)) {
      #############updateCheckboxGroupInput(session, "state", selected = character(0))
      attemptsPoints = attemptsPoints[attemptsPoints$'Incident City' %in% input$city,]
      missingPoints = missingPoints[missingPoints$'Missing City' %in% input$city,]
    }
    
    # check for state
    if (!is.null(input$state)) {
      attemptsPoints = attemptsPoints[attemptsPoints$'Incident State' %in% input$state,]
      missingPoints = missingPoints[missingPoints$'Missing State' %in% input$state,]
    }
    
    # get just the zip codes
    attemptsPoints = cbind(attemptsPoints$LNG, attemptsPoints$LAT)
    missingPoints = cbind(missingPoints$LNG, missingPoints$LAT)
    
    # check for data selected (missing/attempts) and plot map
    if ("miss" %in% input$mapOption & "inci" %in% input$mapOption) {
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircles(data = attemptsPoints) %>%
        addCircles(data = missingPoints, color = "red")
    } else if ("miss" %in% input$mapOption) {
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircles(data = missingPoints, color = "red")
    } else if ("inci" %in% input$mapOption) {
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircles(data = attemptsPoints)
    } else {
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        )
    }
    })
  
})
