setwd("~/Desktop/Hackathon/Team10/MissingChildrenMap")

# read in the data
attempts = read.csv("../../Attempts Hackathon 5 Years of Data.csv")
missing = read.csv("../../Hackathon Missing Child 5 Years of Data.csv")
latLonVals = read.csv("../../zipToLatLon.csv")

# merge lat/lon values into datasets by zip code
colnames(attempts)[which(names(attempts) == "Incident.Zip")] <- "ZIP"
colnames(missing)[which(names(missing) == "Missing.Zip")] <- "ZIP"
attempts = merge(attempts, latLonVals, by = "ZIP")
missing = merge(missing, latLonVals, by = "ZIP")

# data cleaning
#attempts$`Incident Date` = as.Date(attempts$`Incident Date`, format = "%d/%m/%Y")
#missing$`Missing Date` = as.Date(missing$`Missing Date`, format = "%d/%m/%Y")

# fix the -1 values in the method columns
attempts$Offender.Method.Animal[attempts$Offender.Method.Animal==-1] = 1
attempts$Offender.Method.Candy[attempts$Offender.Method.Candy==-1] = 1
attempts$Offender.Method.Money[attempts$Offender.Method.Money==-1] = 1
attempts$Offender.Method.Other[attempts$Offender.Method.Other==-1] = 1
attempts$Offender.Method.Ride[attempts$Offender.Method.Ride==-1] = 1

shinyServer(function(input, output, session) {
  
  output$mymap = renderLeaflet({
    
    attemptsPoints = attempts
    missingPoints = missing
    
    # check for case id
    if (!is.null(input$caseID)) {
      attemptsPoints = attemptsPoints[attemptsPoints$Case.Number %in% input$caseID,]
      missingPoints = missingPoints[missingPoints$Case.Number %in% input$caseID,]
    }
    
    # check for zip
    if (!is.null(input$zip)) {
      attemptsPoints = attemptsPoints[attemptsPoints$'ZIP' %in% input$zip,]
      missingPoints = missingPoints[missingPoints$'ZIP' %in% input$zip,]
    }
    
    # check for city
    if (!is.null(input$city)) {
      #############updateCheckboxGroupInput(session, "state", selected = character(0))
      attemptsPoints = attemptsPoints[attemptsPoints$Incident.City %in% input$city,]
      missingPoints = missingPoints[missingPoints$Missing.City %in% input$city,]
    }
    
    # check for state
    if (!is.null(input$state)) {
      attemptsPoints = attemptsPoints[attemptsPoints$Incident.State %in% input$state,]
      missingPoints = missingPoints[missingPoints$Missing.State %in% input$state,]
    }
    
    # check the date
    #if (input$dateRange[1] != Sys.Date()) {
      #from = format(input$dateRange[1])
      #to = format(input$dateRange[2])
      #cat(as.Date(from))
      #cat('\n')
      #cat(to)
      #dateSeq = seq(from, to, by=1)
      #attemptsPoints = attemptsPoints[attemptsPoints$`Incident Date` %in% dataSeq,]
      #missingPoints = missingPoints[missingPoints$`Missing Date` %in% dataSeq,]
    #}
    
    # check gender
    if (!is.null(input$gender)) {
      b1 = attemptsPoints$Offender.Gender.1 %in% input$gender
      b2 = attemptsPoints$Offender.Gender.2 %in% input$gender
      b3 = attemptsPoints$Offender.Gender.3 %in% input$gender
      attemptsPoints = attemptsPoints[b1 | b2 | b3,]
      missingPoints = missingPoints[missingPoints$Gender %in% input$gender,]
    }
    
    # check race
    if (!is.null(input$race)) {
      b1 = attemptsPoints$Child.Race.1 %in% input$race
      b2 = attemptsPoints$Child.Race.2 %in% input$race
      b3 = attemptsPoints$Child.Race.3 %in% input$race
      b4 = attemptsPoints$Child.Race.4 %in% input$race
      b5 = attemptsPoints$Child.Race.5 %in% input$race
      b6 = attemptsPoints$Child.Race.6 %in% input$race
      attemptsPoints = attemptsPoints[b1 | b2 | b3 | b4 | b5 | b6,]
      missingPoints = missingPoints[missingPoints$Race %in% input$race,]
    }
    
    # check vehicle style
    if (!is.null(input$vehstyle)) {
      b1 = attemptsPoints$Vehicle.Style.1 %in% input$vehstyle
      b2 = attemptsPoints$Vehicle.Style.2 %in% input$vehstyle
      attemptsPoints = attemptsPoints[b1 | b2,]
      missingPoints = missingPoints[missingPoints$Vehicle.Style %in% input$vehstyle,]
    }
    
    # check vehicle color
    if (!is.null(input$vehcolor)) {
      b1 = attemptsPoints$Vehicle.Color.1 %in% input$vehcolor
      b2 = attemptsPoints$Vehicle.Color.2 %in% input$vehcolor
      attemptsPoints = attemptsPoints[b1 | b2,]
      missingPoints = missingPoints[missingPoints$Vehicle.Color %in% input$vehcolor,]
    }
    
    # check case type
    if (!is.null(input$casetype)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Case.Type %in% input$casetype,]
    }
    
    # check case source
    if (!is.null(input$casesource)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Source %in% input$casesource,]
    }
    
    # check case status
    if (!is.null(input$casestatus)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Status %in% input$casestatus,]
    }
    
    # check address
    if (!is.null(input$address)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Incident.Location %in% input$address,]
    }
    
    # check location type
    if (!is.null(input$locationtype)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Incident.Location.Type %in% input$locationtype,]
    }
    
    # check the county
    if (!is.null(input$county)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Incident.County %in% input$county,]
    }
    
    # check the child id
    if (!is.null(input$childID)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      b1 = attemptsPoints$Child.ID.1 %in% input$childID
      b2 = attemptsPoints$Child.ID.2 %in% input$childID
      b3 = attemptsPoints$Child.ID.3 %in% input$childID
      b4 = attemptsPoints$Child.ID.4 %in% input$childID
      b5 = attemptsPoints$Child.ID.5 %in% input$childID
      b6 = attemptsPoints$Child.ID.6 %in% input$childID
      attemptsPoints = attemptsPoints[b1 | b2 | b3 | b4 | b5 | b6,]
    }
    
    # check how they got away
    if (!is.null(input$gotaway)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$How.Got.Away %in% input$gotaway,]
    }
    
    # check offender gender
    if (!is.null(input$offendergender)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      b1 = attemptsPoints$Offender.Gender.1 %in% input$offendergender
      b2 = attemptsPoints$Offender.Gender.2 %in% input$offendergender
      b3 = attemptsPoints$Offender.Gender.3 %in% input$offendergender
      attemptsPoints = attemptsPoints[b1 | b2 | b3,]
    }
    
    # check offender age
    if (!is.null(input$offenderage)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      b1 = attemptsPoints$Offender.Age.1 %in% input$offenderage
      b2 = attemptsPoints$Offender.Age.2 %in% input$offenderage
      b3 = attemptsPoints$Offender.Age.3 %in% input$offenderage
      attemptsPoints = attemptsPoints[b1 | b2 | b3,]
    }
    
    # check offender perceived age
    if (!is.null(input$offenderpercage)) {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      b1 = attemptsPoints$Offender.Perceived.Age.1 %in% input$offenderperage
      b2 = attemptsPoints$Offender.Perceived.Age.2 %in% input$offenderperage
      b3 = attemptsPoints$Offender.Perceived.Age.3 %in% input$offenderperage
      attemptsPoints = attemptsPoints[b1 | b2 | b3,]
    }
    
    if (input$animal == "Yes") {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Offender.Method.Animal==1,]
    }
    
    if (input$candy == "Yes") {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Offender.Method.Candy==1,]
    }
    
    if (input$money == "Yes") {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Offender.Method.Money==1,]
    }
    
    if (input$ride == "Yes") {
      updateSelectInput(session, "mapOption", selected = c("inci"))
      attemptsPoints = attemptsPoints[attemptsPoints$Offender.Method.Ride==1,]
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
