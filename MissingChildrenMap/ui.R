
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

attempts = read.csv("../../Attempts Hackathon 5 Years of Data.csv")
missing = read.csv("../../Hackathon Missing Child 5 Years of Data.csv")
latLonVals = read.csv("../../zipToLatLon.csv")

# merge lat/lon values into datasets by zip code
colnames(attempts)[which(names(attempts) == "Incident.Zip")] <- "ZIP"
colnames(missing)[which(names(missing) == "Missing.Zip")] <- "ZIP"
attempts = merge(attempts, latLonVals, by = "ZIP")
missing = merge(missing, latLonVals, by = "ZIP")

# fix the dates
attempts$Incident.Date = as.Date(attempts$Incident.Date, format = "%d/%m/%Y")
missing$Missing.Date = as.Date(missing$Missing.Date, format = "%d/%m/%Y")

# fix the -1 values in the method columns
attempts$Offender.Method.Animal[attempts$Offender.Method.Animal==-1] = 1
attempts$Offender.Method.Candy[attempts$Offender.Method.Candy==-1] = 1
attempts$Offender.Method.Money[attempts$Offender.Method.Money==-1] = 1
attempts$Offender.Method.Other[attempts$Offender.Method.Other==-1] = 1
attempts$Offender.Method.Ride[attempts$Offender.Method.Ride==-1] = 1

shinyUI(
  dashboardPage(
    dashboardHeader(title = "NCMEC Map View"),
    dashboardSidebar(collapsed = FALSE,
                     
                     tags$head(
                       tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto;}"))
                     ),
                     
                     sidebarMenu(
                       menuItem("Main Control", icon = icon("chevron-right"), startExpanded = TRUE,
                                checkboxGroupInput("mapOption",
                                                   "Choose Databases",
                                                   choices = c("Missing Children Data" = "miss",
                                                               "Incidents Data" = "inci"),
                                                   selected = c("miss","inci")),
                                
                                actionButton("reset", "Reset Map"),
                                
                                div(style="display:inline-block",
                                    downloadButton("downloadData1", "Download Incident Query"),
                                    style="float:center"),
                                
                                div(style="display:inline-block",
                                    downloadButton("downloadData2", "Download Missing Query"),
                                    style="float:center")
                       )
                     ),
                     sidebarMenu(
                       
                       menuItem("Both Sets Data", icon = icon("chevron-right"), startExpanded = TRUE,
                                
                                sidebarMenu(
                                  
                                  menuItem("Location", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("state", "State", choices = sort(c(as.character(attempts$Incident.State),
                                                                                                  as.character(missing$Missing.State))),
                                                       multiple = TRUE),
                                           
                                           selectInput("city", "City", choices = sort(c(as.character(attempts$Incident.City),
                                                                                                as.character(missing$Missing.City))),
                                                       multiple = TRUE),
                                           
                                           selectInput("zip", "Zip", choices = sort(c(attempts$ZIP,missing$ZIP)),
                                                       multiple = TRUE)
                                           
                                  )),
                                
                                sidebarMenu(
                                  menuItem("Child Info", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("caseID", "Case ID", choices = sort(c(attempts$Case.Number,missing$Case.Number)),
                                                       multiple = TRUE),
                                           
                                           selectInput("gender", "Gender", choices = sort(c(as.character(attempts$Child.Gender.1),
                                                                                        as.character(missing$Gender))),
                                                       multiple = TRUE),
                                           
                                           selectInput("race", "Race", choices = sort(c(as.character(attempts$Child.Race.1),
                                                                                        as.character(missing$Race))),
                                                       multiple = TRUE)
                                           
                                  )),
                                
                                sidebarMenu(
                                  menuItem("Offender Info", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("vehstyle", "Vehicle Style", choices = sort(c(as.character(attempts$Vehicle.Style.1),
                                                                                                     as.character(missing$Vehicle.Style))),
                                                       multiple = TRUE),
                                           
                                           selectInput("vehcolor", "Vehicle Color", choices = sort(c(as.character(attempts$Vehicle.Color.1),
                                                                                                     as.character(missing$Vehicle.Color))),
                                                       multiple = TRUE))                 )
                       )
                     ),
                     sidebarMenu(
                       
                       menuItem("Attempts Only Data", icon = icon("chevron-right"), startExpanded = TRUE,
                                
                                sidebarMenu(
                                  
                                  menuItem("Case Info", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("casetype", "Case Type", choices = sort((as.character(attempts$Case.Type))),
                                                       multiple = TRUE),
                                           
                                           selectInput("casestatus", "Case Status", choices = sort(as.character(attempts$Status)),
                                                       multiple = TRUE),
                                           
                                           selectInput("casesource", "Case Source", choices = sort(as.character(attempts$Source)),
                                                       multiple = TRUE)
                                  )),
                                
                                sidebarMenu(
                                  menuItem("Location Info", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("address", "Address", choices = sort(as.character(attempts$Incident.Location)),
                                                       multiple = TRUE),
                                           
                                           selectInput("locationtype", "Location Type", choices = sort(as.character(attempts$Incident.Location.Type)),
                                                       multiple = TRUE),
                                           
                                           selectInput("county", "County", choices = sort(as.character(attempts$Incident.County)),
                                                       multiple = TRUE)
                                  )),
                                
                                sidebarMenu(
                                  menuItem("Child Info", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("childID", "Child ID", choices = sort(c(attempts$Child.ID.1,
                                                                                               attempts$Child.ID.2,
                                                                                               attempts$Child.ID.3,
                                                                                               attempts$Child.ID.4,
                                                                                               attempts$Child.ID.5,
                                                                                               attempts$Child.ID.6)),
                                                       multiple = TRUE),
                                           
                                           selectInput("gotaway", "How They Got Away", choices = sort(as.character(attempts$How.Got.Away)),
                                                       multiple = TRUE)
                                  )),
                                sidebarMenu(
                                  menuItem("Offender Info", tabName = "rawdata", icon=icon("toggle-right"),startExpanded = TRUE,
                                           
                                           selectInput("offendergender", "Offender Gender", choices = sort(c(as.character(attempts$Offender.Gender.1),
                                                                                                             as.character(attempts$Offender.Gender.2),
                                                                                                             as.character(attempts$Offender.Gender.3))),
                                                       multiple = TRUE),
                                           
                                           selectInput("offenderage", "Offender Age", choices = sort(c(as.character(attempts$Offender.Age.1),
                                                                                                       as.character(attempts$Offender.Age.2),
                                                                                                       as.character(attempts$Offender.Age.3))),
                                                       multiple = TRUE),
                                           
                                           selectInput("offenderpercage", "Offender Perceived Age", choices = sort(c(as.character(attempts$Offender.Perceived.Age.1),
                                                                                                                     as.character(attempts$Offender.Perceived.Age.2),
                                                                                                                     as.character(attempts$Offender.Perceived.Age.3))),
                                                       multiple = TRUE),
                                           
                                           radioButtons("animal", "Used Animals", choices = c("No", "Yes")),
                                           
                                           radioButtons("candy", "Used Candy", choices = c("No", "Yes")),
                                           
                                           radioButtons("money", "Used Money", choices = c("No", "Yes")),
                                           
                                           radioButtons("ride", "Offered a Ride", choices = c("No", "Yes"))
                                  ))
                       )
                     )
                     
    ),
    dashboardBody(
      useShinyjs(),
      tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
      leafletOutput("mymap")
    )
  )
)