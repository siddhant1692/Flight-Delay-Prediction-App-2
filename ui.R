library(shinydashboard)
library(shiny)
library(caret)
library(xgboost)
library(methods)


options(shiny.maxRequestSize=300*1024^2)

origin47 <- read.csv("origin47.csv")
tail47<-read.csv("tailno47.csv")
fit_xgboost47<-readRDS("model47.rds")

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Flight Delay Prediction",titleWidth = 350),
    dashboardSidebar(width = 350,
                     sidebarMenu(
                         h3(menuItem("Project Summary", tabName = "Background", icon = icon("list-alt"))),
                         tags$hr(),
                         h3(menuItem("Input Data", tabName = "InputData", icon = icon("file"))),
                         tags$hr(),
                         h3(menuItem("Random data file", tabName = "Referencedata", icon = icon("tachometer-alt"))),
                         tags$hr()
                     )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "Background",
                fluidRow(column(width = 12,
                                h1(
                                    "Business Objective:",style = "color: black",tags$br()
                                )),
                         column(width = 12, h3(
                             "Flight delay cost the industry an estimated $25 billion every year. The bill for passengers an extra 25$ per hour and an extra 62$ per minute for carriers.",tags$br(), tags$br(),
                             "Our objective is to predict delay based on input data",tags$br(),tags$br(),
                             "With our model, millions of rupees can be saved",tags$br()
                         )),
                         htmlOutput("Flight")
                )
            ),
            tabItem(
                tabName = "InputData",
                fluidRow(column(width = 12, h3("OUTPUT WILL BE DISPLAYED FOR 2015 DATASET")),
                         column(width = 4, h4(numericInput("MONTH", "Enter Month", 1, 1, 12))),
                         column(width = 4, h4(numericInput("DAY","Enter Day of the month", 1,1,31))),
                         column(width = 4, h4(selectInput("AIRLINE","Select Airline",c("AA","AS","B6","DL","EV","F9","HA","MQ","NK","OO","UA","US","VX","WN")))),
                         column(width = 4, h4(numericInput("FLIGHT_NUMBER","Enter Flight Number",1,1,9999))),
                         column(width = 4, h4(selectInput("TAIL_NUMBER","Select Tail Number",c(tail47)))),
                         column(width = 4, h4(selectInput("ORIGIN_AIRPORT","Select Origin Airport",c(origin47)))),
                         column(width = 4, h4(numericInput("SCHEDULED_DEPARTURE","Enter the Scheduled Departure Time (in hhmm format)",1,1,2400))),
                         column(width = 4, h4(numericInput("DEPARTURE_TIME","Enter the Actual Departure Time ......(in hhmm format)",1,1,2400))),
                ),tags$hr(),column(width = 5),
                actionButton( "Enter","SUBMIT",class = "btn-primary btn-lg  active"),tags$hr(),verbatimTextOutput("depdelay")
            ),
            tabItem(tabName = "Referencedata",
                    fileInput("data","Upload the file in  .CSV Format, features like 
                             (MONTH, DAY, AIRLINE, FLIGHT_NUMBER, TAIL_NUMBER, ORIGIN_AIRPORT, SCHEDULED_DEPARTURE, DEPARTURE_TIME)",multiple = FALSE, 
                              buttonLabel = "Browse..",accept =".csv", ),
                    actionButton("File","SUBMIT",class = "btn-primary btn-lg active"),tags$hr(),
                    textOutput("Cal") ,
                    downloadButton('downloadData', 'Download CSV Report', 
                                   style="display: block; margin: 0 auto; width: 230px;color: black;")
            )
        )
    )
)


