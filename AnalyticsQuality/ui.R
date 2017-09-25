library(shiny)
library(plotly)
library(rpivotTable)
library(DT)
library(shinydashboard)


dashboardPage(
    dashboardHeader(title = "IBM - Trello Report"),
    dashboardSidebar(
        #textInput("TrelloURL", "Please provide your Trello URL"),
        textInput("fileNameF", "Please provide the file name."),
        #radioButtons("varTime", "Please select Cycle Time Format:",
        #             c("Days" = "days","Hours" = "hours", 
        #               "Minutes" = "mins")),
        #checkboxInput("closed", "Archived", value = FALSE),
        actionButton("do", "Get Report"),
        #uiOutput("choose_year"),
        #uiOutput("choose_week"),
        #uiOutput("choose_listName"),
        downloadButton("downloadData", "Download Report")
        #downloadButton("downloadTP", "Download Agile Metrics")
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)

        fluidRow(
            textOutput("text1")
        )
        
    )
)