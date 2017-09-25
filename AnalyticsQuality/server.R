library(httr)
library(plyr)
library(magrittr)
library(lubridate)
library(trelloR)
library(jsonlite)
library(rpivotTable)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)
library(shiny)

shinyServer(function(input, output) {

    observeEvent(input$do, {
      
        my_token <- trello_get_token("d00e8afb53f716936477840488ad72f5", 
                                     "7e46b4cff5903e5c74d88cc26fa262a1b347edacfabe7b95157f6ca52c67aa7f")
        #load("C:/Users/johnalva/Desktop/ConectionTrello.RData")
        ae <- "https://trello.com/b/i4u41Lhr/analytics-sdr.json"
        req <- httr::GET(ae, my_token, paging = TRUE)
        json <- httr::content(req, paging = TRUE)
        #class(json) # Return a list.
        ibd <- get_id_board(ae, my_token)
        
        cards <- sapply(json$cards, function(x) c(x$id,x$name,x$dateLastActivity,
                                                  x$idList, x$closed, x$desc))
        
        cards <- data.frame(t(cards), stringsAsFactors=FALSE)
        names(cards) <- c('idCard','cardName','dateLastActivity','idList','closed',
                          'description')
        
        cards$dateLastActivity <- as.POSIXct(strptime(paste0(cards$dateLastActivity), 
                                                      tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))-(6*3600)
        
        # Cycle Time Calculation ----------------------------------------
        cards$creationDate <- as.POSIXct(strtoi(paste0('0x',substr(cards$idCard,1,8))), 
                                         origin="1970-01-01")
        
        # Important: pass dates to same metric unit to create the cycle time
        
        cards$creationDate <- as.character(cards$creationDate)
        cards$dateLastActivity <- as.character(cards$dateLastActivity)
        
        
        cards$cycleTimeDays <- difftime(cards$dateLastActivity, cards$creationDate, 
                                        units = "days")
        
        
        #cards$cycleTimeDays <- (cards$dateLastActivity - cards$creationDate)
        
        cards$WeekEnding <- week(cards$dateLastActivity)
        cards$Year <- year(cards$dateLastActivity)
        
        # List names merge-----------------------------------------------
        list1 <- sapply(json$lists, function(x) c(x$id,x$name))
        listdf <- as.data.frame(t(list1))
        names(listdf) <- c("idList","Listname")
        c1 <- merge(listdf,cards, by = "idList")
        c1 <- c1[,-1]
        
        #  ------------------------------------------------------------------------
        
        # Members definitons ------------------------------------------------------
        
        membersList <- get_board_members(ibd,my_token)
        membersList <- as.data.frame(membersList)
        
        #  ------------------------------------------------------------------------
        
        # Members Section ----------------------------------------------------------
        
        members <- sapply(json$cards, function(x) c(x$id, x$idMembers[1:5]))
        members <- data.frame(t(members), stringsAsFactors = FALSE)
        names(members) <- c('idCard','Assignee1', 'Assignee2','Assignee3','Assignee4',
                            'Assignee5')
        
        # Assign names ------------------------------------------------------------
        
        membersAssignation <- function(x,b){
            #if(x[[b]] != "" | !is.null(x[[b]])){
            if(!is.null(x[[b]])){
                j <- intersect(x[[b]], membersList$id)
                k <- filter(membersList, id==j)
                k$fullName
            }else{
                "n"
            }
        }
        
        membersTest <- members
        
        for(i in 1:nrow(members)){
            membersTest$Assignee1[[i]] <- membersAssignation(members$Assignee1,i)
            membersTest$Assignee2[[i]] <- membersAssignation(members$Assignee2,i)
            membersTest$Assignee3[[i]] <- membersAssignation(members$Assignee3,i)
            membersTest$Assignee4[[i]] <- membersAssignation(members$Assignee4,i)
            membersTest$Assignee5[[i]] <- membersAssignation(members$Assignee5,i)
        }
        
        
        # Merge assingee with Master Report ---------------------------------------
        
        c2 <- merge(c1,membersTest, by = "idCard")
        
        c2$Assignee1 <- as.character(c2$Assignee1)
        c2$Assignee2 <- as.character(c2$Assignee2)
        c2$Assignee3 <- as.character(c2$Assignee3)
        c2$Assignee4 <- as.character(c2$Assignee4)
        c2$Assignee5 <- as.character(c2$Assignee5)
        
        
        #  ------------------------------------------------------------------------
        
        # Squad Section -----------------------------------------------------------
        
        
        lbl <- sapply(json$cards, function(x) c(x$id, list(x$idLabels)))
        sq1 <- data.frame(t(lbl), stringsAsFactors=FALSE)
        names(sq1) <- c('idCard', 'Squad')
        
        for(i in 1:nrow(sq1)){
            if(length(sq1$Squad[[i]]) == (0))
                sq1$Squad[[i]] <- ""
        }
        
        Squads <- get_board_labels(ibd,my_token)
        Squads <- as.data.frame(Squads)
        Squads1 <- select(Squads[c(1,2,4,5,8),], id, name)
        Squads1
        squadsDefinition <- function(x,b){
            if(x$Squad[[b]] != ""){
                j <- intersect(sq1$Squad[[b]], Squads1$id)
                if(length(j > 0)){
                    k <- filter(Squads1, id==j)
                    k$name
                }else{
                    "No Squad Assigned"
                }
            }else{
                "No Squad Assigned"
            }
        }
        
        
        for(i in 1:nrow(sq1)){
            sq1$Squad[[i]] <- squadsDefinition(sq1,i)
        }
        
        # Merge
        c2 <- merge(c2, sq1, by = "idCard")
        
        # Change from list to character
        c2$Squad <- as.character(c2$Squad)
        
        #  ------------------------------------------------------------------------
        
        # Metrics / Exceptions Definition -----------------------------------------
        
        exceptions <- get_board_labels(ibd,my_token)
        exceptions <- as.data.frame(exceptions)
        exceptions <- select(exceptions[c(3,6,7,9,10,11,12,13,14,15,16,17),], id, name)
        exceptions
        #  ------------------------------------------------------------------------
        
        # Metric Section ----------------------------------------------------------
        
        exe <- sapply(json$cards, function(x) c(x$id, list(x$idLabels)))
        exe <- data.frame(t(exe), stringsAsFactors = FALSE)
        names(exe) <- c('idCard','exeptionList')
        
        # Clean List
        
        for(i in 1:nrow(exe)){
            if(length(exe$exeptionList[[i]]) == (0))
                exe$exeptionList[[i]] <- ""
        }
        
        # Assign Exceptions --------------------------------------------------------
        
        exeAssignation <- function(x,b){
            #if(x[[b]] != "" | !is.null(x[[b]])){
            # Overtime
            if(!is.null(x[[b]])){
                j <- intersect(x[[b]], exceptions$id)
                k <- any(exceptions, b)
                k$fullName
            }else{
                "n"
            }
        }
        
        exe1 <- exe
        for(i in 1:nrow(exe1)){
            exe1$Green <- "n"
            exe1$Amber <- "n"
            exe1$Red <- "n"
            exe1$Overtime <- "n"
            exe1$ReWork <- "n"
            exe1$Projects <- "n"
            exe1$Task <- "n"
            exe1$Highlight <- "n"
            exe1$Lowlight <- "n"
            exe1$Blocker <- "n"
            exe1$HighImportance <- "n"
            exe1$Automation <- "n"
            
        }
        
        
        for(i in 1:nrow(exe1)){
            
            
            if(any(exe1$exeptionList[[i]] == "58d165d0ced82109ffd5b1b6")){
                exe1$Green[[i]] <- "Green"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "58c852e4ced82109ffbb75b5")){
                exe1$Amber[[i]] <- "Amber"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "58c853f916bb0fbc12cc8e84")){
                exe1$Overtime[[i]] <- "Overtime"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "58c852e4ced82109ffbb75b6")){
                exe1$Red[[i]] <- "Red"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "58d199f4fe82832591540160")){
                exe1$ReWork[[i]] <- "Re-Work"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "5926582ee6b7b8df080b5b08")){
                exe1$Projects[[i]] <- "Projects"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "59265836864f22c71f5bd0db")){
                exe1$Task[[i]] <- "Task"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "592658c79b34d5815cd5a1d7")){
                exe1$Highlight[[i]] <- "Highlight"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "592658d1e37f09ef898b902a")){
                exe1$Lowlight[[i]] <- "Lowlight"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "5926e23935a8eae611df8954")){
                exe1$Blocker[[i]] <- "Blocker"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "5926e249437fddc0b870046b")){
                exe1$HighImportance[[i]] <- "High Importance"
            }else{
                "n"
            }
            
            if(any(exe1$exeptionList[[i]] == "593aa6212d308cf1ac6e3d79")){
                exe1$Automation[[i]] <- "Automation"
            }else{
                "n"
            }
            
        }
        
        exe1 <- exe1[,-2]
        
        # Merge assingee with Master Report ---------------------------------------
        
        c2 <- merge(c2,exe1, by = "idCard")
        
        c2$Green <- as.character(c2$Green)
        c2$Amber <- as.character(c2$Amber)
        c2$Red <- as.character(c2$Red)
        c2$Overtime <- as.character(c2$Overtime)
        c2$ReWork <- as.character(c2$ReWork)
        c2$Projects <- as.character(c2$Projects)
        c2$Task <- as.character(c2$Task )
        c2$Highlight <- as.character(c2$Highlight)
        c2$Lowlight <- as.character(c2$Lowlight)
        c2$Blocker <- as.character(c2$Blocker)
        c2$HighImportance <- as.character(c2$HighImportance)
        
        c2 <<- c2
        
        output$text1 <- renderText({
            "Request has been completed and is ready to download."
        })
          
    })    
        
    output$downloadData <- downloadHandler(
        filename = function(){paste(input$fileNameF,'.csv')},
        content = function(file){
            write.csv(c2, file)
        }
    )

    
})
