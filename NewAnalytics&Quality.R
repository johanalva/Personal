library(httr)
library(plyr)
library(magrittr)
library(lubridate)
library(trelloR)
library(httr)
library(jsonlite)
library(rpivotTable)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)

# Conections --------------------------------------------------------------

apic <- oauth_app("trello", "d00e8afb53f716936477840488ad72f5", "7e46b4cff5903e5c74d88cc26fa262a1b347edacfabe7b95157f6ca52c67aa7f")
ae <- "https://trello.com/b/i4u41Lhr/analytics-sdr.json"
rq <- "https://trello.com/1/OAuthGetRequestToken"
au <- "https://trello.com/1/OAuthAuthorizeToken"
ac <- "https://trello.com/1/OAuthGetAccessToken"
otep <- oauth_endpoint(rq, au, ac, ae)

my_token <- oauth1.0_token(otep, apic)


req <- httr::GET(ae, my_token, paging = TRUE)

raws <- rawToChar(req$content)
this.content <- fromJSON(raws)
cards <- this.content$cards

cardsDisplay <- select(cards, id, name, desc, closed, due, dueComplete,
                       labels, dateLastActivity, idMembers, idList)

# Squad Creation
for(i in 1:nrow(cardsDisplay)){
    if(dim(cardsDisplay$labels[[i]]) !=0){    
        if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Account DA")){
            cardsDisplay$Squad[i] <- "Account DA"
        }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "LDA")){
            cardsDisplay$Squad[i] <- "LDA"
        }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "NA QA")){
            cardsDisplay$Squad[i] <- "NA QA"
        }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "PASIR")){
            cardsDisplay$Squad[i] <- "PASIR"
        }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Service Engineering / BPO")){
            cardsDisplay$Squad[i] <- "Service Engineering / BPO"
        }}else{
            cardsDisplay$Squad[i] <- "No Squad Selected"
        }
}
    
# Metrics Creation
cardsDisplay$Green <- "n"
cardsDisplay$Amber <- "n"
cardsDisplay$Red <- "n"
cardsDisplay$Overtime <- "n"
cardsDisplay$Re_Work <- "n"
cardsDisplay$Projects <- "n"
cardsDisplay$Task <- "n"
cardsDisplay$Highlight <- "n"
cardsDisplay$Lowlight <- "n"
cardsDisplay$Blocker <- "n"
cardsDisplay$HighImportance <- "n"
cardsDisplay$Automation <- "n"

metricsEvaluate <- function(x){
    if(any(unlist(cardsDisplay$labels[[i]][3]) %in% x)){
        metric <- x
    }else{
        metric <- "n"
    }
}

for(i in 1:nrow(cardsDisplay)){
    if(dim(cardsDisplay$labels[[i]]) !=0){
        cardsDisplay$Green[i] <- metricsEvaluate("Green")
        cardsDisplay$Amber[i] <-metricsEvaluate("Amber")
        cardsDisplay$Red[i] <- metricsEvaluate("Red")
        cardsDisplay$Overtime[i] <- metricsEvaluate("Overtime")
        cardsDisplay$Re_Work[i] <- metricsEvaluate("Re-Work")
        cardsDisplay$Projects[i] <- metricsEvaluate("Projects")
        cardsDisplay$Task[i] <- metricsEvaluate("Task")
        cardsDisplay$Highlight[i] <- metricsEvaluate("Highlight")
        cardsDisplay$Lowlight[i] <- metricsEvaluate("Lowlight")
        cardsDisplay$Blocker[i] <- metricsEvaluate("Blocker")
        cardsDisplay$HighImportance[i] <- metricsEvaluate("High Importance")
        cardsDisplay$Automation[i] <- metricsEvaluate("Automation")
        
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Green")){
        #     cardsDisplay$Green[i] <- "Green"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Amber")){
        #     cardsDisplay$Amber[i] <- "Amber"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Red")){
        #     cardsDisplay$Red[i] <- "Red"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Overtime")){
        #     cardsDisplay$Overtime[i] <- "Overtime"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Re-Work")){
        #     cardsDisplay$Re_Work[i] <- "Re-Work"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Projects")){
        #     cardsDisplay$Projects[i] <- "Projects"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Task")){
        #     cardsDisplay$Task[i] <- "Task"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Highlight")){
        #     cardsDisplay$Highlight[i] <- "Highlight"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Lowlight")){
        #     cardsDisplay$Lowlight[i] <- "Lowlight"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Blocker")){
        #     cardsDisplay$Blocker[i] <- "Blocker"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "High Importance")){
        #     cardsDisplay$HighImportance[i] <- "High Importance"
        # }
        # if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Automation")){
        #     cardsDisplay$Automation[i] <- "Automation"
        # }
    }
}

# Members:

cardsDisplay$Assignee1 <- 'n'
cardsDisplay$Assignee2 <- 'n'
cardsDisplay$Assignee3 <- 'n'
cardsDisplay$Assignee4 <- 'n'
cardsDisplay$Assignee5 <- 'n'

# Functions to obtain Assignee name
assigneeMember <- function(i, pos){
    if(any(unlist(cardsDisplay$idMembers[[i]][pos]) %in% this.content$members$id)){
        id.Identify <- (unlist(cardsDisplay$idMembers[[i]][pos]))
        memberName <- subset(this.content$members, id == id.Identify)
        memberName <- memberName[6]
    }else
        memmberName <- 'n'
}

# Cycle to fill Members columns
for(i in 1:nrow(cardsDisplay)){
    if(length(cardsDisplay$idMembers[[i]]) !=0){
        cardsDisplay$Assignee1[i] <- assigneeMember(i,1)
        cardsDisplay$Assignee2[i] <- assigneeMember(i,2)
        cardsDisplay$Assignee3[i] <- assigneeMember(i,3)
        cardsDisplay$Assignee4[i] <- assigneeMember(i,4)
        cardsDisplay$Assignee5[i] <- assigneeMember(i,5)
    }
    else{
        cardsDisplay$Assignee1[i] <- 'No any member assignee to this card'
    }
}

#  ------------------------------------------------------------------------

View(cardsDisplay)
