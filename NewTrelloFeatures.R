# library(trelloR)
# library(dplyr)
# library(httr)
# library(plyr)
# library(jsonlite)
# 
# apic <- oauth_app("trello", "d00e8afb53f716936477840488ad72f5", "7e46b4cff5903e5c74d88cc26fa262a1b347edacfabe7b95157f6ca52c67aa7f")
# ae <- "https://trello.com/b/i4u41Lhr/analytics-sdr"
# rq <- "https://trello.com/1/OAuthGetRequestToken"
# au <- "https://trello.com/1/OAuthAuthorizeToken"
# ac <- "https://trello.com/1/OAuthGetAccessToken"
# otep <- oauth_endpoint(rq, au, ac, ae)
# 
# 
# 
# 
# 
# my_token <- oauth1.0_token(otep, apic)
# req <- httr::GET(ae, my_token, paging = TRUE)

setwd("C:/Users/johnalva/Box Sync/My Documents/R")
source(file = "NewAnalytics&QualityV1.R")

board <- get_id_board(ae, my_token)
actions <- get_board_actions(board, my_token, paging = TRUE)
get_board_lists(board, my_token)

dim(actions)
names(actions)



# Adding Json -------------------------------------------------------------
# 
# ae2 <- paste0(ae,".json")
# req2 <- httr::GET(ae2, my_token, paging = TRUE)
# raws <- rawToChar(req2$content)
# this.content <- fromJSON(raws)
# cards <- this.content$cards



#  ------------------------------------------------------------------------



createList <- select(filter(actions, type == "copyCard" | type == "createCard" | 
                                type == "moveCardToBoard" | 
                                type == "convertToCardFromCheckItem"), 
                     data.card.id, date, data.list.name, type, memberCreator.username, 
                     type, data.card.name, member.initials, data.card.closed,
                     data.text)

mtto <- select(filter(actions, c(data.listAfter.id == "58c852f2942a8757a82418e9" & 
                                data.listBefore.id == "58c852ee21c59832f8bc1848")),
                data.card.id, date, data.list.name, type,
                memberCreator.username, 
                type, data.listBefore.name,
                data.listAfter.name)


names(mtto)[names(mtto) == "date"] <- "DateMtto"
createList <- (unique(createList))
mtto <- unique(mtto)

mer <- merge(createList, mtto, by = "data.card.id", all.x = TRUE)
mer$date <- as.POSIXct(strptime(paste0(mer$date),
                                tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))-(6*3600)

mer$DateMtto <- as.POSIXct(strptime(paste0(mer$DateMtto),
                                tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))-(6*3600)

mer$mtto <- round(difftime(mer$DateMtto, mer$date, units = c("hours")), digits = 2)
names(mer)[names(mer) == "data.card.id"] <- "id"
names(mer)

mer2 <- merge(cardsDisplay, mer[,c("id","date","type.x","DateMtto","type.y",
                                "data.listBefore.name","data.listAfter.name",
                                "mtto")], by = "id", all.x = TRUE)


mer3 <- select(mer2,"name", "desc", "closed", "type.x", "date", "type.y",
                    "DateMtto", "data.listBefore.name","data.listAfter.name",
                    "mtto","dateLastActivity", "due", "dueComplete",
                    "cycleTimeDays", "WeekEnding","Year","Squad","List",
                    "Assignee1","Assignee2","Assignee3","Assignee4","Assignee5",
                    "Green","Amber","Red","Overtime","Re_Work","Projects",
                    "Task","Highlight","Lowlight","Blocker","HighImportance",
                    "Automation")

names(mer3)[names(mer3) == "date"] <- "Creation Date"
names(mer3)[names(mer3) == "type.x"] <- "Created by"
names(mer3)[names(mer3) == "type.y"] <- "Updated by"
names(mer3)[names(mer3) == "DateMtto"] <- "To WIP Date"
names(mer3)[names(mer3) == "data.listAfter.name"] <- "To List"
names(mer3)[names(mer3) == "data.listBefore.name"] <- "From List"
names(mer3)[names(mer3) == "mtto"] <- "Start before (hours)"
View(actions)
View(mer3)

write.csv(actions, "actions.csv")
getwd()
