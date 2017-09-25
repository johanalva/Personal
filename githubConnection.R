#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Suscribe",
                   key = "63673c02b3491618cd92",
                   secret = "800cb936e09f148e44eb4af35e76cb84f420717e")



# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/repos/johanalva/IBMTrello/issues", gtoken)
GET("https://api.github.com/repos/johanalva/IBMTrello/issues", gtoken)
content(req)
req$content

raw.data <- rawToChar(req$content)

json <- fromJSON(raw.data)
View(json$body)
View(json)

GET("https://api.zenhub.io/p1/repositories/95308419/issues", gtoken)

# 
# # Take action on http error
# stop_for_status(req)
# 
# # Extract content from a request
# json1 = content(req)
# 
# # Convert to a data.frame
# gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
# 
# # Subset data.frame
# gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 