library(RCurl)
library(httr)
library(RJSONIO)
library(ROAuth)

httput <- httpPUT("https://api.ibm.com/watsonanalytics/run/oauth2/v1/config",
        content = toJSON(list('clientName' = "johnalva", 
                              'redirectURIs' = "https://localhost:8080" , 
                              "ownerName" = "johnalva", 
                              "ownerEmail" = "johnalva@cr.ibm.com", 
                              "ownerCompany" = "IBM", 
                              "ownerPhone" = "85347012")),
    .opts = list(httpheader = c('Content-Type' = 'application/json', 
        'X-IBM-Client-Secret' = 'N0oD5dE3wD1iE7xR4oI5eR3oU1yK4gN4uE2kV1gK3oY0fK0oX7', 
        'X-IBM-Client-Id' = '12e62179-dd02-4802-88c6-4280cb8d91cd')))

httput
fromJSON(httput)
#

getURI(httput)


tk <- authorize(key, secret)





pt <- POST("https://api.ibm.com/watsonanalytics/run/clientauth/v1/auth?response_type=code&client_id=12e62179-dd02-4802-88c6-4280cb8d91cd&scope=userContext&redirect_uri=https://localhost:8080")
class(pt)
pt$headers
rt <- rawToChar(pt$content)

pt <- getURL("https://idaas.iam.ibm.com/idaas/mtfim/sps/authsvc?PolicyId=urn:ibm:security:authentication:asf:basicldapuser")
pt$status_code
pt$cookies$name

GET(BROWSE("https://api.ibm.com/watsonanalytics/run/clientauth/v1/auth?response_type=code&client_id=12e62179-dd02-4802-88c6-4280cb8d91cd&scope=userContext&redirect_uri=https://localhost:7280"))
#

tk <- getURL("https://api.ibm.com/watsonanalytics/run/accounts/v1/auth?",
     .opts = list(httpheader = c('Content-Type' = 'application/json', 
     'X-IBM-Client-Secret' = 'N0oD5dE3wD1iE7xR4oI5eR3oU1yK4gN4uE2kV1gK3oY0fK0oX7', 
     'X-IBM-Client-Id' = '12e62179-dd02-4802-88c6-4280cb8d91cd')))
tk

t <- getURI("https://api.ibm.com/watsonanalytics/run/clientauth/v1/auth?response_type=code&client_id=12e62179-dd02-4802-88c6-4280cb8d91cd&scope=userContext&redirect_uri=https://localhost:7280", config = authenticate("johnalva@cr.ibm.com", "johALVME143"))
fromJSON(t)
#







url1 <- getURL("https://idaas.iam.ibm.com/idaas/mtfim/sps/authsvc?PolicyId=urn:ibm:security:authentication:asf:basicldapuser",
       .opts = list(httpheader = c('Content-Type' = 'application/json', 
        'X-IBM-Client-Secret' = 'N0oD5dE3wD1iE7xR4oI5eR3oU1yK4gN4uE2kV1gK3oY0fK0oX7', 
       'X-IBM-Client-Id' = '12e62179-dd02-4802-88c6-4280cb8d91cd')))

uri1 <- getURI("https://api.ibm.com/watsonanalytics/run/oauth2/v1/config",
       .opts = list(httpheader = c('Content-Type' = 'application/json', 
       'X-IBM-Client-Secret' = 'N0oD5dE3wD1iE7xR4oI5eR3oU1yK4gN4uE2kV1gK3oY0fK0oX7', 
       'X-IBM-Client-Id' = '12e62179-dd02-4802-88c6-4280cb8d91cd')))

urlcont <- getURLContent("https://api.ibm.com/watsonanalytics/run/oauth2/v1/config",
              .opts = list(httpheader = c('Content-Type' = 'application/json', 
              'X-IBM-Client-Secret' = 'N0oD5dE3wD1iE7xR4oI5eR3oU1yK4gN4uE2kV1gK3oY0fK0oX7', 
              'X-IBM-Client-Id' = '12e62179-dd02-4802-88c6-4280cb8d91cd')))



getCurlHandle(.opts = list(httpheader = c('Content-Type' = 'application/json', 
    'X-IBM-Client-Secret' = 'N0oD5dE3wD1iE7xR4oI5eR3oU1yK4gN4uE2kV1gK3oY0fK0oX7', 
    'X-IBM-Client-Id' = '12e62179-dd02-4802-88c6-4280cb8d91cd')))


library(httr)
library(jsonlite)
library(trelloR)
?get_id_board

GET("https://api.github.com/users/johanalva/", authenticate ("johnalva@cr.ibm.com", "johALVME143"))

johnalva <- fromJSON("https://api.github.com/users/johanalva/")

zh <- GET("https://app.zenhub.com/workspace/o/johanalva/ibmtrello/boards?repos=95308419.json")
zh$content


fromJSON("https://app.zenhub.com/workspace/o/johanalva/ibmtrello/boards?repos=95308419")
zhr <- rawToChar(zh$content)
json <- fromJSON(zhr)

my_token <- "ea510b7355f84d972cbe8ff6057e319881451b6b377eefe39add84fe99715e7b7b3296d51c35c56a"





