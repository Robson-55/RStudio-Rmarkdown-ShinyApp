library(shiny)
library(shinythemes)
numberofrows=length(L$id)

propertyType=unique(L$property_type)
propertyType=toString(propertyType)
propertyType=as.list(strsplit(propertyType, ",")[[1]])

roomType=unique(L$room_type)
roomType=toString(roomType)
roomType=as.list(strsplit(roomType, ",")[[1]])

accommodatesType=unique(L$accommodates)
accommodatesType=toString(accommodatesType)
accommodatesType=as.list(strsplit(accommodatesType, ",")[[1]])
accommodatesType=as.numeric(accommodatesType)
accommodatesType=as.list(accommodatesType)

bathroomsType=unique(L$bathrooms)
bathroomsType=toString(bathroomsType)
bathroomsType=as.list(strsplit(bathroomsType, ",")[[1]])
bathroomsType=as.numeric(bathroomsType)
bathroomsType=as.list(bathroomsType)

bedroomsType=unique(L$bedrooms)
bedroomsType=toString(bedroomsType)
bedroomsType=as.list(strsplit(bedroomsType, ",")[[1]])
bedroomsType=as.numeric(bedroomsType)
bedroomsType=as.list(bedroomsType)


# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("cerulean"),
        navbarPage(
            "Airbnb Insights App",
            
            tabPanel("SCAN HOSTS",
                     sidebarPanel(
                         tags$h2("Introduce Host ID:"),
                         textInput(inputId = "enter_hostid",label = "Host ID:"),
                         
                     ),
                     mainPanel(
                         h2("Number of Sites registered by Host ID:"),
                         verbatimTextOutput("Hoster_NumberOf_Apartments"),
                     )
                     
                     
            ),
            
            tabPanel("SCAN NEIGHBOURHOOD",
                     
                     sidebarPanel(
                         tags$h2("Insert a Neighbourhood:"),
                         textInput(inputId = "enter_neighbourhoodid",label = "Neighbourhood ID:"),
                         
                     ),
                     mainPanel(
                         h2("Mean price for this Neighbourhood:"),
                         verbatimTextOutput("Neighbourhood_meanPrice"),
                     )
                  ),
            
            tabPanel(" SPECS-PRICE",
                     
                     sidebarPanel(
                         tags$h2("Introduce Property Type:"),
                         textInput(inputId = "enter_propertytype",label = "Property Type:"),
                         tags$h2("Introduce Room Type:"),
                         textInput(inputId = "enter_roomtype",label = "Room Type:"),
                         tags$h2("Introduce Accommodates:"),
                         textInput(inputId = "enter_accommodatestype",label = "Accommodates Number:"),
                         tags$h2("Introduce Bathrooms:"),
                         textInput(inputId = "enter_bathroomstype",label = "Bathrooms Number:"),
                         tags$h2("Introduce Bedrooms:"),
                         textInput(inputId = "enter_bedroomstype",label = "Bedrooms umber:"),
                         
                         
                     ),
                     mainPanel(
                         h2("Mean price for your specifications:"),
                         verbatimTextOutput("specs_meanPrice"),
                         h2("Minimum price for your specifications:"),
                         verbatimTextOutput("specs_minPrice"),
                         h2("Maximum price for your specifications:"),
                         verbatimTextOutput("specs_maxPrice"),
                     )
            ),
            tabPanel("FREQUENCY NEIGHBOURHOOD-TIME",
                     
                     sidebarPanel(
                         tags$h2("Introduce a Neighbourhood:"),
                         textInput(inputId = "enter_neighbourhoodAv",label = "Neighbourhood Availability :"),
                         
                     ),
                     mainPanel(
                         h2("Average days availability for the following 30 days:"),
                         verbatimTextOutput("meanAv_30"),
                         h2("Average days availability for the following 60 days:"),
                         verbatimTextOutput("meanAv_60"),
                         h2("Average days availability for the following 90 days:"),
                         verbatimTextOutput("meanAv_90"),
                         h2("Average days availability for the following 365 days:"),
                         verbatimTextOutput("meanAv_365"),
                     )
            )
            
            
            
                )
        )

server <- function(input,output){
    
    output$Hoster_NumberOf_Apartments<-renderText({
        equal1=(L$host_id==as.numeric(input$enter_hostid)) 
        lista1<-list()
        
        for (i in 1:numberofrows){
            if (equal1[i]==TRUE){lista1<-c(lista1,1)}
        }
        
        Hoster_NumberOf_Apartments<-length(lista1)
        Hoster_NumberOf_Apartments
    })
    
    output$Neighbourhood_meanPrice<-renderText({
        
        sum<-0
        count<-0
        equal2=(L$neighbourhood_cleansed==input$enter_neighbourhoodid)
        prices <- as.numeric(gsub('[$,]', '', L$price))
        for (j in 1:numberofrows){
            if (equal2[j]==TRUE){
                sum<-sum+prices[j]
                count<-count+1
            }
        }
        Neighbourhood_meanPrice<-sum/count   #solution
        
    })
    
    output$specs_meanPrice<-renderText({
        
        selecto<-L[L$property_type==input$enter_propertytype,]
        selecto<-selecto[selecto$room_type==input$enter_roomtype,]
        selecto<-selecto[selecto$accommodates==input$enter_accommodatestype,]
        selecto<-selecto[selecto$bathrooms==input$enter_bathroomstype,]
        selecto<-selecto[selecto$bedrooms==input$enter_bedroomstype,]
        
        prices1<- as.numeric(gsub('[$,]', '', selecto$price))
        prices1<-prices1[!is.na(prices1)]
        
        specs_meanPrice=sum(prices1)/length(prices1)
    })
    
    output$specs_maxPrice<-renderText({
        selecto<-L[L$property_type==input$enter_propertytype,]
        selecto<-selecto[selecto$room_type==input$enter_roomtype,]
        selecto<-selecto[selecto$accommodates==input$enter_accommodatestype,]
        selecto<-selecto[selecto$bathrooms==input$enter_bathroomstype,]
        selecto<-selecto[selecto$bedrooms==input$enter_bedroomstype,]
        
        prices1<- as.numeric(gsub('[$,]', '', selecto$price))
        prices1<-prices1[!is.na(prices1)]
        specs_maxPrice=max(unlist(prices1))})
    
    output$specs_minPrice<-renderText({
        selecto<-L[L$property_type==input$enter_propertytype,]
        selecto<-selecto[selecto$room_type==input$enter_roomtype,]
        selecto<-selecto[selecto$accommodates==input$enter_accommodatestype,]
        selecto<-selecto[selecto$bathrooms==input$enter_bathroomstype,]
        selecto<-selecto[selecto$bedrooms==input$enter_bedroomstype,]
        
        prices1<- as.numeric(gsub('[$,]', '', selecto$price))
        prices1<-prices1[!is.na(prices1)]
        specs_minPrice=min(unlist(prices1))})
    
    output$meanAv_30<-renderText({
        
        sum_30<-0
        countAv<-0
        equal4=(L$neighbourhood_cleansed==input$enter_neighbourhoodAv)
        for (j in 1:numberofrows){
            if (equal4[j]==TRUE){
                sum_30<-sum_30+L$availability_30[j]
                countAv<-countAv+1
            }
        }
        meanAv_30<-sum_30/countAv   #solution
        
    })
    
    output$meanAv_60<-renderText({
        
        sum_60<-0
        countAv<-0
        equal4=(L$neighbourhood_cleansed==input$enter_neighbourhoodAv)
        for (j in 1:numberofrows){
            if (equal4[j]==TRUE){
                sum_60<-sum_60+L$availability_60[j]
                countAv<-countAv+1
            }
        }
        meanAv_60<-sum_60/countAv   #solution
        
    })
    
    output$meanAv_90<-renderText({
        
        sum_90<-0
        countAv<-0
        equal4=(L$neighbourhood_cleansed==input$enter_neighbourhoodAv)
        for (j in 1:numberofrows){
            if (equal4[j]==TRUE){
                sum_90<-sum_90+L$availability_90[j]
                countAv<-countAv+1
            }
        }
        meanAv_90<-sum_90/countAv   #solution
        
    })
    
    output$meanAv_365<-renderText({
        
        sum_365<-0
        countAv<-0
        equal4=(L$neighbourhood_cleansed==input$enter_neighbourhoodAv)
        for (j in 1:numberofrows){
            if (equal4[j]==TRUE){
                sum_365<-sum_365+L$availability_365[j]
                countAv<-countAv+1
            }
        }
        meanAv_365<-sum_365/countAv   #solution
        
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
