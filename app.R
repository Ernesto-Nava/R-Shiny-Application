library(shiny)
library(shinyWidgets)
library(gplots)
library(data.table)
library(dplyr)
library(tidyr)
library(reshape)
library(ggthemes)
library(rpart)
library(rpart.plot)
library(leaps)
library(reshape2)
library(pivottabler)
library(plyr)
library(DT)
library(lubridate) 
library(plotly)
library(leaflet)
library(sf) 
library(tigris)
library(leaflet.extras)
library(readr)
library(esquisse)

# esquisse::esquisser()

housing_data <- read.csv("Housing_2020.csv")
housing_data[housing_data==""] <- NA

#Tab 1 Preparation: 
mylist <- as.list(housing_data$ZIP.OR.POSTAL.CODE)
housing_hist <- housing_data
housing_hist$PRICE <- log(housing_hist$PRICE)

#Tab 2 Preparation:
housing_medp <- subset(housing_data, select = c("PROPERTY.TYPE", "PRICE",
                                                "ZIP.OR.POSTAL.CODE"))

housing_medp2 <- dcast(housing_medp, ZIP.OR.POSTAL.CODE ~ PROPERTY.TYPE, 
                       value.var = ("PRICE"), fill=NaN, fun=median)

housing_medp3 <- housing_medp2[, c(1,6,7)]

rownames(housing_medp3) <- housing_medp3[,1] #Assigning row names from 1st column 

housing_medp3[,1] <- NULL

housing_medp3

housing_medp4 <- datatable(housing_medp3, options = list(
    order = list(list(1, 'desc'))
))

#Tab 3 Preparation: 
housing_box <- subset(housing_data, select = c("BEDS", "BATHS", "SQUARE.FEET", 
                                               "LOT.SIZE", "PRICE", "YEAR.BUILT", "CITY"))

#housing_box$YEAR.BUILT <- as.Date(as.character(housing_box$YEAR.BUILT), format = "%Y")
#housing_box$YEAR.BUILT <- year(housing_box$YEAR.BUILT)

#housing_box$YEAR.BUILT<-format(housing_box$YEAR.BUILT, format="%Y")

housing_box2 <- tibble::rowid_to_column(housing_box, "ID")

housing_box2$YEAR.BUILT <- as.numeric(housing_box2$YEAR.BUILT)

yearlist <- unique(housing_box2$YEAR.BUILT)
yearlist <- sort(yearlist)

#Tab 4 Preparation: 
housing_map <- subset(housing_data, select = c("PROPERTY.TYPE", "BEDS", "BATHS", 
                                               "SQUARE.FEET", "LOT.SIZE", "PRICE", "YEAR.BUILT", "ADDRESS", "CITY", 
                                               "STATE.OR.PROVINCE", "ZIP.OR.POSTAL.CODE", "LOCATION", "LATITUDE", "LONGITUDE"))

housing_map$YEAR.BUILT <- as.Date(as.character(housing_map$YEAR.BUILT), format = "%Y")
housing_map$YEAR.BUILT <- year(housing_map$YEAR.BUILT)

housing_map$YEAR.BUILT<-format(housing_map$YEAR.BUILT, format="%Y")

citylist <- unique(housing_map$CITY)
citylist <- sort(citylist)

#Define UI

ui <- fluidPage(
    
    titlePanel("Dallas Area Housing Price App"),
    
    tabsetPanel(
        #Tab 1:
        tabPanel("Distribution of Price by Zip Code", fluid = TRUE,
                 sidebarLayout(
                     
                     # Sidebar panel for Inputs ----
                     sidebarPanel(
                         
                         # Input: Select variable for y-axis ----
                         pickerInput(inputId = "zipcodes",
                                     
                                     HTML("Select Zip Code(s): <br/> 
                                      Note: All Zip Codes are Selected by Default."),
                                     
                                     choices = mylist, 
                                     
                                     options = list(`actions-box` = TRUE), multiple = T,
                                     selected = 75001:76262
                         ),
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                         # Output: Histrogram ----
                         plotOutput(outputId = "hist")
                     )
                 )
        ), 
        #Tab 2:
        tabPanel("Median Price by Zip Code and Property Type", fluid = TRUE,
                 fluidRow(
                     column(12,
                            dataTableOutput('table')
                     )
                 )
        ),
        #Tab 3: 
        tabPanel("Price Comparison by City", fluid = TRUE,
                 sidebarLayout(
                     
                     # Sidebar panel for Inputs ----
                     sidebarPanel(
                         
                         # # Input: Select variable for y-axis ----
                         # sliderInput(inputId = "beds",
                         #             
                         #             HTML("Select Number of Beds: <br/> 
                         #              Note: If an error message appears, the number of beds selected is not available. Please select a different number"),
                         #             
                         #             min = 0,
                         #             
                         #             max = 16, 
                         #             
                         #             value = 0, 
                         # ), 
                         # # BATHS
                         # sliderInput(inputId = "baths",
                         #             
                         #             HTML("Select Number of Baths: <br/> 
                         #              Note: If an error message appears, the number of baths selected is not available. Please select a different number."),
                         #             
                         #             min = 0,
                         #             
                         #             max = 10, 
                         #             
                         #             value = 0, 
                         #             
                         # ), 
                         
                         pickerInput(inputId = "City2",
                                     
                                     HTML("Select a City or Cities: "),
                                     
                                     choices = citylist, 
                                     
                                     options = list(`actions-box` = TRUE), multiple = T, 
                                     
                         ),
                         
                         # sliderInput(inputId = "Year",
                         # 
                         #       HTML("Select Year: <br/>
                         #       Note: If an error message appears, the Year selected is not available. Please select a different year."),
                         # 
                         #       min = 1868,
                         # 
                         #       max = 2021,
                         # 
                         #       value = 1868,
                         #       
                         #       sep = "", 
                         # 
                         #             ),
                         
                     ),
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                         # Output: Boxplot ----
                         plotOutput(outputId = "newboxplot")
                     )
                 )
        ),
        
        #Tab 4: 
        tabPanel("Dallas Area Map", fluid = TRUE,
                 sidebarLayout(
                     
                     # Sidebar panel for Inputs ----
                     sidebarPanel(
                         
                         # Input: Select variable for y-axis ----
                         pickerInput(inputId = "City",
                                     
                                     HTML("Select a City or Cities: "),
                                     
                                     choices = citylist, 
                                     
                                     options = list(`actions-box` = TRUE), multiple = T, 
                                     selected = "Dallas"
                         ),
                     ),
                 
                 
                 mainPanel(
                     leafletOutput(outputId = "map"), 
                 )
             )
        )
        
    )
)


# Define server logic 
server <- function(input, output) {
    
    # Create Historgram 
    output$hist <- renderPlot({
        
        validate(
            need(input$zipcodes != "", "Please select a zip code.")
        )
        
        histFilter <- subset(housing_hist$PRICE, 
                             housing_hist$ZIP.OR.POSTAL.CODE == input$zipcodes)
        
        hist(histFilter,
             xlab = "Distribution of House Prices",
             main = "Distribution of House Prices by Zip Code",
             col = "yellow")
    })
    
    # Create Table 
    output$table <- renderTable({
        output$table <- renderDataTable(housing_medp4)
    })
    
    #Create Boxplot 
    output$newboxplot <- renderPlot({
        
        # boxFilter <- subset(housing_box2$PRICE, housing_box2$BEDS == input$beds,
        #                     housing_box2$BATHS == input$baths,
        #                     housing_box2$YEAR.BUILT == input$year
        #                     #housing_box2$YEAR.BUILT == input$beds,
        #                     #housing_box2$SQUARE.FEET == input$beds,
        #                     )
       
        # boxplot(log(PRICE) ~ (YEAR.BUILT==input$year), 
        #         housing_box2, xlab="Beds", ylab="Median House Price", 
        #         main="Box Plot of Median House Price Based on Number of Beds")
        # 
        # housing_box2 %>%
        #     req(YEAR.BUILT == input$year) %>%
        #     ggplot(housing_box2) +
        #     aes(x = YEAR.BUILT, y = PRICE) +
        #     geom_boxplot(fill = "#0c4c8a") +
        #     theme_minimal()
        
        validate(
            need(input$City2 != "", "Please select a city.")
        )

        housing_box2 %>%
            filter(CITY == input$City2) %>%
            ggplot() +
            aes(x = CITY, y = log(PRICE)) +
            geom_boxplot(fill = "#9ecae1") +
            labs(y = "Price") + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    #Create Map
    output$map <- renderLeaflet ({
        
        validate(
            need(input$City != "", "Please select a city.")
        )
        
        leaflet(housing_map) %>% 
            setView(lng = -96.8351, lat = 32.8025, zoom =9) %>%
            addTiles() %>%
            addAwesomeMarkers(data = housing_map %>% filter(CITY == input$City), 
                              lat = ~ LATITUDE, lng = ~ LONGITUDE, 
                              popup = ~as.character(paste("Price: ", PRICE, "<br>",
                                                          "Year: ", YEAR.BUILT, "<br>", 
                                                          "Beds: ", BEDS))
            )
        
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
