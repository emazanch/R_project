## AirBnB Paris interactive mapping tool
## R4BD project
## by Ekaterina Mazanchenko

# load required packages
if(!require(shiny)) install.packages('shiny', repos = 'http://cran.us.r-project.org')
if(!require(leaflet)) install.packages('leaflet', repos = 'http://cran.us.r-project.org')
if(!require(ggplot2)) install.packages('ggplot2', repos = 'http://cran.us.r-project.org')
if(!require(dplyr)) install.packages('dplyr', repos = 'http://cran.us.r-project.org')


# load data
load(url('https://plmbox.math.cnrs.fr/f/6b0e16bbf4974b78ba01/?dl=1'))



### DATA PROCESSING ###

# df reducing:
L_filtered <- select(L, id, host_id, host_name, name, listing_url, street, neighbourhood_cleansed, 
                     zipcode, latitude, longitude, property_type, room_type, accommodates, 
                     bedrooms, bathrooms, beds, bed_type, price, weekly_price, guests_included, 
                     availability_30, review_scores_rating, cancellation_policy)

# Columns format ordered:
L_filtered$bathrooms <- as.integer(L_filtered$bathrooms)
L_filtered$price <- as.numeric(gsub('\\$|,', '', L_filtered$price))
L_filtered$weekly_price <- as.numeric(gsub('\\$|,', '', L_filtered$weekly_price))
L_filtered$zipcode <- as.numeric(as.character(L_filtered$zipcode))

# Creating and cleaning 'arrondissements':
arr <- {}
for (i in 1:length(L_filtered$zipcode)){
  if (!is.na(L_filtered$zipcode[i])){
    if ((L_filtered$zipcode[i] > 75000) & (L_filtered$zipcode[i] <= 75120)){
      arr[i] <- L_filtered$zipcode[i] - round(L_filtered$zipcode[i], -2)
    } else if ((L_filtered$zipcode[i] > 90000) & (L_filtered$zipcode[i] <= 99000)){
      arr[i] <- L_filtered$zipcode[i]%/%1000
    }   
  }
}

L_filtered <- cbind(L_filtered,arr)
L_filtered <- L_filtered[,c(1:8,24,9:23)]
data <- L_filtered

# For plots:
no_prop <- L_filtered %>% count(host_id)

R$date <- as.Date(R$date, format = "%Y-%m-%d")
R %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  count(year, month)

L_join <- L_filtered %>% select(id, arr)
R_join <- R
colnames(R_join) <- c('id', 'date')
joined <- inner_join(L_join, R_join, by = 'id')
joined <- mutate(joined, month = format(date, '%m'), year = format(date, '%Y'))



### SHINY UI ###
library(shiny)

ui <- fluidPage(
  titlePanel('AirBnB Paris'),
  sidebarLayout(
    sidebarPanel(
      
      # Price:
      sliderInput('price', label = 'Price range:',        #Finalement 6000 max!
                  value = c(100,200), 0, 1000, step = 5),
      
      # Apartment type:
      selectInput('ap_type',label = 'Choose apartment type:',
                  choices = unique(data$property_type), 
                  selected = 'Apartment'),
      
      # Room type:
      checkboxGroupInput('room_type', label = 'Choose a room:',
                         choices = unique(data$room_type), 
                         selected = ''),
      
      # Accommodates:
      sliderInput('acc',label = 'No of accommodates:',
                  value = c(2,4), 
                  min = min(data$accommodates), 
                  max = max(data$accommodates),
                  step = 1), 
      
      # Bed type:
      selectInput('bed_type', label = 'Choose a bed:',
                  choices = unique(data$bed_type), selected = ''),

      # Arrondissement:
      checkboxGroupInput('arr', label = 'Choose a district:',
                         choices = c(1:20, 91:95),
                         selected = '',
                         inline = TRUE),

      # Cancellation policy:
      checkboxGroupInput('cancel', label = 'Cancellation policy:',
                         choices = unique(data$cancellation_policy),
                         selected = 'flexible')
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Map', leafletOutput('map', width = '600', height = '600')),
        tabPanel('Plots', plotOutput('hist_ownership'),
                 plotOutput('price'),
                 plotOutput('frequency'))
                 
      )
    )  
  )
)


### SHINY SERVER ###
library(ggplot2)
library(dplyr)
library(leaflet)

server <- function(input, output, session) {
  
  map_data_react <- reactive({
    
    df_map <- filter(data,
                     price >= input$price[1] & price <= input$price[2],
                     property_type %in% input$ap_type,
                     room_type %in% input$room_type,
                     accommodates > input$acc[1] & accommodates <= input$acc[2],
                     bed_type %in% input$bed_type,
                     arr %in% input$arr,
                     cancellation_policy %in% input$cancel)
  })
  
  observe({
    map_data <- map_data_react()
    proxy <- leafletProxy('map', data = map_data)
    proxy %>% clearMarkers()
    if (nrow(data)!=0) {
      proxy %>% addMarkers(lng = ~longitude, lat = ~latitude,
                           label = ~paste(as.character(price),"$"),
                           popup = ~paste(name, "<br>",
                                          listing_url, "<br>",
                                          "Host:", host_name, ",", "score =", review_scores_rating, "<br>",     #Replace NA in score by empty 
                                          "Guests included:", guests_included, "<br>",
                                          "Bedrooms:", bedrooms, "<br>",
                                          "Beds:", beds, "<br>",
                                          "Bathrooms:", bathrooms))
      
    }
  })
  
  # Map:
  output$map <- renderLeaflet({
    
    leaflet(map_data) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(2.3488, 48.8534, zoom = 12) %>%
      addTiles()
  })


  # Plots:
  # Number of apartments per owner:   
  output$hist_ownership <- renderPlot({
    ggplot(data = no_prop) + 
      geom_point(aes(x = host_id, y = n)) +
      ggtitle('Number of apartments by owner') + 
      xlab('Hosts id') + 
      ylab('Number of apartments')
  })    
  
  # Price vs features:   
  output$price <- renderPlot({
    ggplot(data = data) + 
      geom_point(aes(x=accommodates, y=price, col=factor(arr))) + 
      scale_x_continuous(breaks = seq(1,16,1)) + 
      scale_y_continuous(breaks = seq(0,1000,100), limits = c(0,1000)) + 
      ggtitle('Apartment price by accommodates') + 
      xlab('Number of guests') + 
      ylab('Price by night, $')
  })    
  
  # Visit frequency by time:  
  output$frequency <- renderPlot({
    ggplot(data=R) + 
      geom_histogram(aes(x=date, y= ..density..), bins = 87, fill='pink') +
      geom_density(aes(x=date), col='darkgreen') + 
      ggtitle('Visits frequency') +
      scale_x_date(date_breaks = '1 year')
  })
}

shinyApp(ui, server)