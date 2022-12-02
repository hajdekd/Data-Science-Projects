library(shiny)
library(shinythemes)
library(gt)
library(plotly)
library(tidyverse)
library(dplyr)

#Reading CSV data sets
Energy.Consumption <- read.csv("Energy.Consumption.csv")

#Reading CSV data sets
GDP.and.Population <- read.csv("GDP.and.Population.csv")


#Filtering the data sets
Energy.Consumption.red <- Energy.Consumption %>% filter(Year >= 1995 & Year <= 2015)
GDP.and.Population.red <- GDP.and.Population %>% filter(Year >= 1995 & Year <= 2015)

#Merging two tables to obtain needed data set
En.Consum.Pop.GDP.joined <-merge(Energy.Consumption.red,GDP.and.Population.red, 
                                            by.x = c("Year","Country"), by.y=c("Year","Country"))

#Creating a new dat set
En.Consum.Pop.GDP.New <- En.Consum.Pop.GDP.joined %>%
  group_by(Year, Country)%>%
  summarize(Amount = mean(Amount, na.rm=T),
            Real.GDP = mean(Real.GDP, na.rm=T),
            Population = mean(Population, na.rm=T),
            .groups = 'drop')%>%
  mutate(Energy.Population = Amount / Population,
         Energy.GDP = Amount / Real.GDP)



###############################################
###############################################
####
####  UI
####
###############################################
###############################################
ui <- fluidPage(
  theme=shinytheme("sandstone"),
  titlePanel(html("<h1><b>Energy Consumption <br/>
                 and Influencing Factors</b>")),
  sidebarLayout(
    sidebarPanel(
      tags$img(src='Energy_Consumption.jpg', height = 350, width = 510),br(),br(),br(),
      tags$h5("Data Used in Visalizations:"), #add a link to a website
      tags$a("Energy Datasets",href="https://www.theshiftdataportal.org/energy"),br(),
      tags$a("World Energy Consumption",href="https://www.kaggle.com/datasets/pralabhpoudel/world-energy-consumption?select=World+Energy+Consumption.csv"),br(),
      tags$a("Historical GDP by Country",href="https://knoema.com/mhrzolg/historical-gdp-by-country-statistics-from-the-world-bank-1960-2019"),br(),
      tags$a("Energy consumption increasing at twice the average rate",href="https://www.energymagazine.com.au/energy-consumption-increasing-at-twice-the-average-rate/"),br(),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Global",
                 h3("Global Energy Consumption"),
                 h5("Graph Represents Global Consumption of Oil, Gas, Coal, and Renewable Energies from 1995 to 2015"),br(),
                 plotlyOutput("plot1", height = "450px"),
                 h6("Mtoe = Million Tonnes of Oil Equivalent")),
        tabPanel("By Country",
                 h3("Energy Consumption of 5 Countries with Highest GDP"),
                 h5("Graph Represents Energy Consumption by 5 Countries With Highest GDP from 1995 to 2015"),br(),
                 plotlyOutput("plot2",height = "450px"),br(),
                 h6("Mtoe = Million Tonnes of Oil Equivalent")),
        tabPanel("Per GDP",
                 h3("Energy Consumption Per GDP"),
                 h5("The Visualization Represents Total Energy Consumption per GDP from 1995 to 2015"),br(),
                 plotlyOutput("plot3"),br(),
                 h6("Inflation adjusted GDP is used to provide more accurate representation")),
        tabPanel("Per Capita",
                 h3("Energy Consumption Per Capita"),
                 h5("The Visualization Represents Total Energy Consumption per Capita from 1995 to 2015"),br(),
                 plotlyOutput("plot4"))
      )
    )
  )
)
###############################################
###############################################
####
####  SERVER
####
###############################################
server <- function(input,output,session) {

  #graph 1
  output$plot1 <- renderPlotly({
    p1 <- Energy.Consumption.red %>% filter(Country == "World")%>%
      plot_ly(x = ~ Year, y = ~ Amount, color=~ Type,stroke = I("black"), colors = "Spectral") %>%
      add_bars(y = ~ Amount/1000)%>%
     layout(yaxis = list(title = "Mtoe",ticksuffix= "K"))%>%
       hide_colorbar()
    
  })
  
  #graph 2
  output$plot2 <- renderPlotly({
    p2 <- Energy.Consumption.red %>% filter(Country != 'World')%>%
      plot_ly(x = ~ Year, y = ~ Amount, color=~ Country,text = ~ Type,stroke = I("black"), colors = "Spectral") %>%
      add_bars(y = ~ Amount/1000)%>%
      layout ( yaxis = list(title = "Mtoe",ticksuffix= "K"))%>%
      hide_colorbar()
    })
  
  #graph 3
  output$plot3 <- renderPlotly({
    p3 <- En.Consum.Pop.GDP.New %>%
      plot_ly(x = ~ Year, y = ~ Energy.GDP, colors = "Spectral") %>%
      add_lines(color=~ Country, y = ~ Energy.GDP * 1000000000)%>%
      layout(yaxis = list(title = "milli toe"))%>%
      hide_colorbar()
    
  })
  
  #graph 4
  output$plot4 <- renderPlotly({
    p4 <- En.Consum.Pop.GDP.New %>%
      plot_ly(x = ~ Year, y = ~ Energy.Population, colors = "Spectral") %>%
      add_lines(color=~ Country,y = ~ Energy.Population * 1000000)%>%
      layout( yaxis = list(title = "toe"))%>%
      hide_colorbar()
  })
  
  

  
  

}
shinyApp(ui,server)




