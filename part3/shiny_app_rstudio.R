
# install.packages(c('ggmap','maptools','fiftystater','usmap','maps'))
install.packages('shiny')
library(ggplot2)
library(ggmap)
library(maps)
library(usmap)
library(fiftystater)
library('ggplot2')
library(shiny)

setwd("C:/Users/disha/Box/Lab 1")

fever_only_data<- read.csv('fever_only.csv')
flu_only_data<- read.csv('flu_only.csv')
all_data<- read.csv('flu_activity.csv')
cdc_data <- read.csv('cdc_data_final.csv')
us <- map_data('state')
data('fifty_states')




ui <- fluidPage(
  titlePanel("Flu Data"),
  tabsetPanel(
    tabPanel("All tweets data vs CDC data",
             fluidRow(column(6,plotOutput('map1')),
                      column(6,plotOutput('map4'))
             )
    ),
    tabPanel("Flu tweets data vs CDC data",
             fluidRow(column(6,plotOutput('map2')),
                      column(6,plotOutput('map5'))
                      )),
    tabPanel("Fever tweets data vs CDC data",
             fluidRow(column(6,plotOutput('map3')),
                      column(6,plotOutput('map6'))
             ))
    ))

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  output$map1 <- renderPlot({
    ggplot(all_data) +
      geom_map(aes(map_id=all_data$state_name, fill = all_data$sum), map = fifty_states, color="grey24", size=.2) +
      scale_fill_gradient('ILI Activity Level', low='green', high='red3') +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      expand_limits(x= fifty_states$long, y = fifty_states$lat) +
      ggtitle("Influenza Tweets By Different States") +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title=element_text(hjust=0.5, face='bold', size=8, family='sans')) +
      labs(x=NULL, y=NULL) 
      
  })
  
  output$map2<-renderPlot({
    ggplot(flu_only_data) +
      geom_map(aes(map_id=flu_only_data$state_name, fill = flu_only_data$sum), map = fifty_states, color="grey24", size=.2) +
      scale_fill_gradient('ILI Activity Level', low='green', high='red3') +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      expand_limits(x= fifty_states$long, y = fifty_states$lat) +
      ggtitle("Flu only Tweets By Different States") +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title=element_text(hjust=0.5, vjust = 0.3, face='bold', size=8, family='sans')) +
      labs(x=NULL, y=NULL) 
  })
  
  output$map3 <- renderPlot({
    ggplot(fever_only_data) +
      geom_map(aes(map_id=fever_only_data$state_name, fill = fever_only_data$sum), map = fifty_states, color="grey24", size=.2) +
      scale_fill_gradient('ILI Activity Level', low='green', high='red3') +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      expand_limits(x= fifty_states$long, y = fifty_states$lat) +
      ggtitle("Fever only Tweets By Different States") +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title=element_text(hjust=0.5, vjust = 0.3, face='bold', size=8, family='sans')) +
      labs(x=NULL, y=NULL) 
  })
  
  output$map4 <- renderPlot({
    ggplot(cdc_data) +
      geom_map(data=us, map=fifty_states, aes(map_id=region), size=0.35) +
      geom_map(map=fifty_states, aes(fill=(cdc_data$level), map_id=cdc_data$state), color='black', size=0.25) +
      scale_fill_gradient('ILI Activity Level', low='green', high='#cc0000', breaks=c(3.33, 6.66, 10), labels=c('Low', 'Medium', 'High')) +
      # scale_fill_manual(names = c('Low', 'Medium', 'High')) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      expand_limits(x= fifty_states$long, y = fifty_states$lat) +
      ggtitle('2018-19 Influenza Season Week 4 ending Jan 26, 2019') +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title=element_text(hjust=0.5, face='bold', size=8, family='Courier')) +
      labs(x=NULL, y=NULL) 
  })
  
  output$map5 <- renderPlot({
    ggplot(cdc_data) +
      geom_map(data=us, map=fifty_states, aes(map_id=region), size=0.35) +
      geom_map(map=fifty_states, aes(fill=(cdc_data$level), map_id=cdc_data$state), color='black', size=0.25) +
      scale_fill_gradient('ILI Activity Level', low='green', high='#cc0000', breaks=c(3.33, 6.66, 10), labels=c('Low', 'Medium', 'High')) +
      # scale_fill_manual(names = c('Low', 'Medium', 'High')) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      expand_limits(x= fifty_states$long, y = fifty_states$lat) +
      ggtitle('2018-19 Influenza Season Week 4 ending Jan 26, 2019') +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title=element_text(hjust=0.5, face='bold', size=8, family='Courier')) +
      labs(x=NULL, y=NULL) 
  })
  
  output$map6 <- renderPlot({
    ggplot(cdc_data) +
      geom_map(data=us, map=fifty_states, aes(map_id=region), size=0.35) +
      geom_map(map=fifty_states, aes(fill=(cdc_data$level), map_id=cdc_data$state), color='black', size=0.25) +
      scale_fill_gradient('ILI Activity Level', low='green', high='#cc0000', breaks=c(3.33, 6.66, 10), labels=c('Low', 'Medium', 'High')) +
      # scale_fill_manual(names = c('Low', 'Medium', 'High')) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      expand_limits(x= fifty_states$long, y = fifty_states$lat) +
      ggtitle('2018-19 Influenza Season Week 4 ending Jan 26, 2019') +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title=element_text(hjust=0.5, face='bold', size=8, family='Courier')) +
      labs(x=NULL, y=NULL) 
  })
  

}


shinyApp(ui, server)


