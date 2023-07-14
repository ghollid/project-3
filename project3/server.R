library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

setwd("C:/Users/Sarah/Documents/project-3")
# read in dataset
GradData <- read.csv(file="data.csv",sep=';')

# Define server logic 
function(input, output, session) {
## Code for image in About tab
  output$image1 <- renderImage({
    width<- "80%"
    height<- "20%"
    list(src = "www/cu-grad.jpg",
         width = width,
         height = "auto",
         align = 'center'
    )
  }, deleteFile = FALSE)
  
## Numerical and graphical summaries tab
  ## Boxplots
  output$plot <- renderPlot({
    if(input$summary=='Boxplots'){
      var <- input$vars
      Target <- "Target"
    ggplot(GradData, aes_string(x=Target, y=var)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)}
    
  
})
  output$table <- renderTable({
    if(input$summary=='Contingency Table'){
      if(input$vars2=='Marital.status'){
        table(GradData$Marital.status,GradData$Target)
      }
      else if(input$vars2=='Application.order'){
        table(GradData$Application.order,GradData$Target)
      }
      else if(input$vars2=='International'){
        table(GradData$International,GradData$Target)
      }
      else if(input$vars2=='Educational.special.needs'){
        table(GradData$Educational.special.needs, GradData$Target)
      }
      else if(input$vars2=='Tuition.fees.up.to.date'){
        table(GradData$Tuition.fees.up.to.date,GradData$Target)
      }
      else if(input$vars2=='Gender'){
        table(GradData$Gender,GradData$Target)
      }
      else if(input$vars2=='Scholarship.holder'){
        table(GradData$Scholarship.holder,GradData$Target)
      }
    }
  })
  
}
