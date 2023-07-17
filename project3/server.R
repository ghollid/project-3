library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

setwd("C:/Users/Sarah/Documents/project-3")
# read in dataset
GradData <- read.csv(file="data.csv",sep=';')
# creating a version of this data with variables that make sense
GradDataV2 <- GradData
for(i in 1:nrow(GradData)){
  # marital.status
  if(GradData[i,1]==1){
    GradDataV2[i,1]<-"Single"
  }
  else if(GradData[i,1]==2){
    GradDataV2[i,1] <- "Married"
  }
  else if(GradData[i,1]==3){
    GradDataV2[i,1] <- "Widower"
  }
  else if(GradData[i,1]==4){
    GradDataV2[i,1] <- "Divorced"
  }
  else if(GradData[i,1]==5){
    GradDataV2[i,1] <- "Facto Union"
  }
  else if(GradData[i,1]==6){
    GradDataV2[i,1] <- "Legally Separated"
  }
  # Daytime/Evening Attendance
  if(GradData[i,5]==1){
    GradDataV2[i,5]<- "Daytime"
  }
  else{GradDataV2[i,5] <- "Evening"}
  # International
  if(GradData[i,21]==1){
    GradDataV2[i,21] <- 'Yes'
  }
  else{GradDataV2[i,21]<- 'No'}
  # Educational Special Needs
  if(GradData[i,15]==1){
    GradDataV2[i,15] <- 'Yes'
  }
  else{GradDataV2[i,15] <- 'No'}
  # Tuition and Fees up to date
  if(GradData[i,17]==1){
    GradDataV2[i,17] <- 'Yes'
  }
  else{GradDataV2[i,17] <- 'No'}
  # Gender
  if(GradData[i,18]==1){
    GradDataV2[i,18] <- 'Male'
  }
  else{GradDataV2[i,18] <- 'Female'}
  # Scholarship holder
  if(GradData[i,19]==1){
    GradDataV2[i,19]<- 'Yes'
  }
  else{GradDataV2[i,19]<-'No'}
}

## removing enrolled students
GradDataV2 <- subset(GradDataV2, Target!='Enrolled')

## Target as a factor
GradDataV2$Target <- as.factor(GradDataV2$Target)


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
    ggplot(GradDataV2, aes_string(x=Target, y=var)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)}
    
  
})
  # contingency tables output
  output$table <- renderTable({
    if(input$summary=='Contingency Tables'){
      if(input$vars2=='Marital.status'){
        table(GradDataV2$Marital.status,GradDataV2$Target)
      }
      else if(input$vars2=='Application.order'){
        table(GradDataV2$Application.order,GradDataV2$Target)
      }
      else if(input$vars2=='International'){
        table(GradDataV2$International,GradDataV2$Target)
      }
      else if(input$vars2=='Educational.special.needs'){
        table(GradDataV2$Educational.special.needs, GradDataV2$Target)
      }
      else if(input$vars2=='Tuition.fees.up.to.date'){
        table(GradDataV2$Tuition.fees.up.to.date,GradDataV2$Target)
      }
      else if(input$vars2=='Gender'){
        table(GradDataV2$Gender,GradDataV2$Target)
      }
      else if(input$vars2=='Scholarship.holder'){
        table(GradDataV2$Scholarship.holder,GradDataV2$Target)
      }
      else if(input$vars2=='Daytime.evening.attendance.'){
        table(GradDataV2$Daytime.evening.attendance.,GradDataV2$Target)
      }
    }
  })
  # mosaic plots output
  output$plot2 <- renderPlot({
      if(input$vars3=='International'){
        International <- table(GradDataV2$International,GradDataV2$Target)
        mosaicplot(International)
      }
      else if(input$vars3=='Educational.special.needs'){
        EducationalSpecialNeeds <- table(GradDataV2$Educational.special.needs, GradDataV2$Target)
        mosaicplot(EducationalSpecialNeeds)
      }
      else if(input$vars3=='Tuition.fees.up.to.date'){
        TuitionUpToDate <- table(GradDataV2$Tuition.fees.up.to.date,GradDataV2$Target)
        mosaicplot(TuitionUpToDate)
      }
      else if(input$vars3=='Gender'){
        Gender <- table(GradDataV2$Gender,GradDataV2$Target)
        mosaicplot(Gender)
      }
      else if(input$vars3=='Scholarship.holder'){
        ScholarshipHolder <- table(GradDataV2$Scholarship.holder,GradDataV2$Target)
        mosaicplot(ScholarshipHolder)
      }
      else if(input$vars3=='Daytime.evening.attendance.'){
        AttendanceTime <- table(GradDataV2$Daytime.evening.attendance.,GradDataV2$Target)
        mosaicplot(AttendanceTime)
      }
  })
  # main panel stuff
  # generalized linear model 
  # first splitting data between training and test set
  # Divide data into training and test sets
  # needs to be updated when the button clicks 
  

  observeEvent(input$submit,{
  output$modfit1 <-renderTable({
      
  train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
  test <- setdiff(1:nrow(GradDataV2), train)
  
  # training and testing subsets
  datTrain1 <- GradDataV2[train, ]
  datTest1 <- GradDataV2[test, ]
  
  if(!is.null(input$varsm)){
        vars <- input$varsm
        vars2 <- str_c(vars,collapse='+')
        formula <- paste('Target ~',vars2)
        form2 <- as.formula(formula)
        mrm <- train(form2,
                     data=datTrain1,
                     method="glm",
                     preProcess = c("center", "scale"), 
                     trControl = trainControl(method = "cv", number = 5))
  
   Results <- as.data.frame(mrm$results)
   Results <- Results[,-1]
   }
  })
})
  
  ## predicting and RMSE
  observeEvent(input$submit,{
  output$modfit12 <-
    renderTable({
    train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
    test <- setdiff(1:nrow(GradDataV2), train)
    
    # training and testing subsets
    datTrain1 <- GradDataV2[train, ]
    datTest1 <- GradDataV2[test, ]
    
    if(!is.null(input$varsm)){
      
      vars <- input$varsm
      vars2 <- str_c(vars,collapse='+')
      formula <- paste('Target ~',vars2)
      form2 <- as.formula(formula)
      mrm <- train(form2,
                   data=datTrain1,
                   method="glm",
                   preProcess = c("center", "scale"), 
                   trControl = trainControl(method = "cv", number = 5))
    #Run on test data
    GLM.predict2 <- predict(mrm, newdata = datTest1)
    
    #Obtain RMSE from test set, which will be used to compare to other models later 
    # but printed out for now 
    GLM.compare2 <- postResample(GLM.predict2, obs=datTest1$Target)
    RMSE <- as.data.frame(GLM.compare2)
    RMSE2 <- RMSE          
    RMSE2<- tibble::rownames_to_column(RMSE2, "row_names")
    colnames(RMSE2)[1] <- ''
    RMSE2
    }
    })
  })
  
  ## classification tree - boosting
  observeEvent(input$submit,{
  output$bfitplot <-
    renderPlot({
    train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
    test <- setdiff(1:nrow(GradDataV2), train)
    
    # training and testing subsets
    datTrain1 <- GradDataV2[train, ]
    datTest1 <- GradDataV2[test, ]
    ## will make a plot and a table here
    if(!is.null(input$varsm)){
    vars <- input$varsm
    vars2 <- str_c(vars,collapse='+')
    formula <- paste('Target ~',vars2)
    form2 <- as.formula(formula)
  boostFit <- train(form2, data=datTrain1, method="gbm",
                    preProcess=c("center","scale"),
                    trControl=trainControl(method='cv',number=5),
                    tuneGrid = expand.grid(n.trees=seq(25,200,50),
                                           interaction.depth=seq(1,4,1),
                                           shrinkage=0.1,
                                           n.minobsinnode=10))
  
  # plot fit
  plot(boostFit) }
  })
  })
  
  ## run bfit model on test data
  observeEvent(input$submit,{
  output$bfitresults <-
    renderTable({
    train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
    test <- setdiff(1:nrow(GradDataV2), train)
    
    # training and testing subsets
    datTrain1 <- GradDataV2[train, ]
    datTest1 <- GradDataV2[test, ]
    
    if(!is.null(input$varsm)){
      vars <- input$varsm
      vars2 <- str_c(vars,collapse='+')
      formula <- paste('Target ~',vars2)
      form2 <- as.formula(formula)
      boostFit <- train(form2, data=datTrain1, method="gbm",
                        preProcess=c("center","scale"),
                        trControl=trainControl(method='cv',number=5),
                        tuneGrid = expand.grid(n.trees=seq(25,200,50),
                                               interaction.depth=seq(1,4,1),
                                               shrinkage=0.1,
                                               n.minobsinnode=10))
  #Run on test data
  boostFit.predict <- predict(boostFit, newdata = datTest1)
  
  #Obtain RMSE from test set, which will be used in automated comparison
  boostFit.compare <- postResample(boostFit.predict, obs = datTest1$Target)
  RMSEf <- as.data.frame(boostFit.compare)
  RMSE2f <- RMSEf          
  RMSE2f<- tibble::rownames_to_column(RMSE2f, "row_names")
  colnames(RMSE2f)[1] <- ''
  RMSE2f}
  })
  
  ## random forest output
  #Create model
  observeEvent(input$submit,{
  output$rforestp <- 
    renderPlot({
    train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
    test <- setdiff(1:nrow(GradDataV2), train)
    
    # training and testing subsets
    datTrain1 <- GradDataV2[train, ]
    datTest1 <- GradDataV2[test, ]
    if(!is.null(input$varsm)){
      vars <- input$varsm
      vars2 <- str_c(vars,collapse='+')
      formula <- paste('Target ~',vars2)
      form2 <- as.formula(formula)
  random.forest.fit <- train(form2, data = datTrain1,
                             method = "rf",
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "cv", number = 5),
                             tuneGrid = data.frame(mtry = 1:27))
  
  # plot fit
  plot(random.forest.fit)
  }
  })
  })
  
  observeEvent(input$submit,{
  output$rforestt <- 
    renderTable({
    train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
    test <- setdiff(1:nrow(GradDataV2), train)
    
    # training and testing subsets
    datTrain1 <- GradDataV2[train, ]
    datTest1 <- GradDataV2[test, ]
    
    if(!is.null(input$varsm)){
      vars <- input$varsm
      vars2 <- str_c(vars,collapse='+')
      formula <- paste('Target ~',vars2)
      form2 <- as.formula(formula)
      random.forest.fit <- train(form2, data = datTrain1,
                                 method = "rf",
                                 preProcess = c("center", "scale"),
                                 trControl = trainControl(method = "cv", number = 5),
                                 tuneGrid = data.frame(mtry = 1:27))
      
  #Run on test data
  random.forest.predict <- predict(random.forest.fit, newdata = datTest1)
  
  #Obtain RMSE from test set, which will be used in automated comparison
  random.forest.compare <- postResample(random.forest.predict, 
                                        obs = datTest1$Target)
  RMSErff <- as.data.frame(random.forest.compare)
  RMSErf2f <- RMSErff          
  RMSErf2f<- tibble::rownames_to_column(RMSErf2f, "row_names")
  colnames(RMSErf2f)[1] <- ''
  RMSErf2f
  }
  })
  })
})
}
