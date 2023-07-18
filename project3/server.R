library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

set.seed(1234)

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
  # displaced
  if(GradData[i,14]==1){
    GradDataV2[i,14] <- 'Yes'
  }
  else{GradDataV2[i,14] <- 'No'}
  # debtor
  if(GradData[i,16]==1){
    GradDataV2[i,16] <- 'Yes'
  }
  else{GradDataV2[i,16]<-'No'}
}

## removing enrolled students
GradDataV2 <- subset(GradDataV2, Target!='Enrolled')

## Target as a factor
GradDataV2$Target <- as.factor(GradDataV2$Target)

## 'class' variables as factors and not numeric
GradDataV2$Course <- as.factor(GradDataV2$Course)
GradDataV2$Application.mode <- as.factor(GradDataV2$Application.mode)
GradDataV2$Application.order <- as.factor(GradDataV2$Application.order)
GradDataV2$Previous.qualification <- as.factor(GradDataV2$Previous.qualification)
GradDataV2$Nacionality<- as.factor(GradDataV2$Nacionality)
GradDataV2$Mother.s.qualification <- as.factor(GradDataV2$Mother.s.qualification)
GradDataV2$Mother.s.occupation <- as.factor(GradDataV2$Mother.s.occupation)
GradDataV2$Father.s.qualification <- as.factor(GradDataV2$Father.s.qualification)
GradDataV2$Father.s.occupation<- as.factor(GradDataV2$Father.s.occupation)
GradDataV2$Displaced <- as.factor(GradDataV2$Displaced)
GradDataV2$Debtor <- as.factor(GradDataV2$Debtor)

## numeric variables as numeric and not character
GradDataV2$Previous.qualification..grade. <- as.numeric(GradDataV2$Previous.qualification..grade.)
GradDataV2$Admission.grade <- as.numeric(GradDataV2$Admission.grade)
GradDataV2$Age.at.enrollment <- as.numeric(GradDataV2$Age.at.enrollment)
GradDataV2$Curricular.units.1st.sem..credited. <- as.numeric(GradDataV2$Curricular.units.1st.sem..credited.)
GradDataV2$Curricular.units.1st.sem..enrolled. <- as.numeric(GradDataV2$Curricular.units.1st.sem..enrolled.)
GradDataV2$Curricular.units.1st.sem..evaluations.<- as.numeric(GradDataV2$Curricular.units.1st.sem..evaluations.)
GradDataV2$Curricular.units.1st.sem..grade.<- as.numeric(GradDataV2$Curricular.units.1st.sem..grade.)
GradDataV2$Curricular.units.1st.sem..without.evaluations. <- as.numeric(GradDataV2$Curricular.units.1st.sem..without.evaluations.)
GradDataV2$Curricular.units.2nd.sem..credited.<- as.numeric(GradDataV2$Curricular.units.2nd.sem..credited.)
GradDataV2$Curricular.units.2nd.sem..enrolled. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..enrolled.)
GradDataV2$Curricular.units.2nd.sem..evaluations.<- as.numeric(GradDataV2$Curricular.units.2nd.sem..evaluations.)
GradDataV2$Curricular.units.2nd.sem..approved. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..approved.)
GradDataV2$Curricular.units.2nd.sem..grade. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..grade.)
GradDataV2$Curricular.units.2nd.sem..without.evaluations. <- as.numeric(GradDataV2$Curricular.units.2nd.sem..without.evaluations.)
GradDataV2$Unemployment.rate <- as.numeric(GradDataV2$Unemployment.rate)
GradDataV2$Inflation.rate <- as.numeric(GradDataV2$Inflation.rate)
GradDataV2$GDP <- as.numeric(GradDataV2$GDP)

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
  
  isolate(
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
   })
  })
 })
  
  observeEvent(input$submit,{
    output$coeff1 <-renderDT({
      
      train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop)
      test <- setdiff(1:nrow(GradDataV2), train)
      
      # training and testing subsets
      datTrain1 <- GradDataV2[train, ]
      datTest1 <- GradDataV2[test, ]
      
      
      isolate(
        if(!is.null(input$varsm)){
          withProgress(message='Running GLM',{
          vars <- input$varsm
          vars2 <- str_c(vars,collapse='+')
          formula <- paste('Target ~',vars2)
          form2 <- as.formula(formula)
          mrm <- train(form2,
                       data=datTrain1,
                       method="glm",
                       preProcess = c("center", "scale"), 
                       trControl = trainControl(method = "cv", number = 5))
          t <- summary(mrm)
          tab <- as.data.frame(t$coefficients)
          tab<- tibble::rownames_to_column(tab, "row_names")
          colnames(tab)[1] <- ''
          tab
      })
     })
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
    
    isolate(
    if(!is.null(input$varsm)){
      withProgress(message='Running GLM',{
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
     })
    })
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
    isolate(
    if(!is.null(input$varsm)){
      withProgress(message='Running Classification Tree',{
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
  plot(boostFit) 
       })
      })
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
    
    isolate(
    if(!is.null(input$varsm)){
      withProgress(message='Running Classification Tree',{
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
  RMSE2f})})
  })
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
    
    isolate(
    if(!is.null(input$varsm)){
      withProgress(message='Running Random Forest',{
      vars <- input$varsm
      vars2 <- str_c(vars,collapse='+')
      formula <- paste('Target ~',vars2)
     form2 <- as.formula(formula)
     random.forest.fit <- train(form2, data = datTrain1,
                            method = "rf",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "cv", number = 5),
                             tuneGrid = data.frame(mtry = 1:37))

  # plot fit
   plot(random.forest.fit)
   
     })
    })
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

     isolate(
     if(!is.null(input$varsm)){
       withProgress(message='Running Random Forest',{
       vars <- input$varsm
       vars2 <- str_c(vars,collapse='+')
      formula <- paste('Target ~',vars2)
      form2 <- as.formula(formula)
      random.forest.fit <- train(form2, data = datTrain1,
                                method = "rf",
                                preProcess = c("center", "scale"),
                                trControl = trainControl(method = "cv", number = 5),
                                tuneGrid = data.frame(mtry = 1:37))

  #Run on test data
  random.forest.predict <- predict(random.forest.fit, newdata = datTest1)

  #Obtain RMSE from test set, which will be used in automated comparison
  random.forest.compare <- postResample(random.forest.predict,
                                        obs = datTest1$Target)
  RMSErff <- as.data.frame(random.forest.compare)
  RMSErf2f <- RMSErff
  RMSErf2f<- tibble::rownames_to_column(RMSErf2f, "row_names")
  colnames(RMSErf2f)[1] <- ''
  RMSErf2f })
    })
   })
  })
   
  ## prediction tab
  ## 37 variables
   # Generalized Linear Model Output

   observeEvent(input$submit2,{
     output$predglm <-renderDT({
      
       isolate(
      if(input$pred=='Generalized Linear Regression'){
       
       df <- matrix()
       if(input$marpred==1){
         df <- cbind(df,Marital.status=input$marstat)
       }
       if(input$appmode==1){
         df <- cbind(df,Application.mode=input$appstat)
       }
       if(input$appord==1){
         df <- cbind(df,Application.order=input$appordpred)
       }
       if(input$course==1){
         df <- cbind(df,Course=input$coursep)
       }
       if(input$dteatten==1){
         df <- cbind(df,Daytime.evening.attendance.=input$dtep)
       }
       if(input$prevq==1){
         df <- cbind(df,Previous.qualification=input$prevqp)
       }
       if(input$prevqg==1){
         df <- cbind(df,Previous.qualification..grade.=input$prevqgp)
       }
       if(input$nac==1){
         df <- cbind(df,Nacionality = input$nacp)
       }
       if(input$mq==1){
         df <- cbind(df,Mother.s.qualification=input$mqp)
       }
       if(input$fq==1){
         df <- cbind(df,Father.s.qualification = input$fqp)
       }
       if(input$mo==1){
         df <- cbind(df,Mother.s.occupation = input$mop)
       }
       if(input$fo==1){
         df <- cbind(df,Father.s.occupation= input$fop)
       }
       if(input$ag==1){
         df <- cbind(df,Admission.grade = input$agp)
       }
       if(input$dp==1){
         df <- cbind(df,Displaced =input$dpp)
       }
       if(input$esn==1){
         df <- cbind(df,Educational.special.needs= input$esnp)
       }
       if(input$deb==1){
         df <- cbind(df,Debtor =input$debp)
       }
       if(input$tuit==1){
         df <- cbind(df,Tuition.fees.up.to.date = input$tuitp)
       }
       if(input$gen==1){
         df <- cbind(df,Gender= input$genp)
       }
       if(input$schol==1){
         df <- cbind(df,Scholarship.holder= input$scholp)
       }
       if(input$age==1){
         df <- cbind(df,Age.at.enrollment= input$agep)
       }
       if(input$int==1){
         df <- cbind(df,International= input$intp)
       }
       if(input$cfsc==1){
         df <- cbind(df,Curricular.units.1st.sem..credited.= input$cfscp)
       }
       if(input$cfse==1){
         df <- cbind(df,Curricular.units.1st.sem..enrolled. = input$cfsep)
       }
       if(input$cfsev==1){
         df <- cbind(df,Curricular.units.1st.sem..evaluations. = input$cfsevp)
       }
       if(input$cfsa==1){
         df <- cbind(df,Curricular.units.1st.sem..approved.=input$cfsap)
       }
       if(input$cfsg==1){
         df <- cbind(df,Curricular.units.1st.sem..grade. = input$cfsgp)
       }
       if(input$cfswoe==1){
         df <- cbind(df,Curricular.units.1st.sem..without.evaluations. = input$cfswoep)
       }
       if(input$cssc==1){
         df <- cbind(df,Curricular.units.2nd.sem..credited. = input$csscp)
       }
       if(input$csse==1){
         df <- cbind(df,Curricular.units.2nd.sem..enrolled. = input$cssep)
       }
       if(input$cssev==1){
         df <- cbind(df,Curricular.units.2nd.sem..evaluations. = input$cssevp)
       }
       if(input$cssa==1){
         df <- cbind(df,Curricular.units.2nd.sem..approved. = input$csap)
       }
       if(input$cssg==1){
         df <- cbind(df,Curricular.units.2nd.sem..grade. = input$cssgp)
       }
       if(input$csswoe==1){
         df <- cbind(df,Curricular.units.2nd.sem..without.evaluations. = input$csswoep)
       }
       if(input$ur==1){
         df <- cbind(df,Unemployment.rate = input$urp)
       }
       if(input$ir==1){
         df <- cbind(df,Inflation.rate =input$irp)
       }
       if(input$gdp==1){
         df <- cbind(df,GDP = input$gdpp)
       }
  

       train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop2)
       test <- setdiff(1:nrow(GradDataV2), train)

       # training and testing subsets
       datTrain1 <- GradDataV2[train, ]
       datTest1 <- GradDataV2[test, ]

       isolate(
         if(!is.null(df)){
           withProgress(message='Predicting GLM',{
           vars <- colnames(df)
           for(i in 1:length(vars)){
             if(is.numeric(GradDataV2[1,vars[i]])){
               vars[i] <- paste('as.numeric(',vars[i],')')
             }
           }
           vars2 <- str_c(vars,collapse='+')
           formula <- paste('Target ~',vars2)
           form2 <- as.formula(formula)
           mrm <- train(form2,
                        data=datTrain1,
                        method="glm",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "cv", number = 5))
           prediction<- predict(mrm,df,interval='prediction')
           prediction <- as.data.frame(prediction)
           prediction
           })
         })
       })
     })
    })
   
   # Boosting output
   observeEvent(input$submit2,{
     output$predboost <-renderDT({
       
       isolate(
       if(input$pred=='Classification Tree'){
         
         df <- matrix()
         if(input$marpred==1){
           df <- cbind(df,Marital.status=input$marstat)
         }
         if(input$appmode==1){
           df <- cbind(df,Application.mode=input$appstat)
         }
         if(input$appord==1){
           df <- cbind(df,Application.order=input$appordpred)
         }
         if(input$course==1){
           df <- cbind(df,Course=input$coursep)
         }
         if(input$dteatten==1){
           df <- cbind(df,Daytime.evening.attendance.=input$dtep)
         }
         if(input$prevq==1){
           df <- cbind(df,Previous.qualification=input$prevqp)
         }
         if(input$prevqg==1){
           df <- cbind(df,Previous.qualification..grade.=input$prevqgp)
         }
         if(input$nac==1){
           df <- cbind(df,Nacionality = input$nacp)
         }
         if(input$mq==1){
           df <- cbind(df,Mother.s.qualification=input$mqp)
         }
         if(input$fq==1){
           df <- cbind(df,Father.s.qualification = input$fqp)
         }
         if(input$mo==1){
           df <- cbind(df,Mother.s.occupation = input$mop)
         }
         if(input$fo==1){
           df <- cbind(df,Father.s.occupation= input$fop)
         }
         if(input$ag==1){
           df <- cbind(df,Admission.grade = input$agp)
         }
         if(input$dp==1){
           df <- cbind(df,Displaced =input$dpp)
         }
         if(input$esn==1){
           df <- cbind(df,Educational.special.needs= input$esnp)
         }
         if(input$deb==1){
           df <- cbind(df,Debtor =input$debp)
         }
         if(input$tuit==1){
           df <- cbind(df,Tuition.fees.up.to.date = input$tuitp)
         }
         if(input$gen==1){
           df <- cbind(df,Gender= input$genp)
         }
         if(input$schol==1){
           df <- cbind(df,Scholarship.holder= input$scholp)
         }
         if(input$age==1){
           df <- cbind(df,Age.at.enrollment= input$agep)
         }
         if(input$int==1){
           df <- cbind(df,International= input$intp)
         }
         if(input$cfsc==1){
           df <- cbind(df,Curricular.units.1st.sem..credited.= input$cfscp)
         }
         if(input$cfse==1){
           df <- cbind(df,Curricular.units.1st.sem..enrolled. = input$cfsep)
         }
         if(input$cfsev==1){
           df <- cbind(df,Curricular.units.1st.sem..evaluations. = input$cfsevp)
         }
         if(input$cfsa==1){
           df <- cbind(df,Curricular.units.1st.sem..approved.=input$cfsap)
         }
         if(input$cfsg==1){
           df <- cbind(df,Curricular.units.1st.sem..grade. = input$cfsgp)
         }
         if(input$cfswoe==1){
           df <- cbind(df,Curricular.units.1st.sem..without.evaluations. = input$cfswoep)
         }
         if(input$cssc==1){
           df <- cbind(df,Curricular.units.2nd.sem..credited. = input$csscp)
         }
         if(input$csse==1){
           df <- cbind(df,Curricular.units.2nd.sem..enrolled. = input$cssep)
         }
         if(input$cssev==1){
           df <- cbind(df,Curricular.units.2nd.sem..evaluations. = input$cssevp)
         }
         if(input$cssa==1){
           df <- cbind(df,Curricular.units.2nd.sem..approved. = input$csap)
         }
         if(input$cssg==1){
           df <- cbind(df,Curricular.units.2nd.sem..grade. = input$cssgp)
         }
         if(input$csswoe==1){
           df <- cbind(df,Curricular.units.2nd.sem..without.evaluations. = input$csswoep)
         }
         if(input$ur==1){
           df <- cbind(df,Unemployment.rate = input$urp)
         }
         if(input$ir==1){
           df <- cbind(df,Inflation.rate =input$irp)
         }
         if(input$gdp==1){
           df <- cbind(df,GDP = input$gdpp)
         }
         
         
         train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop2)
         test <- setdiff(1:nrow(GradDataV2), train)
         
         # training and testing subsets
         datTrain1 <- GradDataV2[train, ]
         datTest1 <- GradDataV2[test, ]
         
         isolate(
           if(!is.null(df)){
             withProgress(message='Predicting Classification Tree',{
             vars <- colnames(df)
             for(i in 1:length(vars)){
               if(is.numeric(GradDataV2[1,vars[i]])){
                 vars[i] <- paste('as.numeric(',vars[i],')')
               }
             }
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
             prediction<- predict(boostFit,df,interval='prediction')
             prediction <- as.data.frame(prediction)
             prediction
           })
         })
       })
     })
   })
  
   # Random Forest Output
   observeEvent(input$submit2,{
     output$predrf <-renderDT({
       
       isolate(
       if(input$pred=='Random Forest'){
         
         df <- matrix()
         if(input$marpred==1){
           df <- cbind(df,Marital.status=input$marstat)
         }
         if(input$appmode==1){
           df <- cbind(df,Application.mode=input$appstat)
         }
         if(input$appord==1){
           df <- cbind(df,Application.order=input$appordpred)
         }
         if(input$course==1){
           df <- cbind(df,Course=input$coursep)
         }
         if(input$dteatten==1){
           df <- cbind(df,Daytime.evening.attendance.=input$dtep)
         }
         if(input$prevq==1){
           df <- cbind(df,Previous.qualification=input$prevqp)
         }
         if(input$prevqg==1){
           df <- cbind(df,Previous.qualification..grade.=input$prevqgp)
         }
         if(input$nac==1){
           df <- cbind(df,Nacionality = input$nacp)
         }
         if(input$mq==1){
           df <- cbind(df,Mother.s.qualification=input$mqp)
         }
         if(input$fq==1){
           df <- cbind(df,Father.s.qualification = input$fqp)
         }
         if(input$mo==1){
           df <- cbind(df,Mother.s.occupation = input$mop)
         }
         if(input$fo==1){
           df <- cbind(df,Father.s.occupation= input$fop)
         }
         if(input$ag==1){
           df <- cbind(df,Admission.grade = input$agp)
         }
         if(input$dp==1){
           df <- cbind(df,Displaced =input$dpp)
         }
         if(input$esn==1){
           df <- cbind(df,Educational.special.needs= input$esnp)
         }
         if(input$deb==1){
           df <- cbind(df,Debtor =input$debp)
         }
         if(input$tuit==1){
           df <- cbind(df,Tuition.fees.up.to.date = input$tuitp)
         }
         if(input$gen==1){
           df <- cbind(df,Gender= input$genp)
         }
         if(input$schol==1){
           df <- cbind(df,Scholarship.holder= input$scholp)
         }
         if(input$age==1){
           df <- cbind(df,Age.at.enrollment= input$agep)
         }
         if(input$int==1){
           df <- cbind(df,International= input$intp)
         }
         if(input$cfsc==1){
           df <- cbind(df,Curricular.units.1st.sem..credited.= input$cfscp)
         }
         if(input$cfse==1){
           df <- cbind(df,Curricular.units.1st.sem..enrolled. = input$cfsep)
         }
         if(input$cfsev==1){
           df <- cbind(df,Curricular.units.1st.sem..evaluations. = input$cfsevp)
         }
         if(input$cfsa==1){
           df <- cbind(df,Curricular.units.1st.sem..approved.=input$cfsap)
         }
         if(input$cfsg==1){
           df <- cbind(df,Curricular.units.1st.sem..grade. = input$cfsgp)
         }
         if(input$cfswoe==1){
           df <- cbind(df,Curricular.units.1st.sem..without.evaluations. = input$cfswoep)
         }
         if(input$cssc==1){
           df <- cbind(df,Curricular.units.2nd.sem..credited. = input$csscp)
         }
         if(input$csse==1){
           df <- cbind(df,Curricular.units.2nd.sem..enrolled. = input$cssep)
         }
         if(input$cssev==1){
           df <- cbind(df,Curricular.units.2nd.sem..evaluations. = input$cssevp)
         }
         if(input$cssa==1){
           df <- cbind(df,Curricular.units.2nd.sem..approved. = input$csap)
         }
         if(input$cssg==1){
           df <- cbind(df,Curricular.units.2nd.sem..grade. = input$cssgp)
         }
         if(input$csswoe==1){
           df <- cbind(df,Curricular.units.2nd.sem..without.evaluations. = input$csswoep)
         }
         if(input$ur==1){
           df <- cbind(df,Unemployment.rate = input$urp)
         }
         if(input$ir==1){
           df <- cbind(df,Inflation.rate =input$irp)
         }
         if(input$gdp==1){
           df <- cbind(df,GDP = input$gdpp)
         }
         
         
         train <- sample(1:nrow(GradDataV2), size = nrow(GradDataV2)*input$prop2)
         test <- setdiff(1:nrow(GradDataV2), train)
         
         # training and testing subsets
         datTrain1 <- GradDataV2[train, ]
         datTest1 <- GradDataV2[test, ]
         
         isolate(
           if(!is.null(df)){
             withProgress(message='Predicting Random Forest',{
             vars <- colnames(df)
             for(i in 1:length(vars)){
               if(is.numeric(GradDataV2[1,vars[i]])){
                 vars[i] <- paste('as.numeric(',vars[i],')')
               }
             }
             vars2 <- str_c(vars,collapse='+')
             formula <- paste('Target ~',vars2)
             form2 <- as.formula(formula)
             random.forest.fit <- train(form2, data = datTrain1,
                                        method = "rf",
                                        preProcess = c("center", "scale"),
                                        trControl = trainControl(method = "cv", number = 5),
                                        tuneGrid = data.frame(mtry = 1:37))
             prediction<- predict(random.forest.fit,df,interval='prediction')
             prediction <- as.data.frame(prediction)
             prediction
        })
       })
      })
     })
   })
   
   ## Data Tab - User can scroll through data, subset (rows and cols) and save data (possibly subset)
   data <- reactive({
     df <- c("Student")
     GradDataOut <- GradDataV2
     GradDataOut<- tibble::rownames_to_column(GradDataOut, "row_names")
     colnames(GradDataOut)[1] <- 'Student'
     
     if(input$marpred2==1){
       df <- c(df,"Marital.status")
     }
     if(input$appmode2==1){
       df <- c(df,'Application.mode')
     }
     if(input$appord2==1){
       df <- c(df,'Application.order')
     }
     if(input$course2==1){
       df <- c(df,'Course')
     }
     if(input$dteatten2==1){
       df <- c(df,'Daytime.evening.attendance.')
     }
     if(input$prevq2==1){
       df <- c(df,'Previous.qualification')
     }
     if(input$prevqg2==1){
       df <- c(df,'Previous.qualification..grade.')
     }
     if(input$nac2==1){
       df <- c(df,'Nacionality')
     }
     if(input$mq2==1){
       df <- c(df,'Mother.s.qualification')
     }
     if(input$fq2==1){
       df <- c(df,'Father.s.qualification')
     }
     if(input$mo2==1){
       df <- c(df,'Mother.s.occupation')
     }
     if(input$fo2==1){
       df <- c(df,'Father.s.occupation')
     }
     if(input$ag2==1){
       df <- c(df,'Admission.grade')
     }
     if(input$dp2==1){
       df <- c(df,'Displaced')
     }
     if(input$esn2==1){
       df <- c(df,'Educational.special.needs')
     }
     if(input$deb2==1){
       df <- c(df,'Debtor')
     }
     if(input$tuit2==1){
       df <- c(df,'Tuition.fees.up.to.date')
     }
     if(input$gen2==1){
       df <- c(df,'Gender')
     }
     if(input$schol2==1){
       df <- c(df,'Scholarship.holder')
     }
     if(input$age2==1){
       df <- c(df,'Age.at.enrollment')
     }
     if(input$int2==1){
       df <- c(df,'International')
     }
     if(input$cfsc2==1){
       df <- c(df,'Curricular.units.1st.sem..credited.')
     }
     if(input$cfse2==1){
       df <- c(df,'Curricular.units.1st.sem..enrolled.')
     }
     if(input$cfsev2==1){
       df <- c(df,'Curricular.units.1st.sem..evaluations.')
     }
     if(input$cfsa2==1){
       df <- c(df,'Curricular.units.1st.sem..approved.')
     }
     if(input$cfsg2==1){
       df <- c(df,'Curricular.units.1st.sem..grade.')
     }
     if(input$cfswoe2==1){
       df <- c(df,'Curricular.units.1st.sem..without.evaluations.')
     }
     if(input$cssc2==1){
       df <- c(df,'Curricular.units.2nd.sem..credited.')
     }
     if(input$csse2==1){
       df <- c(df,'Curricular.units.2nd.sem..enrolled.')
     }
     if(input$cssev2==1){
       df <- c(df,'Curricular.units.2nd.sem..evaluations.')
     }
     if(input$cssa2==1){
       df <- c(df,'Curricular.units.2nd.sem..approved.')
     }
     if(input$cssg2==1){
       df <- c(df,'Curricular.units.2nd.sem..grade.')
     }
     if(input$csswoe2==1){
       df <- c(df,'Curricular.units.2nd.sem..without.evaluations.')
     }
     if(input$ur2==1){
       df <- c(df,'Unemployment.rate')
     }
     if(input$ir2==1){
       df <- c(df,'Inflation.rate')
     }
     if(input$gdp2==1){
       df <-c(df,'GDP')
     }
     
     if(!is.null(df)){
       vars <- df
       dt <- subset(GradDataOut, select=vars)
       
     }
     dt <- dt[1:input$rows,]
     
   })
   
   output$dt <-renderDT({
       
       data()
   })
   
   # Downloadable csv of selected dataset ----
   output$download <- downloadHandler(
     filename = function() {
       paste('GradData',".csv", sep = "")
     },
     content = function(file) {
       write.csv(data(), file, row.names = FALSE)
     }
   )
  
}
