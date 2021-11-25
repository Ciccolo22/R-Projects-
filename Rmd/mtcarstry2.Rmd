---
title: "Exploring Flexdashboard via Linear Reg"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    theme: 
      version: 4 
      navbar-bg: "#000"
      heading_font: 
        google: "Prompt"
    vertical_layout: fill
runtime: shiny
---




```{r setup, include=FALSE}
library(flexdashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(data.table)
library(DT)
library(jtools)
library(broom)
library(visreg)
```



```{r}
###Data
data <- mtcars


Variables <- data %>% select(everything(),-mpg) 


##gives me confidnece intervals
#mod1out <- lm(mpg~.,data = mtcars) %>% 
 # confint()

  
#mod1out

```



Column {.sidebar}
====================================================



```{r}

##Add select input
useShinyjs(rmd = TRUE)



selectInput(inputId = "Var", label = h4("Predictors"),choices = names(Variables),
            selected = NULL,
             multiple = TRUE)

renderText("Target Variable = mpg")

actionButton(inputId = "apply", label = "Apply", icon = icon(name = "play", lib = "font-awesome"))

actionButton("reset", label = "Reset", icon = icon("sync"))

observeEvent(eventExpr = input$reset, handlerExpr = {
  
  updateSelectInput(session = session, inputId = "Var", choices = names(Variables), selected = "cyl")
  

  shinyjs::delay(ms = 300, expr = {
    shinyjs::click(id = "apply")
  })
  
}, ignoreNULL = TRUE)





```


Data Overview
==============================================================

```{r}

renderDT(data, options=list(scrollY='400px'))


```


Model Output 
==============================================================


```{r}

model1 <- eventReactive(eventExpr = input$apply, valueExpr ={
  vars <- as.matrix(data[, input$Var])
  lm(mpg ~ vars, data = data)
    

}, ignoreNULL = TRUE)

renderPrint({ 
   
summ(model1())
  
})


```

Conditional Plots 
=====================================================================

### Select One Predictor at a time

```{r}


fit2 <- eventReactive(eventExpr = input$apply, valueExpr ={
  vars <- mtcars[, input$Var]
  lm(mpg ~ vars, data = mtcars) %>%  
    visreg()
    
  
  
})


renderPlot({ 
   
fit2() 
})





```



