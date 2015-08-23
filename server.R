library(shiny)
library(ggplot2)
library(caret)

source("model.R")
histplot <- ggplot(data=data.frame(mileage=c(mpg$cty,mpg$hwy), 
                                 type=c(rep("city driving", nrow(mpg)), 
                                        rep("highway driving", nrow(mpg)))))
histplot <- histplot + geom_histogram(mapping=aes(x=mileage),binwidth=1) 
histplot <- histplot + facet_grid(facets=type~.)
histplot <- histplot + ggtitle("Comparison to all cars in data set")

shinyServer(function(input, output){
    ctyest <- reactive({
      if(input$submit > 0){
        predict(ctymodel, newdata=list(manufacturer = input$manufacturer,
                                       cyl = input$cyl,
                                       trtype = input$trans,
                                       drv = input$drv,
                                       class = input$class))
      }        
    })
    hwyest <- reactive({
      if(input$submit > 0){
        predict(hwymodel, newdata=list(manufacturer = input$manufacturer,
                                       cyl = input$cyl,
                                       trtype = input$trans,
                                       drv = input$drv,
                                       class = input$class))
      }
    })
    output$ctympg <- renderText({
      if(input$submit > 0){
        paste("Your car's predicted city driving mileage is", 
              round(ctyest(), 2),
              "mpg.")
      } else {
        NULL
      }
    })
    output$hwympg <- renderText({
      if(input$submit > 0){
        paste("Your car's predicted highway driving mileage is", 
              round(hwyest(), 2),
              "mpg.")
      } else {
        NULL
      }
    })
    output$histtext <- renderText({
      if(input$submit > 0){
        "The plot below compares your car's predicted gas mileage to all the 
        car models in the data set."
      } else {
        NULL
      }
    })
    output$boxtext <- renderText({
      if(input$submit > 0){
        "The plot below compares your car's predicted gas mileage to the subsets 
        of cars which share the same manufacturer, the same number of cylinders, 
        the same type of transmission, the same drive wheel configuration, or 
        the same class of vehicle."
      } else {
        NULL
      }
    })
    boxplots <- reactive({
      if(input$submit > 0) {
        mpgsubset <- data.frame(mileage=c(mpg[mpg$manufacturer==input$manufacturer,"cty"],
                                          mpg[mpg$manufacturer==input$manufacturer,"hwy"],
                                          mpg[mpg$cyl==input$cyl,"cty"],
                                          mpg[mpg$cyl==input$cyl,"hwy"],
                                          mpg[mpg$trtype==input$trans,"cty"],
                                          mpg[mpg$trtype==input$trans,"hwy"],
                                          mpg[mpg$drv==input$drv,"cty"],
                                          mpg[mpg$drv==input$drv,"hwy"],
                                          mpg[mpg$class==input$class,"cty"],
                                          mpg[mpg$class==input$class,"hwy"]),
                                comparison=c(rep(as.character(manufacturers[input$manufacturer]), 
                                                 2*nrow(mpg[mpg$manufacturer==input$manufacturer,])),
                                             rep(as.character(cylinders[input$cyl]), 
                                                 2*nrow(mpg[mpg$cyl==input$cyl,])),
                                             rep(as.character(transmissions[input$trans]), 
                                                 2*nrow(mpg[mpg$trtype==input$trans,])),
                                             rep(as.character(drivewheels[input$drv]), 
                                                 2*nrow(mpg[mpg$drv==input$drv,])),
                                             rep(as.character(classes[input$class]), 
                                                 2*nrow(mpg[mpg$class==input$class,]))),
                                type=c(rep(c("city driving", "highway driving"), 
                                           each=nrow(mpg[mpg$manufacturer==input$manufacturer,])),
                                       rep(c("city driving", "highway driving"), 
                                           each=nrow(mpg[mpg$cyl==input$cyl,])),
                                       rep(c("city driving", "highway driving"), 
                                           each=nrow(mpg[mpg$trtype==input$trans,])),
                                       rep(c("city driving", "highway driving"), 
                                           each=nrow(mpg[mpg$drv==input$drv,])),
                                       rep(c("city driving", "highway driving"), 
                                           each=nrow(mpg[mpg$class==input$class,]))))
        boxes <- ggplot(data=mpgsubset) + geom_boxplot(aes(x=comparison, y=mileage)) 
        boxes <- boxes + facet_grid(facets=type~., scales="free_y")
        boxes <- boxes + ggtitle("Comparison to cars with similar characteristics")
        boxes <- boxes + xlab("variable")
        boxes
      } else {
        NULL
      }
    })
    output$hist <- renderPlot({
      if(input$submit > 0){
        histplot + geom_vline(aes(xintercept = estimate,
                                    colour = factor(type)), 
                                data=data.frame(estimate=c(ctyest(), hwyest()), 
                                                type=c("city driving", 
                                                       "highway driving")),
                                show_guide=TRUE) + labs(colour="mpg")
      } else {
        NULL
      }
    })
    output$boxes <- renderPlot({
      if(input$submit > 0){
        boxplots() + geom_hline(aes(yintercept=estimate,
                                    colour=factor(type)),
                                data=data.frame(estimate=c(ctyest(), hwyest()),
                                                type=c("city driving", 
                                                       "highway driving")),
                                show_guide=TRUE) + labs(colour="mpg")
      } else {
        NULL
      }
    })
  }
)


