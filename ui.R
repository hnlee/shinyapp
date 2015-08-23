capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

library(ggplot2)
data(mpg)
manufacturers <- levels(mpg$manufacturer)
names(manufacturers) <- sapply(manufacturers, capwords)
cylinders <- levels(as.factor(mpg$cyl))
names(cylinders) <- sapply(cylinders, as.character)

library(shiny)
shinyUI(fluidPage(
  titlePanel("Gas mileage predictor"),
  sidebarLayout(position="left",
                sidebarPanel(
                  selectInput("manufacturer",
                              label=p("Who is the manufacturer of your car?"),
                              choices=manufacturers),
                  br(),
                  radioButtons("cyl",
                              label=p("How many cylinders?"),
                              choices=cylinders),
                  br(),
                  radioButtons("trans",
                               label=p("What type of transmission?"),
                               choices=list("Automatic"="auto",
                                            "Manual"="manual")),
                  br(),
                  radioButtons("drv",
                               label=p("What type of drive wheel configuration?"),
                               choices=list("Front-wheel drive"="f",
                                            "Rear-wheel drive"="r",
                                            "Four-wheel drive"="4")),
                  br(),
                  selectInput("class",
                              label=p("What type of car?"),
                              choices=list("Two-seater"="2seater",
                                           "Subcompact sedan"="subcompact",
                                           "Compact sedan"="compact",
                                           "Midsize sedan"="midsize",
                                           "Pickup truck"="pickup",
                                           "Minivan"="minivan",
                                           "Sport utility vehicle"="suv")),
                  br(),
                  actionButton("submit",
                               label="Predict")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("About",
                             br(),
                             p("This app predicts the gas mileage of cars
                               based on their manufacturer, number of cylinders,
                               type of transmission, drive wheel configuration,
                               and vehicle class. The underlying function is a
                               generalized linear model, which was trained and 
                               tested against the", code("mpg"), "data set,
                               comprised of 38 popular car models from 1999 to 
                               2008, curated from the fuel economy data published
                               by the EPA at", 
                               a(href="http://fueleconomy.gov", 
                                 "http://fueleconomy.gov")),
                             p("Input the specifications of the car whose gas 
                               mileage you wish to estimate, and press the 
                               Predict button to obtain the gas mileage expected 
                               from the car under city driving and highway 
                               driving conditions. Changing specifications will 
                               automatically update the predictions. Click on 
                               the Plots tab to see how your car compares to 
                               the car models in the data set."),
                             br(),
                             strong(textOutput("ctympg")),
                             br(),
                             strong(textOutput("hwympg"))
                             ),
                    tabPanel("Plots",
                             br(),
                             p("Input the specifications of the car whose gas 
                               mileage you wish to estimate, and press the 
                               Predict button to view plots. Changing
                               specifications will automatically update
                               the plots. Click on the About tab to view
                               the numerical estimates of your car's gas
                               mileage."),
                             textOutput("histtext"),
                             plotOutput("hist"),
                             br(),
                             textOutput("boxtext"),
                             plotOutput("boxes")
                             )
                  )
                )
  )
))