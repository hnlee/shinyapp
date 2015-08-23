library(ggplot2)
library(caret)
set.seed(1234)
data(mpg)

auto <- grep("^auto", mpg$trans)
mpg$trtype[auto] <- "auto"
mpg$trtype[-auto] <- "manual"
mpg$trtype <- factor(mpg$trtype)
mpg$cyl <- factor(mpg$cyl)

ctydata <- mpg[, c("manufacturer","cyl","drv","trtype","class","cty")]
hwydata <- mpg[, c("manufacturer","cyl","drv","trtype","class","hwy")]

ctypartition <- createDataPartition(mpg$cty, p=0.9, list=FALSE)
hwypartition <- createDataPartition(mpg$hwy, p=0.9, list=FALSE)

ctytraining <- ctydata[ctypartition,]
ctytesting <- ctydata[-ctypartition,]
ctymodel <- train(cty ~ ., 
                  data=ctytraining, 
                  method="glm", 
                  trControl=trainControl(method="cv",number=50))

hwytraining <- hwydata[hwypartition,]
hwytesting <- hwydata[-hwypartition,]
hwymodel <- train(hwy ~ ., 
                  data=hwytraining, 
                  method="glm", 
                  trControl=trainControl(method="cv",number=50))

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

manufacturers <- sapply(levels(mpg$manufacturer), capwords)
names(manufacturers) <- levels(mpg$manufacturer)
cylinders <- list("4"="4-cylinder",
                  "5"="5-cylinder",
                  "6"="6-cylinder",
                  "8"="8-cylinder")
transmissions <- list("auto"="Automatic",
                      "manual"="Manual")
drivewheels <- list("f"="Front-wheel drive",
                    "r"="Rear-wheel drive",
                    "4"="Four-wheel drive")
classes <- list("2seater"="Two-seater",
                "subcompact"="Subcompact sedan",
                "compact"="Compact sedan",
                "midsize"="Midsize sedan",
                "pickup"="Pickup truck",
                "minivan"="Minivan",
                "suv"="Sport utility vehicle")

