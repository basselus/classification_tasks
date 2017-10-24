
#Short description: knn is a predictive modelling an instance based 
#learning algorithm capable to predict unseen data labels outcomes based on 
#previous values. In this simple example, i will use the iris dataset avalaible
#in R built in datasets package

#Step 1 : load and get to know your data

#load and check data structure
iris<-data.frame(datasets::iris)
dim(iris)
str(iris)
summary(iris)

#Step 2 : Exploratory analysis

#Visualize data to retrieve the main patterns
library(psych)
library(lattice)
pairs.panels(iris[,1:4])

xyplot(Sepal.Length ~ Sepal.Width, data=iris, groups=Species,
       panel=panel.superpose,
       type=c("p", "smooth"), span=0.75,
       auto.key=list(x=0.15, y=0.85, bty="l")
)

xyplot(Sepal.Length ~ Petal.Length, data=iris, groups=Species,
       panel=panel.superpose,
       type=c("p", "smooth"), span=0.75,
       auto.key=list(x=0.15, y=0.85, bty="l")
)

xyplot(Sepal.Length ~ Petal.Width, data=iris, groups=Species,
       panel=panel.superpose,
       type=c("p", "smooth"), span=0.75,
       auto.key=list(x=0.15, y=0.85, bty="l")
)

xyplot(Sepal.Width ~ Petal.Length, data=iris, groups=Species,
       panel=panel.superpose,
       type=c("p", "smooth"), span=0.75,
       auto.key=list(x=0.15, y=0.85, bty="l")
)

xyplot(Sepal.Width ~ Petal.Width, data=iris, groups=Species,
       panel=panel.superpose,
       type=c("p", "smooth"), span=0.75,
       auto.key=list(x=0.15, y=0.85, bty="l")
)

xyplot(Petal.Length ~ Petal.Width, data=iris, groups=Species,
       panel=panel.superpose,
       type=c("p", "smooth"), span=0.75,
       auto.key=list(x=0.15, y=0.85, bty="l")
)




