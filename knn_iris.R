
#Short description: knn is a predictive modelling and instance based 
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

#Print the correlation matrixes for each of the classes of the target variable:
#(Setosa, Versicolor,Virginica)

#overall correlation between numeric variables
cor(iris[,1:4]) 

# Return the values of iris levels
x=levels(iris$Species) 


#Print all the correlation matrixes by classes
# Setosa correlation matrix
print(x[1]) 
cor(iris[iris$Species==x[1],1:4])

# Versicolor correlation matrix
print(x[2]) 
cor(iris[iris$Species==x[2],1:4])

# Virginica correlation matrix
print(x[3]) 
cor(iris[iris$Species==x[3],1:4])

#look how data is shaped and distributed around species
round(prop.table(table(iris$Species)*100), digits=1)
summary(iris)

