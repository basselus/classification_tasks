
#Short description: knn is a predictive modelling and instance based 
#learning algorithm capable to predict unseen data labels outcomes based on 
#previous values. In this simple example, i will use the iris dataset avalaible
#in R built in datasets package
#The goal of the project is to predict the species (target variable) of the flowers based on the measures
#of numeric variables 

#*****************************************
#STEP 1 : LOAD AND GET TO KNOW THE DATA
#*****************************************

#load and check data structure
iris<-data.frame(datasets::iris)
dim(iris)
str(iris)
summary(iris)


#*****************************************
#STEP 2 : EXPLORATORY ANALYSIS
#*****************************************

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

#*****************************************************
#STEP 3 : SPLIT DATA BETWEEN TRAINING AND TEST SUBSETS
#*****************************************************

#The target variable  is not included (species)

  #****************************************************
  #Method 1:using the set.seed() and sample() functions
  #****************************************************

#Shuffle and randomize the data

set.seed(1234)
seed<-sample(2, nrow(iris), replace = TRUE, prob = c(0.67, 0.33))

#split the data

iris_train<-iris[seed==1, 1:4]
iris_test<-iris[seed==2, 1:4]
head(iris_train)
head(iris_test)

  #**************************************************************
  #Method 1:using the createdatapartition function (caret library)
  #**************************************************************

library(caret)

#Create a partition to split based on labels
partition<-createDataPartition(iris$Species, p=0.75, list=FALSE)

#Subset training set based on partition
iris_train_2<-iris[partition,]
iris_test_2<-iris[-partition,]

#Store the class labels into factor vectors 
#and divide them into training and test class labels subsets

#************************
#using partition method 1
#************************

iris_train_labels<-iris[seed==1,5]
iris_test_labels<-iris[seed==2,5]


#************************
#using partition method 2
#************************

iris_train_labels_2<-iris_train_2[,5]
iris_test_labels_2<-iris_test_2[,5]

#*****************************************
#STEP 4 : TRAIN THE MODEL ON THE DATA
#*****************************************

#Build the classifier

library(class)

#*********************************************
#Build the classifier using partition method 1
#*********************************************

iris_pred<-knn(train =iris_train ,   #contains numeric training data 
               test = iris_test,     #contains numeric test data
               cl=iris_train_labels, # factor vector containing the class for each row
               k=3 )                 # number of neighbors

#*********************************************
#Build the classifier using partition method 2
#*********************************************

iris_pred_2<-knn(train =iris_train_2[,c(1:4)] , 
                 test = iris_test_2[,c(1:4)], 
                 cl=iris_train_labels_2, k=3 )

#inspect the model
summary(iris_pred)
summary(iris_pred_2)

#*****************************************
#STEP 5 : EVALUATE THE MODEL PERFORMANCE
#*****************************************

#********************************
#Method 1 : using gmodels package
#********************************

library(gmodels)
CrossTable(x=iris_test_labels, y=iris_pred, prop.chisq = FALSE)

#Comment : the table from crosstable function produces a 3x3 confusion matrix that categorizes 
#predictions according to whether they match the actual value.

#***************************
#Method 2 : using raw labels
#***************************

#Put iris_test_labels into a dataframe
iris_test_labels<-data.frame(iris_test_labels)

#merge iris_test_labels with and iris_pred
merge<-data.frame(iris_pred, iris_test_labels)

#Specify the column names for 'merge'
names(merge)<-c("Predicted Species", "Observed Species")

merge

#COMMENT : as illustrated in the merge dataset below, all the Species on the unseen dataset have been correctly predicted ,except of one (line 29) where the
#actual species class was "virginica" instead of "versicolor". we can conclude the model
#displays a fairly good performance.

#       Predicted Species   Observed Species
#1             setosa           setosa
#2             setosa           setosa
#3             setosa           setosa
#4             setosa           setosa
#5             setosa           setosa
#6             setosa           setosa
#7             setosa           setosa
#8             setosa           setosa
#9             setosa           setosa
#10            setosa           setosa
#11            setosa           setosa
#12            setosa           setosa
#13        versicolor       versicolor
#14        versicolor       versicolor
#15        versicolor       versicolor
#16        versicolor       versicolor
#17        versicolor       versicolor
#18        versicolor       versicolor
#19        versicolor       versicolor
#20        versicolor       versicolor
#21        versicolor       versicolor
#22        versicolor       versicolor
#23        versicolor       versicolor
#24        versicolor       versicolor
#25         virginica        virginica
#26         virginica        virginica
#27         virginica        virginica
#28         virginica        virginica
#29        versicolor        virginica
#30         virginica        virginica
#31         virginica        virginica
#32         virginica        virginica
#33         virginica        virginica
#34         virginica        virginica
#35         virginica        virginica
#36         virginica        virginica
#37         virginica        virginica
#38         virginica        virginica
#39         virginica        virginica
#40         virginica        virginica

#********************************************************
#STEP 6 : MODEL EVALUATION USING MAE ON NUMERIC VARIABLES
#********************************************************
