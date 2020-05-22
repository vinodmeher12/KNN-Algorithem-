# Install required packages 

install.packages("class")
install.packages("gmodels")
library(class)
library(gmodels)

# ****************PART I : Part 1: Diagnosing breast cancer with the kNN algorithm *******

#Step 1: Collecting data 

wbcd <- read.csv(file.choose(), stringsAsFactors = FALSE) # use the Breat cancer data set 
dim(wbcd)
str(wbcd)

#Step 2: Exploring and preparing data 

table(wbcd$diagnosis)
wbcd <- wbcd[-1]

#Transformation 

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Normalizing the data 
# Customized function for scaling the data 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

# Creating test and train data sets along with lables 

wbcd_train <- wbcd_n[1:469, ]
head(wbcd_train)
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#Training Model on dataset 
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
head(wbcd_test_pred)
#Evaluating model performance 
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

# Improving model Perofrmance
# using Z-score Transformation 

wbcd_z <- as.data.frame(scale(wbcd[2:31]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
head(wbcd_train)
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# Using Alternate K-values 
# for K= 1
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

# for K= 13
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

# for K= 29
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

#******************Part 2: banknote authentication using kNN algorithm ************
#Step 1: Collecting data 
bnap <- read.csv(file.choose()) # use the bank notes data set 
str(bnap)
summary(bnap)
table(bnap$class)

#Step 2: Exploring and preparing data 

#Transformation 
bnap$class <- factor(bnap$class, levels = c(0, 1), labels = c("Forged", "RealNote"))
round(prop.table(table(bnap$class)) * 100, digits = 1)
#Normalizing the data 
# Customized function for scaling the data 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

bnap_n <- as.data.frame(lapply(bnap[1:4], normalize))
summary(bnap_n$curtosisWavelet)

# Creating test and train data sets along with lables 

set.seed(101)
ind<-sample(2,nrow(bnap), replace = TRUE,prob = c(0.7,0.3))
bnap_train<- bnap_n[ind==1,]
bnap_test<- bnap_n[ind==2,]
bnap_train_labels <- bnap[ind==1, 5]
bnap_test_labels <- bnap[ind==2, 5]
str(bnap_train)
head(bnap_train_labels)

#Training Model on dataset 
bnap_test_pred <- knn(train = bnap_train, test = bnap_test,
                      cl = bnap_train_labels, k=31)
head(bnap_test_pred)
#Evaluating model performance 

CrossTable(x = bnap_test_labels, y = bnap_test_pred, prop.chisq=FALSE)

# Improving model Perofrmance
# using Z-score Transformation 

bnap_z <- as.data.frame(scale(bnap[1:4]))

set.seed(101)
ind<-sample(2,nrow(bnap), replace = TRUE,prob = c(0.7,0.3))
bnap_train<- bnap_z[ind==1,]
bnap_test<- bnap_z[ind==2,]
bnap_train_labels <- bnap[ind==1, 5]
bnap_test_labels <- bnap[ind==2, 5]
str(bnap_train)
head(bnap_train_labels)

bnap_test_pred <- knn(train = bnap_train, test = bnap_test,
                      cl = bnap_train_labels, k=31)
CrossTable(x = bnap_test_labels, y = bnap_test_pred,
           prop.chisq=FALSE)

# Using Alternate K-values 
# for K= 1
bnap_test_pred <- knn(train = bnap_train, test = bnap_test,
                      cl = bnap_train_labels, k=1)
CrossTable(x = bnap_test_labels, y = bnap_test_pred, prop.chisq=FALSE)
# for K= 13
bnap_test_pred <- knn(train = bnap_train, test = bnap_test,
                      cl = bnap_train_labels, k=13)
CrossTable(x = bnap_test_labels, y = bnap_test_pred, prop.chisq=FALSE)
# for K= 39
bnap_test_pred <- knn(train = bnap_train, test = bnap_test,
                      cl = bnap_train_labels, k=39)
CrossTable(x = bnap_test_labels, y = bnap_test_pred, prop.chisq=FALSE)
