## Common codes for analytics and modeling
## Compiled by Alina_Yang



#############################################
#Basic Data Manipulation 
#############################################

######### read file
data <- read.csv(file="Data.csv", head=T, stringsAsFactors = FALSE)
### "head" : Default of R is True, which means the first row of values is set as column names.
### "stringsAsFactors" : Default of R is Ture, which means to convert character strings into factors, which may make it difficult to do such things as replace values.

######## store to csv fie
write.csv(Data,"~/Desktop/basket_transactions.csv",quote=FALSE, row.names=TRUE)


######### understand the data
View(data)
head(data)
summary(data)
nrow(data)
ncol(data)
class(data)
length(dara)   # number of elements or components
str(data)    # structure of an object
names(data)  # column names


######### columns & rows
## select columns
myvars <- c("v1", "v2", "v3")
newdata <- data[myvars]
newdata <- data[c(1,5:10)]
##select rows
newdata <- data[ which(data$gender=='F'& data$age > 65), ]
newdata <- mydata[1:5,]
## attach column name
names(data) <-c('new_name1','new_name2')
## create a new column
data$new_column <- mean(data$column1)
## Remove first 52 rows/ keep 53th to the last row
data <- data[53:nrow(data),]
## Remove columns
##
data<-data[, !names(data)%in%c('column_name_to_drop')]
##or 
newdata <- data[c(-3,-5)]
##or assign NULL value 
data$v3 <- data$v5 <- NULL


###########filter out rows with missing value
#complete cases will return a logical vector indicating which rows have no missing values. 
newdata <-data[complete.cases(data),]



############ convert data type
#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

#mutate function is used to edit or add new columns to dataframe.
install.packages('dplyr')
library(dplyr)
data %>% mutate(Description = as.factor(Description))
data %>% mutate(Country = as.factor(Country))

######### create vectors with zero value
######### fill in new values 

### get the number of subjects we are using
nS <-length(Subjects)

### rep(0,nS) creates a vector with ns zeros
### for example we will create 3 vectors to store the mean, .25 quantile, and .75 quantile
MeanValues <- rep(0,nS) 
FirstQaurtile <- rep(0,nS)
ThirdQaurtile<- rep(0,nS)

### This is a for loop 
for (j in 1:nS){
  MeanValues[j] <- mean(DATA[,j], na.rm = TRUE)
  FirstQaurtile[j]<-quantile(DATA[,j],0.25, na.rm = TRUE)
  ThirdQaurtile[j]<-quantile(DATA[,j],0.75, na.rm = TRUE)



#############################################
#Regression Model
#############################################

#OLS
model <- lm(revenue~., data=xxx)
coef(model)
summary(model)

##select significant variables based on AIC
selectedols <- step(ols)


#############################################
#Check for collinearity and multicollinearity
#############################################
install.packages('corrplot')
library(corrplot)
corrplot(cor(xxx), method = "number") # Display the correlation coefficient

#Checks Multicolinearity
###### variance Inflation Factor：the quotient of the variance in a model with multiple terms by the variance of a model with one term alone
###### It means the inflation of standard error of parameter estimate have due to multicollinearity
#### IF vif>100: strong multicollinearity
#### 100>vif>10: multicollinearity
####     vif<10: slight multicollinearity
####      vif=1: no multic0p  0ollinearity
vif(model)


#############################################
# plot
#############################################
samplesize <- seq(from=10000,to=200000, by=10000)
power.samplesizes <- power.prop.test(n=samplesize,p1=0.03,p2=0.033, sig.level=0.05)$power
plot(samplesizes,
     power.samplesizes,
     xlim=c(0,200000),
     xlab="Sample size",
     ylab="Expected power",
     ylim=c(0,1),
     type="b",
     col="darkorange",
     lwd=5,axes=FALSE)
axis(1,at=c(0,50000,100000,150000,200000))
axis(2,at=c(0,0.25,0.5,0.75,1),labels=paste(c(0,25,50,75,100),"%"))




