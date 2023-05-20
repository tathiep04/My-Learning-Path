install.packages("dplyr")
install.packages("caret")
install.packages("readr")
install.packages("reshape2")
library("reshape2")
library("ggplot2")
library("dplyr")
library("caret")
library("readr")

###STEP1
data <- read.csv(file.choose(), header=T)
dim(data) #data.shape
head(data) #data.head()
str(data) #equivalent to data.dtypes in Python
summary(data) #data.describe()

###STEP2
sapply(data, class)
data <- subset(data, select = c('Survived', 'Pclass', 'Sex',
                                'Age', 'SibSp', 'Parch', 'Fare', 'Embarked'))
head(data)
colSums(is.na(data)) #Check NA values, equivalent data.isna().sum()
#return average age.
aggregate(Age ~ Pclass, data = data, FUN = mean)

#
fill_age <- function(Age, Pclass) {
  if (is.na(Age)) {
    if (Pclass == 1) {
      return(38)
    } else if (Pclass == 2) {
      return(30)
    } else {
      return(25)
    }
  } else {
    return(Age)
  }
}
data$Age <- mapply(fill_age, data$Age, data$Pclass)
## 
data <- data %>% 
  mutate(Embarked = case_when(
    Embarked == 'S' ~ 0,
    Embarked == 'C' ~ 1,
    Embarked == 'Q' ~ 2,
    TRUE ~ NA_real_
  ))
colSums(is.na(data))
data[is.na(data$Embarked), ]
#boxplot(Fare ~ Embarked, data = data)
data$Embarked <- ifelse(is.na(data$Embarked), 1, data$Embarked)
colSums(is.na(data))

data <- data %>%
  mutate(Sex = ifelse(Sex == "male", 1, 0))
colSums(is.na(data))

###STEP 3
summary(data)
# Plot histograms for all columns
par(mfrow=c(3,3))
for(i in 1:ncol(data)){
  hist(data[,i], main=names(data)[i], col="red", border="white", xlab="")
}



# Calculate the correlation matrix
df_corr <- cor(data)
melted_cor <- melt(df_corr)

ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label = round(value,2)), color = "white") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, space="Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  labs(title="Correlation Plot")
