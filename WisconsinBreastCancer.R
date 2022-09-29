library("rpart")
library("ggplot2")
library("tidyverse")
library("psych")
library("corrplot")
library("RColorBrewer")
library("caret")

#Load Dataset

trainx=read.csv("/Users/abhinavram/Documents/IDS572 Data Mining/Assignment 1/trainX.csv")
trainy=read.csv("/Users/abhinavram/Documents/IDS572 Data Mining/Assignment 1/trainY.csv")
testx=read.csv("/Users/abhinavram/Documents/IDS572 Data Mining/Assignment 1/testX.csv")
testy=read.csv("/Users/abhinavram/Documents/IDS572 Data Mining/Assignment 1/testY.csv")

#Column Names

colnames(trainx)<-c('radius','texture','perimeter','area','smoothness','compactness','concavity','no_of_concave_contour', 'symmetry', 'fractal_dim', 
                    'radius_sd','texture_sd','perimeter_sd','area_sd','smoothness_sd','compactness_sd','concavity_sd','no_of_concave_contour_sd', 'symmetry_sd', 'fractal_dim_sd',
                    'radius_lv','texture_lv','perimeter_lv','area_lv','smoothness_lv','compactness_lv','concavity_lv','no_of_concave_contour_lv', 'symmetry_lv', 'fractal_dim_lv')
colnames(testx)<-c('radius','texture','perimeter','area','smoothness','compactness','concavity','no_of_concave_contour', 'symmetry', 'fractal_dim', 
                   'radius_sd','texture_sd','perimeter_sd','area_sd','smoothness_sd','compactness_sd','concavity_sd','no_of_concave_contour_sd', 'symmetry_sd', 'fractal_dim_sd',
                   'radius_lv','texture_lv','perimeter_lv','area_lv','smoothness_lv','compactness_lv','concavity_lv','no_of_concave_contour_lv', 'symmetry_lv', 'fractal_dim_lv')

#Summary of dataset

summary(trainx)

#Starting elements of dataset

head(trainx)

#Checking for missing values

sapply(trainx, function(x) sum(is.na(x)))

#Combine trainx and trainy

train_x_y <- cbind(trainx, trainy)
colnames(train_x_y)<-c('radius','texture','perimeter','area','smoothness','compactness','concavity','no_of_concave_contour', 'symmetry', 'fractal_dim', 
                       'radius_sd','texture_sd','perimeter_sd','area_sd','smoothness_sd','compactness_sd','concavity_sd','no_of_concave_contour_sd', 'symmetry_sd', 'fractal_dim_sd',
                       'radius_lv','texture_lv','perimeter_lv','area_lv','smoothness_lv','compactness_lv','concavity_lv','no_of_concave_contour_lv', 'symmetry_lv', 'fractal_dim_lv','diagnosis')

#Box Plot for Outliers and cleaning the data

boxplot(train_x_y)

outliers<- as.data.frame(sapply(train_x_y, function(train_x_y) (abs(train_x_y- mean(train_x_y))/ sd(train_x_y))))
outliers                                                                                               
train_x_y_new <- train_x_y[!rowSums(outliers>3), ]
train_x_y_new

trainx_new = select(train_x_y_new, -diagnosis)
train_x_y_new$diagnosis = as.factor(train_x_y_new$diagnosis)

#Bivariate Analysis

ggplot(data=train_x_y_new, aes(x=diagnosis, y=radius, group = 1)) +
  
  geom_jitter(alpha=0.3,
              color =" blue",
              width = 0.2) +
  labs(title="Breast Cancer Wisconsin Data", x="Diagnosis", y="Radius Mean")

ggplot(data=train_x_y_new, aes(x=radius, fill=diagnosis)) +
  geom_density(alpha=.3)

#Correlation Plot

cor_graph <- cor(trainx)
corrplot(cor_graph, type="upper",order= "hclust", tl.cex = 0.7,col=brewer.pal(n=8, name="RdYlBu"))

#Decision Tree

library(rpart.plot)
fit=rpart(diagnosis~.,data=train_x_y_new, parms = list(split="information"), method = 'class')
rpart.plot(fit, extra = 106)

train_x_y_new$diagnosis <- as.numeric(train_x_y_new$diagnosis)
cor_graph <- cor(train_x_y_new)
corrplot(cor_graph, type="upper",order= "hclust",tl.cex = 0.7,col=brewer.pal(n=8, name="RdYlBu"))

#Confusion Matrix for combined

t_pred=predict(fit, trainx_new, type='class')
table(train_x_y_new$diagnosis, t_pred)
confusion_mat = table(train_x_y_new$diagnosis, t_pred)
acc = sum(diag(confusion_mat))/sum(confusion_mat)

#ggplot

df=data.frame(imp=fit$variable.importance)
df2=df %>%
  tibble::rownames_to_column()%>%
  dplyr::rename("variable"= rowname)%>%
  dplyr::arrange(imp)%>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) + 
  geom_col(aes(x=variable, y=imp), col="black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

#Confusion Matrix for test

t_pred=predict(fit, testx, type='class')
names(testy)=c("diagnosis")

#Accuracy for matrix

fit_test <- rpart(diagnosis~.,data=train_x_y_new, parms = list(split="information"), method = 'class',control=rpart.control(minsplit=5,minbucket=3,cp=0.01))

t_pred=predict(fit_test, testx, type='class')
table(testy$diagnosis, t_pred)
confusion_mat_test = table(testy$diagnosis, t_pred)
accTest = sum(diag(confusion_mat_test))/sum(confusion_mat_test)








