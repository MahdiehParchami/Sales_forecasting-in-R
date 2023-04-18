


# install.packages("readr")
# install.packages('readxl')

library(readr)
library(readxl)
df <- read_excel("D:/Retails_dataset.xlsx")

# EDA part
str(df)
summary(df)


# install.packages("dplyr")
library(dplyr)
glimpse(df)

head(df)
tail(df)

# check for missing value

sum(is.na(df))

# #see miss value
# install.packages("visdat")

library(visdat)
vis_miss(df)


missing_row <- nrow(df)- sum(complete.cases(df))
missing_row 

missing_coulumn <- unlist(lapply(df, function(x) any(is.na(x))))
missing_coulumn


# install.packages("purrr")

library(purrr)

map_int(df, function(.x) sum(is.na(.x)))



# # check for duplicate
sum(duplicated(df))



# change variables name
df1 <- rename(df,TARGET= "TARGET VARIABLE" , BCI = "EMEA, Business Confidence Indicator (BCI)",
              CLI = "EMEA, CLI Normalized" , CCI = "EMEA, Consumer Confidence Indicator (CCI)" ,
              COP = "EMEA, Crude oil prices" , ER = "EMEA, Employment Rate" , GDP = "EMEA, GDP Normalized",
              GIBC = "EMEA, Germany ifo Business Climate" , IBE = "EMEA, ifo Business Expectations",
              IBS = "EMEA, ifo Business Situation" , PTM = "EMEA, Production in total manufacturing Index",
              PTC = "EMEA, Production of total construction Index" , PTI = "EMEA, Production of total industry Index",
              PTMG = "EMEA, Production of total manufactured intermediate goods Index",
              PTMIG = "EMEA, Production of total manufactured investment goods Index" ,
              RPSNBD = "EMEA, Residential Property Sales of Newly Built Dwelings" , 
              VA = "EMEA, VDMA Agriculture" , VC = "EMEA, VDMA Construction" ,
              VMB = "EMEA, VDMA Machine Building" , VMH = "EMEA, VDMA Material Handling" ,
              VOH = "EMEA, VDMA Oil Hydraulic" , PMI = "EMEA, PMI")

head(df1)
tail(df1)

summary(df1)


#  treat missing values

df1$ER <- ifelse(is.na(df1$ER),mean(df1$ER,na.rm = TRUE),df1$ER)
df1$PTMG <- ifelse(is.na(df1$PTMG),median(df1$PTMG,na.rm = TRUE),df1$PTMG)
df1$PTMIG <- ifelse(is.na(df1$PTMIG),median(df1$PTMIG,na.rm=TRUE),df1$PTMIG)
df1$RPSNBD <- ifelse(is.na(df1$RPSNBD),mean(df1$RPSNBD,na.rm=TRUE),df1$RPSNBD)
df1$COP <- ifelse(is.na(df1$COP),median(df1$COP,na.rm=TRUE),df1$COP)
df1$PTM <- ifelse(is.na(df1$PTM),median(df1$PTM,na.rm=TRUE),df1$PTM)
df1$PTC <- ifelse(is.na(df1$PTC),mean(df1$PTC,na.rm=TRUE),df1$PTC)
df1$PTI <- ifelse(is.na(df1$PTI),mean(df1$PTI,na.rm=TRUE),df1$PTI)
df1$VA <- ifelse(is.na(df1$VA),median(df1$VA,na.rm=TRUE),df1$VA)
df1$VC <- ifelse(is.na(df1$VC),median(df1$VC,na.rm=TRUE),df1$VC)
df1$VMB <- ifelse(is.na(df1$VMB),median(df1$VMB,na.rm=TRUE),df1$VMB)
df1$VMH <- ifelse(is.na(df1$VMH),mean(df1$VMH,na.rm=TRUE),df1$VMH)
df1$VOH <- ifelse(is.na(df1$VOH),median(df1$VOH,na.rm=TRUE),df1$VOH)

# check for missing value

sum(is.na(df1))
library(visdat)
vis_miss(df1)

# Check for distribution
# 
library(Hmisc)
par(mfrow=c(2,4))

hist.data.frame <- function(x, ..., colors=rainbow(ncol(x))) {
  col<-1
  hist<-function(...) {
    graphics::hist(..., col=colors[col])
    col <<- col+1
  }
  f <- Hmisc:::hist.data.frame
  environment(f) <- environment()
  f(x,...)
}

hist(df1[,3:24])

# # Outliers Findings
# 
# # install.packages("tidyverse")
# 
library(tidyverse)
# 
# 
par(mfrow=c(2,2))
for(i in 4:ncol(df1)) {
  color <-which(colnames(df1) == names(df1[,i]))
  boxplot(df1[,i],  main= paste("Outliers of contributive Variable ",names(df1)[i]), 
          col = color , outpch=25, outbg="green")}
# 
# 
# 
# # correlation plot

library(ggcorrplot)
corr <- round(cor(df1[c(-1,-2)]), 1)
ggcorrplot(corr , hc.order = TRUE, type = "lower",
           lab = TRUE)

# Scatter plot

par(mfrow=c(2,4))
for(i in 4:ncol(df1)) {
  x <- df1[,i]
  colnames(x) <- "v"
  color <-which(colnames(df1) == names(df1[,i]))
  
  plot( x$v,df1$TARGET,ylab ="Target" , xlab = names(df1[,i]),col = color,
        main= paste("Relationship between Taget and ",names(df1)[i]))
}

# line plot

dev.off()

df7 <- df1 %>% 
  group_by(Year) %>% 
  summarise(unique(ave(TARGET)))

df8 <- rename(df7 , Target= `unique(ave(TARGET))`)

plot(df8$Year,df8$Target, type = "l", lty = 1 , col = "green",lwd=3.0 , xlab = "Years" , 
     ylab = "Sales",
     main = "Danfoss's sales between 2008 to 2022")

# ****************************************************************************

# Perform Machine learning method
# Random forest


# install.packages("randomForest")

library(randomForest)
library(caret)
library(e1071)


# **********************************************************************


#random forest

# Create train and test data

library(caTools)
set.seed(12345)
split <- sample.split(df1$TARGET, SplitRatio = 0.7)


train <- subset(df1, split == "TRUE")
test<- subset(df1, split == "FALSE")

# Define the control

library(randomForest)
library(caret)
library(e1071)

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


set.seed(1234)

# Run the model
rf_default <- train(TARGET~.,
                    data = train,
                    method = "rf",
                    metric = "RMSE",
                    trControl = trControl)

# Print the results
print(rf_default)


# Search best mtry

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 23))
rf_mtry <- train(TARGET~.,
                 data = train,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE
                 
)
print(rf_mtry)

# Best RMSE
min(rf_mtry$results$RMSE)

# Best mtry
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry


# Search the best maxnodes

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(15: 30)) {
  set.seed(1234)
  rf_maxnode <- train(TARGET~.,
                      data = train,
                      method = "rf",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)

summary(results_mtry) # 20

## Search the best ntrees

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 
                2000,3000,3500,4000,4500,5000,6000)) {
  set.seed(5678)
  rf_maxtrees <- train(TARGET~.,
                       data = train,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 20,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) # 600

# we have our final model. we can train the random forest with the following parameters:
#   
# ntree =600: 600 trees will be trained
# mtry= 8: 8 features is chosen for each iteration
# maxnodes = 20: Maximum 28 nodes in the terminal nodes (leaves)

# perform final model  


fit_rf <- train(TARGET~.,
                data = train,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 600,
                maxnodes = 20)

print(fit_rf)

prediction1 <-predict(fit_rf, test)

prediction1

# Visualize Result

fit_rf1 <- randomForest(TARGET~.,
                        data = train,
                        method = "rf",
                        metric = "RMSE",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        ntree = 600,
                        maxnodes = 20)

print(fit_rf1)


prediction <-predict(fit_rf1, test)

imp <- varImpPlot(fit_rf1)


# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- rep(1, 23) # random var category

# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 

ggplot(imp, aes(x=reorder(varnames, IncNodePurity), y=IncNodePurity, color=as.factor(var_categ))) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncNodePurity)) +
  scale_color_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()+
  labs(title="Variable Importance plot")

# perform for dataset


fit_rf_final <- train(TARGET~.,
                      data = df1,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      ntree = 600,
                      maxnodes = 20)

print(fit_rf_final)

# predict df1 data set- main data set
prediction2 <-predict(fit_rf_final, df1)
prediction2


#***********************************************************************

#XGBoost forest

# Create train and test data


#install.packages("xgboost")
library(xgboost) #for fitting the xgboost model
library(caret)   #for general data preparation and model fitting
library(caTools)

set.seed(12345)

#split into training (80%) and testing set (20%)

parts = createDataPartition(df1$TARGET, p = .8, list = F)
train = df1[parts, ]
test = df1[-parts, ]


#define predictor and response variables in training set
train_x = data.matrix(train[, -3])
train_y = train[,3]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -3])
test_y = test[,3]
xgb_all_x = data.matrix(df1[,-3])
xgb_all_y = data.matrix(df1[,3])

#define final training and testing sets


xgb_train = xgb.DMatrix(data = train_x, label = train_y$TARGET)
xgb_test = xgb.DMatrix(data = test_x, label = test_y$TARGET)
xgb_all = xgb.DMatrix(data = xgb_all_x , label= xgb_all_y)


#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)



# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(xgb_train), model = final)
importance_matrix

# plot
# Load library

# install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)

imp_matrix  <- xgb.plot.importance(importance_matrix[1:6,])


xgb.ggplt<-xgb.ggplot.importance(importance_matrix = imp_matrix, top_n = 6)
# increase the font size for x and y axis
xgb.ggplt+theme( text = element_text(size = 15),
                 axis.text.x = element_text(size = 15, angle = 45, hjust = 1))

# predict df1 data set- main data set

pred_y_all = predict(final, xgb_all)
pred_y_all
pred_y_all <- as.data.frame(pred_y_all)





