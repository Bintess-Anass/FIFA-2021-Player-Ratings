############################# FIFA 2021 Players value Prediction ########################

######### Introduction #######
#Required Packages
options(warn=-1)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages ("ggplot2" , repos="http://cran.us.r-project.org")
if(!require(scales)) install.packages ("scales" , repos="http://cran.us.r-project.org")
if(!require(dplyr)) install.packages ("dplyr" , repos="http://cran.us.r-project.org")
if(!require(corrplot)) install.packages ("corrplot" , repos="http://cran.us.r-project.org")
if(!require(RCurl)) install.packages ("RCurl" , repos="http://cran.us.r-project.org")
if(!require(hydroGOF)) install.packages ("hydroGOF" , repos="http://cran.us.r-project.org")
if(!require(rpart)) install.packages ("rpart" , repos="http://cran.us.r-project.org")
if(!require(randomForest)) install.packages ("randomForest" , repos="http://cran.us.r-project.org")
if(!require(knitr)) install.packages ("knitr" , repos="http://cran.us.r-project.org")
if(!require(magrittr)) install.packages ("magrittr" , repos="http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages ("Hmisc" , repos="http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages ("gridExtra" , repos="http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages ("rpart.plot" , repos="http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages ("kableExtra" , repos="http://cran.us.r-project.org")

library(tidyverse)
library(readxl)
library(caret)
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(corrplot)
library(RCurl)
library(hydroGOF)
library(rpart)
library(randomForest)
library(knitr)
library(magrittr)
library(Hmisc)
library(gridExtra)
library(rpart.plot)
library(kableExtra)

##### DATA COLLECTION ########

#Read the Excel Document and store it in FIFA 
FIFA <- read_excel("FIFA 2021 Statistics.xlsx")

#Explore Data
head(FIFA)
str(FIFA)
summary(FIFA)


## Visualise Tables
data.frame(Variable = names(FIFA),
           Class = sapply(FIFA, typeof),
           Example = sapply(FIFA, function(x) paste0(head(x),  collapse = ", ")),
           row.names = NULL) %>% 
  kable(format="latex", booktabs=TRUE) %>%
  kable_styling(latex_options=c("scale_down","hold_position"))

#FIFA dataset head
kable(head(FIFA), format="latex", booktabs=TRUE) %>%
  kable_styling(latex_options=c("scale_down","hold_position"))


######### Data cleaning and Processing ########

#Get Value, Total Statistics, Age and Potential as numerical values
FIFA <- FIFA %>% mutate(Value=as.numeric(Value),
                        `Total Stats`=as.numeric(`Total Stats`),
                        Age=as.numeric(Age),
                        Potential=as.numeric(Potential))

#Get Club and Position to factor as categorical
FIFA <- FIFA %>% mutate(Club=as.factor(Club),
                        Position=as.factor(Position))

#Ensure that changes were well made
str(FIFA)

#Remove all NAs
FIFA <- na.omit(FIFA)

#Select only the variables we are interested in (Drop Rating and Nationality)
FIFA <- FIFA %>% select(Name,Age,Club,Position,Potential,Value,`Total Stats`)

#Summary of our data
summary(FIFA)


#Hypothesis: Most expensive players are the highest rated
Most_expensive <- FIFA %>%
  arrange(desc(Value)) %>%
  group_by(Name)%>%
  head(15)

min(Most_expensive$`Total Stats`) #Understand the size range
min(FIFA$`Total Stats`) # Lowest statistics in all FIFA 2021 to compare with


## Ploting expensive players and stats
Most_expensive%>%
  ggplot(aes(Name,Value, color=`Total Stats`, size=`Total Stats`))+
  geom_point()+
  scale_size_continuous(limits=c(1300, 2300), breaks=seq(1300, 2300, by=200))+
  scale_color_continuous(limits=c(1300, 2300), breaks=seq(1300, 2300, by=200), low = "cyan",high = "red") +
  coord_flip()+
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  labs(x="Player", y="Value (Euro)")+
  guides(color= guide_legend(), size=guide_legend())


#Players with highest total statistics
Highest_Stats <- FIFA %>%
  arrange(desc(`Total Stats`)) %>%
  group_by(Name) %>%
  head(15)

#Visualize Highest statistics
kable(Highest_Stats, format="latex", booktabs=TRUE) %>%
    kable_styling(latex_options=c("scale_down","hold_position"))

# Most expensive players per club
Most_expensive %>%
  ggplot(aes(Name,Value, fill=Club))+
  geom_col()+
  coord_flip()+
  labs(x="Player", y ="Value (Euro)")+
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  guides(color=guide_legend())

#Get information about clubs to have more understanding of the data
str(FIFA)
summary(FIFA$Club)

#15 Clubs with most valuable players
FIFA %>% group_by(Club) %>%
  mutate(Club_Value=mean(Value)) %>%
  summarise(Club,Club_Value) %>%
  arrange(desc(Club_Value)) %>%
  distinct() %>%
  head(15) %>%
  ggplot(aes(Club,Club_Value, color=Club, fill=Club))+
  geom_col()+
  guides(color=FALSE, fill=guide_legend(ncol=2))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  labs(x="Club", y="Mean Player Value (Euro)")

#100 Clubs with most valuable players
FIFA %>% group_by(Club) %>%
  mutate(Club_Value=mean(Value)) %>%
  summarise(Club,Club_Value) %>%
  arrange(desc(Club_Value)) %>%
  distinct() %>%
  head(100) %>%
  ggplot(aes(Club,Club_Value, color=Club, fill=Club))+
  geom_col()+
  guides(color=FALSE, fill=FALSE)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  labs(x="Club", y="Mean Player Value (Euro)")

#Mean players value per age
FIFA %>% group_by(Age) %>%
  mutate(Age_Value=mean(Value)) %>%
  summarise(Age,Age_Value) %>%
  arrange(desc(Age_Value)) %>%
  distinct() %>% 
  ggplot(aes(Age,Age_Value, color=Age, fill=Age))+
  geom_col()+
  guides(color=FALSE, fill=FALSE)+
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  labs(x="Age", y="Mean Player Value (Euro)")

#Remove players older than 43
FIFA <- FIFA %>% filter(Age<44) %>% view()

#Most expensive player by position
 FIFA %>%
  arrange(desc(Value)) %>%
  group_by(Name)%>%
  head(20) %>%
  ggplot(aes(Name,Value,fill=Position))+
  geom_col() +
  coord_flip() + 
  labs(x="Player", y="Value (Euro)")+
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  guides(color= guide_legend())

#Most expensive positions per club
FIFA %>%
  ggplot(aes(Position, Value, fill=Club))+
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  guides(color= guide_legend(), fill=FALSE)+
  labs(y="Total Value (Euro)")

#Most expensive per potential
Most_expensive %>%
  ggplot(aes(Name,Potential, fill=Value))+
  geom_col() +
  coord_flip() + 
  guides(color= guide_legend())+
  scale_fill_continuous(label = scales::comma_format(big.mark = '.'))+
  labs(x="Player")

#Get information about potential for players
Most_expensive$Potential
Most_expensive$Potential[which.min(Most_expensive$Potential)]
Most_expensive$Potential[which.max(Most_expensive$Potential)]
FIFA$Potential[which.min(FIFA$Potential)]
FIFA$Potential[which.max(FIFA$Potential)]
mean(FIFA$Potential)

#Mean value of player per its potential
FIFA %>%
  group_by(Potential)%>%
  mutate(Mean_Pot = mean(Value)) %>% 
  summarise(Potential, Mean_Pot) %>% distinct() %>%
  ggplot(aes(Potential,Mean_Pot, fill=Potential)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(big.mark = '.'))+
  guides(color= guide_legend(), fill=FALSE)+
  labs(y="Player Mean value(Euro)")

#Summarizing relations for a player's value
Value_Club <- ggplot(FIFA, aes(x=`Total Stats`, y=log(Value), color=Club))+
  geom_point(size=1, shape=15)+
  geom_smooth(method ="loess") +
  labs(x="Total Statistics" , y="Value")+
  guides(color=FALSE)+
  theme_minimal()

Value_Position <- ggplot(FIFA, aes(x=Position, y=log(Value), color=Potential))+
  geom_point(size=1, shape=15)+
  geom_smooth(method ="loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Position" , y="Value")

summary_Value <- grid.arrange(Value_Club, Value_Position,
                              ncol=1, nrow=2)


################# CORRELATIONS ####################

#Correlation between players value and total statistics
Value_Stat_Cor <- FIFA %>% select("Value","Total Stats")
cor_stat <- cor(Value_Stat_Cor, use="pairwise.complete.obs", method="spearman")
plot_stat <- corrplot.mixed(cor_stat, mar=c(0,0,1,0), lower="color", upper="number", number.cex = 1.5, tl.col = "black")


## Correlation value of clubs and statistics
-
#Get data grouped by club
Value_Clubs <- FIFA %>% select ("Club","Value","Total Stats") %>%
  group_by(Club) %>%
  summarise(Value =sum(Value), Stats =mean(`Total Stats`))

#Check correlations for club grouping
Value_Clubs_cor <- Value_Clubs %>% select ("Value", "Stats")
cor_club <- cor(Value_Clubs_cor, use="pairwise.complete.obs", method="spearman")
plot_club <- corrplot.mixed(cor_club, mar=c(0,0,1,0), lower="color", upper="number", number.cex = 1.5, tl.col = "black")


## Correlation value of age and statistics
#Get data grouped by age
Value_Age <- FIFA %>% select ("Age","Value","Total Stats") %>%
  group_by(Age) %>%
  summarise(Value =sum(Value), Stats =mean(`Total Stats`))

#Check correlations for age grouping
Value_Age_cor <- Value_Age %>% select ("Value", "Stats")
cor_age <- cor(Value_Age_cor, use="pairwise.complete.obs", method="spearman")
plot_age <- corrplot.mixed(cor_age, mar=c(0,0,1,0), lower="color", upper="number", number.cex = 1.5, tl.col = "black")


## Correlation value of position and statistics
#Get data grouped by position
Value_Positions <- FIFA %>% select ("Position","Value","Total Stats") %>%
  group_by(Position) %>%
  summarise(Value =sum(Value), Stats =mean(`Total Stats`))

#Check correlations for position grouping
Value_Positions_cor <- Value_Positions %>% select ("Value", "Stats")
cor_position <- cor(Value_Positions_cor, use="pairwise.complete.obs", method="spearman")
plot_position <- corrplot.mixed(cor_position, mar=c(0,0,1,0), lower="color", upper="number", number.cex = 1.5, tl.col = "black")


## Correlation value of potential and statistics
#Get data grouped by potential
Value_Potential <- FIFA %>% select ("Potential","Value","Total Stats") %>%
  group_by(Potential) %>%
  summarise(Value =sum(Value), Stats =mean(`Total Stats`))

#Check correlations for potential grouping
Value_Potential_cor <- Value_Potential %>% select ("Value", "Stats")
cor_potential <- cor(Value_Potential_cor, use="pairwise.complete.obs", method="spearman")
plot_potential <- corrplot.mixed(cor_potential, mar=c(0,0,1,0), lower="color", upper="number", number.cex = 1.5, tl.col = "black")


############### PREDICTIONS - TRAINING #################

#Create test and validation data sets
# The Validation subset will be 30% of the total dataset.
set.seed(1)
test_index <- createDataPartition(y=FIFA$Value, times = 1, p = 0.3, list = FALSE)
test <- FIFA[-test_index,]
validation <- FIFA[test_index,]

#Verify size of test and validation
dim(test) 
dim(validation)

#Split Test into training set (70%) and test set (30%)
test_index_test <- createDataPartition(y=test$Value, times = 1, p = 0.3, list = FALSE)

train_set <- test[-test_index_test,]
test_set <- test[test_index_test,]

#Verify size of training and test sets
dim(train_set)
dim(test_set)


########### RMSE

# Compute the dataset's mean rating, test the RMSE
# Mean of observed values
set.seed(1)
mu_train <- mean(train_set$Value)
mu_test <- mean(test$Value)

#RMSE Definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Mean RMSE to compare
mean_rmse <- RMSE(mu_train,test_set$Value)
mean_rmse

# Test Results table 
test_rmse_results <- data_frame(Method="Mean",
                            RMSE = mean_rmse )

test_rmse_results %>% knitr::kable() 


####### Linear Regression

set.seed(1)
# Player effect
py <- train_set %>%
  group_by(Name) %>%
  summarise(p_y = mean(Value - mu_train))

y_hat_py <- mu_train + test_set %>%
  left_join(py, by='Name') %>%
  .$p_y

player_rmse <- rmse(y_hat_py, test_set$Value, na.rm=TRUE)
player_rmse

# Test Results table 
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Player Effect",
                                      RMSE = player_rmse ))
test_rmse_results %>% knitr::kable()  
 
# Potential effect
p <- train_set %>% 
  group_by(Potential) %>% 
  summarise(p_t = mean(Value - mu_train))

pred_p <- mu_train + test_set %>% 
  left_join(p, by="Potential") %>%
  .$p_t

mean_rmse_p <- rmse(pred_p, test_set$Value, na.rm=TRUE)
mean_rmse_p

# Test Results table 
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Potential Effect",
                                      RMSE = mean_rmse_p))
test_rmse_results %>% knitr::kable() 

# Player with Potential effect
bc <- train_set %>% 
  left_join(py, by = "Name") %>%
  group_by(Potential) %>% 
  summarise(b_c = mean(Value- mu_train-p_y))

pred_bc <- mu_train + test_set %>% 
  left_join(py, by='Name') %>%
  left_join(bc, by="Potential") %>%
  mutate(pred = mu_train + b_c + p_y) %>%
  .$pred

pp_rmse <- rmse(pred_bc, test_set$Value, na.rm=TRUE)
pp_rmse

# Test Results table 
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Player and potential Effect",
                                      RMSE = pp_rmse ))
test_rmse_results %>% knitr::kable()

#Adding Age effect
bu <- train_set %>% 
  left_join(py, by = "Name") %>%
  left_join(bc, by = "Potential") %>%
  group_by(Age) %>% 
  summarise(b_u = mean(Value - mu_train - p_y - b_c))

pred_bu <- mu_train + test_set %>% 
  left_join(py, by='Name') %>%
  left_join(bc, by="Potential") %>%
  left_join(bu, by="Age") %>%
  mutate(pred_u = mu_train + b_u + b_c + p_y) %>%
  .$pred_u

Age_rmse <- rmse(pred_bu, test_set$Value, na.rm=TRUE)
Age_rmse

# Test Results table 
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Player with potential and age Effect",
                                      RMSE = Age_rmse ))
test_rmse_results %>% knitr::kable()  

#Adding Total Statistics effect
ba <- train_set %>%
  left_join(py, by = "Name") %>%
  left_join(bc, by = "Potential") %>%
  left_join(bu, by = "Age") %>%
  group_by(`Total Stats`) %>% 
  summarise(b_a = mean(Value- mu_train - p_y - b_c -b_u))

pred_ba <- mu_train + test_set %>% 
  left_join(py, by='Name') %>%
  left_join(bc, by="Potential") %>%
  left_join(bu, by="Age") %>%
  left_join(ba, by="Total Stats") %>%
  mutate(pred_age = mu_train + b_a + b_c + b_u + p_y) %>%
  .$pred_age

stats_rmse <- rmse(pred_ba, test_set$Value, na.rm=TRUE)
stats_rmse

# Test Results table 
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Player, potential, age, and total statistics Effect",
                                      RMSE = stats_rmse ))
test_rmse_results %>% knitr::kable()  


######## Decision Tree
set.seed(1)
rpart.control <- rpart.control(minbucket = 1, maxcompete = 4, maxsurrogate = 6, 
                               usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdepth = 20, na.action = na.exclude) 

#training the tree model
tree <- rpart(train_set$Value~ .,data = train_set[,-c(1)],control=rpart.control)
plot(tree, margin = 0.1)
text(tree, cex = 0.75)

printcp(tree)

#Vizualize the tree
rpart.plot(tree, type = 2, box.palette ="RdYlGn", shadow.col ="gray", nn=TRUE,roundint=FALSE)

#RMSE of tree
pred_values_tree = predict(tree,test_set)
tree_rmse <- rmse(test_set$Value,pred_values_tree)
tree_rmse

# Test Results table
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Decision tree",
                                      RMSE = tree_rmse))
test_rmse_results %>% knitr::kable()  


#######Random Forest
#Make variables legal (otherwise the Random forest shows errors)
names(train_set) <- make.names(names(train_set))
names(test_set) <- make.names(names(test_set))

#Make Club character instead of factor as random forest cannot handle categorical predictors with more than 53 categories
train_set$Club <- as.character(train_set$Club)
test_set$Club <- as.character(test_set$Club)

#Training the model
set.seed(1)
rf_model<-randomForest(Value~ ., train_set, ntree=500, importance=TRUE, proximity=TRUE )
rf_model
plot(rf_model)

#Best variables that were important in  modeling 
varImpPlot(rf_model)

#Higher IncNodePurity values indicate more impact on value
pred_values_forest = predict(rf_model,test_set)

forest_rmse <- rmse(test_set$Value,pred_values_forest)
forest_rmse

# Test Results table
test_rmse_results <- bind_rows(test_rmse_results,
                           data_frame(Method="Random Forest",
                                      RMSE = forest_rmse))
test_rmse_results %>% knitr::kable()  


##################Results and Validation set

#From testing in the earlier section , I chose to apply the validation data on Total stats and potential effect in linear model
set.seed(1)
#################################### Mean RMSE to start with ########################################
validation_rmse <- RMSE(mu_test, validation$Value)
validation_rmse

# Results table 
rmse_results <- data_frame(Method="Mean",
                            RMSE= validation_rmse/1000000)

rmse_results %>% knitr::kable()  

######################################## Total Statistics effect -Linear regression ############################################
bts <- test %>%
  group_by(`Total Stats`) %>%
  summarise(b_ts=mean(Value-mu_test))

pred_bts <- mu_test + validation %>%
  left_join(bts, by="Total Stats") %>%
  .$b_ts

bts_mean_rmse <- rmse(pred_bts, validation$Value, na.rm=TRUE)
bts_mean_rmse

# Results table 
rmse_results <- bind_rows(rmse_results,
                           data_frame(Method="Total Stats Effect - Linear regression",
                                      RMSE = bts_mean_rmse/1000000))
rmse_results %>% knitr::kable()  

########################################### Potential effect -Linear regression ##############################################
bp <- test %>%
  group_by(Potential) %>%
  summarise(b_p =mean(Value-mu_test))

pred_bp <- mu_test + validation %>%
  left_join(bp, by="Potential") %>%
  .$b_p

bp_mean_rmse <- rmse(pred_bp, validation$Value, na.rm=TRUE)
bp_mean_rmse

# Results table 
rmse_results <- bind_rows(rmse_results,
                           data_frame(Method="Potential Effect - Linear regression",
                                      RMSE = bp_mean_rmse/1000000))
rmse_results %>% knitr::kable()  

####################################### Decision tree ####################################################
set.seed(1)
Final_rpart.control <- rpart.control(minbucket = 1, maxcompete = 4, maxsurrogate = 6, 
                               usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdepth = 20, na.action = na.exclude) 

#Training the tree model
Final_Tree <- rpart(test$Value~ .,data = test[,-c(1)],control=Final_rpart.control)
plot(Final_Tree, margin = 0.1)
text(Final_Tree, cex = 0.75)

printcp(Final_Tree)

#Visualize the tree
rpart.plot(Final_Tree, type = 2, box.palette ="RdYlGn", shadow.col ="gray", nn=TRUE,roundint=FALSE)

#summary(tre)
Final_pred_values_tree = predict(Final_Tree,validation)
Final_tree_rmse <- rmse(validation$Value,Final_pred_values_tree)
Final_tree_rmse

#Results table
rmse_results <- bind_rows(rmse_results,
                           data_frame(Method="Decision tree",
                                      RMSE = Final_tree_rmse/1000000))
rmse_results %>% knitr::kable()  

###################################################   Random Forest   ####################################################

#Make variables legal (otherwise the Random forest shows errors)
names(test) <- make.names(names(test))
names(validation) <- make.names(names(validation))

#Make Club character instead of factor as random forest cannot handle categorical predictors with more than 53 categories
test$Club <- as.character(test$Club)
validation$Club <- as.character(validation$Club)

set.seed(1)
Final_rf_model<-randomForest(Value~ ., test, ntree=500, importance=TRUE, proximity=TRUE )
Final_rf_model
plot(Final_rf_model)

#Most important variables in modeling
varImpPlot(Final_rf_model)

Final_pred_values = predict(Final_rf_model,validation)

Final_forest_rmse <- rmse(validation$Value,Final_pred_values)
Final_forest_rmse

#Results table
rmse_results <- bind_rows(rmse_results,
                           data_frame(Method="Random Forest",
                                      RMSE = Final_forest_rmse/1000000))
rmse_results %>% knitr::kable()  


# Gather RMSE Results to be shown in a table
kable(rmse_results, format="latex", booktabs=TRUE) %>%
    kable_styling(latex_options=c("hold_position"))


## Plot Decision Tree Results
rpart.plot(Final_Tree, type = 2, box.palette ="RdYlGn", shadow.col ="gray", nn=TRUE,roundint=FALSE)


## Plot Random Forest Results
varImpPlot(Final_rf_model)


## Checking some specific effects on the linear regression
# Potential effect
pot <- test %>% 
group_by(Potential) %>% 
summarise(pot_ = mean(Value - mu_test))

pred_pot <- mu_test + validation %>% 
left_join(pot, by="Potential") %>%
 .$pot_
 
mean_rmse_pot <- rmse(pred_pot, validation$Value, na.rm=TRUE)
mean_rmse_pot

#Age Effect
old <- test %>% 
group_by(Age) %>% 
summarise(old_ = mean(Value - mu_test))

pred_old <- mu_test + validation %>% 
left_join(old, by="Age") %>%
 .$old_
 
mean_rmse_old <- rmse(pred_old, validation$Value, na.rm=TRUE)
mean_rmse_old