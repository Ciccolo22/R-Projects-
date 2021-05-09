##load packages
library(correlationfunnel) ##EDA
library(AER) ##Data
library(tidymodels) ##data preprocessing
library(h2o)##ML model building 
library(forcats)## categorical munging
library(DALEX)  ## ML explain
library(DALEXtra) # ML explain
library(modelStudio) #html report for ml explain

##Load data from AER package, use affairs dataset for binary classification problem##
##will use h2o to create a some quick models and modelstudio for explainability

data('Affairs')

##quick glance at dataset
glimpse(Affairs)

#explore response tally
table(Affairs$affairs)

## bin response variable
Affairs$affairs <- ifelse(Affairs$affairs==0,0,1)


## wrangling
Affairs$religiousness <- as.factor(Affairs$religiousness)
Affairs$rating <- as.factor(Affairs$rating)
Affairs$occupation <- as.factor(Affairs$occupation)
levels(Affairs$occupation)
Affairs$education <-  as.factor(Affairs$education)
apply(is.na(Affairs),2,sum)

## wrangle
Affairs$religiousness <- fct_recode(Affairs$religiousness, "slightly"='3')
Affairs$religiousness <- fct_recode(Affairs$religiousness, "anti"='1', 'not at all'='2',
                                    'somewhat'='4','very'='5')
Affairs$rating <- fct_recode(Affairs$rating, "very unhappy"='1', 'somewhat unhappy'='2', 'average'='3',
                                    'happier than average'='4','very happy'='5')


Affairs$occupation <- fct_recode(Affairs$occupation, "laborer"='1', 'unskilled worker'='2', 'semiskilled worker'='3',
                             'skilled manual worker'='4','clerical sales'='5', 'semiprofessional'='6',
                             'salaried worker'='7')

Affairs$affairs <- as.factor(Affairs$affairs)

##correlation funnel EDA -- not any super strong correlations
options(ggrepel.max.overlaps = Inf)

Affairs%>%
  binarize()%>%
  correlate(affairs__1)%>%
  plot_correlation_funnel(interactive = FALSE)


##split data for ML models (tidymodels for preprocessing)

data_split <- initial_split(Affairs, prop = .80, strata = affairs)
training_data <- training(data_split)
test_data <- testing(data_split)


##create my recipe using tidy framework for preprocessing
affairs_rec <- recipe(affairs~., data=training_data)%>%
  step_dummy(all_nominal(), -affairs, -all_numeric())%>%
  step_center(all_predictors())%>%
  step_scale(all_predictors())

#prep recipe  
trained_rep <- prep(affairs_rec, training = training_data)
trained_rep

#create training and test with all preprocessing
train_data_real <- bake(trained_rep, new_data = training_data)
test_data_real <- bake(trained_rep, new_data = test_data)


##create some folds for resampling to use later

folds <- vfold_cv(training_data, v = 5, repeats = 1)

##create model using tidy framework (to be used later)

xgmod <- boost_tree()%>%
  set_engine('xgboost')%>%
  set_mode('classification')


xgflow <- 
  workflow() %>%
  add_model(xgmod) %>%
  add_recipe(affairs_rec)

str(train_data_real)

##try h2o framework for quick and dirty model, setup my data in h2o
h2o.init()

h2o.trained <- as.h2o(train)
h2o.test <- as.h2o(test_data_real)

train <- as.data.frame(h2o.trained)
test <- as.data.frame(h2o.test)

train$affairs <- as.factor(train$affairs)
test$affairs <- as.factor(test$affairs)


y <- 'affairs'
x <- setdiff(names(h2o.trained),y)


##train basic model
aml <- h2o.automl(x=x, y=y, training_frame = h2o.trained, balance_classes = TRUE, max_runtime_secs = 30, seed = 146)

lb <- aml@leaderboard
print(lb, n=nrow(lb))

##if I want to pull an extra model other than the leader, do it this way
model_ids <- as.vector(aml@leaderboard$model_id)

index <- 2

##Extract Model 2
model_2 <- h2o.getModel(model_ids[2])
model_1 <- aml@leader


#predict against holdout
perf <- h2o.performance(model_2, h2o.test)
perf2 <- h2o.performance(model_1, h2o.test)

## evaluate results of the model -- not great... 
perf


##trying out DALEX explainers and modelstudio interpretablemL
test$affairs <- as.numeric(test$affairs)

##create explainer in h2o
explainer <- explain_h2o(model_2, data=test[,-3], y=test$affairs, label = 'h2o')

##create new observations for evaluations
new_observation <- test[1:2,]
rownames(new_observation) <- c("id1", "id2")


##create model report
modelStudio(explainer, new_observation,
            B = 5)

##breakdown plot exploration
bd_try <- predict_parts_break_down(explainer,new_observation)

bd_try
plot(bd_try)

model_performance(explainer)
model_performance(explainer2)
