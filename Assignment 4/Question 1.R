# Import library
library(readxl) # read xlsx file
library(DataExplorer) # correlation plot
library(reshape2) # data reshape for ggplot
library(tidymodels) # model fitting
library(vip) # variable importance plot

# Import dataset
raisin <- read_xlsx("Raisin_Dataset/Raisin_Dataset.xlsx") %>% # read dataset
  as.data.frame() %>% # change to data.frame type
  mutate(across(Class, factor)) %>% # change the target variable to factor
  mutate(across(where(is.numeric), scale)) # scale the numeric variables

# Investigation
# Textual
dim(raisin) # 900 observations and 8 variables (7 predictors and 1 target)
str(raisin) # data type of each variable
colSums(is.na(raisin)) # there is no missing values

# There are 50% Besni raisin and 50% of Kecimen raisin, balanced data
raisin %>% 
  group_by(Class) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# Graphical
# A correlation plot
raisin %>% 
  plot_correlation()

# Boxplots on all numeric variables
raisin %>% 
  melt(id='Class') %>% # change to long format for plotting
  ggplot(aes(x = variable, y = value, color = Class)) + 
  geom_boxplot() + # boxplots
  ggtitle("Boxplot of each numeric predictor") # add a title

# Concluding from both the correlation plot and the boxplot, the variable Extent 
# does not seems to distinguish two types of raisin well.
# Therefore consider removing it from model training.

# Create train and test set
set.seed(120)
data_split <- initial_split(raisin, prop = 2/3, strata = Class)
train_raisin <- training(data_split) # 600 training data
test_raisin <- testing(data_split) # 300 testing data

# Recipe
raisin_recipe <- recipe(Class ~ ., train_raisin) %>% 
  # remove variables with zero variances
  step_nzv(all_predictors(), freq_cut = 0, unique_cut = 0) %>%
  # remove Extent variable
  step_rm(Extent)

# Classification tree model
# Reason: the target variable Class is a categorical variable with 2 levels:
# Besni and Kecimen, therefore a classification tree model is more appropriate.

# 10-fold cross validation
raisin_cv <- vfold_cv(train_raisin, v = 10, strata = Class)

# Specify a decision tree model
raisin_tree <- decision_tree() %>% # use decision tree model
  set_mode("classification") %>% # classification tree
  set_engine("rpart") # use rpart package

# Tune parameters
raisin_tree <- raisin_tree %>%
  set_args(cost_complexity = tune(),
           tree_depth = tune(),
           min_n = tune()) # add tuning

# Define workflow
raisin_flow <- workflow() %>% 
  add_recipe(raisin_recipe) %>%
  add_model(raisin_tree)

# Tune the variables and set a grid of values to test out
raisin_grid <- grid_regular(cost_complexity(range = c(-5, -1)),
                            tree_depth(range = c(3, 6)), 
                            min_n(range = c(5, 10)), levels = 4)
tree_rs <- tune_grid(
  raisin_flow,
  Class ~ .,
  resamples = raisin_cv,
  grid = raisin_grid,
  metrics = metric_set(accuracy))

# Determine the best model
best_model <- select_best(tree_rs)
best_model

# Update the workflow with the best model
raisin_tree_best <- finalize_workflow(raisin_flow, best_model)

# Fit the best model
raisin_fit <- fit(raisin_tree_best, data = train_raisin)
raisin_fit

# Prediction
# Confusion matrix
augment(raisin_fit, new_data = test_raisin) %>%
  conf_mat(truth = Class, estimate = .pred_class)

# Mis-classification rate = 1 - accuracy
augment(raisin_fit, new_data = test_raisin) %>%
  accuracy(truth = Class, estimate = .pred_class)

# Tree details
raisin_fit %>%
  extract_fit_engine() %>%
  rattle::fancyRpartPlot()

# Variable importance
raisin_fit %>% 
  extract_fit_parsnip() %>% 
  vip()