library(tidymodels)
data(ames)

# View data ----
# Plot Sale Price
ames %>% 
  ggplot(aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white")

# Log Transform Sale Price
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# Plot transformed sale price
ames %>%
  ggplot(aes(x = Sale_Price)) +
  geom_histogram()

# End of chapter 4 code ----
library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# Split data ----
# Split data into training and testing data
set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

# End of chapter 5 code ----
library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

# Create model ----
# Predicting sales price
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit

# View model results ----

model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
param_est

# View results in tidy format
tidy(lm_form_fit)

# End of chapter 6 code ----
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

# Creating a workflow ----
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow

# Add preprocessor
lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

# Fit
lm_fit <- fit(lm_wflow, ames_train)
lm_fit

# Predict
predict(lm_fit, ames_test %>% slice(1:3))

# Update preprocessor
lm_fit %>% update_formula(Sale_Price ~ Longitude)

# Add variables to the workflow ----
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))
lm_wflow

# Special formulas ----
library(lme4)
library(nlme)

lmer(distance ~ Sex + (age | Subject), data = nlme::Orthodont)

library(multilevelmod)

multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = nlme::Orthodont)
multilevel_fit

# Workflow sets ----
library(workflowsets)

location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models

location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models

# Evaluating the test set ----
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)
collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)

# End of chapter 7 code ----
library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow, ames_train)

# Creating a recipe ----
library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames

# Adding a recipe to the workflow
lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>%  # Remove existing preprocessor
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:3))

# Get the recipe after it has been estimated:
lm_fit %>% 
  extract_recipe(estimated = TRUE)

# To tidy the model fit: 
lm_fit %>% 
  # This returns the parsnip object:
  extract_fit_parsnip() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)

# Create a new "other" level for neighbourhoods
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>%  # Groups bottom 1% of neighbourhoods
  step_dummy(all_nominal_predictors())

# Plot relationship gross living area and building type
ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")

# Create an interaction term using step_interact
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

# Create an extended recipe to tidy
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

tidy(ames_rec)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

estimated_recipe <- 
  lm_fit %>% 
  extract_recipe(estimated = TRUE)

tidy(estimated_recipe, id = "my_id")

tidy(estimated_recipe, number = 2)


# Creating roles
# ames_rec %>% update_role(address, new_role = "street address")

# End of chapter 8 code ----
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

# Produce predictions and evaluate results
ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res

ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

# RMSE
rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

# Binary classification performance metrics - hard class predictions
data(two_class_example)
tibble(two_class_example)

## A confusion matrix: 
conf_mat(two_class_example, truth = truth, estimate = predicted)

## Accuracy:
accuracy(two_class_example, truth, predicted)

## Matthews correlation coefficient:
mcc(two_class_example, truth, predicted)

## F1 metric:
f_meas(two_class_example, truth, predicted)

## Combining these three classification metrics together
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

# Binary classification performance metrics - predicted probabilities

two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve

roc_auc(two_class_example, truth, Class1)

autoplot(two_class_curve)

# Multiclass performance metrics - hard class predictions
data(hpc_cv)
tibble(hpc_cv)

accuracy(hpc_cv, obs, pred)
mcc(hpc_cv, obs, pred)

sensitivity(hpc_cv, obs, pred, estimator = "macro")
sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")
sensitivity(hpc_cv, obs, pred, estimator = "micro")

# Multiclass performance metrics - predicted probabilities
roc_auc(hpc_cv, obs, VF, F, M, L)

roc_auc(hpc_cv, obs, VF, F, M, L, estimator = "macro_weighted")

hpc_cv %>% 
  group_by(Resample) %>% 
  accuracy(obs, pred)

# Four 1-vs-all ROC curves for each fold
hpc_cv %>% 
  group_by(Resample) %>% 
  roc_curve(obs, VF, F, M, L) %>% 
  autoplot()

# Fitting a random forest
library(ranger)
rf_model <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wflow <-
  workflow() %>%
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
      Latitude + Longitude) %>%
  add_model(rf_model)

rf_fit <- rf_wflow %>% fit(data = ames_train)

# Comparing the linear model to the random forest model
estimate_perf <- function(model, dat) {
  # Capture the names of the `model` and `dat` objects
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)

  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)

  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(Sale_Price)) %>%
    reg_metrics(Sale_Price, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}

estimate_perf(rf_fit, ames_train)

estimate_perf(lm_fit, ames_train)

# Cross validation
set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds

# For the first fold:
ames_folds$splits[[1]] %>% analysis() %>% dim()

# Creating a validation set
set.seed(1002)
val_set <- validation_split(ames_train, prop = 3/4)
val_set

# Create a bootstrap sample
bootstraps(ames_train, times = 5)

# Rolling forecasting origin resampling
time_slices <-
  tibble(x = 1:365) %>%
  rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)

data_range <- function(x) {
  summarize(x, first = min(x), last = max(x))
}

map_dfr(time_slices$splits, ~   analysis(.x) %>% data_range())

map_dfr(time_slices$splits, ~ assessment(.x) %>% data_range())

# Fitting using resamples
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- 
  rf_wflow %>% 
  fit_resamples(resamples = ames_folds, control = keep_pred)
rf_res

# Tidy metrics
collect_metrics(rf_res)

# Tidy predictions
assess_res <- collect_predictions(rf_res)
assess_res

# Visualize predictions
assess_res %>% 
  ggplot(aes(x = Sale_Price, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")

# View the over predicted values
over_predicted <- 
  assess_res %>% 
  mutate(residual = Sale_Price - .pred) %>% 
  arrange(desc(abs(residual))) %>% 
  slice(1:2)
over_predicted

ames_train %>% 
  slice(over_predicted$.row) %>% 
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)

# Validation set
val_res <- rf_wflow %>% fit_resamples(resamples = val_set)
val_res

collect_metrics(val_res)

# Parallel processing

# The number of physical cores in the hardware:
parallel::detectCores(logical = FALSE)

# The number of possible independent processes that can 
# be simultaneously used:  
parallel::detectCores(logical = TRUE)

# Unix and macOS only
library(doMC)
registerDoMC(cores = 2)

# Now run fit_resamples()...

# To reset the computations to sequential processing
registerDoSEQ()

# All operating systems
library(doParallel)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

# Now run fit_resamples()`...

stopCluster(cl)

# Preserving model components during resampling
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <-  
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(linear_reg() %>% set_engine("lm")) 

lm_fit <- lm_wflow %>% fit(data = ames_train)

# Select the recipe: 
extract_recipe(lm_fit, estimated = TRUE)

# Create the function that will be used in extract
get_model <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

# Create control_resamples with the newly created function
ctrl <- control_resamples(extract = get_model)

lm_res <- lm_wflow %>%  fit_resamples(resamples = ames_folds, control = ctrl)
lm_res

# View the extracts column
lm_res$.extracts[[1]]

# To get the results
lm_res$.extracts[[1]][[1]]

# All results flattened and collected
all_coef <- map_dfr(lm_res$.extracts, ~ .x[[1]][[1]])

filter(all_coef, term == "Year_Built")


# End of chapter 10 code ----
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)

# Creating multiple models with workflow sets
library(tidymodels)
tidymodels_prefer()

basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, list(lm = linear_reg()), cross = FALSE)
lm_models

# Resampling each of the models in turn
lm_models <- 
  lm_models %>% 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               seed = 1101, verbose = TRUE,
               # Options to `fit_resamples()`: 
               resamples = ames_folds, control = keep_pred)
lm_models

# Collect rmse metric
collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")

# Add the random forest model to the workflow set
four_models <- 
  as_workflow_set(random_forest = rf_res) %>% 
  bind_rows(lm_models)
four_models

# Visualize the confidence intervals of each model
library(ggrepel)
autoplot(four_models, metric = "rsq") +
  geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")

# Resample-to-resample component of variation (within-resample correlation)
rsq_indiv_estimates <- 
  collect_metrics(four_models, summarize = FALSE) %>% 
  filter(.metric == "rsq") 

rsq_wider <- 
  rsq_indiv_estimates %>% 
  select(wflow_id, .estimate, id) %>% 
  pivot_wider(id_cols = "id", names_from = "wflow_id", values_from = ".estimate")

corrr::correlate(rsq_wider %>% select(-id), quiet = TRUE)

# Visualizing the high correlation of individual resampling statistics
rsq_indiv_estimates %>% 
  mutate(wflow_id = reorder(wflow_id, .estimate)) %>% 
  ggplot(aes(x = wflow_id, y = .estimate, group = id, color = id)) + 
  geom_line(alpha = .5, lwd = 1.25) + 
  theme(legend.position = "none")

# Statistical test to show confidence in within-resample correlation
rsq_wider %>% 
  with( cor.test(basic_lm, splines_lm) ) %>% 
  tidy() %>% 
  select(estimate, starts_with("conf"))

# Comparing linear models with the ANOVA model
compare_lm <- 
  rsq_wider %>% 
  mutate(difference = splines_lm - basic_lm)

lm(difference ~ 1, data = compare_lm) %>% 
  tidy(conf.int = TRUE) %>% 
  select(estimate, p.value, starts_with("conf"))

# Alternatively, a paired t-test could also be used: 
rsq_wider %>% 
  with( t.test(splines_lm, basic_lm, paired = TRUE) ) %>%
  tidy() %>% 
  select(estimate, p.value, starts_with("conf"))

# Determine Bayesian model and fit with resampling statistics (R2)
library(tidyposterior)
library(rstanarm)

# The rstanarm package creates copious amounts of output; those results
# are not shown here but are worth inspecting for potential issues. The
# option `refresh = 0` can be used to eliminate the logging. 
rsq_anova <-
  perf_mod(
    four_models,
    metric = "rsq",
    prior_intercept = rstanarm::student_t(df = 1),
    chains = 4,
    iter = 5000,
    seed = 1102
  )

# View the posterior distributions
model_post <- 
  rsq_anova %>% 
  # Take a random sample from the posterior distribution
  # so set the seed again to be reproducible. 
  tidy(seed = 1103) 

glimpse(model_post)

# Visualize the posterior distributions
model_post %>% 
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50, color = "white", fill = "blue", alpha = 0.4) + 
  facet_wrap(~ model, ncol = 1)

autoplot(rsq_anova) +
  geom_text_repel(aes(label = workflow), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")

# Comparing two linear models and visualize results
rqs_diff <-
  contrast_models(rsq_anova,
                  list_1 = "splines_lm",
                  list_2 = "basic_lm",
                  seed = 1104)

rqs_diff %>% 
  as_tibble() %>% 
  ggplot(aes(x = difference)) + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_histogram(bins = 50, color = "white", fill = "red", alpha = 0.4)

# Compute the mean of the distribution and confidence intervals
summary(rqs_diff) %>% 
  select(-starts_with("pract"))

# Compute the Region of Practical Equivalance (ROPE) using 2%
summary(rqs_diff, size = 0.02) %>% 
  select(contrast, starts_with("pract"))

# Compare RF with a linear model using ROPE and a 2% practical effect size
autoplot(rsq_anova, type = "ROPE", size = 0.02) +
  geom_text_repel(aes(label = workflow)) +
  theme(legend.position = "none")

# Calculate likelihood statistics of different models
library(tidymodels)
tidymodels_prefer()

llhood <- function(...) {
  logistic_reg() %>% 
    set_engine("glm", ...) %>% 
    fit(Class ~ ., data = training_set) %>% 
    glance() %>% 
    select(logLik)
}

bind_rows(
  llhood(),
  llhood(family = binomial(link = "probit")),
  llhood(family = binomial(link = "cloglog"))
) %>% 
  mutate(link = c("logit", "probit", "c-log-log"))  %>% 
  arrange(desc(logLik))

# Resample statistics and separate data (modeling vs performance estimation data)
set.seed(1201)
rs <- vfold_cv(training_set, repeats = 10)

# Return the individual resampled performance estimates:
lloss <- function(...) {
  perf_meas <- metric_set(roc_auc, mn_log_loss)  # mn_log_loss() used to estimate negative log likelihood
    
  logistic_reg() %>% 
    set_engine("glm", ...) %>% 
    fit_resamples(Class ~ A + B, rs, metrics = perf_meas) %>% 
    collect_metrics(summarize = FALSE) %>%
    select(id, id2, .metric, .estimate)
}

resampled_res <- 
  bind_rows(
    lloss()                                    %>% mutate(model = "logistic"),
    lloss(family = binomial(link = "probit"))  %>% mutate(model = "probit"),
    lloss(family = binomial(link = "cloglog")) %>% mutate(model = "c-log-log")     
  ) %>%
  # Convert log-loss to log-likelihood:
  mutate(.estimate = ifelse(.metric == "mn_log_loss", -.estimate, .estimate)) %>% 
  group_by(model, .metric) %>% 
  summarize(
    mean = mean(.estimate, na.rm = TRUE),
    std_err = sd(.estimate, na.rm = TRUE) / sum(!is.na(.estimate)), 
    .groups = "drop"
  )

resampled_res %>% 
  filter(.metric == "mn_log_loss") %>% 
  ggplot(aes(x = mean, y = model)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = mean - 1.64 * std_err, xmax = mean + 1.64 * std_err),
                width = .1) + 
  labs(y = NULL, x = "log-likelihood")

# Specifying different types of arguments in parsnip models
rand_forest(trees = 2000, min_n = 10) %>%                   # <- main arguments
  set_engine("ranger", regularization.factor = 0.5)         # <- engine-specific

# Sepcifying tuning parameters
neural_net_spec <- 
  mlp(hidden_units = tune()) %>%  # uses tune() to tell tidymodels this parameter is to be tuned
  set_engine("keras")

# Specifying tuning parameters with a name to identify similar parameters
ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train)  %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = tune()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Longitude, deg_free = tune("longitude df")) %>%
  step_ns(Latitude,  deg_free = tune("latitude df"))

# Tuning parameters with functions of the same name in the dials package
hidden_units()
threshold()

# Tuning parameters with functions of different name in the dials package
spline_degree()

# identify the parameter using the id value:
wflow_param %>% extract_parameter_dials("threshold")

recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param

# Combining recipe and model specification to show parameters for tuning
wflow_param <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(neural_net_spec) %>%
  extract_parameter_set_dials()
wflow_param

# Update range of parameters
extract_parameter_set_dials(ames_rec) %>% 
  update(threshold = threshold(c(0.8, 1.0)))

# Illustrating and example where the bounds of a parameter set is missing [?]
rf_spec <- 
  rand_forest(mtry = tune()) %>% 
  set_engine("ranger", regularization.factor = tune("regularization"))

rf_param <- extract_parameter_set_dials(rf_spec)
rf_param

# Updating the parameter set to properly set the bounds
rf_param %>% 
  update(mtry = mtry(c(1, 70)))

# Updating the paremeter set after a recipe using finalize()
pca_rec <- 
  recipe(Sale_Price ~ ., data = ames_train) %>% 
  # Select the square-footage predictors and extract their PCA components:
  step_normalize(contains("SF")) %>% 
  # Select the number of components needed to capture 95% of
  # the variance in the predictors. 
  step_pca(contains("SF"), threshold = .95)
  
updated_param <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(pca_rec) %>% 
  extract_parameter_set_dials() %>% 
  finalize(ames_train)
updated_param

updated_param %>% extract_parameter_dials("mtry")

# Looking at the transformation of a penalty parameter
penalty()

# correct method to have penalty values between 0.1 and 1.0
penalty(c(-1, 0)) %>% value_sample(1000) %>% summary()

# incorrect:
penalty(c(0.1, 1.0)) %>% value_sample(1000) %>% summary()

# Changing the scale of the penalty values
penalty(trans = NULL, range = 10^c(-10, 0))

# Specify a classification model for tuning
library(tidymodels)
tidymodels_prefer()

mlp_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", trace = 0) %>% 
  set_mode("classification")

# Use the extract_parameter_dials function to extract the arguments that are being tuned
# and set their "dials" object, print default ranges
mlp_param <- extract_parameter_set_dials(mlp_spec)

mlp_param %>% extract_parameter_dials("hidden_units")

mlp_param %>% extract_parameter_dials("penalty")

mlp_param %>% extract_parameter_dials("epochs")

# Example of a regular grid
crossing(
  hidden_units = 1:3,
  penalty = c(0.0, 0.1),
  epochs = c(100, 200)
)

# Example of creating a regular grid with the grid_regular function
grid_regular(mlp_param, levels = 2)

mlp_param %>% 
  grid_regular(levels = c(hidden_units = 3, penalty = 2, epochs = 2))

# Example of creating an irregular grid with the grid_random function
set.seed(1301)
mlp_param %>% 
  grid_random(size = 1000) %>% # 'size' is the number of combinations
  summary()

# Illustration of overlapping parameter combinations when using grid_random
library(ggforce)
set.seed(1302)
mlp_param %>% 
  # The 'original = FALSE' option keeps penalty in log10 units
  grid_random(size = 20, original = FALSE) %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) + 
  geom_point() +
  geom_blank() +
  facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) + 
  labs(title = "Random design with 20 candidates")

# Illustration of space filling designs to create parameter combinations
set.seed(1303)
mlp_param %>% 
  grid_latin_hypercube(size = 20, original = FALSE) %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) + 
  geom_point() +
  geom_blank() +
  facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) + 
  labs(title = "Latin Hypercube design with 20 candidates")

# Set-up classification data set for model tuning
library(tidymodels)
data(cells)
cells <- cells %>% select(-case)

# Compute performance metrics using 10-fold cross validation
set.seed(1304)
cell_folds <- vfold_cv(cells)

# Create a recipe to normalize predictors and create principle components
mlp_rec <-
  recipe(class ~ ., data = cells) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%  # Encourages symmetric distributions
  step_normalize(all_numeric_predictors()) %>%  # Ensure predictors are on the same scale
  step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
  step_normalize(all_numeric_predictors())

# Add recipe to workflow with neural network model specification
mlp_wflow <- 
  workflow() %>% 
  add_model(mlp_spec) %>% 
  add_recipe(mlp_rec)

# Modify default ranges of tuning parameters
mlp_param <- 
  mlp_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    epochs = epochs(c(50, 200)),  # Tuning the neural network
    num_comp = num_comp(c(0, 40))  # Tuning the PCA transformation of predictors
  )

# Evaluate a regular grid
roc_res <- metric_set(roc_auc)
set.seed(1305)
mlp_reg_tune <-
  mlp_wflow %>%
  tune_grid(
    cell_folds,
    grid = mlp_param %>% grid_regular(levels = 3),
    metrics = roc_res
  )
mlp_reg_tune

# Extract the results using autoplot
autoplot(mlp_reg_tune) + 
  scale_color_viridis_d(direction = -1) + 
  theme(legend.position = "top")

# Extract the best results using show_best
show_best(mlp_reg_tune) %>% select(-.estimator)

# Use a space filling design by setting the grid argument to an int
set.seed(1306)
mlp_sfd_tune <-
  mlp_wflow %>%
  tune_grid(
    cell_folds,
    grid = 20,  # Tells tune_grid() to use a space filling design
    # Pass in the parameter object to use the appropriate range: 
    param_info = mlp_param,
    metrics = roc_res
  )
mlp_sfd_tune

autoplot(mlp_sfd_tune)

# Extract the best results using show_best
show_best(mlp_sfd_tune) %>% select(-.estimator)

# Extract the best results from the regular grid results
select_best(mlp_reg_tune, metric = "roc_auc")

# Create a final workflow from a manual set of parameters

# Parameters created manually from results from the autoplot
logistic_param <- 
  tibble(
    num_comp = 0,
    epochs = 125,
    hidden_units = 1,
    penalty = 1
  )

final_mlp_wflow <- 
  mlp_wflow %>% 
  finalize_workflow(logistic_param)
final_mlp_wflow

# Fit the final workflow
final_mlp_fit <- 
  final_mlp_wflow %>% 
  fit(cells)

# Create R code for tuning a model using usemodels
library(usemodels)

use_xgboost(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
              Latitude + Longitude, 
            data = ames_train,
            # Add comments explaining some of the code:
            verbose = TRUE)

# The resulting code
xgboost_recipe <- 
  recipe(formula = Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
    Latitude + Longitude, data = ames_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  ## This model requires the predictors to be numeric. The most common 
  ## method to convert qualitative predictors to numeric is to create 
  ## binary indicator variables (aka dummy variables) from these 
  ## predictors. However, for this model, binary indicator variables can be 
  ## made for each of the levels of the factors (known as 'one-hot 
  ## encoding'). 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) 

xgboost_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
    loss_reduction = tune(), sample_size = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

set.seed(69305)
xgboost_tune <-
  tune_grid(xgboost_workflow, 
            resamples = stop("add your rsample object"), 
            grid = stop("add number of candidate points"))

# Submodel optimization
c5_spec <- 
  boost_tree(trees = tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

set.seed(1307)
c5_spec %>%
  tune_grid(
    class ~ .,
    resamples = cell_folds,
    grid = data.frame(trees = 1:100),
    metrics = roc_res
  )

# Accessing global variables when using parallel processing
coef_penalty <- 0.1
spec <- linear_reg(penalty = coef_penalty) %>% set_engine("glmnet")
spec

# The penalty will show up as a "quosure" and show that it exists in the global env
spec$args$penalty

# Use !! to insert the actual data into the objects
spec <- linear_reg(penalty = !!coef_penalty) %>% set_engine("glmnet")
spec$args$penalty

# Use !!! for multiple variables
mcmc_args <- list(chains = 3, iter = 1000, cores = 3)

linear_reg() %>% set_engine("stan", !!!mcmc_args)

library(stringr)
ch_2_vars <- str_subset(names(cells), "ch_2")
ch_2_vars

# Still uses a reference to global data (~_~;)
recipe(class ~ ., data = cells) %>% 
  step_spatialsign(all_of(ch_2_vars))

# Inserts the values into the step ヽ(•‿•)ノ
recipe(class ~ ., data = cells) %>% 
  step_spatialsign(!!!ch_2_vars)

# Racing methods
library(finetune)

set.seed(1308)
mlp_sfd_race <-
  mlp_wflow %>%
  tune_race_anova(
    cell_folds,
    grid = 20,
    param_info = mlp_param,
    metrics = roc_res,
    control = control_race(verbose_elim = TRUE)
  )

show_best(mlp_sfd_race, n = 10)

# End of chapter 13 code ----
library(tidymodels)

data(cells)
cells <- cells %>% select(-case)

set.seed(1304)
cell_folds <- vfold_cv(cells)

roc_res <- metric_set(roc_auc)

# Defining a SVM model
library(tidymodels)
tidymodels_prefer()

svm_rec <- 
  recipe(class ~ ., data = cells) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

svm_wflow <- 
  workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(svm_rec)

# View the tuning parameters defaults
cost()

rbf_sigma()

# Change the kernel parameter range (for illustration)
svm_param <- 
  svm_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(rbf_sigma = rbf_sigma(c(-7, -1)))

# Create a small regular grid
set.seed(1401)
start_grid <- 
  svm_param %>% 
  update(
    cost = cost(c(-6, 1)),
    rbf_sigma = rbf_sigma(c(-6, -4))
  ) %>% 
  grid_regular(levels = 2)

set.seed(1402)
svm_initial <- 
  svm_wflow %>% 
  tune_grid(resamples = cell_folds, grid = start_grid, metrics = roc_res)

collect_metrics(svm_initial)

# Initial substrate of the Gaussian Process model
ctrl <- control_bayes(verbose = TRUE)

set.seed(1403)
svm_bo <-
  svm_wflow %>%
  tune_bayes(
    resamples = cell_folds,
    metrics = roc_res,
    initial = svm_initial,
    param_info = svm_param,
    iter = 25,
    control = ctrl
  )

# Show results
show_best(svm_bo)

autoplot(svm_bo, type = "performance")

ctrl_sa <- control_sim_anneal(verbose = TRUE, no_improve = 10L)

# Simulated annealing
set.seed(1404)
svm_sa <-
  svm_wflow %>%
  tune_sim_anneal(
    resamples = cell_folds,
    metrics = roc_res,
    initial = svm_initial,
    param_info = svm_param,
    iter = 50,
    control = ctrl_sa
  )

# Show results
autoplot(svm_sa, type = "performance")

autoplot(svm_sa, type = "parameters")

# Modeling concrete mixture strength

# Define data splitting and resampling schemes
library(tidymodels)
tidymodels_prefer()
data(concrete, package = "modeldata")
glimpse(concrete)

# Remove duplicate mixtures by averaging comprehensive strength for those mixtures
concrete <- 
   concrete %>% 
   group_by(across(-compressive_strength)) %>% 
   summarize(compressive_strength = mean(compressive_strength),
             .groups = "drop")
nrow(concrete)

# Split the data
set.seed(1501)
concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test  <- testing(concrete_split)

# Resample using 10 fold cross validation, 5 repeats
set.seed(1502)
concrete_folds <- 
   vfold_cv(concrete_train, strata = compressive_strength, repeats = 5)

# Create recipes to address necessary preprocessing for some models
## Normalizing required for neural networks, KNN, and support vector machines
normalized_rec <- 
   recipe(compressive_strength ~ ., data = concrete_train) %>% 
   step_normalize(all_predictors()) 

## Other models may require traditional response surface design model expansion (quadratic and two-way interactions)
poly_recipe <- 
   normalized_rec %>% 
   step_poly(all_predictors()) %>% 
   step_interact(~ all_predictors():all_predictors())

# Create a set of model specifications using parsnip
library(rules)
library(baguette)

linear_reg_spec <- 
   linear_reg(penalty = tune(), mixture = tune()) %>% 
   set_engine("glmnet")

nnet_spec <- 
   mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
   set_engine("nnet", MaxNWts = 2600) %>% 
   set_mode("regression")

mars_spec <- 
   mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
   set_engine("earth") %>% 
   set_mode("regression")

svm_r_spec <- 
   svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
   set_engine("kernlab") %>% 
   set_mode("regression")

svm_p_spec <- 
   svm_poly(cost = tune(), degree = tune()) %>% 
   set_engine("kernlab") %>% 
   set_mode("regression")

knn_spec <- 
   nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
   set_engine("kknn") %>% 
   set_mode("regression")

cart_spec <- 
   decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
   set_engine("rpart") %>% 
   set_mode("regression")

bag_cart_spec <- 
   bag_tree() %>% 
   set_engine("rpart", times = 50L) %>% 
   set_mode("regression")

rf_spec <- 
   rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
   set_engine("ranger") %>% 
   set_mode("regression")

xgb_spec <- 
   boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
              min_n = tune(), sample_size = tune(), trees = tune()) %>% 
   set_engine("xgboost") %>% 
   set_mode("regression")

cubist_spec <- 
   cubist_rules(committees = tune(), neighbors = tune()) %>% 
   set_engine("Cubist")

# Ensure neural network has up  to 27 hidden units using extract_parameter_set_dials()
nnet_param <- 
   nnet_spec %>% 
   extract_parameter_set_dials() %>% 
   update(hidden_units = hidden_units(c(1, 27)))

# Workflow set example
## Combine the recipe that only standardizes the predictors to the nonlinear models 
## that require the predictors to be in the same units

normalized <- 
   workflow_set(
      preproc = list(normalized = normalized_rec), 
      models = list(SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, 
                    KNN = knn_spec, neural_network = nnet_spec)
   )
normalized

# Extract the workflow
normalized %>% extract_workflow(id = "normalized_KNN")

# Add the neural network parameter object via the option column
normalized <- 
   normalized %>% 
   option_add(param_info = nnet_param, id = "normalized_neural_network")
normalized

# NOTE: The result column is a placeholder for the output of the tuning or resampling functions

# Create workflow set for the other models using dplyr selectors for outcome and predictors
model_vars <- 
   workflow_variables(outcomes = compressive_strength, 
                      predictors = everything())

no_pre_proc <- 
   workflow_set(
      preproc = list(simple = model_vars), 
      models = list(MARS = mars_spec, CART = cart_spec, CART_bagged = bag_cart_spec,
                    RF = rf_spec, boosting = xgb_spec, Cubist = cubist_spec)
   )
no_pre_proc

# Create workflow set for models that use nonlinear terms and interactions
with_features <- 
   workflow_set(
      preproc = list(full_quad = poly_recipe), 
      models = list(linear_reg = linear_reg_spec, KNN = knn_spec)
   )

# Combine all workflow sets
all_workflows <- 
   bind_rows(no_pre_proc, normalized, with_features) %>% 
   # Make the workflow ID's a little more simple: 
   mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))
all_workflows

# Create tuning parameter combinations for all of the models
grid_ctrl <-
   control_grid(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
   )

grid_results <-
   all_workflows %>%
   workflow_map(
      seed = 1503,  # used to ensure that each execution of tune_grid() consumes the same random numbers
      resamples = concrete_folds,
      grid = 25,
      control = grid_ctrl
   )

grid_results

# Examine results using RMSE metric
grid_results %>% 
   rank_results() %>% 
   filter(.metric == "rmse") %>% 
   select(model, .config, rmse = mean, rank)

# Visualize the best results for each model
autoplot(
   grid_results,
   rank_metric = "rmse",  # <- how to order models
   metric = "rmse",       # <- which metric to visualize
   select_best = TRUE     # <- one point per workflow
) +
   geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
   lims(y = c(3.5, 9.5)) +
   theme(legend.position = "none")

# Visualize the tuning parameters for a specific model
autoplot(grid_results, id = "Cubist", metric = "rmse")

# NOTE: collect_predictions() and collect_metrics() can be used

# Screen model with the racing approach using workflow_map() to speed up evaluation
library(finetune)

race_ctrl <-
   control_race(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
   )

race_results <-
   all_workflows %>%
   workflow_map(
      "tune_race_anova",  # Function to apply to the workflows
      seed = 1503,
      resamples = concrete_folds,
      grid = 25,
      control = race_ctrl
   )

# Note the resuts contain "race[+]"
race_results

# Visualize results of the models
autoplot(
   race_results,
   rank_metric = "rmse",  
   metric = "rmse",       
   select_best = TRUE    
) +
   geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
   lims(y = c(3.0, 9.5)) +
   theme(legend.position = "none")

# Determine if results are similar to full grid
matched_results <- 
   rank_results(race_results, select_best = TRUE) %>% 
   select(wflow_id, .metric, race = mean, config_race = .config) %>% 
   inner_join(
      rank_results(grid_results, select_best = TRUE) %>% 
         select(wflow_id, .metric, complete = mean, 
                config_complete = .config, model),
      by = c("wflow_id", ".metric"),
   ) %>%  
   filter(.metric == "rmse")

library(ggrepel)

matched_results %>% 
   ggplot(aes(x = complete, y = race)) + 
   geom_abline(lty = 3) + 
   geom_point() + 
   geom_text_repel(aes(label = model)) +
   coord_obs_pred() + 
   labs(x = "Complete Grid RMSE", y = "Racing RMSE") 
