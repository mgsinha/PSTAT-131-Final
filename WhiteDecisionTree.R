# White Decision Tree 
# decision tree specification
wtree_spec <- decision_tree() %>%
  set_engine("rpart", model=TRUE)

# setting mode to classification
wtree_spec_class <- wtree_spec %>%
  set_mode("classification")

wclass_tree_fit <- wtree_spec_class %>%
  fit(quality ~ volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = white_train)


wDecisionTreePre <- wclass_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(main="White Wine Decision Tree")

# augmented on training 
wDecisionTreeAccPre <- augment(wclass_tree_fit, new_data = white_train) %>%
  accuracy(truth = quality, estimate = .pred_class)

wDecisionTreeConfMatrixPre <- augment(wclass_tree_fit, new_data = white_train) %>%
  conf_mat(truth = quality, estimate = .pred_class) %>% autoplot(type = "heatmap")

# augmentsed on testing 
augment(wclass_tree_fit, new_data = white_test) %>%
  conf_mat(truth = quality, estimate = .pred_class)  %>% autoplot(type = "heatmap")

augment(wclass_tree_fit, new_data = white_test) %>%
  accuracy(truth = quality, estimate = .pred_class)

# tuning cost complexity 
wclass_tree_wf<- workflow() %>%
  add_model(wtree_spec_class %>% 
              set_args(cost_complexity = tune())) %>% 
  add_formula(quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

param_grid <- grid_regular(cost_complexity(range = c(-3,-1)), levels = 10)

tune_res_white <- tune_grid(
  wclass_tree_wf,
  resamples = white_fold,
  grid = param_grid,
  metric = metric_set(accuracy)
)

wAutoPlot <- autoplot(tune_res_white)

# extracting the best cost complexitiy parameter
wbest_rocauc <- collect_metrics(tune_res_white) %>% arrange(desc(mean))

wbest_complexity <- select_best(tune_res_white)

wclass_tree_final <- finalize_workflow(wclass_tree_wf, wbest_complexity)

wclass_tree_final_fit <- fit(wclass_tree_final, data = white_train)

wDecisionTree <- wclass_tree_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot(main="White Wine Decision Tree")

# augmented on training 
wDecisionTreeAcc <- augment(wclass_tree_final_fit, new_data = white_train) %>%
  accuracy(truth = quality, estimate = .pred_class)

wDecisionTreeConfMatrix <- augment(wclass_tree_final_fit, new_data = white_train) %>%
  conf_mat(truth = quality, estimate = .pred_class)

# augmented on testing 
wDecisionTreeAccTest <-augment(wclass_tree_final_fit, new_data = white_test) %>%
  conf_mat(truth = quality, estimate = .pred_class) 

wDecisionTreeConfMatrixTest <-augment(wclass_tree_final_fit, new_data = white_test) %>%
  accuracy(truth = quality, estimate = .pred_class)

# saving files 
save(wDecisionTreePre, wDecisionTreeAccPre, wDecisionTreeConfMatrixPre, wAutoPlot, wbest_rocauc, wDecisionTree, wAutoPlot, wDecisionTreeAcc, wDecisionTreeConfMatrix, wDecisionTreeAccTest, wDecisionTreeConfMatrixTest, file = "WhiteWineDecisionTree.rda")
