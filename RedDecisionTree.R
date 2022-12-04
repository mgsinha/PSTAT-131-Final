# Red Decision Tree
# decision tree specification
rtree_spec <- decision_tree() %>%
  set_engine("rpart", model=TRUE)

# setting mode to classificiation
rtree_spec_class <- rtree_spec %>%
  set_mode("classification")

rclass_tree_fit <- rtree_spec_class %>%
  fit(quality ~ volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = red_train)


rDecisionTreePre <- rclass_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(main = "Red Wine Decision Tree")

# augmentsed on training 
rDecisionTreeAccPre <- augment(rclass_tree_fit, new_data = red_train) %>%
  accuracy(truth = quality, estimate = .pred_class)

rDecisionTreeConfMatrixPre <- augment(rclass_tree_fit, new_data = red_train) %>%
  conf_mat(truth = quality, estimate = .pred_class)

# augmentsed on testing 
augment(rclass_tree_fit, new_data = red_train) %>%
  conf_mat(truth = quality, estimate = .pred_class) 

augment(rclass_tree_fit, new_data = red_train) %>%
  accuracy(truth = quality, estimate = .pred_class)

# tuning cost complexity 
rclass_tree_wf<- workflow() %>%
  add_model(rtree_spec_class %>% 
              set_args(cost_complexity = tune())) %>% 
  add_formula(quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

param_grid <- grid_regular(cost_complexity(range = c(-3,-1)), levels = 10)

tune_res_red <- tune_grid(
  rclass_tree_wf,
  resamples = red_fold,
  grid = param_grid,
  metric = metric_set(accuracy)
)

rAutoPlot <- autoplot(tune_res_red)

# extracting the best cost complexitiy parameter

rbest_rocauc <-collect_metrics(tune_res_red) %>% arrange(desc(mean))

rbest_complexity <- select_best(tune_res_red)

rclass_tree_final <- finalize_workflow(rclass_tree_wf, rbest_complexity)

rclass_tree_final_fit <- fit(rclass_tree_final, data = red_train)


rDecisionTree <- rclass_tree_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot(main = "Red Wine Decision Tree")

# augmented on training 
rDecisionTreeAcc <- augment(rclass_tree_final_fit, new_data = red_train) %>%
  accuracy(truth = quality, estimate = .pred_class)

rDecisionTreeConfMatrix <- augment(rclass_tree_final_fit, new_data = red_train) %>%
  conf_mat(truth = quality, estimate = .pred_class)

# augmented on testing 

rDecisionTreeAccTest <-augment(rclass_tree_final_fit, new_data = red_test) %>%
  conf_mat(truth = quality, estimate = .pred_class) 

rDecisionTreeConfMatrixTest <-augment(rclass_tree_final_fit, new_data = red_test) %>%
  accuracy(truth = quality, estimate = .pred_class)

# saving files
save(rDecisionTreePre, rDecisionTreeAccPre, rDecisionTreeConfMatrixPre, rAutoPlot, rbest_rocauc, rDecisionTree, rAutoPlot, rDecisionTreeAcc, rDecisionTreeConfMatrix, rDecisionTreeAccTest, rDecisionTreeConfMatrixTest, file = "RedWineDecisionTree.rda")

