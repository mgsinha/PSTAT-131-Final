# Red Boosted Trees 
rboost_spec <-  boost_tree(tree_depth = 5) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

rboost_wf <- workflow() %>%
  add_model(rboost_spec %>%
              set_args(trees = tune())) %>% 
  add_formula(quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

param_grid4 <- grid_regular(trees(range = c(10,2000)), levels = 10)

rtune_res_boosted <- tune_grid(
  rboost_wf,
  resamples = red_fold,
  grid = param_grid4
)
rBoostedAutoPlot <- autoplot(rtune_res_boosted)
rBoostedAutoPlot

rbest_rocauc2 <- collect_metrics(rtune_res_boosted) %>% arrange(desc(mean))
print(rbest_rocauc2)

rbest_metric2 <- select_best(rtune_res_boosted)
print(rbest_metric2)

rboost_final <- boost_tree(tree_depth = 5, trees = 231)%>% 
  set_engine("xgboost") %>% 
  set_mode("classification")
rboost_fit_final <- fit(rboost_final, formula = quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = red_train)

rpredicted <- augment(rboost_fit_final, new_data = red_test) 
rpredicted %>% roc_auc(quality, .pred_3,.pred_4, .pred_5 , .pred_6 , .pred_7, .pred_8)
rBoostedROCCurveTesting <- rpredicted %>% roc_curve(quality, .pred_3,.pred_4, .pred_5 , .pred_6 , .pred_7, .pred_8) %>% autoplot()
rBoostedConfusionMatrixTesting <- rpredicted %>% conf_mat(truth = quality, estimate = .pred_class) %>% autoplot(type = "heatmap")
rBoostedConfusionMatrixTesting
rBoostedROCCurveTesting
# saving 
save(rBoostedAutoPlot, rbest_rocauc2, rboost_final, rboost_fit_final, rpredicted,rBoostedROCCurveTesting,rBoostedConfusionMatrixTesting, file = "RedWineBoostedTrees.rda")

