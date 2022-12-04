# White Boosted Trees 

wboost_spec <-  boost_tree(tree_depth = 5) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

wboost_wf <- workflow() %>%
  add_model(wboost_spec %>%
              set_args(trees = tune())) %>% 
  add_formula(quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

param_grid3 <- grid_regular(trees(range = c(10,2000)), levels = 10)

wtune_res_boosted <- tune_grid(
  wboost_wf,
  resamples = white_fold,
  grid = param_grid3
)

wBoostedAutoPlot <- autoplot(wtune_res_boosted)

wbest_rocauc2 <- collect_metrics(wtune_res_boosted) %>% arrange(desc(mean))
print(wbest_rocauc2)

wbest_metric2 <- select_best(wtune_res_boosted)
print(wbest_metric2)

wboost_final <- boost_tree(tree_depth = 5, trees = 231)%>% 
  set_engine("xgboost") %>% 
  set_mode("classification")
wboost_fit_final <- fit(wboost_final, formula = quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = white_train)

wpredicted <- augment(wboost_fit_final, new_data = white_test) 
wpredicted %>% roc_auc(quality, .pred_3,.pred_4, .pred_5 , .pred_6 , .pred_7, .pred_8)
wBoostedROCCurveTesting <- wpredicted %>% roc_curve(quality, .pred_3,.pred_4, .pred_5 , .pred_6 , .pred_7, .pred_8) %>% autoplot()
wBoostedConfusionMatrixTesting <- wpredicted %>% conf_mat(truth = quality, estimate = .pred_class) %>% autoplot(type = "heatmap")
wBoostedConfusionMatrixTesting
# saving 
save(wBoostedAutoPlot, wbest_rocauc2, wboost_final, wboost_fit_final, wpredicted,wBoostedROCCurveTesting,wBoostedConfusionMatrixTesting, file = "WhiteWineBoostedTrees.rda")

