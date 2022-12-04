# White Wine Random Forest 

# setting random forest model up
wrandfor <- rand_forest() %>% set_engine("ranger", importance = "impurity") %>% set_mode("classification")

wrandfor_wf <- workflow() %>%
  add_model(wrandfor %>%
              set_args(mtry = tune(), trees = tune(), min_n = tune())) %>% 
  add_formula(quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol)

# tuning the model to find the best arguments 
param_grid1 <- grid_regular(mtry(range = c(1,9)), trees(range = c(15,17)), min_n(range = c(30,50)), levels = 8)

wtune_res_randfor <- tune_grid(
  wrandfor_wf,
  resamples = white_fold,
  grid = param_grid1,
  metric = metric_set(accuracy)
)

wAutoPlotRF <- autoplot(wtune_res_randfor)


# collecting metrics to find best mean
wbest_rocauc1 <-collect_metrics(wtune_res_randfor) %>% arrange(desc(mean))

wbest_metric1 <- select_best(wtune_res_randfor)

# wrandfor_final <- finalize_workflow(wrandfor_wf, wbest_metric1)
# wrandfor_fit_final <- fit(wrandfor_final, data = white_train)

wrandfor_final <- rand_forest(mtry = 2, trees = 17, min_n = 30) %>% set_engine("ranger", importance = "impurity") %>% set_mode("classification")
wrandfor_fit_final <- fit(wrandfor_final, formula = quality ~ volatile.acidity + fixed.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = white_train)

wVIP <- vip(wrandfor_fit_final)
wVIP 

# saving 
save(wAutoPlotRF, wbest_rocauc1, wrandfor_final, wVIP, wrandfor_fit_final, file = "WhiteWineRandomForest.rda")

