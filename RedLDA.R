red_recipe <- recipe(quality ~ volatile.acidity + fixed.acidity + citric.acid + 
                       residual.sugar + chlorides + free.sulfur.dioxide + 
                       total.sulfur.dioxide + density + pH + sulphates + alcohol, data = red_train) 
%>% step_dummy(all_nominal_predictors()) %>% step_normalize(all_predictors())
