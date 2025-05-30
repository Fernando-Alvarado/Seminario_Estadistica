#Cosas generales de los modelos 

# Folds de validación cruzada
cv_folds <- vfold_cv(train_data, v = 5, strata = Y) #strata = Y, para asegurar que la variable de estratificación sea Y


# Malla para los hiperparametros, para tunear al modelo
my_grid <- tibble(
  penalty = 10^seq(-3, 2, length.out = 50)
)

#Funcion para outomatizar el preprocesamiento de los datos 
trans <- function(training_data){
  rep <-  recipe(Y ~ ., data = training_data) %>%
        step_nzv(all_predictors()) %>%
        step_center(all_numeric_predictors()) %>%
         step_scale(all_numeric_predictors()) %>%
         step_dummy(all_nominal_predictors())
  return(rep)
}



salidaModelos <- function(grid, workflow, trainingDat =train_data, testing_data = test_data ){
  mejores_hiperpar <- select_best(grid, metric = "accuracy") #grid_fit

modelo_glm <- finalize_workflow(
                x = workflow, #workflow_modelado
                parameters = mejores_hiperpar
              )

modelo_glm_fit <-  modelo_glm %>%
                   fit(
                     data = trainingDat
                   )

#Predicciones 
#=====================================
predicciones <- modelo_glm_fit %>%
  predict(
    new_data = testing_data,
    type = "class"  #Especifico que estoy trabajando con una regresion 
  ) %>% bind_cols(testing_data %>% select(Y))


# MÉTRICAS Accuracy
# ====================================
accuracy_general <- predicciones %>%
  accuracy(truth = Y, estimate = .pred_class)

# TCC clase 1 (Sensibilidad clase 1)
tcc_clase_1 <- predicciones %>%
  sens(truth = Y, estimate = .pred_class, event_level = "second")

# TCC clase 0 (Sensibilidad clase 0)
tcc_clase_0 <- predicciones %>%
  sens(truth = Y, estimate = .pred_class, event_level = "first")

# Guardando los resultados en un data frame, que es lo que vamos a guardar en el df_Final 
out <- list(
  accuracy = accuracy_general$.estimate,
  tcc_clase_0 = tcc_clase_0$.estimate,
  tcc_clase_1 = tcc_clase_1$.estimate
)


return(out)
}









# Seleccion de modelos 
StepwiseBack <- function(modelo, link = "logit",  datos){
  
   formula <- as.formula(paste("Y ~ ", modelo ))
  
   model <- glm(formula, family = binomial(link = link), data = datos)
   
   #print( summary(model))
   #Haciendolo Backward
   modelo_step <- stats::step(
      object = model,
      direction = "backward",
      scope = list(upper = model, lower = ~1),
      trace = FALSE
  )
    
  return(modelo_step) 
}
