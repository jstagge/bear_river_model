
fit_wrapper <- function(data, predictors, month_loop, y_name) {

### Fit model
coef_matrix <- matrix(NA, length(predictors)+1, 12)
alpha_lambda_matrix <- matrix(NA, 2, 12)

### Loop to fit the regression model
for (i in month_loop) {
	### Extract month
	flow_fit_month <- subset(data, month==i)

	### Fit elastic net
	fit_model <- fit_elastic(data = flow_fit_month, x_names=predictors, y_name=y_name)

	### Save coefficients and alpha/lambda
	coef_matrix[,i] <- fit_model$coef
	alpha_lambda_matrix[,i] <- fit_model$alpha_lambda

}

rownames(coef_matrix) <- rownames(fit_model$coef)
rownames(alpha_lambda_matrix) <- rownames(fit_model$alpha_lambda)

### Predict values
data$estimate <- NA
for (i in month_loop) {
	### Extract month
	month_test <- data$month == i
	flow_fit_month <- data[month_test,]

	x_names <- rownames(coef_matrix)
	coef <-  coef_matrix[,i]

	### Predict values
	pred_values <- predict_elastic(flow_fit_month, coef=coef, x_names=x_names)
	
	### Save values
	data$estimate[month_test] <- pred_values
}


return(list(estimate = data$estimate, coef=coef_matrix, alpha_lambda=alpha_lambda_matrix))

}




fit_elastic <- function(data, x_names, y_name) {
	require(assertthat)
	require(glmnet)
	require(caret)

# Set elastic grid search parameters
	lambda_grid <- 10^seq(2,-6,length=100)
	#lambda_grid <- seq(0,1,length=10)
	alpha_grid <- seq(0.2,1,length=20)
	eGrid <- expand.grid(.alpha = alpha_grid, .lambda = lambda_grid)

	### Set control for runs, 10-fold repeated measures, 8 iterations
	Control <- trainControl(method = "repeatedcv",repeats = 8, number=10, verboseIter =FALSE)
	
	### Create parallel clusters
	require(doParallel)
	cores <- detectCores()
	cl <- makePSOCKcluster(cores)
	registerDoParallel(cl)
	
	### subset to only predictors and y variable	
	data_subset <- data[,c(x_names, y_name)]

	### Subset to only non NA values
	data_subset <- data_subset[complete.cases(data_subset),]
	
	### Extract the values
	x <- as.matrix(data_subset[,x_names])
	y <- data_subset[,y_name]
	
	### Fit the model
	netFit <- train(x =x, y = y,
         method = "glmnet",
         tuneGrid = eGrid,
         trControl = Control)
		
	### Extract the model with best tuning parameters (alpha)
	my_glmnet_model <- netFit$finalModel
	model_result <- my_glmnet_model
		
	### Extract coefficients by applying best lambda to model with best alpha
	model_coef <- coef(my_glmnet_model, s = netFit$bestTune$lambda)		
	model_results <- matrix(model_coef)
	rownames(model_results) <- dimnames(model_coef)[[1]]

	### Extract alpha and lambda
	alpha_lambda_temp <- matrix(unlist(netFit$bestTune))
	rownames(alpha_lambda_temp) <- names(netFit$bestTune)

	### Stop clusters
	stopCluster(cl)
		
	return(list(coef=model_results, alpha_lambda=alpha_lambda_temp, model=model_result)		)
}


predict_elastic <- function(data, coef, x_names) {

	### Only predictor names
	intercept_test <- x_names == "(Intercept)"
	pred_names <- x_names[!intercept_test]

	### subset to only predictors and y variable	
	data_subset <- data[,c(pred_names)]
			
	### Calculate final result
	coeffs <- coef[!intercept_test]
	values <- coef[intercept_test]
	for (k in seq(1, length(coeffs))) {
		#var_name <- pred_names[[k]]
		values <- values + coeffs[[k]] * data_subset[,k]
	}

	return(values)
}

