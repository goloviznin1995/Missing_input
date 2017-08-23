add_NA_bool_vars <- function(dat, prefix = 'miss_'){
        for (i in 1:ncol(dat)){
                if (length(dat[,i][!complete.cases(dat[,i])]) > 0){
                        name <- colnames(dat)[i]
                        new_name <- paste(prefix, name, sep = '')
                        vect <- c(!complete.cases(dat[,i]))
                        dat[,ncol(dat)+1] <- vect
                        colnames(dat)[ncol(dat)] <- new_name
                }
        }
        return(dat)
}

na_approx_xgb <- function(dat, 
                          n_rounds = 200, 
                          algorithm = 'xgb',
                          add_na_features = T){
        if (add_na_features == T){
                dat <- add_NA_bool_vars(dat)
                print(ncol(dat))
        }
        only_missing_msg = 'Variable contains only missing values:'
        m1 = 'Found '
        m3 = '% missing values'
        m4 = ' in variable: '
        #Verbose 
        
        for (i in 1:ncol(dat)){
                #print(i)
                compl <- length(dat[,i][complete.cases(dat[,i])])
                if (compl == 0){
                        name <- colnames(dat)[i]
                        print(paste0(only_missing_msg, name))
                }
                
                if (compl < nrow(dat) & compl > 0){
                        train <- dat[complete.cases(dat[,i]),]
                        
                        prediction <- test[,i]
                        test <- dat[!complete.cases(dat[,i]),]
                        
                        m2 <- as.character(nrow(test)/nrow(dat))
                        m5 <- as.character(colnames(dat)[i])
                        mvect <- paste0(m1, m2, m3, m4, m5)                        
                        print(mvect)
                        
                                
                        train_matrix <- data.matrix(train)
                        
                        test_matrix <- data.matrix(test)

                        xgb <- xgboost(train_matrix, 
                                       label = train[,i], 
                                       nrounds = n_rounds,
                                       verbose = F)
                        ncol(test)
                        test[,i] <- predict(xgb, newdata = test_matrix)
                        #test[,ncol(test)+1] <- predict(xgb, newdata = test_matrix)
                        #test <- data.frame(test[,1:i-1], 
                        #                  test[,ncol(test)], 
                        #                  test[,ncol(test) -1])
                        
                        # Probably add predictions
                        dat <- rbind(test, train) 
                        
                        local_score <- min(xgb$evaluation_log$train_rmse)
                        lsc <- as.character(local_score)
                        s1 <- 'Approximated NAs with algorithm: '
                        s2 <- ' with local score = '
                        success_msg <- paste0(s1,algorithm,s2,lsc)
                        print(success_msg)
                        # some bug here yeah
                }
        }
        return(dat)
}
