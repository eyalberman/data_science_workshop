remove_large_null_columns <- function(table_name, null_parameter) {
  Null_Counter <- apply(table_name, 2, function(x) length(which(x == "" | is.na(x) |   x == "NA" | x == "-999" | x == "0"))/length(x))
  Null_Name <- colnames(table_name)[Null_Counter < null_parameter]
  return(table_name[,Null_Name])
}

remove_duplicate_columns <- function(table_name) {
  ynames = names(table_name)[grepl( "\\.y" , names( table_name ))]
  names_to_keep <-  setdiff(names(table_name), ynames)
  return(table_name[,names_to_keep])
}

prepare_data_df_to_pivot <- function(table_name) {
  table_name <- table_name[!is.na(table_name$Country.Code), -grep("Series.Code",names(table_name))]
  table_name <- table_name[,c(grep("Country.Name|Country$",names(table_name)),grep("Country.Code",names(table_name)),grep("Series.Name",names(table_name)),4:length(table_name))]
  names(table_name) <- gsub("X(\\d+).*", "\\1", names(table_name))
  table_name <- gather(table_name, 'year', 'value', 4:length(table_name)) %>% 
    spread(Series.Name, value)
  table_name$year <- as.numeric(paste(table_name$year))
  return(table_name)
}

prepare_african_development_df <- function(africa_development) {
  africa_development <- africa_development[!is.na(africa_development$Country.Code), ]
  africa_development <- africa_development[, -4]
  names(africa_development) <- gsub("X(\\d+).*", "\\1", names(africa_development))
  africa_development <- africa_development[order(africa_development$Country.Name,africa_development$Series.Name,africa_development$"1966"), ] 
  africa_development <- distinct(africa_development,Country.Name, Country.Code,Series.Name, .keep_all = TRUE )
  africa_development <- gather(africa_development, 'year', 'value', 4:ncol(africa_development)) %>% 
    spread(Series.Name, value)
  africa_development$year <- as.numeric(paste(africa_development$year))
  africa_development <- africa_development[, -1746]
  return(africa_development)
}


plot_chart <- function(field, title, year_vector) {
  data_for_chart <- data.frame(year = as.factor(african_joined_table.imputed$year), country = african_joined_table.imputed$Country, 
  african_joined_table.imputed[field],mortality_rate = african_joined_table.imputed$Mort_Rate_under_5)
  ggplot(data=data_for_chart %>% dplyr::filter(year %in% year_vector), aes_string(x = field, y="mortality_rate", group="year")) +
    geom_line(aes(color=year))+
    geom_point(aes(color=year))+
    geom_smooth(method="lm",se=FALSE, (aes(color=year)))+
    ggtitle(paste("Mortality rate among children under 5 vs ",title))
}

prepare_target_data <- function(target_data) {
  target_data = target_data %>% filter(Series.Name == 'Mortality rate, under-5 (per 1,000 live births)')
  names(target_data) <- gsub("X(\\d+).*", "\\1", names(target_data))
    target_data <- melt(target_data, id=c(1:4))
  target_data <- target_data[, c(1,2,5,6)]
  colnames(target_data)[(names(target_data) == "value")] <- "Mort_Rate_under_5"
  colnames(target_data)[(names(target_data) == "variable")] <- "year"
  target_data$year <- as.numeric(paste(target_data$year))
  return(target_data)
}
         
create_joined_table <- function() {
  joined_table <- left_join(x = target_data, y = africa_development,by = c("Country.Code" = "Country.Code", "year" = "year"), copy= FALSE) 
  joined_table <- remove_duplicate_columns(joined_table)
  joined_table <- left_join(x = joined_table, y = gender,by = c("Country.Code" = "Country.Code", "year" = "year")) 
  joined_table <- remove_duplicate_columns(joined_table)
  joined_table <- left_join(x = joined_table, y = health_nutrition, by = c("Country.Code" = "Country.Code", "year" = "year")) 
  joined_table <- remove_duplicate_columns(joined_table)
  joined_table <- left_join(x = joined_table, y = poverty, by = c("Country.Code" = "Country.Code", "year" = "year"))
  joined_table <- remove_duplicate_columns(joined_table)
  joined_table <- joined_table[,-grep('Country.Name.x.x',colnames(joined_table))]
  valid_column_names <- make.names(names=names(joined_table), unique=TRUE, allow_ = TRUE)
  names(joined_table) <- valid_column_names
  return(joined_table)
}

train_model <- function(num_tree,num_features) {
  set.seed(123)
  return(randomForest(Mort_Rate_under_5 ~ . - Country.Code - Country.Name.x - year -Country, data = train ,
               ntree = num_tree, mtry = num_features, importance = TRUE)) 
}

train_predict_and_evaluate_model <-function(test, ntree, nfeatures){
  rf.model <- train_model(ntree, nfeatures)
  prediction <- predict(rf.model, test)
  compare_df <- data.frame(prediction, test$Mort_Rate_under_5, prediction - test$Mort_Rate_under_5)
  rmse_prediction <- rmse(compare_df$prediction, compare_df$test.Mort_Rate_under_5)
  cat(paste("rMSE for",ntree,"trees and",nfeatures,"features is",round(rmse_prediction,3),"\n"))
}

print_model_parameter_testing_results <- function() {
  cat("  rMSE for 600 trees and 100 features is 10.051
  rMSE for 600 trees and 30 features is 9.689 
  rMSE for 600 trees and 20 features is 10.129 
  rMSE for 600 trees and 10 features is 10.31 
  rMSE for 300 trees and 24 features is 9.837 
  rMSE for 300 trees and 30 features is 9.815 
  rMSE for 300 trees and 20 features is 10.239 
  rMSE for 300 trees and 10 features is 10.259 
  rMSE for 150 trees and 24 features is 10.131 
  rMSE for 150 trees and 30 features is 9.728 
  rMSE for 150 trees and 20 features is 10.16 
  rMSE for 150 trees and 10 features is 10.334 
  rMSE for 50 trees and 24 features is 10.099 
  rMSE for 50 trees and 30 features is 10.431 
  rMSE for 50 trees and 20 features is 11.005 
  rMSE for 50 trees and 10 features is 11.248 ")
}
