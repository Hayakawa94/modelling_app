
source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")

library(parallel)

exp <- "cars"


exp_path <- glue("{here::here()}/{exp}") 
if ( !file.exists(exp_path)){
  
  dir.create(exp_path)
}
# # specify modelling data 
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)  # Convert 'cyl' to categorical
mtcars$carb <- as.factor(mtcars$carb)
train <-mtcars
train %>% mutate(exposure = 1) %>% as.data.table() -> train

fts <- train %>% select(-mpg  ,- exposure    ) %>% names %>% sort

set.seed(1)
train$idx = 1:nrow(train)
train %>% sample_frac(0.1) -> test
train[setdiff(train$idx , test$idx)] %>% select(-idx) -> train
test<- test %>% select(-idx)
nrow(train) + nrow(test)

# specify model spec
model_spec <-list(mpg = list(exposure = 'exposure',
                             response = "mpg",
                             objective = 'reg:squarederror',
                             eval_metric='rmse',
                             fam = gaussian()))


selected_model = "mpg"




file.copy(from = "Server.R" , to = glue("{exp}/Server.R"),overwrite = T)
# ShinyTester::ShinyHierarchy(directory =exp_path ,ui ="UI.R" , server ="Server.R" )
shiny::runApp(glue("{exp}/Server.R"),launch.browser = TRUE)
