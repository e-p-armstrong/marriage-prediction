library(tidyverse)
library(tidymodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Data source: https://archive.ics.uci.edu/ml/datasets/student+performance
df_init <- read_delim("adult.csv",delim=",")
head(df_init)
df <- df_init
#df <- select(df_init, romantic, freetime, goout, G3,)
#filter(df, romantic == "yes") # There's a good number of normies, ~ 1/3. Aim 
                              # for above 75% accuracy.
                              # G3 is out of 20

df <- mutate(df, `marital-status` = ifelse(`marital-status` == "Married-civ-spouse", 1, 0))



columns_to_encode <- c("workclass","education","occupation","race","gender","income")

df |> group_by(!!rlang::sym(c("education"))) |> summarize(count = n())
one_hot <- function(bool){
  print("This is bool\n\n")
  print(bool)
  if (bool){
    return(1)
  }
  return(0)
}
for (column in columns_to_encode){
  print("This is the column")
  print(column)
  new_cols = df |> group_by(!!rlang::sym(c(column))) |> summarize(count = n()) |> pull(1)
  cat("This is new_cols:")
  print(new_cols)
  for (item in new_cols){
    cat("This is item:")
    print(item)
    df <- df |> mutate( "{item}" := !!rlang::sym(c(column)) == item)
    df <- mutate(df, "{item}" := ifelse(!!rlang::sym(c(item)), 1, 0))
    #df <- df |> mutate( "{item}" := one_hot(!!rlang::sym(c(item))))
  }
}
df
df_for_dectree <- select(df, -age,-workclass,-fnlwgt,-education,-`educational-num`,-occupation,-relationship,-`capital-gain`,-`capital-loss`,-`hours-per-week`,-`native-country`,-`income`,-`race`,-`gender`)
df_for_dectree
#mutate(df, across(16:61, one_hot))

split <- initial_split(df_for_dectree, prop=0.75, strata = `marital-status`)
df_train <- training(split)
df_train
df_test <- testing(split)
df_test

write_delim(df, file = "student-mat-clean.csv",delim = ",")
write_delim(df_train, file = "adult-one-hot-train.csv",delim = ",")
write_delim(df_test, file = "adult-one-hot-test.csv",delim = ",")
