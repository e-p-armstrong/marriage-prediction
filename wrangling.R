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



columns_to_encode <- c("workclass","education","occupation","race","gender")

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


mutate(df, "hi" = 5)
df

head(df)
#group_by(df,romantic) |> summarize(count = n())
#132/395

split <- initial_split(df_for_dectree, prop=0.75, strata = `marital-status`)
df_train <- training(split)
df_train
df_test <- testing(split)
df_test

write_delim(df, file = "student-mat-clean.csv",delim = ",")
write_delim(df_train, file = "adult-one-hot-train.csv",delim = ",")
write_delim(df_test, file = "adult-one-hot-test.csv",delim = ",")

# For decision tree, categorical
df_2 <- select(df_init, romantic, freetime, studytime, higher, famrel, traveltime, Pstatus, goout, G3, activities, health)
df_2 <- mutate(df_2, romantic = ifelse(romantic == "yes", 1, 0))
df_2 <- mutate(df_2, higher = ifelse(higher == "yes", 1, 0))
df_2 <- mutate(df_2, activities = ifelse(activities == "yes", 1, 0))
df_2 <- mutate(df_2, Pstatus = ifelse(Pstatus == "T", 1, 0))
df_2
df_2 <- mutate(df_2, across(everything(), as.character))

df_2 |> filter()

split_2 <- initial_split(df_2, prop=0.75, strata = romantic)
df_train_2 <- training(split_2)
df_train_2
df_test_2 <- testing(split_2)
df_test_2

write_delim(df_2, file = "student/student-mat-clean-dt.csv",delim = ",")
write_delim(df_test_2, file = "student/student-mat-clean-dt-test.csv",delim = ",")
write_delim(df_train_2, file = "student/student-mat-clean-dt-train.csv",delim = ",")

ggplot(df,aes(x = G3, color = as.factor(romantic))) +
  geom_histogram() +
  labs(x = "Grades/20 (higher = better)", y = "Count", color = "Has a romantic relationship? 0 = no")
