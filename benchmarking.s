library(rbenchmark)
source("sd_thresholding.s")
source("sd_thresholding_vec.s")
source("sd_thresholding_for_categorical_outcome_variables.s")
source("sd_thresholding_for_categorical_outcome_variables_vec.s")
set.seed(60)
df <- as.data.frame(t(data.frame(x = rnorm(10, mean = 100, sd = 10),
                                 y = rnorm(10, mean = 100, sd = 10), z = rnorm(10, mean = 100, sd = 10))))
outcome <- rnorm(100, mean = 100, sd = 10)

benchmark(sd_thresholding(df,outcome),
          sd_thresholding_vec(df,outcome))

benchmark(sd_thresholding_for_categorical_outcome_variables(df,.25),
          sd_thresholding_for_categorical_outcome_variables_vec(df,.25))