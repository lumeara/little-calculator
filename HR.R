#Installing required packages
library(tidyverse)
library(stringr)


#CALCULADORA DE SEQUENCIA DE HR(95%)

#Inputs
raw_input <- readLines("C:/Users/luisa/OneDrive/Documentos/number.txt")
raw_input

study_names <- readLines("C:/Users/luisa/OneDrive/Documentos/study_names.txt")
study_names

data <- data.frame(Study = c(study_names), HR_CI = c(raw_input))
data

#Loop that iterates over the given set of HR(95% CI)
for(input in raw_input) {
  
  #Extracting the numerical data from the input
  input_vector <- str_split_1(input, " \\(")
  CI_string <- str_sub(input_vector[2], end = -2)
  CI_vector <- str_split_1(CI_string, ", ")
  HR_string <- input_vector[1]
  HR <- as.numeric(HR_string)
  CI <- as.vector(sapply(CI_vector, as.numeric))
  
  HR
  CI
  
  #Math operations
  log_upper <- log(CI[2])
  log_lower <- log(CI[1])
  variance <- (((log_upper - log_lower) / (2 * 1.96))^2)
  SE <- round(sqrt(variance), 4)
  logHR <- round(log(HR), 4)
  
  
  print(logHR)
  print(SE)
  
}











#CALCULADORA DE HR(95%) UNICO
input <- readline("Enter HR and CI:")
input

#Extracting the numerical data from the input
input_vector <- str_split_1(input, " \\(")
print(input)
CI_string <- str_sub(input_vector[2], end = -2)
CI_vector <- str_split_1(CI_string, ", ")
HR_string <- input_vector[1]
HR <- as.numeric(HR_string)
CI <- as.vector(sapply(CI_vector, as.numeric))

HR
CI

#Math operations
log_upper <- log(CI[2])
log_lower <- log(CI[1])
variance <- (((log_upper - log_lower) / (2 * 1.96))^2)
SE <- round(sqrt(variance), 4)
logHR <- round(log(HR), 4)

logHR
SE
