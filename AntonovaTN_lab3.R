# Антонова ТН, вариант 2 

# 1. Загрузка данных
census_income <- read.csv("/Users/tatyana/Downloads/data_lab3.csv")

# Посмотрим есть ли NA: ответ - есть
summary(census_income)

# 2. Предварительная обработка данных

# Удаление строк с пропусками
data <- na.omit(census_income)

summary(data) # Теперь NA нет

# Преобразование категориальных переменных в факторы
data$workclass[data$workclass=='Private'] <- 'Private'
data$workclass[data$workclass=='Self-emp-not-inc'] <- 'Self-emp-not-inc'
data$workclass[data$workclass=='Self-emp-inc'] <- 'Self-emp-inc'
data$workclass[data$workclass=='Federal-gov'] <- 'Federal-gov'
data$workclass[data$workclass=='Local-gov'] <- 'Local-gov'
data$workclass[data$workclass=='State-gov'] <- 'State-gov'
data$workclass[data$workclass=='Without-pay'] <- 'Without-pay'
data$workclass[data$workclass=='Never-worked'] <- 'Never-worked'
data$workclass <- as.factor(data$workclass)

data$education[data$education=='Bachelors'] <- 'Bachelors'
data$education[data$education=='Some-college'] <- 'Some-college'
data$education[data$education=='11th'] <- '11th'
data$education[data$education=='HS-grad'] <- 'HS-grad'
data$education[data$education=='Prof-school'] <- 'Prof-school'
data$education[data$education=='Assoc-acdm'] <- 'Assoc-acdm'
data$education[data$education=='Assoc-voc'] <- 'Assoc-voc'
data$education[data$education=='9th'] <- '9th'
data$education[data$education=='7th-8th'] <- '7th-8th'
data$education[data$education=='12th'] <- '12th'
data$education[data$education=='Masters'] <- 'Masters'
data$education[data$education=='1st-4th'] <- '1st-4th'
data$education[data$education=='10th'] <- '10th'
data$education[data$education=='Doctorate'] <- 'Doctorate'
data$education[data$education=='5th-6th'] <- '5th-6th'
data$education[data$education=='Preschool'] <- 'Preschool'
data$education <- as.factor(data$education)

data$marital.status[data$marital.status=='Married-civ-spouse'] <- 'Married-civ-spouse'
data$marital.status[data$marital.status=='Divorced'] <- 'Divorced'
data$marital.status[data$marital.status=='Never-married'] <- 'Never-married'
data$marital.status[data$marital.status=='Separated'] <- 'Separated'
data$marital.status[data$marital.status=='Widowed'] <- 'Widowed'
data$marital.status[data$marital.status=='Married-spouse-absent'] <- 'Married-spouse-absent'
data$marital.status[data$marital.status=='Married-AF-spouse'] <- 'Married-AF-spouse'
data$marital.status <- as.factor(data$marital.status)

data$occupation[data$occupation=='Tech-support'] <- 'Tech-support'
data$occupation[data$occupation=='Craft-repair'] <- 'Craft-repair'
data$occupation[data$occupation=='Other-service'] <- 'Other-service'
data$occupation[data$occupation=='Sales'] <- 'Sales'
data$occupation[data$occupation=='Exec-managerial'] <- 'Exec-managerial'
data$occupation[data$occupation=='Prof-specialty'] <- 'Prof-specialty'
data$occupation[data$occupation=='Handlers-cleaners'] <- 'Handlers-cleaners'
data$occupation[data$occupation=='Machine-op-inspct'] <- 'Machine-op-inspct'
data$occupation[data$occupation=='Adm-clerical'] <- 'Adm-clerical'
data$occupation[data$occupation=='Farming-fishing'] <- 'Farming-fishing'
data$occupation[data$occupation=='Transport-moving'] <- 'Transport-moving'
data$occupation[data$occupation=='Priv-house-serv'] <- 'Priv-house-serv'
data$occupation[data$occupation=='Protective-serv'] <- 'Protective-serv'
data$occupation[data$occupation=='Armed-Forces'] <- 'Armed-Forces'
data$occupation <- as.factor(data$occupation)

data$relationship[data$relationship=='Wife'] <- 'Wife'
data$relationship[data$relationship=='Own-child'] <- 'Own-child'
data$relationship[data$relationship=='Husband'] <- 'Husband'
data$relationship[data$relationship=='Not-in-family'] <- 'Not-in-family'
data$relationship[data$relationship=='Other-relative'] <- 'Other-relative'
data$relationship[data$relationship=='Unmarried'] <- 'Unmarried'
data$relationship <- as.factor(data$relationship)

data$race[data$race=='White'] <- 'White'
data$race[data$race=='Asian-Pac-Islander'] <- 'Asian-Pac-Islander'
data$race[data$race=='Amer-Indian-Eskimo'] <- 'Amer-Indian-Eskimo'
data$race[data$race=='Black'] <- 'Black'
data$race[data$race=='Other'] <- 'Other'
data$race <- as.factor(data$race)

data$sex[data$sex=='Male'] <- 'Male'
data$sex[data$sex=='Female'] <- 'Female'
data$sex <- as.factor(data$sex)

summary(data)

