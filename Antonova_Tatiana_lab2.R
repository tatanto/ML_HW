#Антонова, четный вариант

data <- read.csv("/Users/tatyana/Downloads/NCbirth.csv")



data$Plural <- as.factor(data$Plural)

data$Sex[data$Sex==1] <- 'male'
data$Sex[data$Sex==2] <- 'female'
data$Sex <- as.factor(data$Sex)

data$Marital[data$Marital==1] <- 'yes'
data$Marital[data$Marital==2] <- 'no'
data$Marital <- as.factor(data$Marital)

data$RaceMom[data$RaceMom==1] <- 'white'
data$RaceMom[data$RaceMom==2] <- 'black'
data$RaceMom[data$RaceMom==3] <- 'american indian'
data$RaceMom[data$RaceMom==4] <- 'chinese'
data$RaceMom[data$RaceMom==5] <- 'japanese'
data$RaceMom[data$RaceMom==6] <- 'hawaiian'
data$RaceMom[data$RaceMom==7] <- 'filipino'
data$RaceMom[data$RaceMom==8] <- 'other asian or pacific islander'
data$RaceMom <- as.factor(data$RaceMom)

data$HispMom[data$HispMom=='N'] <- 'not Hispanic'
data$HispMom[data$HispMom=='P'] <- 'Puerto Rico'
data$HispMom[data$HispMom=='M'] <- 'Mexican'
data$HispMom[data$HispMom=='S'] <- 'Central/South America'
data$HispMom[data$HispMom=='O'] <- 'Other Hispanic'
data$HispMom[data$HispMom=='C'] <- 'Cuban'
data$HispMom <- as.factor(data$HispMom)

data$Smoke[data$Smoke==1] <- 'yes'
data$Smoke[data$Smoke==0] <- 'no'
data$Smoke <- as.factor(data$Smoke)

data$Low[data$Low==1] <- 'yes'
data$Low[data$Low==0] <- 'no'
data$Low <- as.factor(data$Low)

data$Premie[data$Premie==1] <- 'yes'
data$Premie[data$Premie==0] <- 'no'
data$Premie <- as.factor(data$Premie)

summary(data)

      # График 1
# График зависимости: Вес новорожденного от возраста матери
plot(data$MomAge, data$BirthWeightGm,
     main = "Вес новорожденного от возраста матери",
     xlab = "Возраст матери, годы",
     ylab = "Вес новорожденного, грамм",
     pch = 20, col = "red")

      #График 2 - гистограмма 
# Гистограмма распределения веса новорожденных,матери которых относятся к белой расе и курят

# Фильтрация данных
filtered_data <- data[data$RaceMom == "white" & data$Smoke == "yes", ]

# Убедимся, что колонка веса имеет числовой тип и удалим пропущенные значения
filtered_data$BirthWeightGm <- as.numeric(as.character(filtered_data$BirthWeightGm))
filtered_data <- na.omit(filtered_data)

# Количество интервалов
breaks <- 25

# Построение гистограммы с относительными частотами
hist(filtered_data$BirthWeightGm, breaks = breaks, probability = TRUE, col = "lightblue", 
     main = "Распределение веса новорожденных", 
     xlab = "Вес новорожденного, граммы", 
     ylab = "Относительная частота")

# Добавление графика плотности вероятности
lines(density(filtered_data$BirthWeightGm), col = "red")


      #График 3 - ящик с усами
# Построение диаграмм размаха распределения веса новорожденных 
# для всех значений факторной переменной Plural
boxplot(BirthWeightGm ~ Plural, 
        data = data, 
        main = "Диаграммы размаха веса новорожденных по числу детей", 
        xlab = "Число новорожденных", 
        ylab = "Вес новорожденного, грамм", 
        col = "lightgreen")

      #График 4 - столбиковая диаграмма
# Рассчет среднего веса для каждой категории Plural
mean_weights <- aggregate(BirthWeightGm ~ Plural, data = data, FUN = mean)

# Построение столбиковой диаграммы
barplot(mean_weights$BirthWeightGm, 
        names.arg = mean_weights$Plural, 
        main = "Средний вес новорожденных по количеству новорожденных", 
        xlab = "Количество новорожденных", 
        ylab = "Средний вес новорожденного, грамм", 
        col = "lightblue", 
        border = "darkblue")


      # График 5 - мозаичная диаграмма 

# Подсчет числа сочетаний разных значений
table_combination <- table(filtered_data2$Smoke, filtered_data2$Premie, filtered_data2$Low)
print(table_combination)

# Установка и загрузка пакета vcd
install.packages("vcd") 
library(vcd)

mosaic(~Smoke+Premie+Low, data = data, main = "Мозаичная диаграмма: Smoke, Premie, Low",
       shade = TRUE)

# Этот код сначала подсчитывает количество сочетаний для трех категориальных переменных
#и затем строит мозаичную диаграмму, которая визуализирует их совместное распределение

      # График 6 

# Установка и загрузка пакета psych
install.packages("psych")  
library(psych)

# Построение матричной диаграммы рассеяния
pairs.panels(data[, c("MomAge", "BirthWeightGm", "Weeks")], 
             main = "Матрица рассеяния: MomAge, BirthWeightGm, Weeks")


      #График 7 Диаграмма Кливленда 

# Фильтрация данных для двоих детей
twins_data <- data[data$Plural == 2, ]

# Определение групп по возрасту матерей
twins_data$AgeGroup <- cut(twins_data$MomAge, 
                           breaks = c(-Inf, 20, 30, Inf), 
                           labels = c("Младше 20", "20-30", "Старше 30"))

# Определение цветов для групп
colors <- c("lightblue", "lightgreen", "lightpink")

# Построение диаграммы Кливленда
dotchart(twins_data$BirthWeightGm, 
         groups = twins_data$AgeGroup, 
         main = "Вес новорожденных из двоен по возрасту матерей", 
         xlab = "Вес новорожденного, грамм", 
         col = colors[as.numeric(twins_data$AgeGroup)], 
         pch = 19, 
         xlim = range(twins_data$BirthWeightGm))

# Добавление легенды
legend("topright", legend = levels(twins_data$AgeGroup), 
       col = colors, pch = 19)

      # График 8

# Установка и загрузка необходимых пакетов
install.packages("ggplot2") 
install.packages("dplyr") 
library(ggplot2)
library(dplyr)

# Подготовка данных
data_summary <- data %>%
  group_by(Sex, BirthWeightGm) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(BirthWeightGm) %>%
  mutate(Proportion = Count / sum(Count))

# Построение пропорциональной stacked area graph
ggplot(data_summary, aes(x = BirthWeightGm, y = Proportion, fill = Sex)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Пропорции новорожденных разного пола от веса при рождении",
       x = "Вес новорожденного, грамм",
       y = "Пропорция") +
  theme_minimal()

