# Проблемы с кодировкой:
# File -> Reopen with Encoding -> UTF-8

# собственно, R
# https://cran.r-project.org/
#
# RStudio - среда для разработки на R
# https://www.rstudio.com
#
# Онлайн IDE для R
# https://rstudio.cloud
#
# Сайт курса Математические методы в зоологии
# https://anlianguzova.github.io/mathmethr/

# Строки, начинающиеся решеткой - это комментарии

# Ctrl+Enter - выполнить активную строку кода


## Математические операции -------------------------------------------------------------
2+2
1024/2
1:10
34*4
2^4
sqrt(27)


## Переменные -------------------------------------------------------------
var_1 <- 1024 / 2
1238 * 3 -> var_2
var_2


## Векторы
23
sqrt(25)

1:10 # от одного до 10
-5:3 # от -5 до 3


## Создание векторов через функции -------------------------------------------------------------
seq(from = 1, to = 5, by = 0.5)

?c # посмотрите хелп к функции

c(2, 4, 6)
c(-9.3, 0, 2.17, 21.3)

vect_num <- -11:12 # численный вектор от -11 до 12 сохранен в переменной vect_num
vect_num_1 <- c(1.3, 1.7, 1.2, 0.9, 1.6, 1.4) # численный вектор, сохранен в переменной vect_num_1


## Адресация внутри векторов -------------------------------------------------------------
vect_num[1] # первый элемент в векторе vect_num
vect_num[10] # 10-й элемент
vect_num[22]

vect_num[3:5] # несколько элементов

vect_num[c(2, 4, 6)] # возвращает 2-й, 4-й и 6-й элементы
vect_num[c(1, 10, 20)] # возвращает 1-й, 10-й и 20-й элементы

vect_num[c(1, 2, 5)] # возвращает 1-й, 3-й и 5-й элементы



vect_num[1, 3, 5] # ошибка
vect_num[15, 9, 1] # ошибка


vect_num[c(15, 9, 1)] # правильно


## Объединение векторов -------------------------------------------------------------
c(1, 1, 5:9)
c(vect_num, vect_num)
c(100, vect_num)

vect_num[c(1, 3, 5, 22:24)]


## Типы данных в R. Текстовые данные -------------------------------------------------------------
"это текст"
'это тоже текст'


## Векторы -------------------------------------------------------------
rainbow <- c("red", "orange", "yellow", "green", "blue", "violet")
rainbow # весь вектор

rainbow[c(1, 6)]

double_rainbow <- c(rainbow, rainbow)
double_rainbow
rainbow[3:6] # элементы с 3 по 6


## Логические данные -------------------------------------------------------------
TRUE # истина
FALSE # ложь


## можно сокращать. но лучше так не делать, нечитабельно становится
c(T, T, T, T, F, F, T, T)

c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)

short_logical_vector <- c(FALSE, TRUE)


## Создание длинных векторов-------------------------------------------------------------
rep(x = 1, times = 3) # 1 повторяется 3 раза
rep(x = "red", times = 5) # "red" повторяется 5 раз
rep(x = TRUE, times = 2) # TRUE повторяется 2 раза

rep(TRUE, 5) # TRUE повторяется 5 раз, аргументы без названий

vect_log <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 4))
vect_log


## Фильтрация данных -------------------------------------------------------------
f_yellow <- double_rainbow == "yellow"
f_yellow

double_rainbow[f_yellow]

f_blue <- double_rainbow == "blue"

f_yellow | f_blue

double_rainbow[f_yellow | f_blue]


## Сокращение записи -------------------------------------------------------------
double_rainbow[double_rainbow == "yellow" | double_rainbow == "blue"]

f_colours <- double_rainbow == "yellow" | double_rainbow == "blue"
double_rainbow[f_colours]


## Фильтрация числовых векторов -------------------------------------------------------------
vect_num[vect_num > 0]

f_5_8 <- (vect_num <= -8) | (vect_num >= 8)
vect_num[f_5_8]


## Факторы -------------------------------------------------------------
snail_colours <- c("red", "green", "green", "green", "yellow", "yellow", "yellow", "yellow")
snail_colours # это текстовый вектор

factor(snail_colours) # а так фактор


double_rainbow # текстовый вектор

f_double_rainbow <- factor(double_rainbow) # а так фактор


## Проверяем классы переменных -------------------------------------------------------------
class(f_double_rainbow)
class(vect_log)
class(vect_num)
class(rainbow)


## Встроенные константы в R -------------------------------------------------------------
rainbow_1 <- c("red", "orange", NA, "green", "blue", "violet")

rainbow_1[198]


## Арифметические операции с векторами -------------------------------------------------------------
vect_num

vect_num + 2
vect_num * 2
vect_num * (-2)
vect_num^2


## Операции с векторами, содержащими константы -------------------------------------------------------------
NAs_NANs <- c(1, 3, NA, 7, 0, 22:24)


NAs_NANs + 2 # останется NA
NAs_NANs * 0 # останется NA
NAs_NANs / 0  # останется NA


sqrt(-1)


## Функции в R -------------------------------------------------------------
NAs_NANs

length(NAs_NANs)

sum(NAs_NANs)

sum(NAs_NANs, na.rm = TRUE)

mean(NAs_NANs, na.rm = TRUE)


## Пользовательские функции -------------------------------------------------------------
mmean <- function(x){
  mean(x, na.rm = TRUE)
  }


## Return в теле функции -------------------------------------------------------------
mmean <- function(x){
  res <- mean(x, na.rm = TRUE)
  return(res)
}

mean(vect_num, na.rm = TRUE)
mmean(vect_num)


## Матрицы -------------------------------------------------------------
matrix(1:9, nrow = 3, ncol = 3)


## Списки -------------------------------------------------------------

my_list <- list(
  12:16,
  snail_colours,
  c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
)

str(my_list)

## Датафреймы -------------------------------------------------------------
len <- 1:9 # числовой
col <- c(rep("green", 4), rep("red", 5)) # текстовый
wid <- seq(from = 2, by = 2, to = 18) # числовой

my_worms <- data.frame(Length = len, Width = wid, Colour = col, stringsAsFactors = TRUE)

class(my_worms) # смотрим, действительно датафрейм


## Превращение других типов данных в датафрейм -------------------------------------------------------------
mat <- matrix(1:9, nrow = 3, ncol = 3)
mat_data <- as.data.frame(mat)
str(mat_data)


## Содержимое датафрейма -----------------------------------------------
my_worms # печать датафрейма
View(my_worms) # просмотр в RStudio
head(my_worms)
tail(my_worms)
fix(my_worms) # ручное редактирование. осторожно! избегайте его использовать, никаких документов о нем не останется


##Смотрим информацию о датафрейме -------------------------------------------------------------
nrow(my_worms)
ncol(my_worms)
length(my_worms) #выдаст количество столбцов


## Адресация внутри датафрейма -------------------------------------------------------------
my_worms$Length
my_worms$Width

my_worms[2, 3] # вторая строка в 3 столбце
my_worms[2, ] # вторая строка целиком
my_worms[1:9, 2] # строки с 1 по 9 во втором столбце
my_worms[, 2] # второй столбец целиком


## Расширение датафрейма -------------------------------------------------------------
my_worms$Site <- c("Sredniy", "Sredniy", "Ogorod", "Vichennaya", "Ogorod", "Vichennaya", "Keret", "Ogorod", "Vichennaya")
my_worms


## Добавление строки -------------------------------------------------------------
my_worms_new <- data.frame(Length = 40, Width = 60, Colour = "blue", Site = "Pashinnikov")
my_worms_final <- rbind(my_worms, my_worms_new)
my_worms_final

## Тибблы                        -------------------------------------------------------------
#install.packages('tibble')
library(tibble)
my_tibble <- tibble(Length = len, Width = wid, Colour = col)
str(my_tibble)

my_tibble2 <- tibble(Length = len * 2, Width = wid * 4.5, Colour = col)
my_tibble2

## Визуализация данных базовой графикой --------------------------------------------------------
barplot(my_worms$Width) # барплот

hist(my_worms$Width) # гистограмма

plot(x = my_worms$Width, y = my_worms$Length) # скаттерплот

plot(x = as.factor(my_worms$Colour), y = my_worms$Length) # боксплот


## Графики из ggplot2 ---------------------------------------------------
# install.packages('ggplot2')
library(ggplot2)


##  Барплот ---------------------------------------------------
ggplot(data = my_worms) +
  geom_bar(aes(x = Width, y = Length), stat = "identity")


## Гистограмма --------------------------------------------------
ggplot(data = my_worms, aes(x = Width)) +
  geom_histogram(binwidth = 4, breaks = c(1, 5, 10, 15, 20))


## Скаттерплот ---------------------------------------------------
ggplot(data = my_worms) +
  geom_point(aes(x = Width, y = Length))


## Боксплот -------------------------------------------------------
ggplot(data = my_worms) +
  geom_boxplot(aes(x = Colour, y = Length))


## Раскрашиваем графики ----------------------------------------------------
ggplot(data = my_worms) +
  geom_point(aes(x = Width, y = Length, colour = Colour))

gg <- ggplot(data = my_worms) +
  geom_point(aes(x = Width, y = Length, colour = Colour))
gg


## Темы графиков ----------------------------------------------------
gg + theme_dark()
gg + theme_light()
gg + theme_classic()


## Установка темы -------------------------------------------------
theme_set(theme_bw())
gg


## Легенды на осях ------------------------------------------------------
gg + labs(x = "Ширина", y = "Длина", colour = "Цвет")


## Фасетки ----------------------------------------------------
gg + facet_wrap(~Colour, nrow = 1)


## Легенды ----------------------------------------
my_worms$col_rus <- factor(my_worms$Colour, levels = c("green", "red"), labels = c("Зеленый", "Красный"))
ggplot(data = my_worms) +
  geom_point(aes(x = Width, y = Length, colour = col_rus)) +
  labs(x = "Ширина", y = "Длина", colour = "Цвет") +
  facet_wrap(~col_rus, nrow = 1)

