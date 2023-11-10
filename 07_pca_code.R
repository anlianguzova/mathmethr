# title: "Анализ главных компонент"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева, Анастасия Лянгузова"

# ## Пример: измерения ирисов ######

# ## Знакомимся с данными
data(iris)
colnames(iris)
colnames(iris) <- c("s_len", "s_wid", "p_len", "p_wid", "Sp") # переименовываем столбцы

# количество ирисов разных видов
table(iris$Sp)

# сколько всего видов
n_species <- length(unique(iris$Sp))
# цвета из Брюеровской палитры 'Dark2'
library(RColorBrewer)
cols <- brewer.pal(n = n_species, name = 'Dark2')
# график измерений
pairs(iris[, 1:4], col = cols[iris$Sp])

### Анализ главных компонент ###########
library(vegan)
# ординация, используем исключительно измерения
ord_iris <- rda(iris[, 1:4], scale = TRUE)
summary(ord_iris)


# 1. Сколько компонент нужно оставить?
# 2. Сколько общей изменчивости объясняют оставленные компоненты?
# 3. Что означают получившиеся компоненты?
# 4. Как располагаются объекты в пространстве главных компонент?


#### 1. Cколько компонент нужно оставить? ############

# собственные числа
eigenvals(ord_iris)

# График собственных чисел
screeplot(ord_iris, bstick = TRUE, type = "lines")

#### 2. Сколько изменчивости объясняют компоненты? #########

# Изменчивость, объясненная каждой из компонент, в процентах
eigenvals(ord_iris) / sum(eigenvals(ord_iris)) * 100

#### 3. Что означают получившиеся компоненты? ############

# Факторные нагрузки
scores(ord_iris, display = "species", choices = c(1, 2),
       scaling = "species", correlation = TRUE)

biplot(ord_iris, scaling = 'species', correlation = TRUE,
       main = 'PCA -  species scaling', display = 'species')

## График факторных нагрузок в ggplot2 #####
library(ggplot2)
theme_set(theme_bw())
library(ggrepel) # для подписей (geom_text_repel)
library(grid) # для стрелочек
# параметры стрелочек
ar <- arrow(length = unit(0.1, 'cm'))

# датафрейм с факторными нагрузками
df_load_iris <- data.frame(scores(ord_iris, display = 'species',
                                  choices = c(1, 2), scaling = 'species', correlation = TRUE))

# график нагрузок
ggloadings <- ggplot(df_load_iris) +
  geom_text_repel(aes(x = PC1, y = PC2,
                      label = rownames(df_load_iris)), segment.alpha = 0.5, size = 3) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = 'grey40', arrow = ar) +
  coord_equal(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8))
ggloadings

##### Факторные координаты ####
scores(ord_iris, display = 'sites',  choices = c(1, 2), scaling = 'sites')

# график посредством базовой графики
biplot(ord_iris, scaling = 'sites', display = 'sites',
       type = 't', main = 'PCA - sites scaling')

###### График в ggplot2 ######
# данные для графика: факторные координаты и исходные переменные
df_scores_iris <- data.frame(iris,
                             scores(ord_iris, display = 'sites', scaling = 'sites',
                                    choices = c(1, 2)))
# график ординации
ggscores <- ggplot(df_scores_iris, aes(x = PC1, y = PC2,
                                       colour = Sp)) +
  geom_point(size = 2) + coord_equal()
ggscores

# смотрим всё вместе
library(cowplot)
plot_grid(ggloadings, ggscores, labels = 'AUTO')


# ## Пример: Морфометрия поссумов ##################
# Данные Lindenmayer et al. (1995)

# ## Знакомимся с данными
library(DAAG)
data(possum)
colnames(possum)

colSums(is.na(possum))
# оставим только строки с полными наблюдениями
pos <- possum[complete.cases(possum), ]

# поссумы из разных сайтов из 2 популяций
table(pos$site, pos$Pop)

# половой состав выборок из разных сайтов
table(pos$sex, pos$site, pos$Pop)

# ## Как связаны признаки между собой?

# Серия графиков с признаками во всех возможных комбинациях
pairs(pos[, 6:14],
      pch =  as.numeric(pos$sex))

# поссумы из разных популяций раскрашены разными цветами
pairs(pos[, 6:14],
      col = pos$Pop,
      pch =  as.numeric(pos$sex))

# поссумы из разных точек раскрашены разными цветами
pairs(pos[, 6:14],
      col = pos$site,
      pch =  as.numeric(pos$sex))


#### Задание 1 ---------------------------------------------

# Сделайте анализ главных компонент:
# 1. Сколько компонент нужно оставить?
# 2. Сколько общей изменчивости объясняют оставленные компоненты?
# 3. Что означают получившиеся компоненты?
# 4. Как располагаются объекты в пространстве главных компонент?

# Как различаются поссумы разных популяций между собой?


