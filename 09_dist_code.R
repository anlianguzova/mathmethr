# title: "Ординация и классификация с использованием мер сходства-различия" ########
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"



## Пример: Морфометрия поссумов ##################

# Данные Lindenmayer et al. (1995)

## Знакомимся с данными
library(DAAG)
data(possum)
head(possum, 2)
colnames(possum)

colSums(is.na(possum))
# оставим только строки с полными наблюдениями
pos <- possum[complete.cases(possum), ]

# поссумы из разных сайтов из 2 популяций
table(pos$Pop, pos$site)


## Неметрическое многомерное шкалирование ########

library(vegan)
ord_euclid <- metaMDS(pos[, 6:14],
                      distance = "euclid",
                      autotransform = FALSE)


## Качество подгонки модели
ord_euclid$stress

# Координаты наблюдений:
head(ord_euclid$points, 10)

# График ординации:
ordiplot(ord_euclid, type = "t", cex = 0.5)

summary(ord_euclid)

## Задание 1 -------------------------------------
#
# При помощи `ggplot2` постройте график
# неметрического многомерного шкалирования.
# Для графика используйте координаты точек
# `ord_euclid$points` и исходные данные.
# Раскрасьте график по значениям переменной `Pop`.
# Сделайте так, чтобы особи разного пола были
# изображены на разных панелях
#
# Дополните код
str(ord_euclid)
library(ggplot2)
# Данные для графика
points_euclid <- data.frame(ord_euclid$points, pos)
head(points_euclid)
# График nMDS ординации
gg_euclid <- ggplot(points_euclid, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(col = Pop), alpha = 0.5) +
  facet_wrap(~ sex)
gg_euclid
str(points_euclid)

## Задание 2 -------------------------------------
#
# Постройте nMDS ординацию при помощи евклидова
# расстояния, по стандартизованным данным

# Дополните код
head(pos, 1)
# Ординация
ord_scaled <- metaMDS(scale(pos[, 6:14]),
                       distance = "euclid",
                       autotransform = FALSE)
# Качество ординации
ord_scaled$stress




## График ординации по матрице евклидовых расстояний,
# рассчитанных по стандартизованным данным
# Данные для графика
points_scaled <- data.frame(ord_scaled$points, pos)
# График nMDS-ординации
gg_scaled <- gg_euclid %+% points_scaled
gg_scaled


## Видно, что графики ординации, полученные
# разными методами, различаются
library(gridExtra)
grid.arrange(gg_euclid + aes(size = age),
             gg_scaled + aes(size = age),
             ncol = 1)


# # Кластерный анализ ############################
#
## Пример: Морфометрия самок поссумов ############

library(DAAG)
data(fossum)


## Создаем "говорящие" названия строк
rownames(fossum) # Было

# Чтобы имена строк были более информативны,
# добавим к ним название популяции
rownames(fossum) <- paste(fossum$Pop,
                          rownames(fossum),
                          sep = "_")
rownames(fossum) # стало

## Отбираем только то, что понадобится
# для кластеризации
sum(is.na(fossum))
fos <- fossum[complete.cases(fossum), 6:14]

## Иерархическая кластеризация ####

## Кластерный анализ начинается с расчета
# матрицы расстояний между объектами

d <- dist(x = fos, method = "euclidean")


## Методы кластеризации ##########################

## Метод ближайшего соседа #######################

hc_single <- hclust(d, method = "single")
str(hc_single)
library(ape)
ph_single <- as.phylo(hc_single)
str(ph_single)
# cex - относительный размер шрифта
plot(ph_single, type = "phylogram",
     direction = "downwards", cex = 0.7)
axisPhylo(side = 2)


## Метод отдаленного соседа ######################

ph_compl <- as.phylo(hclust(d, method = "complete"))
plot(ph_compl, type = "phylogram",
     direction = "downwards", cex = 0.7)
axisPhylo(side = 2)


## Метод невзвешенного попарного среднего ########

ph_avg <- as.phylo(hclust(d, method = "average"))
plot(ph_avg, type = "phylogram",
     direction = "downwards", cex = 0.8)
axisPhylo(side = 2)


## Метод Варда ###################################

ph_w2 <- as.phylo(hclust(d, method = "ward.D2"))
plot(ph_w2, type = "phylogram",
     direction = "downwards", cex = 0.8)
axisPhylo(side = 2)



# Оценка качества кластеризации ##################

## Кофенетическая корреляция #####################

# Кофенетические расстояния
c_single <- as.dist(cophenetic(ph_single))
c_compl <- as.dist(cophenetic(ph_compl))
c_avg <- as.dist(cophenetic(ph_avg))
c_w2 <- as.dist(cophenetic(ph_w2))

# Кофенетические корреляции
cor(d, c_single)
cor(d, c_compl)
cor(d, c_avg)
cor(d, c_w2)


## Бутстреп ######################################

library(pvclust)

system.time({
cl_boot <- pvclust(scale(t(fos)),
                   method.hclust = "average",
                   method.dist = "euclidean",
                   nboot = 500,
                   parallel = TRUE,
                   iseed = 42)
})


plot(cl_boot, cex.pv = 0.8, cex = 0.8)


## Построение деревьев по генетическим данным ####

## Пример: Митохондриальная ДНК приматов. ########

# Датасет собан Dr. Masami Hasegawa
# (Institute of Statistical Mathematics, Tokyo),
# по данным сиквенирования Kenji Hayasaka,
# Takashi Gojobori, Satoshi Horai
# (Molecular Biology and Evolution 5: 626-644, 1988).
# Исходный файл в формате PHYLIP
# можно загрузить по ссылке:
# http://evolution.genetics.washington.edu/book/primates.dna

webpage <- "https://felsenst.github.io/book/primates.dna"
primates.dna <- read.dna(webpage)
d_pri <- dist.dna(primates.dna, model = "K80")
hc_pri <- hclust(d_pri, method = "average")
ph_pri <- as.phylo(hc_pri)
plot(ph_pri, cex = 0.8)
axisPhylo()

library(phangorn)
# neighbor-joining (Saitou, Nei, 1987)
ph_pri_NJ <- NJ(d_pri)
plot(ph_pri_NJ, cex = 0.8)

# Неиерархическая кластеризация ####

## K-means ####
# стандартизация
f_sc <- scale(fos)

# выбор количества кластеров
library(factoextra)
fviz_nbclust(f_sc, kmeans, method = "wss")

f_kmeans <- kmeans(f_sc, centers = 4, nstart = 20) # проводим кластеризацию

my_col <- c("#2E9FDF", "#FF5AD9", "#B5FF0A", "#F37352") # создаём вектор цветов для раскраски

# визуализируем
fviz_cluster(f_kmeans, data = f_sc,
             palette = my_col,
             ggtheme = theme_bw())

## DBSCAN ####

# вложенные кластеры

library(dbscan)
library(factoextra)
library(dplyr)

data('multishapes')

multi_circle <- multishapes %>% filter(shape == c(1, 2))
multi <- multi_circle[, 1:2]

ggplot(multi, aes(x, y)) + geom_point()

# K-means не работает :(
set.seed(123)
circle_kmeans <- kmeans(multi, centers = 2, nstart = 20)
my_col_circle <- c("#2E9FDF", "#E7B800")
fviz_cluster(circle_kmeans, data = multi,
             palette = my_col_circle)

## Задание 3 ###
## Кластеризуйте данные по самкам поссумов, используя DBSCAN
