# title: "Регрессионный анализ, часть 2"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева, Анастасия Лянгузова"

# # Множественная линейная регрессия

# ## Пример: реки штата Нью-Йорк ##########

# В 70-е годы в штате Нью Йорк обследовали 20 речных бассейнов (Haith, 1976), чтобы оценить качество воды. Как влияют особенности землепользования на среднюю концентрацию азота (мг/л) в воде?
# (Датасет river из пакета bstats, источник Chatterjee & Hadi, 2006)

# 20 рек в штате Нью Йорк
# - `River`	- название реки
# - `Agr` - процент сельскохозяйственных земель
# - `Forest` - процент земли, занятой лесом
# - `Rsdntial` - процент земель, занятых поселениями
# - `ComIndl`	- процент земель, занятых коммерцией и промышленностью
# - `Nitrogen` - средняя концентрация азота в воде, мг/л
# Т.е. мы хотим подобрать модель вида:
# $Nitrogen_i = b_0 + b_1 Agr_i + b_2 Forest_i + b_3 Rsdntial_i + b_4 ComIndl_i + e_i$



# ## Читаем данные из файла одним из способов ###########

# ### Чтение из xlsx
library(readxl)
river <- read_excel(path = "data/river.xlsx", sheet = "river-data")

# ### Чтение из csv
river <- read.table("data/river.csv", header = TRUE, sep = "\t")

# ## Все ли правильно открылось?

# ## Знакомимся с данными

# Есть ли пропущенные значения?

# Каков объем выборки?

# ## Парные графики для всех числовых переменных
pairs(river[, -1], gap = 0.25, oma=rep(1.75, 4))


# ## Задача 1 ---------------------------------------------
#
# Подберите модель множественной линейной регрессии, чтобы описать, как
# зависит концентрация азота от особенностей землепользования.

# $Nitrogen_i = b_0 + b_1 Agr_i + b_2 Forest_i +
#             + b_3 Rsdntial_i + b_4 ComIndl_i + e_i$

# Запишите уравнение этой линейной модели с коэффициентами.



# ## Влияние выбросов ##########
library(gridExtra)

x_norm <- rnorm(100, mean = 18, sd = 3)
y_norm <- 3.5 + 2 * x_norm
data_norm <- data.frame(x_norm, y_norm)
model_norm <- lm(y_norm ~ x_norm, data = data_norm)
norm_plot <- ggplot(data_norm, aes(x = x_norm, y = y_norm)) +
  geom_point() + geom_smooth(method = 'lm')

data_outliers <- data.frame(x_norm = 11,
                            y_norm = 75)
new_data <- rbind(data_norm, data_outliers)
new_model <- lm(y_norm ~ x_norm, data = new_data)
out_plot <- ggplot(new_data, aes(x = x_norm, y = y_norm)) +
  geom_point() + geom_smooth(method = 'lm')

grid.arrange(norm_plot, out_plot)


# # Проверка условий применимости линейной регрессии ############

# ## 1. Проверка на коллинеарность предикторов ==================
library(car)
vif(river_lm1) # variance inflation factors



# ## Данные для анализа остатков
library(ggplot2) # там есть функция fortify()
river_diag3 <- fortify(river_lm3)
head(river_diag3, 2)


# ## 2. График расстояния Кука для всех наблюдений ==============
ggplot(data = river_diag3, aes(x = 1:nrow(river_diag3), y = .cooksd)) +
  geom_bar(stat = "identity")



# ## Новая модель, на очищенных данных
# данные без выброса
river_subset <- river[river$Rsdntial < 25, ]
# новая модель
river_lm4 <- lm(Nitrogen ~ Agr + Rsdntial, data = river_subset)
# диагностический датафрейм
river_diag4 <- fortify(river_lm4)
# график расстояния Кука
ggplot(data = river_diag4, aes(x = 1:nrow(river_diag4), y = .cooksd)) +
  geom_bar(stat = "identity")

# ## Задание 2 ---------------------------------------------------------
# постройте с использованием данных из `river_diag4`
# 3. График зависимости стандартизованных остатков от предсказанных значений =====





# ## 4. Квантильный график стандартизованных остатков ===================
library(car)
qqPlot(river_lm4, id = FALSE) # из пакета car



# ## Интерпретация коэффициентов регрессии ===========================
# Обычные коэффициенты
coef(river_lm4)

# Стандартизованные коэффициенты
scaled_river_lm4 <- lm(Nitrogen ~ scale(Agr) + scale(Rsdntial), data = river_subset)
coef(scaled_river_lm4)

# ## Задание 3 --------------------------------------------------
#
# Определите по значениям стандартизованных коэффициентов, какие предикторы
# сильнее всего влияют на концентрацию азота в воде?





# ## Оценка качества подгонки модели ===========================
# Коэффициент детерминации можно добыть из `summary()` модели в виде отдельного числа. Как?
smr <- summary(river_lm4)
smr


