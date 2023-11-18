# ---
# title: "Дискриминантный анализ"
# subtitle: "Математические методы в зоологии с использованием R"
# author: "Марина Варфоломеева"

# Пакеты и функции ======================================================
# Для дискриминантного анализа
library(MASS)
source("LDA_helper_functions.R")
# Графики
library(ggplot2)
# Чтение данных
library(readxl)

# ## Пример: Морфометрия ирисов ==========================================
# Сверхзадача --- научиться классифицировать ирисы по нескольким измерениям цветка
data(iris)
head(iris)
colnames(iris)
colnames(iris) <- c("s_len", "s_wid", "p_len", "p_wid", "Sp")

# ## По каким переменным легче всего различить группы?
pairs(iris[, -5], col = iris$Sp)

#I. Дискриминантный анализ на тренировочных и тестовых данных #############

# 1) Разделяем на тренировочные и тестовые данные -------------------------

# устанавливаем зерно для воспроизводимости результатов
set.seed(764737)
# доля от объема выборки, которая пойдет в тренировочный датасет
smp_size <- floor(0.80 * nrow(iris))
# индексы строк, которые пойдут в тренировочный датасет
in_train <- sample(1:nrow(iris), size = smp_size)

# 2) На тренировочных данных получаем стандартизованные коэффициенты главных компонент -----

pca_tr_scaled <- rda(iris[in_train, -5], scale = TRUE)
summary(pca_tr_scaled)

# 3) Извлекаем из тренировочных данных site scores по всем главным компонентам -----

sites_tr_pca <- scores(pca_tr_scaled, display = 'sites', choices = c(1:4),
                       scaling = 'sites', correlation = TRUE)
head(sites_tr_pca, 2)

# 4) На тренировочных данных получаем стандартизованные коэффициенты дискриминантных функций -----

lda_tr_pca <- lda(sites_tr_pca, iris$Sp[in_train])
# коэффициенты дискриминантных функций
lda_tr_pca$scaling

# 5) На тренировочных данных получаем функции классификации ---------------

lda_tr <- lda.class(sites_tr_pca, iris$Sp[in_train])
# Коэф. функций классификации
lda_tr$class.funs

# 6) Оцениваем качество классификации на тренировочных данных -------------

lda_tr_pred <- predict(lda_tr)
table(iris$Sp[in_train], lda_tr_pred$class)


# 7) График классификации тренировочных данных  --------------------------

class_df <- data.frame(lda_tr_pred$x,
                       gr = lda_tr_pred$class,
                       real_gr = iris$Sp[in_train])
ggplot(data = class_df, aes(x = LD1, y = LD2, colour = gr)) +
  geom_text(size = 3, aes(label = real_gr)) +
  theme(legend.position = "none")

# 8) Оценка качества классификации на тестовых данных ---------------------

# получаем PCA на тестовых данных
pca_pred_scaled <- rda(iris[-in_train, -5], scale = TRUE)

# извлекаем site scores
sites_pred_pca <- as.data.frame(scores(pca_pred_scaled, display = 'sites', choices = c(1:4),
                                       scaling = 'sites', correlation = TRUE))

# делаем предсказания
lda_test_pred <- predict(lda_tr, sites_pred_pca)
table(iris$Sp[-in_train], lda_test_pred$class)

# 9) График классификации тестовых данных ---------------------------------

class_df <- data.frame(lda_test_pred$x,
                       new = lda_test_pred$class,
                       real = iris$Sp[-in_train])
class_df$Group <- factor(paste(class_df$real, class_df$new, sep = " as "))

ggplot(data = class_df, aes(x = LD1, y = LD2)) +
  geom_point(aes(colour = Group))

# II. Дискриминантный анализ с кросс-валидацией ###########################
# главные компоненты
pca_cv_scaled <- rda(iris[, -5], scale = TRUE)

# site scores
sites_cv_pca <- scores(pca_cv_scaled, display = 'sites', choices = c(1:4),
                       scaling = 'sites', correlation = TRUE)

# дискриминантый анализ и кросс-валидация

lda_cv <- lda(sites_cv_pca, iris$Sp, CV = TRUE)

table(iris$Sp, lda_cv$class)

# График классификации
ggplot(data = iris, aes(x = p_len,
                        y = s_wid,
                        colour = Sp,
                        shape = lda_cv$class)) +
  geom_point(size = 3) +
  scale_shape_discrete("Classified as")

# DAPC одной функцией (пакет adegenet) -------------
library(adegenet)

## ищем сколько должно быть PCA
iris_xval <- xvalDapc(iris[, -5], iris$Sp, n.pca.max = 300, training.set = 0.8,
                      result = "overall", center = TRUE, scale = TRUE,
                      n.pca = NULL, n.rep = 100, xval.plot = TRUE)
iris_xval$`Number of PCs Achieving Highest Mean Success`
iris_xval$`Number of PCs Achieving Lowest MSE`
iris_xval$`Root Mean Squared Error by Number of PCs of PCA`

## сам анализ
iris_dapc <- dapc(iris[, -5], n.pca = 3, iris$Sp)
iris_dapc

my_col <- c("#50A8FF", "#50FF7F", "#FF50D7") #выбираем красивые цвета

## Визуализация #####
scatter(iris_dapc, col = my_col, xax = 1, yax = 2, cex = 2, scree.da=FALSE, legend = FALSE, grp = iris$Sp)

## вероятность определения правильных групп
assignplot(iris_dapc)
compoplot(iris_dapc, lab="", ncol=1, col = my_col)


# Проверка условий применимости ###########################################

# 1) Mногомерная нормальность
x <- as.matrix(iris[, -5])
d <- mahalanobis(x, colMeans(x), cov(x))
qqplot(x = qchisq(p = ppoints(nrow(x)), df = ncol(x)),
       y = d,
       main="QQ график для оценки многомерной нормальности",
       ylab="Расстояние Махаланобиса")
abline(a = 0, b = 1)

# 2) Гомогенность ковариационных матриц
# функция из файла LDA_helper_functions.R
BoxMTest(as.matrix(iris[, -5]), iris$Sp)

# Квадратичный дискриминантый анализ ----------------------------------------------

# Тренировочные данные
qda_tr <- qda(sites_tr_pca, iris$Sp[in_train])
qda_tr_pred <- predict(qda_tr)
table(qda_tr_pred$class, iris$Sp[in_train])

# Тестовые данные
qda_test_pred <- predict(qda_tr, sites_pred_pca)
table(qda_test_pred$class, iris$Sp[-in_train])

# Кросс-валидация QDA

qda_cv <- qda(sites_cv_pca, iris$Sp, CV = TRUE)
table(iris$Sp, lda_cv$class)

# График классификации
ggplot(data = iris, aes(x = p_len,
                        y = s_wid,
                        colour = Sp,
                        shape = lda_cv$class)) +
  geom_point(size = 3) +
  scale_shape_discrete("Classified as")



# Задание: Классификация без PCA --------------------------------------------------------
# Попробуйте произвести дискриминантый анализ на тех же данных прямо, не выделяя главные компоненты.

