## ----setup, include = FALSE, cache = FALSE, purl = TRUE------------------------------
source("assets/xaringan_setup.R")
library(xaringanExtra)
use_tile_view()
use_scribble()
use_search(show_icon = FALSE)
use_progress_bar(color = "#98BF64", location = "bottom", height = "10px")
use_freezeframe()
# use_webcam()
# use_panelset()
# use_extra_styles(hover_code_line = TRUE)

# http://tachyons.io/docs/
# https://roperzh.github.io/tachyons-cheatsheet/
use_tachyons()
style_mono_light(base_color = "#9FFFD6")
source('support_mathmethr.R')


## ------------------------------------------------------------------------------------
library(readxl)
teeth <- as.data.frame(read_excel(path = 'data/nuclear_teeth.xlsx', sheet = 1))


## ------------------------------------------------------------------------------------
teeth <- read.table(file = 'data/nuclear_teeth.csv', header = TRUE, sep = ',')


## ------------------------------------------------------------------------------------
str(teeth)      # Структура данных
head(teeth, 3)     # Первые 3 строки файла


## ------------------------------------------------------------------------------------
colnames(teeth)


## ------------------------------------------------------------------------------------
colnames(teeth) <- c('birth', 'c14')


## ------------------------------------------------------------------------------------
colnames(teeth)


## ------------------------------------------------------------------------------------
colSums(is.na(teeth))


## ------------------------------------------------------------------------------------
nrow(teeth)


## ----gg_base_1-----------------------------------------------------------------------
library(ggplot2)
ggplot()


## ----gg_base_2-----------------------------------------------------------------------
ggplot(data = teeth)


## ----gg_aes--------------------------------------------------------------------------
ggplot(data = teeth, aes(x = c14, y = birth))


## ----gg-point------------------------------------------------------------------------
ggplot(data = teeth, aes(x = c14, y = birth)) + 
  geom_point()


## ----gg-labs-------------------------------------------------------------------------
ggplot(data = teeth, aes(x = c14, y = birth)) + 
  geom_point() + 
  labs(x = 'Содержание нестабильного изотопа углерода, %', 
       y = 'Год рождения человека')


## ----gg-var--------------------------------------------------------------------------
gg_teeth <- ggplot(data = teeth, aes(x = c14, y = birth)) + 
  geom_point() + 
  labs(x = 'Содержание нестабильного изотопа углерода, %', 
       y = 'Год рождения человека')
gg_teeth


## ----gg-themes-----------------------------------------------------------------------
gg_teeth + theme_classic()


## ----gg-theme-set--------------------------------------------------------------------
theme_set(theme_bw())
gg_teeth


## ----gg-save, eval=FALSE-------------------------------------------------------------
ggsave(filename = 'teeth_c14.png', plot = gg_teeth)
ggsave(filename = 'teeth_c14.pdf', plot = gg_teeth)
















## ----echo=FALSE----------------------------------------------------------------------
teeth_lm <- lm(birth ~ c14, data = teeth)
gg_teeth + 
  geom_abline(slope = coef(teeth_lm)[2], 
              intercept = coef(teeth_lm)[1], 
              colour = 'steelblue3', size = 1) +
  geom_abline(slope = -0.06426, intercept = 1993.76737, 
              colour = 'orange3', size = 1) +
  geom_abline(slope = -0.04226, intercept = 1990.26737,
              colour = 'green4', size = 1)


## ----echo=FALSE----------------------------------------------------------------------
gg_teeth + 
  geom_abline(slope = coef(teeth_lm)[2], 
              intercept = coef(teeth_lm)[1], 
              colour = 'steelblue3', size = 1) +
  geom_segment(aes(xend =c14 , yend = predict(teeth_lm)), linetype = 'dashed')




## ----pop-code, fig.show='hide'-------------------------------------------------------
pop_x <- rnorm(1000, 10, 3)
pop_y <- 10 + 10*pop_x + rnorm(1000, 0, 20)
population <- data.frame(x = pop_x, y = pop_y)

pop_plot <- ggplot(population, aes(x = x, y = y)) + 
  geom_point(alpha = 0.3, color = "red") + 
  geom_abline(aes(intercept = 10, slope = 10), 
              color="blue", size = 2) +
  theme(text = element_text(size = 15))
pop_plot


## ----pop-plot, echo = FALSE, fig.height = 5------------------------------------------
pop_plot 


## ---- echo=FALSE, fig.align='center', fig.width=6, fig.height = 6--------------------
samp_coef <- data.frame(b0 = rep(NA, 100), b1 = rep(NA, 100))
for(i in 1:100) {
  samp_num <- sample(1:1000, 20)
  samp <- population[samp_num, ]
  fit <- lm(y ~ x, data = samp)
  samp_coef$b0[i] <- coef(fit)[1]
  samp_coef$b1[i] <- coef(fit)[2]
  
 }

ggplot(population, aes(x = x, y = y)) + 
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(aes(intercept = b0, slope = b1), data = samp_coef) +
  geom_abline(aes(intercept = 10, slope = 10), color = "blue", size = 2) +
  theme(text = element_text(size = 18))










## ------------------------------------------------------------------------------------
summary(teeth_lm)












## ------------------------------------------------------------------------------------
library(car)
Anova(teeth_lm)




## ----eval=FALSE----------------------------------------------------------------------
gg1 <- gg_teeth + 
  labs(title = '95% доверительная зона')
gg1
gg2 <- gg_teeth + 
  labs(title = '99% доверительная зона')
gg2

library(cowplot)
plot_grid(gg1, gg2, nrow = 1, labels = 'AUTO')




## ------------------------------------------------------------------------------------
summary(teeth_lm)


## ------------------------------------------------------------------------------------
new_data1 <- data.frame(c14 = c(125, 314, 565)) 
new_data1


## ------------------------------------------------------------------------------------
pr1 <- predict(teeth_lm, newdata = new_data1, 
                interval = 'confidence', se = TRUE)
pr1$fit


## ------------------------------------------------------------------------------------
# значения, для которых предсказываем
(pr2 <- predict(teeth_lm, newdata = new_data1, 
                interval = 'prediction', se = TRUE))


## ---- teeth-pr-all-------------------------------------------------------------------
pr_all <- predict(teeth_lm, interval = 'prediction')
teeth_with_pred <- data.frame(teeth, pr_all)
head(teeth_with_pred)


## ---- teeth-plot-all-----------------------------------------------------------------
gg_teeth + 
  geom_smooth(method = 'lm', 
              aes(fill = 'Доверительный \nинтервал'), 
              alpha = 0.4) +
  geom_ribbon(data = teeth_with_pred, 
              aes(y = fit, ymin = lwr, ymax = upr, 
                  fill = 'Доверительная \nобласть значений'), 
              alpha = 0.2) +
  scale_fill_manual('Интервалы', values = c('green', 'blue'))

