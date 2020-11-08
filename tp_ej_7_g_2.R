# cemento.txt
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(multiColl)

file <- "/home/leogm99/Escritorio/aprendizaje_estadistico/ejs_practica/Set de datos para los ejercicios-20201106/cemento.txt"

cemento <- read.table(file, header = TRUE)
cemento <- cemento[-1]

#---- obs importantes ----#
# x3 y x4 tienen la mayor correlacion (en modulo) -> puede que haya problemas de colinealidad
# x1 y x2 tambien tienen el mismo problema
# mirando el pairplot, la variable que mas puede contribuir para explicar y es x3
# luego cercana x4, luego x2, x1 y por ultimo x5

ggpairs(cemento, upper = list(continuous = wrap("cor", family="sans")))


#--- obs importantes ----#
#------------------p-v
#x1               0.701
#x2               0.258
#x3               0.609
#x4               0.875
#x5               0.806
#vemos que si h0 es verdadera, entonces b3 = 0, entonces la probabilidad de 
#obtener una muestra por lo menos mas extrema de la que se obtuvo es p-v = 0.875
#no estamos seguros si la hipotesis nula es verdadera sin embargo
#en definitiva, como cada p-v > alfa = 0.05, no hay evidencia suficiente
#para rechazar la hipotesis nula para cada bi. 
#Hay que observar tambien que la muestra es muy chica.
reg <- lm(y ~ ., data = cemento)
summary(reg)

#sin embargo vemos que la regresion es significativa, ya que p-v << .05, 5 ordenes 
#de magnitud menor. Es decir que las variables sirven para explicar y

#aca lo que podemos empezar a buscar son los intervalos de confianza para los bi
#empezamos por los simples
X <- model.matrix(reg)
ic_bis = matrix(0, nrow = ncol(X), ncol = 2) # para todos los bi, i= 0..5
n <- nrow(X)
p <- ncol(X)


coeffs <- summary(reg)$coefficients[,1]
std_errs <- summary(reg)$coefficients[,2] ## std.errs /std dev
n - p 
t_prob <- qt(p = 1- (0.05 /2), df = n-p)
for (i in 1:p){
  ic_bis[i, ] <- c(coeffs[i] - t_prob * std_errs[i],
                   coeffs[i] + t_prob * std_errs[i])
}

std_errs[4]
ic_r <- confint(reg)
ic_r <- ic_r[-1,] # sin el del intercept
# todos los bi contienen al cero, no puedo decir nada sobre la significacion 
# de los coeficientes
names(reg$coefficients)[-1]
df2 <- data.frame(cbind(names(reg$coefficients)[-1], ic_r))

#notamos que la suma de las columnas es casi 100 * [1,...,1]
#esto es un indicio de que hay colinealidad
rowSums(cemento[1:5])
residuo <- rep(100, 14) -rowSums(cemento[1:5]) 
#colinealidad: ver seber p. 255
#si alguno de los autovalores de la matrix de correlacion es cercano a cero
#entonces la varianza individual de los estimadores bi correspondientes es grande
#como el determinante es el producto de los autovalores, si es chico
#entonces alguno (o todos) de los autovalores son chicos, por ende, las varianzas
#son grandes. Sabemos que las varianzas de los estimadores son grandes cuando
#hay colinealidad. Esto tambien lo puedo sacar de que la varianza de los estimadores
#es sigma^2 (xt * x)^-1, si el det de (xt * x)^-1 es muy grande, tambien lo es la var

m <- cor(cemento)
corr_matriz <- cor(cemento)
ggcorrplot(m, type="lower", outline.color = "white",
           ggtheme = ggplot2::theme_void(), lab = TRUE, 
           colors = c("#6D9EC1", "white", "#E46726"))

matriz_cemento <- as.matrix(cemento)
prod(eigen(cor(matriz_cemento))$values) #producto de los autovalores

multiCol(X)

#para el calculo de vif, se ignora la columna del intercept
#por lo tanto, problematica involucrando al intercept no se tiene en cuenta
#de haberla. Esto esta fijandose en la colinealidad esencial entre las variables
VIF(X) 
#notamos que el vif para cada variable es muy grande, salvo para x5, la unica variable
#que no se puede explicar en funcion de las otras

summary(reg) # std error gigante para la intercept

# para ver el inconviente, graficamos x1 vs x2 y x3 vs x4 
x1_vs_x2 <- ggplot(cemento, aes(x=x1, y=x2)) + geom_smooth(method = "lm", formula = y~x) + geom_point()
x3_vs_x4 <- ggplot(cemento, aes(x=x3, y=x4)) + geom_smooth(method = "lm", formula = y~x) + geom_point()
x1_vs_x2
x3_vs_x4

#quiero checkear esto haciendo intervalos de confianza simultaneos
#se que si bj - bi contiene al cero, entonces puede que sean iguales
#los que quiero testear son b1 - b2 = 0 y b3 - b4 = 0
#por bonferroni
b_ics <- matrix(0, )