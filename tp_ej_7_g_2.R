# cemento.txt
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(multiColl)
library(leaps)
library(xtable)
file <- "cemento.txt"

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
summary(reg)$coefficients



mi_t <- function(x){
  dt(x=x, df=8)
}
#----------graficos de densidad----------#

for(i in 1:6){
  print(ggplot(data.frame(x=seq(-4, 4, 0.001)), aes(x)) +
    stat_function(fun = mi_t) + stat_function(fun = mi_t, geom="area", xlim= c(qt(p=.975, df=8), 4),fill="blue", alpha=0.4) +
    stat_function(fun = mi_t, geom="area", xlim= c(-4, -qt(p=.975, df=8)),fill="blue", alpha=0.4) + 
    geom_vline(xintercept = summary(reg)$coefficients[i,3], linetype="dotted", color="red", size=0.8) + xlab("t values")) + ylab("Densidad")
}

#sin embargo vemos que la regresion es significativa, ya que p-v << .05, 5 ordenes 
#de magnitud menor. Es decir que las variables sirven para explicar y

#aca lo que podemos empezar a buscar son los intervalos de confianza para los bi
#empezamos por los simples
X <- model.matrix(reg)

ic_r <- confint(reg)
# todos los bi contienen al cero, por lo tanto, no puedo decir absolutamente 
# nada sobre la significacion de las columnas hasta el momento
# vemos que el std error de la intercept es muy grande (de todas en general)

#y ademas notamos que la suma de las columnas es casi 100 * [1,...,1]
#esto es un indicio de que hay colinealidad (ya que la columna del intercept es casi 100 * la suma de las cols)
x6 <- rowSums(cemento[1:5])
X[,1]
mean(x6-X[,1])
cov(x=x6, y=X[,1])
obs_sum <- data.frame(cbind(c(1:14), x6))
bar_plot <- ggplot(data = obs_sum, aes(x = V1, y = x6)) + geom_col(fill = "#E69F00") + xlab("Observacion")
bar_plot + scale_x_continuous(breaks = seq(1, 14, 1)) + geom_abline(intercept = 100, slope = 0, colour = "red") + ylab("Peso total")
#colinealidad: ver seber p. 255
#si alguno de los autovalores de la matrix de correlacion es cercano a cero
#entonces la varianza individual de los estimadores bi correspondientes es grande
#como el determinante es el producto de los autovalores, si es chico
#entonces alguno (o todos) de los autovalores son chicos, por ende, las varianzas
#son grandes. Sabemos que las varianzas de los estimadores son grandes cuando
#hay colinealidad. Esto tambien lo puedo sacar de que la varianza de los estimadores
#es sigma^2 (xt * x)^-1, si el det de (xt * x)^-1 es muy grande, tambien lo es la var

#el parecido es justificable desde lo practico.
#cada variable representa el porcentaje del peso de cada componente
#por lo tanto, estamos en condiciones de estimar a una de las variables en funcion de las otras
#de modo que es posible dropear al menos una de las columnas. # esto ultimo no es tan cierto

m <- cor(cemento)
corr_matriz <- cor(cemento)
ggcorrplot(m, type="lower", outline.color = "white",
           ggtheme = ggplot2::theme_void(), lab = TRUE, 
           colors = c("#6D9EC1", "white", "#E46726"))

matriz_cemento <- as.matrix(cemento)
prod(eigen(cor(matriz_cemento))$values) #producto de los autovalores

multiCol(X)

#la matriz de diseño esta mal condicionada, ya que si bien sus columnas no son 
#combinaciones lineales, estan proximas a serlo (intercept es simil a la suma de x1 a x5)

#dropear la intercept estaria asumiendo que cuando xi = 0 entonces y=0, lo cual
#desde el punto de vista practico si es cierto, pero tambien es cierto que nuestras 
#observaciones no contienen a la interseccion. Esto tambien nos dice que estariamos 
#tratando de extrapolar el origen, lo cual puede introducir variabilidad en el resto de los estimadores.

mean(reg$residuals)


#modelo sin intercept


reg2 <- lm(y ~ . + 0, data = cemento)

summary(reg2)

ggplot(data = reg, aes(x = reg2$fitted.values, y = reg2$residuals)) + 
  geom_point() + geom_abline(slope = 0) + xlab("Valores Ajustados") + 
  ylab("Residuos")


qqnorm(rstandard(reg2))
abline(0,1)

norm(cemento$y - reg$fitted.values, 2)

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
#los que quiero testear son b1 + b2 = 0 y b3 + b4 = 0
#pq asi? pq puede que sean inversamente proporcionales

#por bonferroni
b_ics <- matrix(0, nrow = 2, ncol = 2)
c1 <- c(0,0)
c2 <- c(1,0)
c3 <- c(1,0)
c4 <- c(0,1)
c5 <- c(0,1)
c6 <- c(0,0)
C <- cbind(c1, c2, c3, c4, c5, c6)
s <- summary(reg)$sigma
q <- 2 #numero de intervalos
alfa <- 0.05
t_prob <- qt(p = 1-(alfa / (2 * q)), df = n-p)
for(i in 1:q){
  b_ics[i, ] <- c(C[i,]%*%reg$coefficients - t_prob * s * sqrt(t(C[i,])%*%solve(t(X)%*%X)%*%C[i,]),
                  C[i,]%*%reg$coefficients + t_prob * s * sqrt(t(C[i,])%*%solve(t(X)%*%X)%*%C[i,]))
}

#vemos que ambos intervalos contienen al 0, pero son muy grandes
#esto significa que puede ser que sean iguales los coeficientes

#entonces, vimos que la matriz de correlacion es cercana a singular 
#esto indicaria colinealidad entre las variables 
#ademas b0 esta consumiendo el problema de que 100* [1,...,1 ] ~= sum(x1 x2 x3 x4 x5)
#vemos si se justifica eliminar al intercept de la regresion

reg2 <- lm(y ~ . + 0, data = cemento)

#vemos que con el modelo sin intercept, x2, x3 y x4 son significativas para 
#la regresion, ya que sus p-v < 0.05, mientras que no es el caso con x1(casi) y x5

summary(reg2)

# generalmente droppear la intercept es malo
# el tema es que la suma de los valores es casi 100
# cada variable es parte de una composicion
x6
ggplot(data = reg, aes(x = reg$fitted.values, y = reg$residuals)) + 
  geom_point() + geom_abline(slope = 0) + xlab("Valores Ajustados") + 
  ylab("Residuos")
plot(reg$fitted.values, reg$residuals)
qqnorm(rstandard(reg))
qqline(rstandard(reg))
rstandard(reg)
reg$residuals

reg3 <- lm(y ~ x1 + x3, data = cemento)

summary(reg3)
modelos <- leaps(x = as.matrix(cemento[1:5]), y=as.matrix(cemento[6]), int = TRUE, method = c("Cp"))


df_cp <- data.frame("Tamaño" = modelos$size, "Cp" = modelos$Cp)
#busco todos aquellos que tengan cp menor a 6 (sino no tiene ningun sentido)
#a vista vemos que los mas cercanos son de 4 y de 5 variables
df_cp <- df_cp[df_cp["Cp"] <= 6,  ]
plot(x = df_cp$Tamaño, y = df_cp$Cp)
abline(0,1)
identify(df_cp)
modelos$which[19, ]
modelos$which[30, ]
modelos$Cp[19]
modelos$Cp[30]


modelos_sin_int <- leaps(x = as.matrix(cemento[1:5]), y=as.matrix(cemento[6]), int = FALSE, method = c("Cp"))

df_cp_no_int <- data.frame("Tamaño" = modelos_sin_int$size, "Cp" = modelos_sin_int$Cp)

df_cp_no_int <- df_cp_no_int[df_cp_no_int["Cp"] <= 5, ]

modelos_sin_int$which[16, ]











#--------tablas a LaTex--------#
xtable(cemento)
xtable(summary(reg)$coefficients)
summary(reg)