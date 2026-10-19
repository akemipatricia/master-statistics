# ============================================================
# Estatística Aplicada
# Grupo de Exercícios de Revisão — Inferência Estatística
# Exercícios 1 a 5
# ============================================================

# Pacotes necessários
# install.packages("pwr")   # rodar uma vez, se ainda não tiver o pacote
library(pwr)


# ------------------------------------------------------------
# Funções
# ------------------------------------------------------------
plot_dist_t <- function(n = 4, mean_sample = 9, dp_sample = 0.6, alpha = 0.05) {
  
  # Valor crítico da distribuição t
  qq <- qt(1 - alpha/2, n - 1)
  
  # Gerando o vetor aleatório (caso queira usá-lo posteriormente no script)
  lista_de_vetores <- rnorm(n, mean = mean_sample, sd = dp_sample)
  
  # Limite estendido para o gráfico não cortar as bordas
  limite_grafico <- qq + 1
  
  # 1. Desenha a curva teórica
  curve(dt(x, df = n - 1), from = -limite_grafico, to = limite_grafico, 
        ylab = "densidade", xlab = "t", col = "blue", lwd = 2)
  
  # 2. Pinta a extremidade esquerda (de -limite_grafico até -qq)
  x_esq <- seq(-limite_grafico, -qq, length = 100)
  y_esq <- dt(x_esq, df = n - 1)
  polygon(c(-limite_grafico, x_esq, -qq), c(0, y_esq, 0), col = rgb(1, 0, 0, 0.3), border = NA)
  
  # 3. Pinta a extremidade direita (de qq até limite_grafico)
  x_dir <- seq(qq, limite_grafico, length = 100)
  y_dir <- dt(x_dir, df = n - 1)
  polygon(c(qq, x_dir, limite_grafico), c(0, y_dir, 0), col = rgb(1, 0, 0, 0.3), border = NA)
  
  # 4. Adiciona as linhas tracejadas verticais nos valores críticos
  abline(v = c(-qq, qq), lty = 2, col = "red", lwd = 1.5)
  
  # Opcional: Retorna a lista de vetores gerada invisivelmente caso precise dela
  return(invisible(lista_de_vetores))
}


############
# Ficha 01 ----
############

# legenda
# t_ : t calculado
# tc : t critico dado alpha

#----------------------------------------------
# exercise 1
#----------------------------------------------

mean_sample = 9
n = 4
sd_sample = 0.6
alpha = 0.05
mean_sample+c(-1,1)*qt(1-alpha/2,n-1)*sd_sample/sqrt(n)

# as condições que justificam usar o intervalo baseado na distribuição student são:   
# 1. amostra ser aleatória iid
# 2. normalidade da população
# 3. sigma desconhecido

#----------------------------------------------
# exercise 2
#----------------------------------------------

n = 30
mu = 12
alpha = 0.05
x2 = c(14.2, 13.8, 14.2, 14.4, 11.5, 16.0, 14.9, 15.3, 13.8, 14.8,
       13.2, 14.4, 12.6, 14.1, 13.1, 11.7, 14.8, 14.1, 13.1, 13.6, 14.1, 14.8, 11.7, 12.4, 12.6, 11.0, 13.2, 14.6,
       12.7, 14.8)
hist(x2,freq=F,ylim=range(0,0.4))
fn <- function(x){dnorm(x,mean(x2),sd(x2))}
curve(fn(x),add=T)
qqnorm(x2)
qqline(x2)

mean_sample = mean(x2)
sd_sample = sqrt(sum((x2-mean_sample)^2)/(n-1))
se = sd_sample/sqrt(n)

# estatistica t

t_ = (mean_sample-mu)/se
tc = qt(1-alpha/2,n-1)

# ic
(mean_sample) + c(-1,1)*tc*se

# verificacao
t.test(x2,mu=12)

#----------------------------------------------
# exercise 3
#----------------------------------------------

mu = 250
n = 24
alpha = 0.05
x3 = c(271, 198, 219, 225, 253, 262, 224, 291, 264,
       211, 268, 243, 230, 275, 284, 282, 216, 288, 253, 236, 295, 252, 272, 294)

hist(x3,freq=F)
qqnorm(x3)
qqline(x3)

mean_sample = mean(x3)
sd_sample = sqrt((sum((x3-mean_sample)^2))/(n-1))
se = sd_sample/sqrt(n)

# estatistica t
tc = qt(1-alpha/2,n-1) # 2.06865
t_ = (mean_sample-mu)/se

# ic
mean_sample + c(-1,1)*tc*se

# verificacao
t.test(x3,mu=250)

# a que nivel se sign rejeitaria a hipotese
t.test(x3,mu=250)$p.value

#----------------------------------------------
# exercise 4
#----------------------------------------------

n = 242
alpha = 0.01
mean_sample = 6992.6
sd_sample = 1176.83
mu = 7725
se = sd_sample/sqrt(n)

# estatistica t
t_ = (mean_sample-mu)/se
tc = qt(1-alpha/2,n-1)
ifelse(abs(t_)>tc,'rejeita','n rejeita')

# p value
pt(t_,n-1)*2

# ic
mean_sample + c(-1,1)*tc*se

#----------------------------------------------
# exercise 5
#----------------------------------------------

n = 25
x5 = c(162, 177, 151, 167, 141,
        153, 143, 157, 123, 161, 147, 157, 141, 157, 151, 134, 134, 128, 151, 112, 142, 121, 130, 134, 120)
alpha = 0.01

# a
hist(x5,freq=F)

#b
mean_sample = mean(x5)
sd_sample = sqrt(sum((x5-mean_sample)^2)/(n-1))
se = sd_sample/sqrt(n)

# c
t_ = (mean_sample-140)/se
tc = qt(1-alpha,n-1)
ifelse(abs(t_)>tc,'rejeita','n rejeita')
mean_sample + c(-1,1)*tc*se
t.test(x5,mu=140,conf.level = 1-alpha,alternative = 'greater')

# d
1-pt(t_,n-1)

#----------------------------------------------
# exercise 6
#----------------------------------------------

n = 3918
mean_sample = 0.9
sd_sample = 0.96
se = sd_sample/sqrt(n)
alpha = 0.05
mu = 1

# hipotese: sample dif pop = bi

# estatistica t
t_ = (mean_sample-mu)/se
tc = qt(1-alpha/2,n-1)
ifelse(abs(t_)>tc,'rejeita','n rejeita')

# p value
2*pt(t_,n-1)

#----------------------------------------------
# exercise 7
#----------------------------------------------

n = 49
mean_sample = 21
sd_sample = 11
se = sd_sample/sqrt(n)

# a - hipotese: sample less than pop = less
mu =30
t_ = (mean_sample-mu)/se
tc = qt(1-alpha,n-1)
ifelse(abs(t_)>tc,'rejeita','n rejeita')

# b - p value
pt(t_,n-1)

#----------------------------------------------
# exercise 8
#----------------------------------------------

n = 24
x8 = c(63, 68, 79, 65, 64, 63, 65, 44, 76, 74, 66, 46, 67, 73, 69, 76, 75, 78, 88, 42, 64, 41, 65, 65)
mean_sample = mean(x8)
sd_sample = sqrt(sum((x8-mean_sample)^2)/(n-1))
se = sd_sample/sqrt(n)
mu = 70

t_ = (mean_sample-mu)/se
pt(t_,n-1)

#----------------------------------------------
# exercise 9
#----------------------------------------------

ga = c(5667, 42, 48, 55, 61, 52, 39, 47, 58, 50, 40, 59, 62, 4457)
gb = c(7834, 37, 72, 27, 55, 6540, 75, 33, 66)

na = length(ga) 
nb = length(gb)

mean_ga = mean(ga)
mean_gb = mean(gb)

sd_ga = sqrt(sum((ga-mean_ga)^2)/(na-1))
sd_gb = sqrt(sum((gb-mean_gb)^2)/(nb-1))

sp <- sqrt(((na-1)*sd(ga)^2 + (nb-1)*sd(gb)^2) / (na+nb-2))
t_ <- (mean(ga) - mean(gb)) / (sp * sqrt(1/na + 1/nb))
p = 2*pt(abs(t_),na+nb-2)

t.test(ga,gb)
t.test(ga,gb,var.equal = T)
var.test(ga,gb) # decidir qual usar, var igual ou nao

#----------------------------------------------
# exercise 10
#----------------------------------------------

n_d = 12
n_n = 15

mean_d = 4.5
mean_n = 3.4

sd_d = 1
sd_n = 1.225

sp = sqrt(((n_d-1)*sd_d^2+(n_n-1)*sd_n^2)/(n_d+n_n-2))
se = sp*sqrt(1/n_d+1/n_n)
t_ = (mean_d-mean_n)/se
tc = qt(1-0.05/2,n_d+n_n-2)
ifelse(abs(t_)>tc,'rejeita', 'n rejeita')
2 * pt(-t_,n_d+n_n-2)

# ic
(mean_d-mean_n)+c(-1,1)*tc*se

#----------------------------------------------
# exercise 11
#----------------------------------------------

# amostras emparelhadas
n = 10
ga = c(13.3,17.6,4.1,17.2,10.1,3.7,5.1,7.9,8.7,11.6)
gb = c(13.4,17.9,4.1,17.0,10.3,4.0,5.1,8.0,8.8,12.0)
d = ga-gb
mean_d = mean(d)
sd_d = sd(d)
se = sd_d/sqrt(n)

t_ = (mean_d)/se
2*pt(t_,n-1)

t.test(ga,gb,paired = T,conf.level = 0.95,var.equal = T)
t.test(ga,gb,paired = T,conf.level = 0.99,var.equal = T)

#----------------------------------------------
# exercise 12
#----------------------------------------------


#----------------------------------------------
# exercise 13
#----------------------------------------------

# a estatistica de teste se calcula com o p0
# o ic se calcula com o p_hat
# A razão é que o teste supõe $H_0$ verdadeira (por isso usa $p_0$), enquanto o IC estima $p$ sem supor nada sobre ele, então só pode usar o que vem da amostra.

n = 150
p0 = 0.05
p_hat = 5/n

z_c = qnorm(1-alpha/2)
z_ = (p_hat-p0)/sqrt(p0*(1-p0)/n)
ifelse(abs(z_)>z_c,'rejeita','n rejeita')
2*pnorm(-abs(z_))

p_hat+c(-1,1)*z_c*sqrt(p_hat*(1-p_hat)/n)

# wilson
prop.test(5,150,p=p0,correct=F)


#----------------------------------------------
# exercise 14
#----------------------------------------------

n = 1000
x = 40
p_hat = x/n
p = 0.02

# estatistica
z_ = (p_hat-p)/(sqrt(p*(1-p)/n))
zc = qnorm(0.995)
ifelse(abs(z_)>zc,'rejeita','n rejeita')
2*pnorm(-abs(z_))

# ic de wald
p_hat+c(-1,1)*zc*sqrt(p_hat*(1-p_hat)/n)
prop.test(x,n,p=p,correct = T,conf.level = 0.995)


#----------------------------------------------
# exercise 15
#----------------------------------------------

n = 670
p_hat = 0.66
alpha = 0.05
p0 = 0.5

# estatistica
z_ = (p_hat-p0)/sqrt(p0*(1-p0)/n)
zc = qnorm(1-alpha/2)
pnorm(-abs(z_))*2

#----------------------------------------------
# exercise 16
#----------------------------------------------

n = 250
x = 238
p_hat = x/n
alpha = 0.05
p0 = 0.9

# estatistica wilson
z_ = (p_hat-p0)/sqrt(p0*(1-p0)/n)
2*pnorm(-abs(z_))

#----------------------------------------------
# exercise 17
#----------------------------------------------



#----------------------------------------------
# exercise 18
#----------------------------------------------

df18 <- matrix(
  c(60, 25, 15, 10,   # Republicano
    30, 40, 45, 45,   # Democrata
    10,  5, 10,  5    # Outro
  ),
  nrow = 3,
  byrow = TRUE
)

rownames(df18) <- c("Republicano", "Democrata", "Outro")
colnames(df18) <- c(">=30000", "[20000,30000[", "[12000,20000[", "<12000")

chisq.test(df18)

#----------------------------------------------
# exercise 19
#----------------------------------------------

df19 = matrix(
  c(42,157,81,119),
  nrow=2,
  byrow=T
)

rownames(df19) = c('Comunidade_1','Comunidade_2')
colnames(df19) = c('S','N')

chisq.test(df19)

prop.table(df19)

#----------------------------------------------
# exercise 20
#----------------------------------------------

x20 = c(4.6, 5.2, 3.6, 5.0, 3.5, 4.2, 3.9, 3.8, 4.2, 3.9, 5.1)
y20 = c(3.8, 4.4, 3.2, 4.0, 4.5, 3.5, 3.5, 3.4, 3.9, 3.3, 4.5)

mean_x = mean(x20)
mean_y = mean(y20)

ssxy = sum((x20-mean_x)*(y20-mean_y))
ssxx = sum((x20-mean_x)^2)
ssyy = sum((y20-mean_y)^2)
corr_ = ssxy/(sqrt(ssxx*ssyy))  
cor.test(x20,y20,method = 'pearson')  

#----------------------------------------------
# exercise 21
#----------------------------------------------






### estatistica computacional


# apresentacao -------------------
# worksheet 1- ex9 (com instrucoes usuais + tidyr) - resolver em markdown
# worksheet 2- ggplot - resolver em markdown
#--------------------

# aula 3

###---- 
# histograma
hist(mtcars$mpg)

plot(mtcars$) # histogram boxplot
boxplot(mtcars$mpg ~ mtcars$cyl)
attach(mtcars)

boxplot(mpg[cyl==4],mpg[cyl==8])
boxplot(mpg~cyl)


install.packages('hflights')
library(hflights)
attach(hflights)
head(hflights)
hflights$ArrDelay
hflights$ArrTime
hflights[,DayOfWeek]
hflights[,c(ArrDelay,ArrTime,DayOfWeek)]
hflights[,c(ArrDelay,ArrTime,DayOfWeek)]



#### worksheet 1 - ex 9

library(tidyverse)
library(faraway)


library(faraway)
help(nepali)   # sex: 1 = menino, 2 = menina; lit: 0 = não alfabetizada, 1 = alfabetizada
head(nepali)

dados <- nepali
dados$sex <- factor(dados$sex, levels = 1:2, labels = c("boy", "girl"))  # códigos viram rótulos
dados$lit <- factor(dados$lit, levels = 0:1, labels = c("no", "yes"))
dados$hh  <- substr(dados$id, 1, 5)   # agregado familiar: id sem o último dígito

# uma linha por mãe (a base repete a mãe em cada visita de cada filho)
maes <- unique(dados[, c("hh", "mage", "lit", "died", "alive")])

# (a) Crianças únicas
length(unique(dados$id))  

# (b) Agregados familiares únicos
length(unique(dados$hh))   # hh foi criado acima com substr(id, 1, 5)

# (c) Falta altura ou peso, mas não ambos?
sum(xor(is.na(dados$ht), is.na(dados$wt)))   # xor: TRUE se só um dos dois falta; sum conta os TRUE

# (d) Outros valores em falta
colSums(is.na(dados))   # nº de NA em cada coluna

# (e) Peso de meninos e meninas
tapply(dados$wt, dados$sex, summary)             # aplica summary ao peso, separado por sexo
tapply(dados$wt, dados$sex, sd, na.rm = TRUE)    # desvio-padrão por sexo, ignorando NA

boxplot(wt ~ sex, data = dados,                  # fórmula: peso em função do sexo
        col = c("blue", "red"), xlab = "Sexo", ylab = "Peso (kg)")

# (f) Idade das mães sem filhos que morreram

summary(maes$mage[maes$died == 0])   # [condição] filtra o vetor; summary dá min, quartis, média, max
sd(maes$mage[maes$died == 0])

# (g) Mães não alfabetizadas têm filhos mais cedo?
ord <- dados[order(dados$id, dados$age), ]     # ordena por criança e por idade
prim <- ord[!duplicated(ord$id), ]             # fica só a 1ª linha (1ª visita) de cada criança
prim$idade_parto <- prim$mage - prim$age / 12  # idade da mãe quando a criança nasceu

boxplot(idade_parto ~ lit, data = prim,
        xlab = "Mãe alfabetizada", ylab = "Idade da mãe ao nascimento (anos)")

# (h) Alfabetização × filhos que morreram

tapply(maes$died, maes$lit, summary)                # resumo de died por grupo
tapply(maes$died > 0, maes$lit, mean)               # proporção com pelo menos 1 filho morto

tab <- table(maes$lit, maes$died)                   # tabela de contingência
prop.table(tab, margin = 1)                         # proporções por linha (cada grupo soma 1)

barplot(prop.table(tab, 1), beside = TRUE,          # barras lado a lado
        legend.text = TRUE, args.legend = list(title = "Alfabetizada"),
        xlab = "Nº de filhos que morreram", ylab = "Proporção")

# (i) Mães com menos de 40 anos e sem filhos que morreram

summary(subset(maes, mage < 40 & died == 0))   # subset filtra linhas pela condição

# (j) Mães mais velhas têm mais filhos que morreram?
cor(maes$mage, maes$died, method = "spearman")       # correlação por postos

plot(jitter(maes$mage), jitter(maes$died),           # jitter afasta pontos sobrepostos
     xlab = "Idade da mãe", ylab = "Nº de filhos que morreram")
lines(lowess(maes$mage, maes$died), col = "red", lwd = 2)   # curva de tendência

# (k) Meninos são mais altos e pesados?
plot(dados$ht, dados$wt,
     col = c("blue", "red")[dados$sex],     # o fator indexa o vetor de cores: boy = azul, girl = vermelho
     xlab = "Altura (cm)", ylab = "Peso (kg)")
legend("topleft", legend = levels(dados$sex), col = c("blue", "red"), pch = 1)

aggregate(cbind(ht, wt) ~ sex, data = dados, FUN = mean)     # médias de altura e peso por sexo
aggregate(cbind(ht, wt) ~ sex, data = dados, FUN = median)

dados[dados$mage == min(dados$mage), c("id", "mage", "age")]   # mãe mais nova
dados[dados$mage == max(dados$mage), c("id", "mage", "age")]   # mãe mais velha

dados$mage > 30    # TRUE quando mage > 30, FALSE caso contrário

dados$mage.cl <- cut(dados$mage, breaks = c(-Inf, 20, 35, Inf),
                     right = FALSE,                      # [a, b): 20 entra em "20-34"
                     labels = c("<20", "20-34", ">34"))

maes2 <- unique(dados[, c("hh", "mage.cl", "died")])     # uma linha por mãe
tapply(maes2$died == 0, maes2$mage.cl, mean)             # proporção sem mortes em cada classe

dados$age.year <- floor(dados$age / 12)    # meses -> anos completos, sem decimais

boxplot(wt ~ sex + age.year, data = dados,           # um boxplot por combinação sexo × idade
        col = c("blue", "red"),                      # as cores alternam: boy, girl, boy, girl...
        las = 2, xlab = "", ylab = "Peso (kg)")      # las = 2 roda os rótulos do eixo x
legend("topleft", legend = c("boy", "girl"), fill = c("blue", "red"))

write.csv(subset(dados, sex == "girl"), "nepali_girls.csv",
          row.names = FALSE)    # sem a coluna extra de nomes das linhas


#### worksheet 2

# a

library(tidyverse)   # carrega ggplot2, dplyr, tibble, forcats etc.
library(ggthemes)    # temas extras

iris_all <- bind_rows(
  iris %>% mutate(Species = "1"),                 # cópia com tudo rotulado "ALL"
  iris %>% mutate(Species = as.character(Species))  # dados originais (como texto, para poder empilhar)
)

ggplot(iris_all, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(size = 1) +                     # um ponto por flor
  geom_smooth(method = "loess") +            # curva suavizada + faixa de confiança cinza
  facet_wrap(~ Species, scales = "free") +   # um painel por grupo, cada um com eixos próprios
  labs(x = "Petal Length", y = "Petal Width") +
  theme_bw() 


# 1(b) Tufte style: mtcars
mtcars %>%
  rownames_to_column("car") %>%                  # nomes dos carros são rownames; vira coluna
  ggplot(aes(x = mpg, y = fct_reorder(car, mpg))) +  # ordena os carros pelo mpg
  geom_point(size = 1) +
  geom_text(aes(label = car), hjust = 0, nudge_x = 0.3, size = 2.5) +  # nome à direita do ponto
  scale_x_continuous(limits = c(10, 40)) +       # eixo x de 10 a 40
  labs(x = "Miles per gallon fuel consumption", y = NULL) +
  theme_bw()


# 1(c) Bubble plot: mtcars

mtcars %>%
  mutate(am = factor(am, labels = c("Manual", "Automatic"))) %>%  # 0/1 vira categoria com rótulo
  ggplot(aes(x = mpg, y = qsec, size = disp, colour = am)) +
  geom_point() +
  labs(x = "Miles per gallon", y = "1/4 mile time",
       size = "Displacement", colour = "Gear") +   # títulos dos eixos e das legendas
  theme_bw()

# 1(d) Série temporal: economics
head(economics)
ggplot(economics, aes(x = date, y = uempmed)) +
  geom_line() +                                   
  expand_limits(y = 0) + # força o eixo y a começar em 0
  labs(title = "Median duration of unemployment (weeks)",
       x = NULL,
       y = "Median duration of unemployment [weeks]") 

# 2. diamonds: preço × quilate × cor
head(diamonds)
ggplot(diamonds, aes(x = carat, y = price, colour = color)) +
  geom_point()
#  geom_point(alpha = 0.05, size = 0.5) +      
geom_smooth(method = "lm", se = FALSE) +     # uma reta de regressão por cor
  scale_x_log10() + scale_y_log10() +
  labs(x = "Carat (log)", y = "Price (log)", colour = "Color") 


# 3. hflights: atrasos por hora e dia da semana
library(hflights)

atrasos <- hflights %>%
  filter(!is.na(ArrDelay), !is.na(ArrTime)) %>%   # remove voos cancelados/desviados
  mutate(
    ArrDelay = pmax(ArrDelay, 0),                 # atrasos negativos (adiantados) viram 0
    hora = (ArrTime %/% 100) %% 24,               # ArrTime é hhmm; divisão inteira dá a hora (24 vira 0)
    dia  = factor(DayOfWeek, levels = 1:7,
                  labels = c("Seg","Ter","Qua","Qui","Sex","Sáb","Dom"))  # 1 = segunda
  ) %>%
  group_by(dia, hora) %>%                         # para cada combinação dia × hora...
  summarise(atraso_medio = mean(ArrDelay),        # ...atraso médio
            prop_atraso  = mean(ArrDelay > 0),    # ...proporção de voos atrasados
            .groups = "drop")

ggplot(atrasos, aes(x = hora, y = dia, fill = atraso_medio)) +
  geom_tile() +                                   # heatmap: um quadrado por dia × hora
  scale_fill_viridis_c() +                        # escala de cor contínua
  labs(x = "Hora de chegada", y = NULL, fill = "Atraso médio (min)") +
  theme_minimal()



###### 15/10

N = 10^4
p = 0.2

lista_medias <- c()
lista_vars <- c()
for (i in 1:N){
  amostra <- rbinom(n,1,p)
  est <- sum(amostra)/length(amostra)
  lista_medias <- c(lista_medias,est)
  
  var <- (amostra-est)^2/length(amostra)
  lista_vars <- c(lista_vars,var)
}

hist(v)
mean(v)
sd(v)


# como eu convenco se e consistente? aumentar o tamanho amostral e ver o quanto altera na variancia

