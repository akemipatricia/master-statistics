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


# ------------------------------------------------------------
# Exercício 1
# ------------------------------------------------------------
# Peso da glândula pituitária em 4 ratinhos: média = 9.0 mg, desvio padrão = 0.6 mg
# IC a 95% para a média populacional

n = 4
mean_sample = 9
dp_sample = 0.6
alpha = 0.05
se = dp_sample/sqrt(n)


plot_dist_t(n = n, mean_sample = mean_sample, dp_sample = dp_sample, alpha = alpha) 
mean_sample + c(-1,1)*qt(1-alpha/2,n-1)*se

# nao rejeita H0

# d3 <- (mean(dados3) - 250) / sd(dados3)   # tamanho de efeito de Cohen
# pwr.t.test(n = length(dados3), d = d3, sig.level = 0.05,
#            type = "one.sample", alternative = "greater")


# ------------------------------------------------------------
# Exercício 2
# ------------------------------------------------------------
# Consumo de oxigénio (ml) em 30 suspensões celulares
# H0: mu = 12   vs   H1: mu != 12   (alpha = 0.05)

x <- c(14.2, 13.8, 14.2, 14.4, 11.5, 16.0, 14.9, 15.3, 13.8, 14.8,
            13.2, 14.4, 12.6, 14.1, 13.1, 11.7, 14.8, 14.1, 13.1, 13.6,
            14.1, 14.8, 11.7, 12.4, 12.6, 11.0, 13.2, 14.6, 12.7, 14.8)

n = length(x)
mu = 12
mean_sample = mean(x)
sd_sample2 = (sum(x^2)-n*(mean_sample^2))/(n-1)
alpha = 0.05
qt(1-alpha/2,n-1)

plot_dist_t(n = n, mean_sample = mean_sample, dp_sample = sd_sample, alpha = alpha) 

t = (mean_sample-mu)/(sqrt(sd_sample2/n))
ic = mean_sample + c(-1,1)*qt(1-alpha/2,n-1)*(sqrt(sd_sample2/n))

qqnorm(x)
qqline(x)

(t.test(x, mu = 12, alternative = "two.sided", conf.level = 0.95))
# rejeita H0

# ------------------------------------------------------------
# Exercício 3
# ------------------------------------------------------------
# Tempo de vida (horas) de 24 bactérias.
# Laboratório A afirma mu > 250h; Laboratório B testa essa afirmação.
# H0: mu <= 250   vs   H1: mu > 250   (alpha = 0.05, teste unilateral à direita)

dados3 <- c(271, 198, 219, 225, 253, 262, 224, 291, 264, 211, 268, 243,
            230, 275, 284, 282, 216, 288, 253, 236, 295, 252, 272, 294)

n = length(dados3)
mean_sample = mean(dados3)
mu = 250
sd_sample2 = (sum(dados3^2)-n*(mean_sample^2))/(n-1)
alpha = 0.05
qt(1-alpha,n-1)

plot_dist_t(n = n, mean_sample = mean_sample, dp_sample = sd_sample2, alpha = alpha) 

t = (mean_sample-mu)/sqrt(sd_sample2/n)
ic = mean_sample + c(-1,1)*qt(1-alpha/2,n-1)*sqrt(sd_sample2/n)

(t.test(dados3, mu = mu, alternative = "greater", conf.level = 0.95))

abline(v = c(-t, t), lty = 2, col = "red", lwd = 1.5)

# nao rejeita H0



---


install.packages("pwr") # apenas uma vez
library(pwr)


# t = 0.7465, df = 23, p-value = 0.2315 (> 0.05) -> Não se rejeita H0.
# (No SPSS, como o teste lá é sempre bilateral, dividir o p-value por 2.)
# Conclusão: os dados não fornecem evidência suficiente para confirmar a
# afirmação do laboratório A.

# Como não se rejeitou H0 -> análise de potência
d3 <- (mean(dados3) - 250) / sd(dados3)   # tamanho de efeito de Cohen
pwr.t.test(n = length(dados3), d = d3, sig.level = 0.05,
           type = "one.sample", alternative = "greater")
# A potência calculada mostra a probabilidade de detetar corretamente um
# efeito desta magnitude, com esta dimensão amostral e este nível de significância.


# ------------------------------------------------------------
# Exercício 4
# ------------------------------------------------------------
# Consumo energético diário em 242 mulheres americanas (> 30 anos)
# Apenas estatísticas-resumo disponíveis (sem dados brutos) -> cálculo "à mão" em R
# H0: mu = 7725   vs   H1: mu != 7725   (alpha = 0.01)

n = 242
mean = 6992.6
sd = 1176.83
alph = 0.01
mu = 7725

qt(1-alpha/2,n-1) # 1.969856

plot_dist_t(n = n, mean_sample = mean, dp_sample = sd, alpha = alpha) 

t = (mean-mu)/(sd/sqrt(n))
ic = mean + c(-1,1)*qt(1-alpha/2,n-1)*sd/sqrt(n)

abline(v = c(-t, t), lty = 2, col = "red", lwd = 1.5)

# rejeita H0

# ------------------------------------------------------------
# Exercício 5
# ------------------------------------------------------------
# Pressão arterial sistólica (mmHg) em 25 indivíduos com patologia cardíaca

dados5 <- c(162, 177, 151, 167, 141, 153, 143, 157, 123, 161, 147, 157, 141,
            157, 151, 134, 134, 128, 151, 112, 142, 121, 130, 134, 120)

n = length(dados5)
mean_a = mean(dados5)
sd2 = (sum(dados5^2)-n*(mean_a^2))/(n-1)
alpha = 0.05


# a - sim
hist(dados5)
qqnorm(dados5)
qqline(dados5)

# b

mu = 140
qt(1-alpha,n-1)
t = (mean_a-mu)/(sqrt(sd2/n))
t.test(dados5,mu=mu,alternative='greater',sig.level=0.05)
plot_dist_t(n = n, mean_sample = mean, dp_sample = sd2, alpha = alpha) 
abline(v = c(-t, t), lty = 2, col = "red", lwd = 1.5)
## nao rejeita Ho, nao pode concluir que a media e superior a 140

# c
mu = 140
t = (mean_a-mu)/(sd2/sqrt(n))
qt(1-alpha/2,n-1)
plot_dist_t(n = n, mean_sample = mean, dp_sample = sd, alpha = alpha) 
abline(v = c(-t, t), lty = 2, col = "red", lwd = 1.5)

(t.test(dados5, mu = mu, alternative = "greater", conf.level = 0.99))

# Como não se rejeitou H0 -> análise de potência
d5 <- (mean(dados5) - 140) / sd(dados5)
pwr.t.test(n = length(dados5), d = d5, sig.level = 0.01,
           type = "one.sample", alternative = "greater")

# (d) Cálculo "à mão" do valor-p a partir da estatística t
t.obs5   <- (mean(dados5) - 140) / (sd(dados5) / sqrt(length(dados5)))
p.valor5 <- 1 - pt(t.obs5, df = length(dados5) - 1)
cat("t =", t.obs5, " | valor-p =", p.valor5, "\n")


# ------------------------------------------------------------
# Exercício 6
# ------------------------------------------------------------
n = 3918
mean_a = 0.9
sd_a = 0.96
alpha = 0.05
mu = 1
qt(1-alpha/2,n-1)

t = (mean_a-mu)/(sd_a/sqrt(n))
ic = mean_a + c(-1,1)*qt(1-alpha/2,n-1)*(sd_a/sqrt(n))

2*pt(t,n-1)

# ------------------------------------------------------------
# Exercício 7
# ------------------------------------------------------------

n = 49 
mean_a = 21
sd_a = 11
alpha = 0.95

# a
t = (mean_a - 30)/(sd_a/sqrt(n))
qt(1-alpha/2,n-1)

# b
pt(t,n-1)

# ------------------------------------------------------------
# Exercício 8 --- NAO CONSEGUI FAZER NA MAO - consegui, estava fazendo o  calculo da var amostral errado
# ------------------------------------------------------------

n = 24
dados8 = c(63, 68, 79, 65, 64, 63, 65, 44, 76, 74, 66, 46, 67, 73, 69, 76, 75, 78, 88, 42, 64, 41, 65, 65)
length(dados8)
mean_a = mean(dados8)
sd2 = (sum(dados8^2)-n*(mean_a^2))/(n-1)

mu = 70
t = (mean_a - mu)/(sqrt(sd2/n))

(t.test(dados8, mu = mu, alternative = "less", conf.level = 0.95))


# ------------------------------------------------------------
# Exercício 9
# ------------------------------------------------------------

dadosa = c(56, 67, 42, 48, 55, 61, 52, 39, 47, 58, 50, 40, 59, 62, 44, 57)
dadosb = c(78, 34, 37, 72, 58, 68, 27, 55, 65, 40, 75, 33, 66)

na = length(dadosa)
nb = length(dadosb)

mean_a = mean(dadosa)
mean_b = mean(dadosb)

sda = (sum(dadosa^2)+na*(mean_a))/(na-1)
sdb = (sum(dadosb^2)+nb*(mean_b))/(nb-1)

# variancias iguais 
t.test(dadosa, dadosb, var.equal = TRUE) # p-value = 0.6736

# variancias diferentes
t.test(dadosa, dadosb, var.equal = FALSE) # p-value = 0.6962


# ------------------------------------------------------------
# Exercício 10
# ------------------------------------------------------------

# a: sindrome Down -- b: sem
na = 12
nb = 15

mean_a = 4.5
mean_b = 3.4

sda = 1
sdb = 1.225

sp <- sqrt(((na-1)*sda^2 + (nb-1)*sdb^2)/(na+nb-2))
t <- (mean_a-mean_b)/(sp*sqrt(1/na + 1/nb))
gl <- na+nb-2

2 * pt(-abs(t), df = gl) # 0.01891113

# ------------------------------------------------------------
# Exercício 11
# ------------------------------------------------------------

2 * pt(-abs(t), df = gl)

ma = c(13.3,17.6,4.1,17.2,10.1,3.7,5.1,7.9,8.7,11.6)
mb = c(13.4,17.9,4.1,17.0,10.3,4.0,5.1,8.0,8.8,12.0)
d = ma-mb
composto = seq(1:10)

(t.test(d, mu = 0, alternative = "two.sided", conf.level = 0.95))


# ------------------------------------------------------------
# Exercício 12
# ------------------------------------------------------------

n = 120
pa = 0.15
prop.test()


# ------------------------------------------------------------
# Exercício 13
# ------------------------------------------------------------

n = 150
p = 0.05
phat = 5/150


ic = phat + c(-1,1)*

# ic de wilson
z = ((phat-p)/(sqrt(p*(1-p)/n)))
SE_hat = sqrt(phat * (1 - phat) / n)
phat + c(-1, 1) * qnorm(1 - alpha / 2) * SE_hat
  
# ic de agresti

waldInterval <- function(x, n, conf.level = 0.95){
  p <- x/n
  sd <- sqrt(p*((1-p)/n))
  z <- qnorm(c( (1 - conf.level)/2, 1 - (1-conf.level)/2)) #returns the value of thresholds at which conf.level has to be cut at. for 95% CI, this is -1.96 and +1.96
  ci <- p + z*sd
  return(ci)
}

#example
# waldInterval(x = 20, n =40) #this will return 0.345 and 0.655

getCoverages <- function(numSamples = 10000,numTrials = 100, method, correct = FALSE){
  probs <- seq(0.001, 0.999, 0.01) 
  coverage <- as.numeric() 
  for (i in 1:length(probs)) {
    x <- rbinom(n = numSamples, size=numTrials, prob = probs[i]) 
    isCovered <- as.numeric() 
    for (j in 1:numSamples) {
      if (method =="wilson"){
        if (correct){
          ci <- prop.test(x = x[j], n = numTrials, correct = TRUE)$conf
        }else {
          ci <- prop.test(x = x[j], n = numTrials, correct = FALSE)$conf
        }
      }else if (method=="clopperpearson"){
        ci <- binom.test(x = x[j], n = numTrials)$conf
      }else if(method=="wald"){
        ci <- waldInterval(x = x[j], n = numTrials)
      }else if(method =="agresticoull"){
        ci <- waldInterval(x = x[j]+2, n = numTrials + 4)
      }
      isCovered[j] <- (ci[1] < probs[i]) &amp; (probs[i] < ci[2])
    }
    coverage[i] <- mean(isCovered)*100 #captures the coverage for each #of the true proportions. ideally, for a 95% ci, this should be more #or else 95%
  }
  return(list("coverage"= coverage, "probs" = probs))
}

ac <- getCoverages(method ="agresticoull")

# ------------------------------------------------------------
# Exercício 14
# ------------------------------------------------------------

n=100
phat = 40/n
p=0.02

# wald
waldInterval(x= 40, n=100, conf.level = 0.95) #0.3039818 0.4960182

# wilson
z = (phat-p)/(sqrt(p*(1-0)/n))
SE_hat = sqrt(phat * (1 - phat) / n)
phat + c(-1, 1) * qnorm(1 - alpha / 2) * SE_hat # 0.3039818 0.4960182
  
prop.test(x = 40, n = 100, correct = FALSE) # nao rejeita H0

prop.test(x = 40, n = 100, correct = TRUE) # correcao de continuidade de yates - utilizar quando o n for pequeno ou a prop for extrema


# ------------------------------------------------------------
# Exercício 15
# ------------------------------------------------------------

n=670
phat = 0.66
p = 0.5
z = (phat-p)/(sqrt(p*(1-p)/n))
pnorm(z)
qnorm(phat, mean = p, sd = (sqrt(p*(1-p)/n)), lower.tail = TRUE, log.p = FALSE)

# ------------------------------------------------------------
# Exercício 16
# ------------------------------------------------------------

n=250
phat = 238/n
alpha = 0.05
p = 0.9
z = (phat-p)/(sqrt((p*(1-p))/n))
1-pnorm(z)

# ------------------------------------------------------------
# Exercício 17
# ------------------------------------------------------------
# ------------------------------------------------------------
# Exercício 18
# ------------------------------------------------------------


n = 300

salario = c(1,2,3,4)
republicano = c(60, 25, 15, 10, 110)
democrata = c(30, 40, 45, 45, 160)
outro = c(10, 5, 10, 5, 30)

sum(republicano)
sum(democrata)
sum(outro)

t_linha = c(110,160,30)
t_col = c(100,70,70,60)

est = c(
t_linha[1]*t_col[1]/n
,t_linha[1]*t_col[2]/n
,t_linha[1]*t_col[3]/n
,t_linha[1]*t_col[4]/n
,t_linha[2]*t_col[1]/n
,t_linha[2]*t_col[2]/n
,t_linha[2]*t_col[3]/n
,t_linha[2]*t_col[4]/n
,t_linha[3]*t_col[1]/n
,t_linha[3]*t_col[2]/n
,t_linha[3]*t_col[3]/n
,t_linha[3]*t_col[4]/n
)

sum(est)

dados <- matrix(
  c(60, 25, 15, 10,   # Republicano
    30, 40, 45, 45,   # Democrata
    10,  5, 10,  5    # Outro
  ),
  nrow = 3,
  byrow = TRUE
)

rownames(dados) <- c("Republicano", "Democrata", "Outro")
colnames(dados) <- c(">=30000", "[20000,30000[", "[12000,20000[", "<12000")

dados

teste <- chisq.test(dados)

teste

t.chisq()

# ------------------------------------------------------------
# Exercício 19
# ------------------------------------------------------------


total_sn = c(124,276)
comu_12 = c(200,200)
n = sum(total_sn)

dados <- matrix(
  c(43,157,   
    81, 119
  ),
  nrow = 2,
  byrow = TRUE
)

teste <- chisq.test(dados)
cor.test()

# ------------------------------------------------------------
# Exercício 20
# ------------------------------------------------------------





# ----- aleatorio

x = c(1.74,1.63, 1.65, 1.60, 1.80, 1.76, 1.58, 1.64, 1.70, 1.78)
mean(x)
t.test(x,mu = 1.7,conf.level = 0.95)





x = c(-3.91, 6.79, -12.4, -18.9, 0.51, -18.76, 0, -9.73, 17.11, -24.09, -15.34, -5.4)
t.test(x,mu=0,conf.level = 0.95,alternative = "less")



wilcox.test(x,mu=0,alternative = "less",conf.int = 0.95)



|Zi| 3.91 6.79 12.4 18.9 0.51 18.76 0 9.73 17.11 24.09 15.34 5.4


x = c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.25)
t.test(x,y,conf.level = 0.05)



#----------------------------------------------
# lista 2 - nao parametrica
#----------------------------------------------

binom.test(8,9,0.5)


binom.test(2,13,0.5)

x = c(4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77)
wilcox.test(x,mu=5.05,alternative='two.sided')


xdiff = x-5.05
x1 = abs(xdiff)
x1_rank = rank(x1)
sinal = ifelse(xdiff<0,-1,1)
sort(x1)
rank = sinal*x1
sum(x1_rank[sinal==-1])
sum(x1_rank[sinal==1])

# ex1
x = c(4,5,8,8,9,6,10,7,6,6)

binom.test(sum(x==5),length(x),0.5)

# ex2

antes = c(1.69, 2.77, 1.00, 1.66, 3.00, 0.85, 1.42, 2.82, 2.58, 1.84, 1.89, 1.91, 1.75, 2.46, 2.35)
depois = c(1.69, 2.22, 3.07, 3.35, 3.00, 2.74, 3.61, 5.14, 2.44, 4.17, 2.42, 2.94, 3.04, 4.62, 4.42)
d = antes-depois



# ex 4
x = c(63, 68, 79, 65, 64, 63, 65, 64,
      76, 74, 66, 66, 67, 73, 69, 76)

wilcox.test(x,70,alternative = 'less')

xdiff = x - 70
x1 = rank(xdiff)
sinal = ifelse()

# ex5

urb = c(35, 26, 27, 21, 27, 38, 23, 25, 25, 27, 45, 46, 33, 26, 46, 41) 
rur = c(29, 50, 43, 22, 42, 47, 42, 32, 50, 37, 34, 31)

wilcox.test(urb,rur)
  

# ex6

exp = c(14.4, 14.2, 13.8, 16.5, 14.1, 16.6, 15.9, 15.6, 14.1, 15.3, 15.7, 16.7, 13.7, 15.3, 14.0)
nexp = c(17.4, 16.2, 17.1, 17.5, 15.0, 16.0, 16.9, 15.0, 16.3, 16.8)

wilcox.test(exp,nexp,alternative = 'less')

# ex7 

x = c(44.0, 46.6, 48.2, 51.8, 60.3, 61.7, 63.6, 72.7, 77.4, 82.4, 96.1, 105.6)
npos = sum(x>60)
wilcox.test(x,60)
binom.test(npos,length(x),0.5, alternative = 'two.sided') # p-value = 0.3877


# ex8
x = c(1.9, 2.0, 2.2, 2.8, 3.1, 3.1, 3.3, 3.4, 3.7)
wilcox.test(x,3.3,alternative = 'less')

# ex9

reg_a = c(0.237, 0.235, 0.423, 0.398, 0.241, 0.237, 0.344, 0.449, 0.741, 0.405)
reg_b = c(0.341, 0.482, 0.464, 0.256, 0.908, 0.286, 0.518, 0.326)

wilcox.test(reg_a,reg_b,alternative = 'two.sided')


# exercicios ficha 03

# 01
power.t.test(n=NULL,d=.5,power=.8,sig.level=0.05)$n

# 02
power.t.test(n=NULL,d=.3,power=.8,sig.level=0.05,alternative = 'two.sided')$n

# 03
power.t.test(n=20,d=.4,power=NULL,sig.level=0.05)$power
power.t.test(n=NULL,d=.4,power=0.8,sig.level=0.05)$n

# 04
power.t.test(n=20,d=.4,power=NULL,sig.level=0.05)$power

# 05
power.t.test(n=NULL,d=.35,power=0.8,sig.level=0.05)$n

# 06
1-power.t.test(n=20,d=.6,power=NULL,sig.level=0.05)$power
1-power.t.test(n=20,d=.6,power=NULL,sig.level=0.1)$power

# 07
cost_per_person = 200
power.t.test(n=NULL,d=.5,power=0.8,sig.level=0.05)$n # per group!
round(power.t.test(n=NULL,d=.5,power=0.8,sig.level=0.05)$n,0)*2*cost_per_person

# increase power
max_budget = 14000
n_teste = max_budget/cost_per_person
power.t.test(n=n_teste,d=.5,power=0.81,sig.level=NULL)$sig.level # per group!
power.t.test(n=n_teste,d=.7,power=0.81,sig.level=NULL)$sig.level # per group!

# 08
power.t.test(n=NULL,d=NULL,power=0.3,sig.level=0.06) # per group!

# 09
pwr.r.test(n=1000,r=.05,power=NULL,sig.level=0.05) # per group!
pwr.r.test(n=1000,r=.05,power=.35,sig.level=NULL) # per group!
pwr.r.test(n=1000,r=.0025,power=NULL,sig.level=0.05)$power # per group!

# ex 10 ?
t.test(70,mu=75,sd)
pwr.r.test(n=50,r=.0025,power=NULL,sig.level=0.05)$power # per group!

# ex 11
propo
pwr.r.test(n=NULL,r=.0025,power=0.8,sig.level=0.05)$power # per group!

