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

---





sum(dados3)      # 6106
sum(dados3^2)    # 1572790

hist(dados3, freq = FALSE, main = "Tempo de vida das bactérias", xlab = "horas")
curve(dnorm(x, mean(dados3), sd(dados3)), add = TRUE, col = "blue", lwd = 2)
qqnorm(dados3); qqline(dados3, col = "red")

teste3 <- t.test(dados3, mu = 250, alternative = "greater", conf.level = 0.95)
teste3
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

n4     <- 242
mean4  <- 6992.6
sd4    <- 1176.83
mu0_4  <- 7725
alpha4 <- 0.01

se4 <- sd4 / sqrt(n4)
z4  <- (mean4 - mu0_4) / se4
p4  <- 2 * (1 - pnorm(abs(z4)))

cat("z =", z4, " | p-value =", p4, "\n")

IC4.low  <- mean4 - qnorm(1 - alpha4 / 2) * se4
IC4.high <- mean4 + qnorm(1 - alpha4 / 2) * se4
cat("IC a 99% para o consumo médio: [", IC4.low, ",", IC4.high, "]\n")
# z ~ -9.68, p-value ~ 0 (< 0.01) -> Rejeita-se H0.
# O valor de referência (7725 kJ) fica fora do IC a 99%, confirmando que o
# consumo médio diário é significativamente diferente do recomendado.


# ------------------------------------------------------------
# Exercício 5
# ------------------------------------------------------------
# Pressão arterial sistólica (mmHg) em 25 indivíduos com patologia cardíaca

dados5 <- c(162, 177, 151, 167, 141, 153, 143, 157, 123, 161, 147, 157, 141,
            157, 151, 134, 134, 128, 151, 112, 142, 121, 130, 134, 120)

# (a) Análise gráfica de simetria
hist(dados5, main = "Pressão arterial sistólica", xlab = "mmHg")
boxplot(dados5, main = "Boxplot da pressão arterial sistólica", horizontal = TRUE)
qqnorm(dados5); qqline(dados5, col = "red")
# O histograma, o boxplot e o Q-Q plot permitem avaliar visualmente se a
# distribuição é razoavelmente simétrica.

# (b) Descrição numérica
summary(dados5)     # mínimo, quartis, mediana, máximo
mean(dados5)
sd(dados5)
var(dados5)
IQR(dados5)          # Q3 - Q1
# Média amostral = 143.76 mmHg -> superior a 140 mmHg.

# (c) Teste de hipóteses: H0: mu <= 140   vs   H1: mu > 140   (alpha = 0.01)
teste5 <- t.test(dados5, mu = 140, alternative = "greater", conf.level = 0.99)
teste5
# t = 1.1587, df = 24, p-value = 0.129 (> 0.01) -> Não se rejeita H0.
# IC a 99% (unilateral): [135.67, +Inf[ -> contém 140, confirmando a não rejeição.

# Como não se rejeitou H0 -> análise de potência
d5 <- (mean(dados5) - 140) / sd(dados5)
pwr.t.test(n = length(dados5), d = d5, sig.level = 0.01,
           type = "one.sample", alternative = "greater")

# (d) Cálculo "à mão" do valor-p a partir da estatística t
t.obs5   <- (mean(dados5) - 140) / (sd(dados5) / sqrt(length(dados5)))
p.valor5 <- 1 - pt(t.obs5, df = length(dados5) - 1)
cat("t =", t.obs5, " | valor-p =", p.valor5, "\n")
