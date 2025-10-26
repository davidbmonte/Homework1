# =====================================================
# Item 1 - Média, mediana e desvio padrão
# =====================================================

dados <- data.frame(
  Idade = c(28,34,46,26,37,29,51,31,39,43,58,44,25,23,52,42,48,33,38,46),
  Nacionalidade = c("Italiana","Inglesa","Belga","Espanhola","Italiana",
                    "Espanhola","Francesa","Belga","Italiana","Italiana",
                    "Italiana","Inglesa","Francesa","Espanhola","Italiana",
                    "Alemana","Francesa","Italiana","Alemana","Italiana"),
  Renda = c(2.3,1.6,1.2,0.9,2.1,1.6,1.8,1.4,1.2,2.8,3.4,2.7,1.6,1.2,1.1,2.5,2.0,1.7,2.1,3.2),
  Experiencia = c(2,8,21,1,15,3,28,5,13,20,32,23,1,0,29,18,19,7,12,23)
)

summary_stats <- data.frame(
  Variável = c("Idade", "Renda", "Experiência"),
  Média = c(mean(dados$Idade), mean(dados$Renda), mean(dados$Experiencia)),
  Mediana = c(median(dados$Idade), median(dados$Renda), median(dados$Experiencia)),
  Desvio_Padrão = c(sd(dados$Idade), sd(dados$Renda), sd(dados$Experiencia))
)

print(summary_stats)


# =====================================================
# Item 2 - Agrupamento por nacionalidade
# =====================================================

install.packages("dplyr")
library(dplyr)

agrupado <- dados %>%
  group_by(Nacionalidade) %>%
  summarise(
    Renda_Média = mean(Renda),
    Experiência_Média = mean(Experiencia)
  )

print(agrupado)


# =====================================================
# Item 3 - Correlação entre experiência e renda desejada
# =====================================================

plot(dados$Experiencia, dados$Renda,
     main = "Relação entre Experiência e Renda Desejada",
     xlab = "Experiência (anos)", ylab = "Renda desejada (mil euros)",
     pch = 19, col = "blue")

abline(lm(Renda ~ Experiencia, data = dados), col = "red")

correlacao <- cor(dados$Experiencia, dados$Renda, method = "pearson")
print(correlacao)


# =====================================================
# Item 4 - Filtro de candidatos com critérios específicos
# =====================================================

filtro <- dados %>%
  filter(Experiencia >= 10, Renda < 2.0) %>%
  select(Nacionalidade, Idade, Renda, Experiencia)

print(filtro)
print(nrow(filtro))


# =====================================================
# Item 5 - Gráficos de idade e renda por nacionalidade
# =====================================================

install.packages("ggplot2")
library(ggplot2)

ggplot(dados, aes(x=Idade, fill=Nacionalidade)) +
  geom_histogram(binwidth=5, alpha=0.7, position="identity") +
  labs(title="Distribuição da idade por nacionalidade", x="Idade", y="Frequência")

ggplot(dados, aes(x=Nacionalidade, y=Renda, fill=Nacionalidade)) +
  geom_boxplot() +
  labs(title="Distribuição da renda desejada por nacionalidade", x="Nacionalidade", y="Renda (mil euros)")

