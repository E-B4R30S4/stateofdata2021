install.packages("pacman")
library(pacman)

pacman::p_load(GGally, dplyr, corrplot, car, nnet,
               AER, lmtest, gtsummary, reshape2, 
               ggplot2)

df = read.csv("State of Data 2021 - Dataset - Pgina1.csv", header = TRUE)

newdf = df[ , c(1,2,7:10,15,19:21)]

newdf = rename(newdf, 
               c("ID"="X..P0....id..",
                 "Idade"="X..P1_a.....Idade..",
                 "Regiao"="X..P1_e_b.....Regiao.onde.mora..",
                 "Regiao de origem"="X..P1_g_b.....Regiao.de.origem..",
                 "Mudou de estado"="X..P1_g_c.....Mudou.de.Estado...",
                 "Nivel de Ensino"="X..P1_h.....Nivel.de.Ensino..",
                 "Gestor"="X..P2_d.....Gestor...",
                 "Faixa Salarial"="X..P2_h.....Faixa.salarial..",
                 "Xp_Area de Dados"="X..P2_i.....Quanto.tempo.de.experiência.na.�.rea.de.dados.você.tem...",
                 "Xp_Engenharia de Software"="X..P2_j.....Quanto.tempo.de.experiência.na.�.rea.de.TI.Engenharia.de.Software.você.teve.antes.de.come�.ar.a.trabalhar.na.�.rea.de.dados..."
))

newdf$ID = seq(newdf$ID)

newdf$Regiao = ifelse(newdf$Regiao == "Norte", "1", newdf$Regiao)
newdf$Regiao = ifelse(newdf$Regiao == "Sul", "2", newdf$Regiao)
newdf$Regiao = ifelse(newdf$Regiao == "Sudeste", "3", newdf$Regiao)
newdf$Regiao = ifelse(newdf$Regiao == "Nordeste", "4", newdf$Regiao)
newdf$Regiao = ifelse(newdf$Regiao == "Centro-oeste", "5", newdf$Regiao)
newdf$Regiao = ifelse(newdf$Regiao == "Exterior", "6", newdf$Regiao)

newdf$`Regiao de origem` = ifelse(newdf$`Regiao de origem` == "Norte", "1", newdf$`Regiao de origem`)
newdf$`Regiao de origem` = ifelse(newdf$`Regiao de origem` == "Sul", "2", newdf$`Regiao de origem`)
newdf$`Regiao de origem` = ifelse(newdf$`Regiao de origem` == "Sudeste", "3", newdf$`Regiao de origem`)
newdf$`Regiao de origem` = ifelse(newdf$`Regiao de origem` == "Nordeste", "4", newdf$`Regiao de origem`)
newdf$`Regiao de origem` = ifelse(newdf$`Regiao de origem` == "Centro-oeste", "5", newdf$`Regiao de origem`)

newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Estudante de Graduação", "1", newdf$`Nivel de Ensino`)
newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Não tenho graduação formal", "1", newdf$`Nivel de Ensino`)
newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Graduação/Bacharelado", "2", newdf$`Nivel de Ensino`)
newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Pós-graduação", "3", newdf$`Nivel de Ensino`)
newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Mestrado", "4", newdf$`Nivel de Ensino`)
newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Doutorado ou Phd", "4", newdf$`Nivel de Ensino`)
newdf$`Nivel de Ensino` = ifelse(newdf$`Nivel de Ensino` == "Prefiro não informar", "0", newdf$`Nivel de Ensino`)

newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "Menos de 1 ano", "1", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "de 1 a 2 anos", "2", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "de 2 a 3 anos", "3", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "de 4 a 5 anos", "4", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "de 6 a 10 anos", "5", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "Mais de 10 anos", "6", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "Não tenho experiência na área de dados", "0", newdf$`Xp_Area de Dados`)

newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "Menos de 1 ano", "1", newdf$`Regiao de origem`)
newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "de 1 a 2 anos", "2", newdf$`Xp_Engenharia de Software`)
newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "de 2 a 3 anos", "3", newdf$`Xp_Engenharia de Software`)
newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "de 4 a 5 anos", "4", newdf$`Xp_Engenharia de Software`)
newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "de 6 a 10 anos", "5", newdf$`Xp_Engenharia de Software`)
newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "Mais de 10 anos", "6", newdf$`Xp_Engenharia de Software`)
newdf$`Xp_Engenharia de Software` = ifelse(newdf$`Xp_Engenharia de Software` == "Não tive experiência na área de TI/Engenharia de Software antes de começar a trabalhar na área de dados", "0", newdf$`Xp_Engenharia de Software`)

newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "Menos de R$ 1.000/mês", "1", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 1.001/mês a R$ 2.000/mês", "2", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 2.001/mês a R$ 3000/mês", "3", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 3.001/mês a R$ 4.000/mês", "4", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 4.001/mês a R$ 6.000/mês", "5", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 6.001/mês a R$ 8.000/mês", "6", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 8.001/mês a R$ 12.000/mês", "7", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 12.001/mês a R$ 16.000/mês", "8", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 16.001/mês a R$ 20.000/mês", "9", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 20.001/mês a R$ 25.000/mês", "10", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 25.001/mês a R$ 30.000/mês", "11", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "de R$ 30.001/mês a R$ 40.000/mês", "12", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "Acima de R$ 40.001/mês", "13", newdf$`Faixa Salarial`)

newdf$Regiao = ifelse(newdf$`Mudou de estado` == 1, newdf$`Regiao de origem`, newdf$Regiao)

newdf = newdf[ , c(-4,-5)]

newdf$Regiao = as.numeric(newdf$Regiao)
newdf$`Nivel de Ensino` = as.numeric(newdf$`Nivel de Ensino`)
newdf$`Faixa Salarial` = as.numeric(newdf$`Faixa Salarial`)
newdf$`Xp_Area de Dados` = as.numeric(newdf$`Xp_Area de Dados`)
newdf$`Xp_Engenharia de Software` = as.numeric(newdf$`Xp_Engenharia de Software`)

glimpse(newdf)

ggpairs(newdf [ , -1])

ggcorr(newdf [ , -1], label=TRUE, nbreaks = 5, palette = "Pastel2")

newdf = newdf[ , c(-3,-5,-8)]

newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "1", "Abaixo", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "2", "Abaixo", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "3", "Abaixo", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "4", "Abaixo", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "5", "Abaixo", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "6", "M�dia", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "7", "M�dia", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "8", "Acima", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "9", "Acima", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "10", "Acima", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "11", "Acima", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "12", "Acima", newdf$`Faixa Salarial`)
newdf$`Faixa Salarial` = ifelse(newdf$`Faixa Salarial` == "13", "Acima", newdf$`Faixa Salarial`)

newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "1", "At� 3 Anos", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "2", "At� 3 Anos", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "3", "At� 3 Anos", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "4", "Mais de 3 Anos", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "5", "Mais de 3 Anos", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "6", "Mais de 3 Anos", newdf$`Xp_Area de Dados`)
newdf$`Xp_Area de Dados` = ifelse(newdf$`Xp_Area de Dados` == "0", "At� 3 Anos", newdf$`Xp_Area de Dados`)

newdf = print(select(newdf,1,4,2,3,5))  

newdf$`Faixa Salarial` = as.factor(newdf$`Faixa Salarial`)
newdf$Idade = as.integer(newdf$Idade)
newdf$`Xp_Area de Dados` = as.factor(newdf$`Xp_Area de Dados`)

glimpse(newdf)

newdf = na.omit(newdf)

levels(newdf$`Faixa Salarial`)

newdf$`Faixa Salarial` = relevel(newdf$`Faixa Salarial`, ref = "Acima")

mod = multinom(`Faixa Salarial` ~ `Xp_Area de Dados` + `Nivel de Ensino` + Idade, data = newdf, model = TRUE)
mod0 = multinom(`Faixa Salarial` ~ 1, data = newdf, model = TRUE)
anova(mod, mod0)

car::Anova(mod, type = "II", test = "Wald")

lmtest::coeftest(mod)

gtsummary::tbl_regression(mod, exponentiate = TRUE)

df2 = table(Observado = newdf$`Faixa Salarial`, Previsto = predict(mod))

acuracia = sum(diag(df2)) / sum(df2)

data_prev = cbind(newdf[ , c(3,4,5)], predict(mod, type = "probs", se = TRUE))

data_prev = reshape2::melt(data_prev,
                           id.vars = c("Idade", "Nivel de Ensino", "Xp_Area de Dados"),
                           value.name = "Probabilidade",
                           variable.name = "Faixa Salarial")

ggplot(data_prev, aes(x = Idade, y = Probabilidade, color = `Faixa Salarial`)) +
  geom_point(color = "grey") +
  geom_smooth(method = "loess", size = 1.5) +
  labs(x = "Idade") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",",
                                                    accuracy = 1),
                     breaks = seq(10, 60, 10)) +
  facet_grid(`Xp_Area de Dados` ~ .) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = NA)))

data_prev = data_prev%>%mutate(Ensino = format(data_prev$`Nivel de Ensino`))

data_prev$Ensino = ifelse(data_prev$`Nivel de Ensino` == "1", "Estudante", data_prev$Ensino)
data_prev$Ensino = ifelse(data_prev$`Nivel de Ensino` == "2", "Gradua��o", data_prev$Ensino)
data_prev$Ensino = ifelse(data_prev$`Nivel de Ensino` == "3", "Especializa��o", data_prev$Ensino)
data_prev$Ensino = ifelse(data_prev$`Nivel de Ensino` == "4", "Acad�mico", data_prev$Ensino)
data_prev$Ensino = as.factor(data_prev$Ensino)

data_prev = dplyr::filter(data_prev, data_prev$Ensino != 0)

ggplot(data_prev, aes(x = Idade, y = Probabilidade, color = `Faixa Salarial`)) +
  geom_point(color = "grey") +
  geom_smooth(method = "loess", size = 1.5) +
  labs(x = "Idade") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",",
                                                    accuracy = 1),
                     breaks = seq(10, 60, 10)) +
  facet_grid(Ensino ~ .) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = NA)))

