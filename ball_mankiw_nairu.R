
# Instalar pacotes necessarios
if (!require("tidyverse")) install.packages("tidyverse", dependencies=TRUE)
if (!require("rbcb")) install.packages("rbcb", dependencies=TRUE)
if (!require("seasonal")) install.packages("seasonal", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("mFilter")) install.packages("mFilter", dependencies=TRUE)

# Carregar bibliotecas
library(tidyverse)
library(rbcb)
library(seasonal)
library(ggplot2)
library(mFilter)
library(ipeadatar)
library(ggthemes)
library(ggplot2)

# Taxa de desocupacao - PNADC
desemprego <- GetBCBData::gbcbd_get_series(
  id = 24369,
  first.date = "2012-03-01",
  last.date = Sys.Date()
) %>% rename(Date = ref.date)

# Criar série temporal
ts_desemprego <- ts(desemprego$value, start=c(2012,3), frequency=12)

# Ajuste sazonal com X13-ARIMA-SEATS
desemprego_sa <- final(seas(ts_desemprego))

# Adiciona a serie dessazonalizada ao dataframe
desemprego <- desemprego %>%
  mutate(desemprego_sa = as.numeric(desemprego_sa))

# Importa expectativa de inflacao acumulada em 12 meses
expectativa_ipca <- ipeadatar::ipeadata("BM12_IPCAEXP1212") %>%
  rename(Date = date, Mediana = value)

# Importa IPCA mensal
ipca_mensal <- ipeadatar::ipeadata("PRECOS12_IPCAG12") %>%
  rename(Date = date, IPCA = value)

# Junta dados do IPCA e expectativa
dados_ipca <- inner_join(ipca_mensal, expectativa_ipca, by='Date') %>%
  mutate(delta_ipca_exp = IPCA - Mediana)

# Junta dados do desemprego
dados <- inner_join(dados_ipca, desemprego, by='Date') %>%
  mutate(desemprego_diff12 = desemprego_sa - lag(desemprego_sa, 12))

# Ajustar modelo de regressao
model <- lm(delta_ipca_exp ~ desemprego_diff12, data=dados)
summary(model)

# Criar mudancas na curva de phillips
shifts_cp <- dados$desemprego_sa + (dados$delta_ipca_exp / coef(model)[1])

# Aplicar filtro HP
filtro_cp <- hpfilter(ts(shifts_cp, frequency=12), freq=129600)
dados$nairu <- as.numeric(filtro_cp$trend)

# Criar graficos
dados_long <- dados %>%
  rename(NAIRU = nairu, `Taxa de Desemprego` = desemprego_sa) %>%
  mutate(NAIRU = as.numeric(NAIRU)) %>%
  pivot_longer(cols=c(`Taxa de Desemprego`, NAIRU), names_to='variable', values_to='value', names_repair = "unique")


# Inspecionar o dataframe
print(head(dados_long))

# Criar gráfico com ggplot2

ggplot(dados_long, aes(x = Date, y = value...16, color = variable)) +
  geom_line(size = 1) +
  theme_excel() +
  labs(
    title = "Evolução da NAIRU e da Taxa de Desemprego",
    subtitle = "Elaboração: Ricardo Melo | Ball & Mankiw (1997)",
    x = "Ano",
    y = "Percentual",
    color = "Variável"
  )

dados <- dados %>%
  mutate(hiato_desemprego = desemprego_sa - nairu)

dados_long <- dados %>%
  rename(NAIRU = nairu, `Taxa de Desemprego` = desemprego_sa, `Hiato do Desemprego` = hiato_desemprego) %>%
  mutate(NAIRU = as.numeric(NAIRU), `Hiato do Desemprego` = as.numeric(`Hiato do Desemprego`)) %>%
  pivot_longer(cols=c(`Taxa de Desemprego`, NAIRU, `Hiato do Desemprego`), names_to='variable', values_to='value', names_repair = "unique")

# Gráfico da NAIRU e da Taxa de Desemprego
ggplot(dados_long %>% filter(variable != "Hiato do Desemprego"), aes(x = Date, y = value...16, color = variable)) +
  geom_line(size = 1) +
  theme_stata() +
  labs(
    title = "Evolução da NAIRU e da Taxa de Desemprego",
    subtitle = "Elaboração: Ricardo Melo | Ball & Mankiw (1997)",
    x = "Ano",
    y = "Percentual",
    color = "Variável"
  ) +
  scale_color_manual(values = c("Taxa de Desemprego" = "blue", "NAIRU" = "red")) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico do Hiato do Desemprego
ggplot(dados_long %>% filter(variable == "Hiato do Desemprego"), aes(x = Date, y = value...16, color = variable)) +
  geom_line(size = 1) +
  theme_stata() +
  labs(
    title = "Hiato do Desemprego",
    subtitle = "Elaboração: Ricardo Melo | Ball & Mankiw (1997)",
    x = "Ano",
    y = "Percentual",
    color = "Variável"
  ) +
  scale_color_manual(values = c("Hiato do Desemprego" = "black")) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
