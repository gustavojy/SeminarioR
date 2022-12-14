#-----------------------------------------------------------------------------
# Tutorial sobre mapas do Brasil em R
# Aplicação conjunta dos pacotes geobr, ggplot2 e tidyverse
#
# Gustavo Jun Yakushiji (gustavojun@usp.br; github.com/gustavojy)
# 30 de setembro de 2022
#-----------------------------------------------------------------------------

# 1. Instalar e carregar pacotes
## 1.1 tidyverse

## 1.2 geobr: https://github.com/ipeaGIT/geobr#readme

#-------------------------------------------------------------------------------

# 2. Pacote geobr


#-------------------------------------------------------------------------------
## Operador pipe (%>%) - atalho (ctrl + shift + M)
# Sem o pipe

# Com o pipe

# Com o pipe + objeto "notas"

#-------------------------------------------------------------------------------

# 3. Mapa do país
# 3.1 Função read_country() + ggplot()

# 3.2 Argumento year =


#-------------------------------------------------------------------------------

# 4. Mapa dos estados
# 4.1 Todos os estados

# 4.2 Selecionando um estado específico

# 4.3 Selecionando regiões

# 4.4 Selecionando mais de um estado

# 4.5 Adicionando cores de acordo com a região

# 4.6 Alterações estéticas básicas
estados %>% 
  ggplot()+
  geom_sf(aes(fill = name_region))+
  theme_void()+
  labs(title = "Mapa dos estados do Brasil\n por região",
       subtitle = "Ano 2010",
       fill = "Regiões",
       caption = "Fonte: IBGE, 2010")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 10))+
  scale_fill_brewer(palette = "Set2",
                    labels = c("CO", "NE", "N", "SE", "S"))+
  #scale_fill_manual(values = c("orange", "red", "lightgreen", "yellow","lightblue"))+
  guides(fill = "none")+
  geom_sf_text(aes(label = abbrev_state), size = 2.5)

## OBS: Dica de site para cores hexadecimais - https://www.rapidtables.com/web/color/RGB_Color.html

# 4.7 Mapas por ano
estados_1872 <- read_state(code_state = "all", year = 1872) %>% 
  mutate(ano = 1872)

estados_1933 <- read_state(code_state = "all", year = 1933) %>% 
  mutate(ano = 1933)

estados_2010 <- read_state(code_state = "all", year = 2010) %>% 
  mutate(ano = 2010)

estados_juntos <- bind_rows(estados_1872, estados_1933, estados_2010)

ggplot()+
  geom_sf(data = estados_juntos)+
  facet_wrap(~ano)

#-------------------------------------------------------------------------------

# 5. Mapa de municípios
# 5.1 Todos os municípios do Brasil

# 5.2 Municípios de um estado

# 5.3 Procurando código de um município específico - Função lookup_muni()

# 5.4 Selecionando mais de um município

#-------------------------------------------------------------------------------

# 6. Mapas Temáticos

# 6.1 Importação de dados
library(readxl)
censo_agro_06_17 <- read_excel("dados_censo_agropec.xlsx")
view(censo_agro_06_17)

# 6.2 Unindo bases de dados
estados <- read_state(code_state = "all", showProgress = F)

dados_juntos <- full_join(estados, censo_agro_06_17,
                          by = "name_state")

# 6.2.1 Número de estabelecimentos - Censo 2006
dados_juntos %>% 
  filter(ano == 2006) %>% 
  ggplot()+
  geom_sf(aes(fill = n_estab_1000))

## Ajustes estéticos
dados_juntos %>% 
  filter(ano == 2006) %>% 
  ggplot()+
  geom_sf(aes(fill = n_estab_1000))+
  scale_fill_distiller(direction = 0,
                       limits = c(0, 800),
                       name = "Nº estabelecimentos (por 1000 und.)",
                       guide = guide_legend(
                         keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top', nrow=1))+
  labs(title = "Censo Agropecuário 2006",
       subtitle = "Número de estabelecimentos agropecuários por estado, a cada 1000 unidades",
       caption = "Fonte: IBGE - Censo Agropecuário 2006")+
  theme_void()+
  theme(plot.title = element_text(size= 15, hjust = 0.5),
        plot.subtitle = element_text(size= 10, hjust = 0.5),
        plot.caption = element_text(size=8, hjust = 0.5, vjust = 7),
        legend.position = c(0.88, 0.12))

# 6.2.2 Número de estabelecimentos - Censo 2017
dados_juntos %>% 
  filter(ano == 2017) %>% 
  ggplot()+
  geom_sf(aes(fill = n_estab_1000))+
  scale_fill_distiller(direction = 0,
                       limits = c(0, 800),
                       name = "Nº estabelecimentos (por 1000 und.)",
                       guide = guide_legend(
                         keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top', nrow=1))+
  labs(title = "Censo Agropecuário 2017",
       subtitle = "Número de estabelecimentos agropecuários por estado, a cada 1000 unidades",
       caption = "Fonte: IBGE - Censo Agropecuário 2017")+
  theme_void()+
  theme(plot.title = element_text(size= 15, hjust = 0.5),
        plot.subtitle = element_text(size= 10, hjust = 0.5),
        plot.caption = element_text(size=8, hjust = 0.5, vjust = 7),
        legend.position = c(0.88, 0.12))


# 6.2.3 Número de estabelecimentos - Censo 2006 e 2017
dados_juntos %>% 
  ggplot()+
  geom_sf(aes(fill = n_estab_1000))+
  scale_fill_distiller(direction = 0,
                       limits = c(0, 800),
                       name = "Nº estabelecimentos (por 1000 und.)",
                       guide = guide_legend(
                         keyheight = unit(3, units = "mm"),
                         keywidth=unit(11, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top', nrow=1))+
  facet_wrap(~ano)+ # Dois mapas, de acordo com o ano
  labs(title = "Censo Agropecuário 2006 e 2017",
       subtitle = "Número de estabelecimentos agropecuários por estado, a cada 1000 unidades",
       caption = "Fonte: IBGE - Censo Agropecuário 2006 e 2017")+
  theme_void()+
  theme(plot.title = element_text(size= 15, hjust = 0.5, vjust = 2),
        plot.subtitle = element_text(size= 11, hjust = 0.5, vjust = 2.5),
        plot.caption = element_text(size=8, hjust = 0.5, vjust = -2),
        legend.position = c(0.49, 0.06))


#-----------------------------------------------------------------------------
# Recados Finais
#
# Apostila "Introdução à ciência de dados em R"
# Disponível em: https://gustavojy.github.io/ApostilaCD-R/
#-----------------------------------------------------------------------------

