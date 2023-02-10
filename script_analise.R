# Script análise
library(tidyverse)

# Get data
dados <- read_csv("data/rev_sist_msc_baruque - tab_rev_artigos.csv", 
                  col_types = cols(sex_autor = col_character()))
qualis <-  readr::read_csv("data/qualis_2016.csv") %>% 
  janitor::clean_names() %>% 
  mutate(titulo2 = tolower(titulo))

dados <- dados %>% 
  left_join(
    qualis %>% 
      select(titulo = titulo2, 
             estrato),
    by = c("periodico" = "titulo")
  ) %>% 
  unique()

dados %>% 
  separate_rows(autor_n, sep = ",") %>% 
  mutate(autor_n = str_squish(autor_n) %>% str_to_title()) %>%
  select(autor_n) %>% 
  write_csv2("data/autores_lates.csv")


tab_desc<- function(dados, var){
  dados %>% 
    summarise("valor mínimo" = min({{var}}, na.rm = TRUE),
              "mediana" = median({{var}}, na.rm = TRUE),
              "média" = mean({{var}}, na.rm = TRUE),
              "desvio padrão" = sd({{var}}, na.rm = TRUE),
              "valor máximo" = max({{var}}, na.rm = TRUE))
}


# Autoria -----------------------------------------------------------------

#autores por ano

dados %>% group_by(ano_artigo) %>% 
  summarise(n_autores = mean(n_autores, na.rm = TRUE)) %>% 
  ggplot(aes(x=ano_artigo, y= n_autores))+
  geom_line(size = 1, color = "lightgrey")+
  geom_hline(yintercept = mean(dados$n_autores, na.rm= TRUE),
             linetype = "dashed", color = "red")+
  geom_text(aes(label = round(n_autores, 2) ), color = "#a83440")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks = seq(2000, 2022))+
  labs(y = "Número de Autores", x = "Ano do Artigo")

dados %>% 
  tab_desc(n_autores)
# Artigos por ano
dados %>% 
  janitor::tabyl(ano_artigo) %>% 
  ggplot( aes(x= ano_artigo, y= n))+
  geom_line(size = 1, color = "lightgrey")+
  geom_hline(yintercept = mean(dados %>% 
                                 janitor::tabyl(ano_artigo) %>% 
                                 pull(n), na.rm= TRUE),
             linetype = "dashed", color = "red")+
  geom_text(aes(label = n), color = "#a83440")+
  scale_x_continuous(breaks = c(2000:2022))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 90))+
  labs(y = "Número de Artigos", x = "Ano do Artigo")
dados$ano_artigo %>% summary()
# Análise Gênero

dados %>% 
  separate_rows(sex_autor) %>% 
  mutate(sex_autor = recode(sex_autor, 
                            `1` = "Masculino", 
                            `2` = "Feminino") %>% replace_na(., "não identificado")) %>% 
  janitor::tabyl(sex_autor) %>% 
  ggplot(aes(x = reorder(sex_autor, -percent), y= percent*100))+
  geom_col(fill = "#a83440", width = .6)+
  geom_text(aes(label = round(percent, 3)*100), vjust =-.5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold")
        )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Sexo dos Autores", 
       y = NULL)

# Universidades

dados %>% 
  separate_rows(uni_autor, sep = ",") %>% 
  mutate(uni_autor = toupper(uni_autor) %>% 
           str_squish()) %>% 
  drop_na(uni_autor) %>% 
  filter(!uni_autor %in% c("-", "")) %>% 
  janitor::tabyl(uni_autor) %>% 
  ggplot(aes(y = reorder(uni_autor, percent),
             x = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust =-.1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x= "%",
       y = "Universidade do Autor")

#Estado do Autor
dados %>% 
  separate_rows(estado_uni_autor, sep = ",") %>% 
  mutate(estado_uni_autor = str_squish(estado_uni_autor)) %>% 
  mutate(estado_uni_autor = toupper(estado_uni_autor)) %>% 
  drop_na(estado_uni_autor) %>% 
  filter(!estado_uni_autor %in% c("-", "")) %>% 
  janitor::tabyl(estado_uni_autor) %>% 
  ggplot(aes(y = reorder(estado_uni_autor, percent),
             x = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 3)*100), hjust =-.1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x= "%",
       y = "Estado do Autor")

geobr::read_state() %>% 
  left_join(dados %>% 
              separate_rows(estado_uni_autor, sep = ",") %>% 
              mutate(estado_uni_autor = str_squish(estado_uni_autor)) %>% 
              mutate(estado_uni_autor = toupper(estado_uni_autor)) %>% 
              #drop_na(estado_uni_autor) %>% 
              #filter(!estado_uni_autor %in% c("-", "")) %>% 
              janitor::tabyl(estado_uni_autor),
            by = c("abbrev_state" = "estado_uni_autor")) %>% 
  #replace_na(list(n = 0, percent = 0)) %>% 
  ggplot()+
  geom_sf(aes(fill = percent*100), color = "lightgrey")+
  geom_sf_text(aes(label = round(percent*100, 3)), color = "black")+
  theme_void()+
  guides(fill=guide_legend(title="Percentual"))+
  scale_fill_viridis_b(na.value = "white",)+
  labs(title = "Distribuição de Autores de Artigos sobre Gastos de Campanha e Obtenção Votos no Brasil")+
  theme(plot.title = element_text(hjust = .5))
# Bibliografico -----------------------------------------------------------


#Citações por ano

dados %>% 
  group_by(ano_artigo) %>% 
  summarise(citacoes = mean(citacoes, na.rm = TRUE)) %>% 
  
  ggplot(aes( x= ano_artigo, y= citacoes))+
  geom_line(size = 1, color = "lightgrey")+
  geom_hline(yintercept = mean(dados$citacoes, na.rm= TRUE),
             linetype = "dashed", color = "red")+
  geom_text(aes(label = round(citacoes, 2)), color = "#a83440")+
  theme_bw()+
  scale_x_continuous(breaks = c(2000:2022))+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x= "Ano do Artigo", y= "Média de Citações")
  
#Qualis Revista

dados %>% 
  janitor::tabyl(estrato) %>% 
  ggplot(aes(
    x = estrato, 
    y = percent*100
  ))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 3)*100), vjust = -.5)+
  labs(x = "Qualis das Revistas Publicadas",
       y = "%")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))

# Revistas
dados %>% 
  janitor::tabyl(periodico) %>% 
  ggplot(aes(
    y=reorder(periodico, percent), 
    x= percent*100
  ))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust = -.2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "%",
       y = "Revista")
  
#Abordagem Metodológica
dados %>% 
  drop_na(abordagem_metodologica) %>% 
  filter(abordagem_metodologica != "-") %>% 
  janitor::tabyl(abordagem_metodologica) %>% 
  ggplot(aes(y = reorder(abordagem_metodologica, percent),
             x = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust = -.2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Distribuição de Ténicas de Pesquisa Empregadas",
       y = "%")

#Técnicas Utilizadas
dados %>%
  drop_na(tec_pesquisa) %>% 
  filter(tec_pesquisa != "-") %>% 
  mutate(tec_pesquisa = str_to_title(tec_pesquisa)) %>% 
  janitor::tabyl(tec_pesquisa) %>% 
  ggplot(aes(y = reorder(tec_pesquisa, percent),
             x = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust = -.2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(y = "Distribuição da Abordagem de Pesquisa",
       x = "%")
#Número de técnicas de pesquisa empregadas

dados %>% 
  drop_na(n_tecnicas) %>% 
  filter(n_tecnicas != "-") %>% 
  janitor::tabyl(n_tecnicas) %>% 
  ggplot(aes(x = reorder(n_tecnicas, -percent),
             y = percent*100))+
  geom_col(fill = "#a83440", width = .5)+
  geom_text(aes(label = round(percent, 4)*100), vjust = -.2)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Número de Técnicas Empregadas na Análise",
       y = "%")

#Ano eleição

dados %>% 
  separate_rows(ano_eleicao, sep = ",") %>% 
  mutate(ano_eleicao = str_squish(ano_eleicao)) %>% 
  drop_na(ano_eleicao) %>% 
  filter(!ano_eleicao %in% c("-", 0)) %>% 
  janitor::tabyl(ano_eleicao) %>% 
    ggplot(aes(x = reorder(ano_eleicao, - percent), 
               y= percent*100))+
    geom_col(fill = "#a83440")+
    geom_text(aes(label = round(percent, 4)*100), vjust = -.2)+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 11, face = "bold"),
          axis.text = element_text(size = 11, face = "bold"))+
    labs(x = "Percentual de Ano de Eleição Análisados",
         y = "%")
#Variável interveniente
dados %>% 
  mutate(var_interveniente = ifelse(
                                    var_interveniente %in% c("-", NA_character_),
                                    "Não Faz Uso", "Faz uso")) %>% 
  janitor::tabyl(var_interveniente) %>% 
  ggplot(aes(x = reorder(var_interveniente, -percent), 
             y= percent*100))+
  geom_col(fill = "#a83440", width = .5)+
  geom_text(aes(label = round(percent, 4)*100), vjust = -.05)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Uso de Variáveis Intervenientes",
       y = "%")

# Serie temporal

dados %>% 
  #separate_rows(ano_eleicao, sep = ",") %>% 
  #mutate(ano_eleicao = str_squish(ano_eleicao)) %>% 
  drop_na(ano_eleicao) %>% 
  filter(!ano_eleicao %in% c("-", 0)) %>% 
  janitor::tabyl(ano_eleicao) %>% 
  ggplot(aes(x = reorder(ano_eleicao, percent), 
             y= percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust = -.05)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Séries Temporais Análisados",
       y = "%")+
  coord_flip()
# Ano eleições
dados %>%
  drop_na(n_eleicoes) %>% 
  filter(!n_eleicoes %in% c("-", 0)) %>% 
  janitor::tabyl(n_eleicoes) %>% 
  ggplot(aes(x= reorder(n_eleicoes, -percent),
             y = percent*100))+
    geom_col(fill = "#a83440")+
    geom_text(aes(label = round(percent, 4)*100), vjust = -.2)+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 11, face = "bold"),
          axis.text = element_text(size = 11, face = "bold"))+
    labs(x = "Percentual de Número de Eleições Analisados",
         y = "%")

#Cargo

dados %>% 
  separate_rows(cargo, sep = ",") %>% 
  drop_na(cargo) %>% 
  filter(!cargo %in% c("-", 0)) %>% 
  mutate(cargo = str_squish(cargo) %>% 
           str_to_title()) %>% 
  janitor::tabyl(cargo) %>% 
  ggplot(aes(x= reorder(cargo, -percent),
             y = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), vjust = -.2)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Cargos Analisados",
       y = "%")

#Variável independete
dados %>% 
  drop_na(abordagem_metodologica) %>% 
  filter(v_independente != "-") %>% 
  janitor::tabyl(v_independente) %>% 
  #arrange(percent) %>% 
  ggplot(aes(y = reorder(v_independente, percent),
             x = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust = -.2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Distribuição de Variável Independente",
       y = "%")

#Variável Dependente
dados %>% 
  drop_na(v_dependente) %>% 
  filter(v_dependente != "-") %>% 
  janitor::tabyl(v_dependente) %>% 
  #arrange(percent) %>% 
  ggplot(aes(y = reorder(v_dependente, percent),
             x = percent*100))+
  geom_col(fill = "#a83440")+
  geom_text(aes(label = round(percent, 4)*100), hjust = -.2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Distribuição de Variável Dependente",
       y = "%")
#Controles Contagem


df<- dados %>%
  #drop_na(controles) %>% 
  #filter(controles != "-") %>% 
  separate_rows(controles, sep = ",") %>% 
  mutate(controles = tolower(controles) %>% 
           str_squish(),
         n_controles = ifelse(!controles %in% c("-", "", "0", NA_character_), 1, 0)) %>% 
  filter(abordagem_metodologica == "inferencial") %>% 
  group_by(autor_n) %>% 
  summarise(ano_artigo,
            controles = sum(n_controles)) %>% 
  unique() 
df$autor_n <-c("Silva e Cervi (2014)",
                          "Silva e Gonçalves (2020)",
                          "Dias, Nossa e Monte-Mor (2018)",
                          "Netto e Speck (2017)",
                          "Heiler, Viana e Santos (2016)",
                          "Lautenshlag (2019)",
                          "Carlomagno e Codato (2015)",
                          "Campos e Machado (2015)",
                          "Rebello, Giora e Scapini, (2016)",
                          "Resende, Schaefer, Epitacio e Barbosa (2020)",
                          "Santos (2020)",
                          "Corrêa e Santos (2020)"
)

df %>% 
  ggplot(aes(y = reorder(autor_n, controles), x= controles))+
  geom_col(fill = "#a83440")+
  geom_vline(xintercept = mean(df$controles), linetype = "dashed", color = "red")+
  geom_text(aes(label = controles), hjust = -.2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "Número de Controles",
       y = "")
#Controles Wordcloud
dados %>% 
  drop_na(controles) %>% 
  filter(controles != "-") %>% 
  separate_rows(controles, sep = ",") %>% 
  mutate(controles = tolower(controles)) %>% 
  ggplot(aes(label = controles))+
  ggwordcloud::geom_text_wordcloud( color = "#a83440"
    )+
  theme_bw()+
  labs(title = "Wordcloud dos Controles Utilizados")+
  theme(plot.title = element_text(hjust = .5))

# Como apresentar as hipóteses?
dados %>% 
  mutate(tem_hipotese = ifelse(!hipotese_pesquisa %in% c("-", NA_character_), "Sim", "Não" )) %>% 
  janitor::tabyl(tem_hipotese) %>% 
  ggplot(aes(x = reorder(tem_hipotese, -percent),
             y = percent*100))+
  geom_col(fill = "#a83440", width = .5)+
  geom_text(aes(label = round(percent, 4)*100), vjust = -.2)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))+
  labs(x = "O Artigo Apresenta Hipótese?",
       y = "%")

dados$hipotese_pesquisa
dados %>% 
  drop_na(hipotese_pesquisa) %>% 
  filter(hipotese_pesquisa != "-") %>% 
  #separate_rows(controles, sep = ",") %>% 
  mutate(controles = tolower(hipotese_pesquisa)) %>% 
  ggplot(aes(label = hipotese_pesquisa))+
  ggwordcloud::geom_text_wordcloud_area( color = "#a83440"
  )+
  theme_bw()+
  labs(title = "Wordcloud Hipóteses de Pesquisa")+
  theme(plot.title = element_text(hjust = .5))


# Power effect

dados %>% glimpse()
