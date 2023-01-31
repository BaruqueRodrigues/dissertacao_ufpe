library(tidyverse)
results <- read_csv("data/results.csv") %>% 
  janitor::clean_names() %>% 
  mutate(index = str_detect(source, 
                            pattern = ".ongresso|.ese|.issertação|.oncurso|.esenha|.ratado|.nais|.oleção"),
         source = tolower(source)
         ) 


qualis <- readr::read_csv("data/qualis_2016.csv") %>% 
  janitor::clean_names() %>% 
  mutate(titulo2 = tolower(titulo))

result_b<-inner_join(results, 
                     qualis,
                     by = c("source" = "titulo2")) %>% 
  unique() %>%
  readr::write_csv("dados_limpos.csv")

readxl::read_excel("C:/Users/quemu/Downloads/classificacoes_publicadas_ciencia_politica_e_relacoes_internacionais_2017_1503422062321.xls")


