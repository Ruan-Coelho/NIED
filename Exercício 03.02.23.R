# Iniciando -------------------------------------------------------------------

# Limpando area de trabalho antes de iniciar
rm(list = ls())

# Retirando notacao cientifica
options(scipen = 999)

# Carregando pacotes, necessário dar install.packages("pacman")
# O pacote 'pacman' faz o trabalho do 'install.packages' e do 'library'
pacman::p_load(tidyverse, janitor, rio, PNADcIBGE, gt, pollster)

# Diretório de trabalho
setwd("")

# Importando usando o pacote do IBGE (offline)
pnad_19 <- read_pnadc("PNADC_2019_visita1.txt",
                      "input_PNADC_2019_visita1_20220224.txt")

# Importando usando o pacote do IBGE (online)
#pnad_19 <- get_pnadc(2019, visita = 1, labels = FALSE, design = FALSE)

# Execício --------------------------------------------------------------------

# Objeto que tenha a proporção de alunos em escola pública e privada
# 1. Por UF
# 2. Por capital
# 3. Objeto com quantidade e proporção de escolas públicas e privadas
# 4. Recodificar a variável sexo; 0 = homem; 1 = mulher
# Fazer tudo ponderado pelo peso

# OBS: count(var) é um atalho para group_by(var) %>% summarise(n = n())"

## Ajustes iniciais -----------------------------------------------------------
# Dando uma olhada na base (sempre é bom fazer isso)
# Repare que as vars de interesse não são numpericas, mas characters
glimpse(pnad_19)

# Variáveis categóricas vieram todas como character, mudando para numérica
pnad_19 <- pnad_19 %>%
  mutate(across(c(UF, Capital, V2007), ~ as.numeric(.)))

# EXTRA: Renomeando as variáveis com tidyverse e depois trasnformar em factor
pnad_19 <- pnad_19  %>%
  mutate(
    # Ajustanto nomes da UF
    UF = case_when(
      UF == 11 ~ "RO",
      UF == 12 ~ "AC",
      UF == 13 ~ "AM",
      UF == 14 ~ "RR",
      UF == 15 ~ "PA",
      UF == 16 ~ "AP",
      UF == 17 ~ "TO",
      UF == 21 ~ "MA",
      UF == 22 ~ "PI",
      UF == 23 ~ "CE",
      UF == 24 ~ "RN",
      UF == 25 ~ "PB",
      UF == 26 ~ "PE",
      UF == 27 ~ "AL",
      UF == 28 ~ "SE",
      UF == 29 ~ "BA",
      UF == 31 ~ "MG",
      UF == 32 ~ "ES",
      UF == 33 ~ "RJ",
      UF == 35 ~ "SP",
      UF == 41 ~ "PR",
      UF == 42 ~ "SC",
      UF == 43 ~ "RS",
      UF == 50 ~ "MS",
      UF == 51 ~ "MT",
      UF == 52 ~ "GO",
      UF == 53 ~ "DF"
    ),
    # lembrando de transformar em factor
    UF = factor(UF),
    
    # Ajustando nomes das capitais
    Capital = case_when(
      Capital == 11 ~ "Porto Velho",
      Capital == 12 ~ "Rio Branco",
      Capital == 13 ~ "Manaus",
      Capital == 14 ~ "Boa Vista",
      Capital == 15 ~ "Belém",
      Capital == 16 ~ "Macapá",
      Capital == 17 ~ "Palmas",
      Capital == 21 ~ "São Luís",
      Capital == 22 ~ "Teresina",
      Capital == 23 ~ "Fortaleza",
      Capital == 24 ~ "Natal",
      Capital == 25 ~ "João Pessoa",
      Capital == 26 ~ "Recife",
      Capital == 27 ~ "Maceió",
      Capital == 28 ~ "Aracaju",
      Capital == 29 ~ "Salvador",
      Capital == 31 ~ "Belo Horizonte",
      Capital == 32 ~ "Vitória",
      Capital == 33 ~ "Rio de Janeiro",
      Capital == 35 ~ "São Paulo",
      Capital == 41 ~ "Curitiba",
      Capital == 42 ~ "Florianópolis",
      Capital == 43 ~ "Porto Alegre",
      Capital == 50 ~ "Campo Grande",
      Capital == 51 ~ "Cuiabá",
      Capital == 52 ~ "Goiânia",
      Capital == 53 ~ "Brasília"
    ),
    # lembrando de transformar em factor
    Capital = factor(Capital),
    
    rede = if_else(V3002A == 1, "Privada", "Publica"),
    # lembrando de transformar em factor
    rede = factor(rede),
    
    # Transformando sexo
    sexo = if_else(V2007 == 1, "Homem", "Mulher"),
    # lembrando de transformar em factor
    sexo = factor(sexo)
  )

# Verificando mudanças com tabyl, arredondando casas decimais em 3 dígitos
tabyl(pnad_19, UF) %>% adorn_rounding(3)
tabyl(pnad_19, Capital) %>% adorn_rounding(3)
tabyl(pnad_19, rede) %>% adorn_rounding(3)
tabyl(pnad_19, sexo) %>% adorn_rounding(3)

## 1. UF ----------------------------------------------------------------------
# Proporção de rede por UF
prop_UF <- pnad_19 %>%
  # removendo NAs em UF e rede
  drop_na(UF, rede) %>%
  # Contando o número de casos de UF e rede, usando peso (wt)
  count(rede, UF, wt = V1032) %>%
  # Preparando para fazer os cálculos dentro das UF
  group_by(UF) %>%
  # Proporção de casos (dentro de cada UF)
  mutate(prop = n / sum(n)) %>%
  # Trasnformando em wider (tabela) os resultados
  # Deixando a coluna 'n' de fora, pois quero só proporção
  # Os nomes das colunas serão as categorias de rede
  # O valores das colunas serão suas respectivas proporções
  pivot_wider(-n, names_from = rede, values_from = prop) %>%
  # Arredondando para 3 casas decimais
  adorn_rounding(3) %>%
  # Tirando qualquer agrupamento do objeto
  ungroup()

# EXTRA: Proporção e n por UF
propn_UF <- pnad_19 %>%
  # removendo NAs em UF e rede
  drop_na(UF, rede) %>%
  # Contando o número de casos de UF e rede, usando peso (wt)
  count(rede, UF, wt = V1032) %>%
  # Preparando para fazer os cálculos dentro das UF
  group_by(UF) %>%
  # Proporção de casos (dentro de cada UF)
  mutate(prop = n / sum(n)) %>%
  # Trasnformando em wider (tabela) os resultados
  # Os nomes das colunas serão as categorias de rede
  # O valores das colunas serão suas respectivas proporções e n
  pivot_wider(names_from = rede, values_from = c(n, prop)) %>%
  # Arredondando para valor inteiro os n
  mutate(across(c(n_Privada, n_Publica), round)) %>% 
  # Arredondando para 3 casas decimais as proporções
  mutate(across(c(prop_Privada, prop_Publica), ~ round(., 3))) %>% 
  # Tirando qualquer agrupamento do objeto
  ungroup()

# CURIOSIDADE: usando o pacote pollster para fazer tabela com pesos amostrais
# Veja como essa função torna mais direto o processo todo
pnad_19 %>%
  # removendo NAs em UF e rede
  drop_na(UF, rede) %>%
  # Veja como o cáculo é direto
  crosstab(UF, rede, weight = V1032, n = FALSE) %>% 
  # Ajustando para ser proporção (0-1), e não porcentagem (0-100)
  mutate(across(c(Privada, Publica), ~ . / 100))

# CURIOSIDADE: Criando uma tabela bonita de proporção e n por UF (pacote gt)
propn_UF %>%
  mutate(n_Publica = round(n_Publica),
         n_Privada = round(n_Privada)) %>%
  gt() %>%
  # Dando título
  tab_header("Proproção de alunos por rede") %>%
  # Criando uma "categoria maior" de coluna (privadas)
  tab_spanner(label = "Privada",
              columns = c(n_Privada, prop_Privada)) %>%
  # Criando uma "categoria maior" de coluna (públicas)
  tab_spanner(label = "Pública",
              columns = c(n_Publica, prop_Publica)) %>%
  # Renomenado colunas
  cols_label(
    n_Privada = "n",
    n_Publica = "n",
    prop_Privada = "%",
    prop_Publica = "%"
  ) %>%
  #Ajustando largura da tabela
  tab_options(table.width = 400) %>%
  # Colocando os nomes da coluna em negrito
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) %>%
  # Alinhando "categoria maior" à direita e deixando em negrito
  tab_style(style = list(cell_text(align  = "right"),
                         cell_text(weight = "bold")),
            locations = cells_column_spanners()) %>%
  # Colocando título em negrito
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_title()) %>% 
  # Colocando vírgulas, pontos e ajustando casas decimais
  fmt_number(
    columns = c(2, 3),
    sep_mark = ".",
    dec_mark = "," ,
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(4, 5),
    sep_mark = ".",
    dec_mark = "," ,
    decimals = 3
  )


## 2. Proporção de alunos por capital -----------------------------------------
# Proporção de rede por capital
prop_Cap <- pnad_19 %>%
  # removendo NAs em Capital e rede
  drop_na(Capital, rede) %>%
  # Contando o número de casos de Capital e rede, usando peso (wt)
  count(rede, Capital, wt = V1032) %>%
  # Preparando para fazer os cáculos dentro das capitais
  group_by(Capital) %>%
  # Proporção de casos (dentro das capitais)
  mutate(prop = n / sum(n)) %>%
  # Trasnformando em wider (tabela) os resultados
  # Deixando a coluna 'n' de fora, pois quero só proporção
  # Os nomes das colunas serão as categorias de rede
  # O valores das colunas serão suas respectivas proporções
  pivot_wider(-n, names_from = rede, values_from = prop) %>%
  # Arredondando para 2 casas decimais
  adorn_rounding(3) %>%
  # Tirando qualquer agrupamento do objeto
  ungroup()

# EXTRA: Proporção e n por capital
propn_Cap <- pnad_19 %>%
  # removendo NAs em Capital e rede
  drop_na(Capital, rede) %>%
  # Contando o número de casos de Capital e rede, usando peso (wt)
  count(rede, Capital, wt = V1032) %>%
  # Preparando para fazer os cáculos dentro das capitais
  group_by(Capital) %>%
  # Proporção de casos (dentro das capitais)
  mutate(prop = n / sum(n)) %>%
  # Trasnformando em wider (tabela) os resultados
  # Os nomes das colunas serão as categorias de rede
  # O valores das colunas serão suas respectivas proporções
  pivot_wider(names_from = rede, values_from = c(n, prop)) %>%
  # Arredondando para valor inteiro os n
  mutate(across(c(n_Privada, n_Publica), round)) %>% 
  # Arredondando para 3 casas decimais as proporções
  mutate(across(c(prop_Privada, prop_Publica), ~ round(., 3))) %>% 
  # Tirando qualquer agrupamento do objeto
  ungroup()

# CURIOSIDADE: usando o pacote pollster para fazer tabela com pesos amostrais
# Veja como essa função torna mais direto o processo todo
pnad_19 %>%
  # removendo NAs em UF e rede
  drop_na(Capital, rede) %>%
  # Veja como o cáculo é direto
  crosstab(Capital, rede, weight = V1032, n = FALSE) %>% 
  # Ajustando para ser proporção (0-1), e não porcentagem (0-100)
  mutate(across(c(Privada, Publica), ~ . / 100))

# CURIOSIDADE: Criando uma tabela bonita de proporção e n de cada capital (gt)
propn_Cap %>%
  gt() %>%
  # Dando título
  tab_header("Proproção de alunos por rede") %>%
  # Criando uma "categoria maior" de coluna (privadas)
  tab_spanner(label = "Privada",
              columns = c(n_Privada, prop_Privada)) %>%
  # Criando uma "categoria maior" de coluna (públicas)
  tab_spanner(label = "Pública",
              columns = c(n_Publica, prop_Publica)) %>%
  # Renomenado colunas
  cols_label(
    n_Privada = "n",
    n_Publica = "n",
    prop_Privada = "%",
    prop_Publica = "%"
  ) %>%
  #Ajustando largura da tabela
  tab_options(table.width = 450) %>%
  # Colocando os nomes da coluna em negrito
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) %>%
  # Alinhando "categoria maior" à direita e deixando em negrito
  tab_style(style = list(cell_text(align  = "right"),
                         cell_text(weight = "bold")),
            locations = cells_column_spanners()) %>%
  # Colocando título em negrito
  tab_style(style = cell_text(weight = "bold"), locations = cells_title()) %>%
  # Alinhando nomes da primeira coluna (capitais)
  tab_style(style = cell_text(align  = "left"), locations = cells_body(1)) %>%
  # Alinhando o título da coluna (Capital)
  tab_style(style = cell_text(align  = "left"),
            locations = cells_column_labels(1)) %>%
  # Colocando vírgulas, pontos e ajustando casas decimais
  fmt_number(
    columns = c(2, 3),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(4, 5),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 3
  )


## 3. Quantidade e proporção de escolas públicas e privadas -------------------
rede_prop <- pnad_19 %>% 
  # Tirando NAs de rede
  drop_na(rede) %>% 
  # Usando count que equivale ao group_by(var) + summarise(n())
  count(rede, wt = V1032) %>% 
  # Arredondando o n, já que o cálculo amostral não é inteiro
  adorn_rounding(0) %>% 
  # Calculando propoção
  mutate(prop = n / sum(n)) %>% 
  # Arredondando proporção para ter apenas 3 dígitos decimais
  mutate(prop = round(prop, 3))


## 4. Recodificar a variável sexo ---------------------------------------------
pnad_19 <- pnad_19 %>% 
  # Criando nova variável binária chamada sexo_bin
  mutate(sexo_bin = if_else(sexo == "Mulher", 1, 0))

# Verificando mudanças
pnad_19 %>% tabyl(sexo_bin) %>% adorn_rounding(3)















