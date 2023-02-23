# Iniciando -------------------------------------------------------------------

# Limpando area de trabalho antes de iniciar
rm(list = ls())

# Retirando notacao cientifica
options(scipen = 999)

# Carregando pacotes, necessário dar install.packages("pacman")
# O pacote 'pacman' faz o trabalho do 'install.packages' e do 'library'
pacman::p_load(tidyverse, janitor, rio, PNADcIBGE, gt, pollster)

# Diretório de trabalho
setwd("C:/Users/Homedesk/OneDrive/PC-RX/PPGSA/Doutorado/NIED")

# Importando usando o pacote do IBGE (offline)
pnad_19 <- read_pnadc("PNADC_2019_visita1.txt",
                      "input_PNADC_2019_visita1_20220224.txt")

# Importando o arquivo com o nome das capitais ( download na pasta do Drive)
capitais <- import("nome.capital.xlsx")

# Ajustes iniciais ------------------------------------------------------------
# Olhando a base
glimpse(pnad_19)

# Variáveis categóricas vieram todas como character
# Tranformando todas as variáveis categóricas em númericas
pnad_19 <- pnad_19 %>%
  mutate(across(where(is.character), ~ as.numeric(.)))

# Criando base só com pais e só filhos ----------------------------------------
# Var V2005:
# 1	- Pessoa responsável pelo domicílio 
# 2	- Cônjuge ou companheiro(a) de sexo diferente
# 3	- Cônjuge ou companheiro(a) do mesmo sexo
# 4 - Filho(a) do responsável e do cônjuge 
# 5 - Filho(a) somente do responsável

# Base de filhos
filhos <- pnad_19 %>% 
  filter(V2005 %in% 4:5)

# Base de pais
pais <- pnad_19 %>% 
  filter(V2005 %in% 1:3)

# Removendo a base principal para liberar memória
rm(pnad_19)
gc()

# Ajustando a base de pais ----------------------------------------------------

# Criando variável último_curso com apenas 3 opções:
# 0 = Menos que ensino médio
# 1 = Ensino médio, 
# 2 = Ensino superior ou mais
pais <- pais %>%
  mutate(
    ultimo_curso = case_when(
      V3009A %in% c(2:5, 7, 8) ~ 0,
      V3009A %in% c(6, 9, 10, 11) ~ 1,
      V3009A %in% c(12:15) ~ 2
    )
  )

# Verificando mudanças
pais %>% 
  count(ultimo_curso)

#  Criando variável de escolaridade
pais <- pais %>% 
  mutate(
    escolaridade = case_when(
      # Se: a pessoa concluiu o último curso que ela frequentou
      # Então: a escolaridade dela é o último curso
      V3014 == 1 ~ ultimo_curso,
      # Se: a pessoa NÂO concluiu o último curso e não tem a menor escolaridade
      # Então: a escolaridade é o nível anterior 
      V3014 == 2 & ultimo_curso != 0 ~ ultimo_curso - 1,
      # Se: a pessoa NÂO concluiu o último curso e tem a menor escolaridade
      # Então: a escolaridade é o último curso pq não tem como ser menor
      V3014 == 2 & ultimo_curso == 0 ~ ultimo_curso,
      
    )
  )

#  Criando variável de maior escolaridade
pais <- pais %>% 
  # Fazendo as operações dentro do domicílio
  group_by(ID_DOMICILIO) %>% 
  # Como valores maiores indicam escolaridade maior, basta ver o máximo
  mutate(maior_escolaridade = max(escolaridade)) %>% 
  ungroup()

# Selecionando apenas as variáveis de interesse 
maior_escol <- pais %>% 
  select(ID_DOMICILIO, maior_escolaridade) %>% 
  # Unique serve para remover linhas iguais. Deve sobrar 1 obs por domicílio
  unique()

# Verificando se cada domicílio tem no máximo 1 observação (TEM!)
maior_escol %>% 
  # Contando quantas vezes cada domicílio aparece
  count(ID_DOMICILIO) %>% 
  # Pegando apenas a variável n
  pull(n) %>% 
  # Retirando o máximo dessa variável
  max()

# levando infos para base dos filhos ------------------------------------------
filhos <- left_join(filhos, maior_escol)

# Vendo os casos na base de filhos
filhos %>% 
  count(maior_escolaridade, wt = V1032)


# DESAFIO ---------------------------------------------------------------------
# Querendo descobrir sexo e posição da família
# Teremos 2 casos, um que apenas um dos pais têm a maior escolaridade
# E casos em que os dois terão a maior escolaridade
# Desse modo, vou recodificar a variável sexo e posição em uma só:
# referencia: 1- Pai, 2- Mãe, 3- Pai e Mãe, 4- Dois Pais, 5- Duas Mães

# Selecionando apenas as vars de interesse
maior_escol_2 <- pais %>% 
  select(ID_DOMICILIO, V2007, V2005, escolaridade, maior_escolaridade)

# Limpando base dos pais para liberar memória
rm(pais)
gc()

# Criando objeto de controle informa quantas observações tem o domicílio
ctrl <- maior_escol_2 %>% 
  # Contando quantas vezes cada domicílio aparece
  count(ID_DOMICILIO)

# Olhando objeto
view(ctrl)

# V2007:
# 1 - Homen
# 2 - Mulher

#V2005:
# 1	- Pessoa responsável pelo domicílio 
# 2	- Cônjuge ou companheiro(a) de sexo diferente
# 3	- Cônjuge ou companheiro(a) do mesmo sexo

# Levando essa informações para o objeto (var n adicionada)
maior_escol_2 <- left_join(maior_escol_2, ctrl)

# Olhando a base
view(maior_escol_2)

# Liberando espaço
rm(ctrl)
gc()

# Recodificando as variáveis
maior_escol_2 <- maior_escol_2 %>%
  mutate(
    referencia = case_when(
      # Domicilio com somente uma pessoa e é homem (Pai)
      n == 1 & V2007 == 1 ~ 1,
      # Domicilio com somente uma pessoa e é mulher (Mãe)
      n == 1 & V2007 == 2 ~ 2,
      # Domicilio com duas pessoas e é conjuge de outro sexo (Pai e Mãe)
      n == 2 & V2005 == 2 ~ 3,
      # Domicilio com pessoas, é um homem e conjuge de mesmo sexo (Dois Pais)
      n == 2 & V2007 == 1 & V2005 == 3 ~ 4,
      # Domicilio com duas pessoas, é mulher e conjuge de mesmo sexo (Duas Mães)
      n == 2 & V2007 == 2 & V2005 == 3 ~ 5
    )
  )

# Selecionando apenas as vars de interesse
maior_escol_2 <- maior_escol_2 %>% 
  select(ID_DOMICILIO, maior_escolaridade, referencia)

# Vale notar que deixei de fora os casos que tinha pessoa de referência quando
# em domicilios com duas pessoas quando recodifiquei ali em cima. Por causa 
# disso, vai aparecer NA na variável nessas linhas, tendo apenas a informações 
# dos cônjuges. Mas isso vai me ajudar!
view(maior_escol_2)

# Vou tirar os NA da variável referencia, pois o que me interessa é o domicílio
maior_escol_2 <- maior_escol_2 %>% 
  drop_na(referencia)

# Temos 123.613 observações agora, justamente o número de domicílios

# No testa apenas dar um join
filhos <- left_join(filhos, maior_escol_2)





















