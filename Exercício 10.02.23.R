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

# Importando o arquivo com o nome das capitais ( download na pasta do Drive)
capitais <- import("nome.capital.xlsx")

# Execício --------------------------------------------------------------------
# 1. Trazer os nomes de capital para o objeto capital_pivot usando uma das 
#    funções join
# 2. Criar objeto só de quem é filho no domicílio a partir da variável V2005
# 3. Criar objeto (nome = pais) de quem é CHEFE ou CÔNJUGE no domicílio
# 4. Combinar variável de posição no domicílio com sexo e criar variável 
#    chamada "pais" onde 1=pai e 2=mãe
# 5. Criar variável "escolaridade", 0=Menos que ensino médio, 1=Ensino médio, 
#    2= Ensino superior ou mais
# 6. Juntar os objetos de pais e filhos via join
# 7. Usando a função pivot wider, criar colunas de escolaridade do pai e 
#    escolaridade da mãe

# Ajustes iniciais ------------------------------------------------------------
# Olhando a base
glimpse(pnad_19)

# Variáveis categóricas vieram todas como character
# Tranformando todas as variáveis categóricas em númericas
pnad_19 <- pnad_19 %>%
  mutate(across(where(is.character), ~ as.numeric(.)))

# Renomenado as categorias de rede
pnad_19 <- pnad_19  %>%
  mutate(rede = if_else(V3002A == 1, "Privada", "Publica"),
         # lembrando de transformar em factor
         rede = factor(rede))

# Exercício 1 -----------------------------------------------------------------
# O objeto capital_pivot, acredito que se refere ao do exercício anterior:
# Proporção de alunos em escola pública e privada por capital

# Recriando capital_pivot
# Proporção de aluno, rede por capital
capital_pivot <- pnad_19 %>%
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

# Olhando o objeto criado
print(capital_pivot, n = Inf)

# Trazendo o nome das capitais para o objeto
capital_pivot <- left_join(capital_pivot, capitais)

# Confirindo o objeto criado após inclusão dos nomes
print(capital_pivot, n = Inf)

# Exercício 2 -----------------------------------------------------------------
# Verificar primeiro como está essa variável
pnad_19 %>% 
  count(V2005)

# É necessário ir no dicionário de dados e descobrir qual é a categoria de filho
# Existem duas categorias de filhos, como não especificou, usarei as duas:
# 4 - Filho(a) do responsável e do cônjuge 
# 5 - Filho(a) somente do responsável

# Criando uma base só com os filhos
filhos <- pnad_19 %>% 
  filter(V2005 %in% 4:5)

# Exercício 3 -----------------------------------------------------------------
# É a mesma variável V2005, só que agora:
# 01	- Pessoa responsável pelo domicílio 
# 02	- Cônjuge ou companheiro(a) de sexo diferente
# 03	- Cônjuge ou companheiro(a) do mesmo sexo

# Criando uma base só com os pais
pais <- pnad_19 %>% 
  filter(V2005 %in% 1:3)

# Exercício 4 -----------------------------------------------------------------
# Olhando no dicionário, a variável de sexo é V2007, sendo que:
# 1 - Homen
# 2 - Mulher

# Verificar primeiro como está essa variável na base só de pais
pais %>% 
  count(V2007)

# Criando a variável no objeto só com pais
# 1 = pai e 2 = mãe
# Como só tem 2 categorias, vou usar 'if_else' ao invés de 'case_when"
pais <- pais %>% 
  mutate(pais = if_else(V2007 == 1, 1, 2),
         pais = factor(pais, labels = c("Pai", "Mãe")))

# Verificar nova variável com os labels de factor
pais %>% 
  count(pais)

# É possível ver os números das categorias ao invés
pais %>% 
  count(pais = as.numeric(pais))

# Exercício 5 -----------------------------------------------------------------
# Imagino que a criação de escolaridade seja apenas para os pais
# Criar essa variável é mais complicada que parece!

# No dicionário nós temos a variável V3009A:
# "Qual foi o curso mais elevado que frequentou anteriormente?"

# Mas nem todo mundo que frequentou o curso concluiu!
# A variável V3014 nos indica se a pessoa concluiu ou não:
# "Concluiu este curso que frequentou anteriormente"

# Vendo a var de "último curso que frequentou"
pais %>% 
  count(V3009A)

# Categorias de V3009A no dicionário
# 02 - Pré-escola
# 03 - Classe de alfabetização - CA
# 04 - Alfabetização de jovens e adultos
# 05 - Antigo primário (elementar)
# 06 - Antigo ginásio (médio 1º ciclo)
# 07 - Regular do ensino fundamental ou do 1º grau
# 08 - Educação de jovens e adultos (EJA) ou supletivo do 1º grau
# 09 - Antigo científico, clássico, etc. (médio 2º ciclo)
# 10 - Regular do ensino médio ou do 2º grau
# 11 - Educação de jovens e adultos (EJA) ou supletivo do 2º grau
# 12 - Superior - graduação
# 13 - Especialização de nível superior
# 14 - Mestrado
# 15 - Doutorado
# Não aplicável

# Vendo a var de "concluiu esse curso?"
pais %>% 
  count(V3014)

# Categorias de V3014 no dicionário
# 1 -	Sim
# 2	- Não 
# Não aplicável

# Recodificar a var V3009A, criarei uma nova chamada de ultimo_curso
pais <- pais %>%
  mutate(
    ultimo_curso = case_when(
      V3009A %in% c(2:5, 7, 8) ~ 0,
      V3009A %in% c(6, 9, 10, 11) ~ 1,
      V3009A %in% c(12:15) ~ 2
    )
  )
      
# Verificando a var até o momento, o NA aqui tem que ser o mesmo de V3009A!
# 23588 NAs
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
  
# Verificando a variável escolaridade
# O NA da var tem que ser o mesmo da var usada com maior NA: V3014 ou V3009A
# A var V3014 tem a maior: 31220
pais %>% 
  count(escolaridade)

# Uma vez acabada a edição da variável, podemos transformar em factor e usar 
# os labels 
pais <- pais %>%
  mutate(escolaridade = factor(
    escolaridade,
    labels = c("Menos que ensino médio",
               "Ensino médio",
               "Ensino superior ou mais")
  ))
      
# Verificando a variável escolaridade
pais %>% 
  count(escolaridade)

# Exercício 6 -----------------------------------------------------------------
# Juntandos as bases só de filhos e pais
pais_e_filhos <- full_join(pais, filhos)

# Exercício 7 -----------------------------------------------------------------
# Suspeito que esteja sendo pedido para calcular proporções como feito no
# exercício da aula passada. É pedido colunas de escolaridade, mas acredito que
# o inverso acaba sendo melhor, devido ao número menor de categorias em pais.
# Também não é especificado onde deve ser a proporção: coluna, linha ou tudo.
# Fiz todas as 3 possibilidades e usei o peso.

# Gerando tabela: total das colunas (pais)
pais_e_filhos %>% 
  drop_na(pais, escolaridade) %>% 
  count(pais, escolaridade, wt = V1032) %>% 
  group_by(pais) %>% 
  mutate(prop =  n / sum(n)) %>% 
  pivot_wider(-n, names_from = pais, values_from = prop) %>% 
  adorn_totals("row") %>% 
  adorn_rounding(2)

# Gerando tabela: total das linhas (escolaridade)
pais_e_filhos %>% 
  drop_na(pais, escolaridade) %>% 
  count(pais, escolaridade, wt = V1032) %>% 
  group_by(escolaridade) %>% 
  mutate(prop =  n / sum(n)) %>% 
  pivot_wider(-n, names_from = pais, values_from = prop) %>% 
  adorn_totals("col") %>% 
  adorn_rounding(2)

# Gerando tabela: total em tudo
pais_e_filhos %>% 
  drop_na(pais, escolaridade) %>% 
  count(pais, escolaridade, wt = V1032) %>% 
  mutate(prop =  n / sum(n)) %>% 
  pivot_wider(-n, names_from = pais, values_from = prop) %>% 
  adorn_totals(c("col", "row")) %>% 
  adorn_rounding(2)





