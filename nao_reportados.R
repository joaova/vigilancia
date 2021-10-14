library(tidyverse)
library(stringi)
library(fuzzyjoin)
library(stringdist)

######## Importanção ############
esus <- read_csv2("R/esus.csv", col_types = cols(.default = "c"))
#gal <- read_csv2("R/data.csv", col_types = cols(.default = "c"), col_select = c(2:4,18))
gal <- read.csv2("R/data.csv", colClasses="character", fileEncoding = "ISO-8859-1")
gal <- as_tibble(gal)

####### Manipulação dos dados ###########
mod_gal <- gal %>%
  select(nome = Paciente, cns = CNS, cpf = CPF, data_cadastro = Dt..Cadastro) %>%
  mutate(nome = gsub(pattern = "[^a-zA-Z ]", "", nome)) %>%
  mutate(nome = stri_trans_general(toupper(str_squish(str_trim(nome))), "Latin-ASCII"))

mod_esus <- esus %>%
  select(n_notificação = `Número da Notificação`, cpf = CPF, nome = `Nome Completo`, 
         data_notificação = `Data da Notificação`, evolução = `Evolução Caso`,
         classificação_final = `Classificação Final`) %>%
  mutate(across(everything(), ~replace_na(.x, "Não preenchido"))) %>%
  mutate(nome = gsub(pattern = "[^a-zA-Z ]", "", nome)) %>%
  mutate(cpf = gsub(pattern="\\D", "", cpf), nome = stri_trans_general(toupper(str_squish(str_trim(nome))), "Latin-ASCII")) %>%
  filter(evolução != "Cancelado") %>%
  arrange(nome)

########### Análise #############

# Paciente do GAL com CPF cadastrado
gal_com_cpf <- mod_gal %>%
  filter(cpf != "")

# Pacientes do GAL sem CPF
gal_sem_cpf <- mod_gal %>%
  filter(cpf == "")

# Pacientes do GAL que não foram cadastrados no esus (procurando pelo CPF)
gal_cpf_sem_esus <- gal_com_cpf %>%
  filter(!(cpf %in% mod_esus$cpf))

# Pacientes do GAL que não foram cadastrados no esus, daqueles sem CPF
gal_nome_sem_esus <- gal_sem_cpf %>%
  filter(!(nome %in% mod_esus$nome))

# Falta analisar aqueles que deveriam ser cadastrados mais uma vez
# Eles tiram o paciente do GAL?

# Total de pacientes não cadastrados, ou cadastrados de forma errada
gal_sem_esus <- rbind(gal_cpf_sem_esus, gal_nome_sem_esus) %>%
  mutate(data_cadastro = as.Date(data_cadastro, format="%d/%m/%Y")) %>%
  arrange(data_cadastro)

gal_esus_nome <- mod_gal %>%
  arrange(nome) %>%
  mutate(id = row_number()) %>%
  left_join(mod_esus, by="nome", suffix=c("_gal", "_esus")) %>%
  mutate(across(.cols = c("data_cadastro", "data_notificação"), as.Date, format="%d/%m/%Y")) %>%
  arrange(nome, data_cadastro, data_notificação) %>%
  filter(!is.na(data_cadastro)) %>%
  relocate(data_notificação, .after = data_cadastro) %>%
  relocate(id, .before = nome)

  
  
