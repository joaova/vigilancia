library(tidyverse)
library(lubridate)
library(stringi)
library(openxlsx)

############### Preparando os arquivos ################

######## Importação ############
file_names <- paste0(c(1:length(dir("C:/Users/joaov/Documents/R/SITE", recursive=TRUE, pattern="*.zip"))), ".zip")

paste("C:/Users/joaov/Documents/R/SITE" , dir("C:/Users/joaov/Documents/R/SITE", recursive=TRUE, pattern="*.zip"), sep="/") %>%
  file.rename(file_names) 

# Unzip files (how to do this puurrrly?)
for (i in 1:length(file_names)) {
  unzip(file_names[i]) %>%
  file.rename(str_interp("data${i}.csv"))
}

# Create tibble
temp <- list.files(path = ".", pattern="data.*.csv")

# Generates a list of dataframes and convert each to tibble
gal <- lapply(temp, read.csv2, colClasses="character", fileEncoding = "ISO-8859-1") %>%
  map_df(as_tibble)

esus <- read_csv2("esus.csv", col_types = cols(.default = "c"))

# Remove files
sapply(paste0("data", 1:length(file_names), ".csv"), unlink)
sapply(paste0("", 1:length(file_names), ".zip"), unlink)
unlink("SITE", recursive=TRUE)
rm(temp, file_names, i)

####### Manipulação dos dados ###########
mod_gal <- gal %>%
  select(nome = Paciente, cpf = CPF, data_cadastro_gal = Dt..Cadastro, data_liberação_resultado_gal = Dt..Liberação,
         laboratorio_cadastro = Laboratório.Cadastro,laboratorio_executor = Laboratório.Executor,resultado_pcr = Resultado) %>%
  mutate(nome = gsub(pattern = "[^a-zA-Z ]", "", nome)) %>%
  mutate(nome = stri_trans_general(toupper(str_squish(str_trim(nome))), "Latin-ASCII")) %>%
  mutate(across(everything(), ~na_if(.x, ""))) %>%
  mutate(across(everything(), ~na_if(.x, " "))) %>%
  mutate(across(everything(), ~replace_na(.x, "Ignorado")))

mod_esus <- esus %>%
  select(n_notificação = `Número da Notificação`, cpf = CPF, nome = `Nome Completo`, 
         data_notificação_esus = `Data da Notificação`, resultado_pcr = `Resultado RT-PCR`,
         evolução = `Evolução Caso`, classificação_final = `Classificação Final`) %>%
  mutate(nome = gsub(pattern = "[^a-zA-Z ]", "", nome)) %>%
  mutate(cpf = gsub(pattern="\\D", "", cpf), nome = stri_trans_general(toupper(str_squish(str_trim(nome))), "Latin-ASCII")) %>%
  mutate(across(everything(), ~replace_na(.x, "Ignorado"))) %>%
  filter(evolução != "Cancelado") %>%
  select(-evolução) %>%
  arrange(nome)

########## Join ###############
# Faz um join entre gal e esus, além de organizar a tabela mudando as colunas de posição
gal_esus_nome <- mod_gal %>%
  arrange(nome) %>%
  mutate(id_gal = row_number()) %>%
  left_join(mod_esus, by="nome", suffix=c("_gal", "_esus")) %>%
  mutate(across(.cols = c("data_cadastro_gal", "data_notificação_esus", "data_liberação_resultado_gal"), as.Date, format="%d/%m/%Y")) %>%
  arrange(nome, data_cadastro_gal, data_notificação_esus) %>%
  filter(!is.na(data_cadastro_gal), !is.na(data_notificação_esus), !is.na(data_liberação_resultado_gal)) %>%
  relocate(data_notificação_esus, .after = data_cadastro_gal) %>%
  relocate(id_gal, .before = nome) %>%
  relocate(n_notificação, .after = id_gal) %>%
  relocate(resultado_pcr_esus, .after = resultado_pcr_gal) %>%
  relocate(cpf_esus, .after = cpf_gal) %>%
  filter(resultado_pcr_esus == "Ignorado") 

####### Cria planilha ########
# Cria worbook excel
wb <- createWorkbook()

# Divide as paginas pelo numero de bolsistas
for (i in 1:6) {
  part <- round(nrow(gal_esus_nome) / 6)
  addWorksheet(wb, sheetName=i)
  df <- gal_esus_nome[max((i-1)*part+1,1):min(i*part, nrow(gal_esus_nome)),]
  writeData(wb, sheet=i, x=df)
}

# Salva em arquivo xlsx
saveWorkbook(wb, str_interp("revisar-resultados-${format(today(), '%d-%m-%Y')}.xlsx"), overwrite = TRUE)
rm(i,part,wb,df)