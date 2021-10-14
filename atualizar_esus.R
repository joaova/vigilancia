library(tidyverse)
library(stringi)
library(openxlsx)

############### Preparando os arquivos ################


######## Importação ############
esus <- read_csv2("R/esus.csv", col_types = cols(.default = "c"))
#gal <- read_csv2("R/data.csv", col_types = cols(.default = "c"), col_select = c(2:4,18))

gal1 <- read.csv2("R/data1.csv", colClasses="character", fileEncoding = "ISO-8859-1") %>%
  as_tibble()
gal2 <- read.csv2("R/data2.csv", colClasses="character", fileEncoding = "ISO-8859-1") %>%
  as_tibble()
gal3 <- read.csv2("R/data3.csv", colClasses="character", fileEncoding = "ISO-8859-1") %>%
  as_tibble()
gal4 <- read.csv2("R/data4.csv", colClasses="character", fileEncoding = "ISO-8859-1") %>%
  as_tibble()
gal5 <- read.csv2("R/data5.csv", colClasses="character", fileEncoding = "ISO-8859-1") %>%
  as_tibble()

gal <- rbind(gal1,gal2,gal3,gal4,gal5)



####### Manipulação dos dados ###########
mod_gal <- gal %>%
  select(nome = Paciente, cpf = CPF, data_cadastro_gal = Dt..Cadastro, data_liberação_resultado_gal = Dt..Liberação,
         resultado_pcr = Resultado) %>%
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
saveWorkbook(wb, "R/revisar-resultados.xlsx", overwrite = TRUE)