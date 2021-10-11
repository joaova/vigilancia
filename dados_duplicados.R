library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

esus <- read_csv2("R/esus.csv", col_types = cols(.default = "c"))
esus_confirmados <- read_csv2("R/confirmados.csv", col_types = cols(.default = "c"))

esus_filtered <- esus %>%
  select(n_notificação = `Número da Notificação`, cpf = CPF, nome_completo = `Nome Completo`, 
         nome_mae = `Nome Completo da Mãe`, contains("Resultado"), 
         data_notificação = `Data da Notificação`, evolução = `Evolução Caso`,
         cnes = `CNES Notificação`, classificação_final = `Classificação Final`) %>%
  
  mutate(across(everything(), ~replace_na(.x, "Não preenchido")))

confirmados <- esus_filtered %>%
  filter(evolução != "Cancelado") %>%
  # preciso verificar quais tem resultado detectavel ou reagente e inserir "Confirmado laboratorialmente"
  mutate(classificação_final = sapply(add_classification(classificação_final))) %>%
  filter(grepl("Confirmado.*$", classificação_final))


# Adiciona classificacao para os casos nao preenchidos, mas com teste laboratorial detectado
add_classification <- function(df) {
  ##TODO
    # Pegar cada item que contem resultado detectavel/reagente e alterar na confirmacao :)
  df %>%
    
}

# Verifica se notificação foi realizada em determinado tempo
is_duplicate <- function(df) {
  # Valores Ãºnicos de CPF
  dup_df <- unique(df$CPF[duplicated(df$CPF)])
  cpf_duplicado <- character()
  num_not <- numeric()
  cnes_n <- numeric()
  for (cpf in dup_df) {
    not_count <- 0
    count_cnes <- 0
    notificacoes_cpf <- sort(as.Date(df[df$CPF == cpf, "Data.da.Notificação"], "%d/%m/%Y"))
    time_diff <- diff(notificacoes_cpf)
    cnes <- summary.factor(df[df$CPF == cpf,"CNES.NotificaÃ§Ã£o"])
    # diferenciar dos que coletaram o teste em =======dias diferentes
    for (time in time_diff) {
      if (time <= 30) {
        not_count <- not_count + 1
      }
    }
    for (num in cnes) {
      if(num > 1) {
        count_cnes <- count_cnes + 1
      }
    }
    if (not_count > 0) {
      cpf_duplicado <- c(cpf_duplicado, cpf)
      num_not <- c(num_not, not_count)
      if (count_cnes == 0) {
        count_cnes <- 1
      }
      cnes_n <- c(cnes_n, count_cnes)
    }
  }
  return(data.frame(cpf_duplicado,num_not,cnes_n, stringsAsFactors=FALSE))
}
