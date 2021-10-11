library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)

esus <- read_csv2("R/esus.csv", col_types = cols(.default = "c"))
esus_confirmados <- read_csv2("R/confirmados.csv", col_types = cols(.default = "c"))

# Filtra a df para as colunas que gostaria de usar,além de substituir os dados NA para "Não preenchido"
esus_filtered <- esus %>%
  select(n_notificação = `Número da Notificação`, cpf = CPF, nome_completo = `Nome Completo`, 
         nome_mae = `Nome Completo da Mãe`, contains("Resultado"), 
         data_notificação = `Data da Notificação`, evolução = `Evolução Caso`,
         cnes = `CNES Notificação`, classificação_final = `Classificação Final`) %>%
  mutate(across(everything(), ~replace_na(.x, "Não preenchido")))

############### Avalia as classificações não preenchidas, porém onde há testagem positiva ######################

# Separa os casos em que a classificação final não foi preenchida
classificacao_ignorada <- esus_filtered %>%
  filter(classificação_final == "Não preenchido")


# Substitui detectável/confirmado por TRUE e o oposto para FALSE
testes_positivos <- classificacao_ignorada %>%
  select(contains("Resultado")) %>%
  mutate(across(.cols = everything(), function(x) (x == "Detectável" | x == "Reagente"))) 

#Junta as colunas dos testes em uma só, sendo TRUE se qualquer teste for positivo, ou FALSE caso nenhum 
merge_bool_columns <- function(df) {
  # Calcula numero de linhas da DF
  len <- nrow(df)
  # Um vetor de booleanos, que representará cada linha da df
  bool_vec <- c() 
  # Um vetor que irá representar a coluna do resultado final
  col_vec <- c()
  
  # Uso o primeiro for para ir de 1 até o número total de linhas
  for (num in 1:len) {
    # "valor" representará cada valor de uma linha na DF
    for (valor in df[num,]) {
      bool_vec <- c(bool_vec, valor)
    }
    # Ao somar todos os valores booleanos no vetor bool_vec, saberemos se existe algum valor TRUE em cada linha da dataframe
    ## Vale lembrar que TRUE = 1 e FALSE = 0. Dessa forma, qualquer soma > 0 corresponderá à TRUE
    col_vec <- c(col_vec, (sum(bool_vec) > 0))
    # Reseta o vetor bool_vec para estar vazio na próxima iteração do loop
    bool_vec <- c()
  }
  
  return(col_vec)
}

# Coluna com booleans que será usada para filtra a DF
filter_col <- merge_bool_columns(testes_positivos)

# Filtra com base no vetor acima e classifica esses pacientes como confirmados
confirmados_sem_classificação <- classificacao_ignorada %>%
  select(-contains("Resultado")) %>%
  filter(filter_col) %>%
  mutate(classificação_final = "Confirmado Laboratorial")

# Pega somente os casos confirmados, independente do tipo de confirmação
# Estou tirando as tabelas de resultado apenas para deixar mais limpo, 
  # mas elas podem ser úteis para análise depois
# IMPORTANTE LEMBRAR QUE O IGG ESTA INCLUÍDO NA TABELA, APESAR DE NÃO SE FAZER DIAG POR IGG
confirmados <- esus_filtered %>%
  select(-contains("Resultado")) %>%
  filter(grepl("Confirmado.*$", classificação_final), evolução != "Cancelado") %>%
  rbind(confirmados_sem_classificação)

############ Testes com gráfico ####################
first_col_values <- colnames(testes_positivos)
second_col_values <- unname(sapply(testes_positivos, sum))

testes_quant <- as_tibble_col(first_col_values) %>%
  cbind(second_col_values) %>%
  filter(!grepl("Resultado Outros.*|.*IgA", value)) %>%
  mutate(value = c("PCR", "LAMP", "sor_IgM", "sor_IgG", "sor_total", "rap_IgM", "rap_IgG", "antigeno"))

# Cria gráfico para teste
ggplot(testes_quant, aes(x=value, y=second_col_values)) +
  xlab("Testes") +
  ylab("Quantidade") +
  ggtitle("Testes positivos em pacientes não classificados") +
  geom_col()

####### Avalia duplicados ############

# Verifica se notificação foi realizada em determinado tempo
# Por enquanto funciona apenas para CPF (não leva em consideração nome)
is_duplicate <- function(df) {
  # Valores únicos de CPF's duplicados
  cpfs_duplicados <- unique(df$cpf[duplicated(df$cpf)])
  cpf_not_exedente <- character()
  not_excedentes <- numeric()
  for (valor in cpfs_duplicados) {
    not_count <- 0
    #notificacoes_cpf <- sort(as.Date(df[df$cpf == cpf, "data_notificação"], "%d/%m/%Y"))
    notificacoes_cpf <- df %>%
      filter(cpf == valor) %>%
      mutate(data_notificação = as.Date(data_notificação, "%d/%m/%Y")) %>%
      select(data_notificação)
    time_diff <- diff(pull(notificacoes_cpf))
    # diferenciar dos que coletaram o teste em =======dias diferentes
    for (time in time_diff) {
      if (time <= 30) {
        not_count <- not_count + 1
      }
    }
    if (not_count > 0) {
      cpf_not_exedente <- c(cpf_not_exedente, valor)
      not_excedentes <- c(not_excedentes, not_count)
    }
  }
  return(tibble(cpf_not_exedente,not_excedentes))
}

# Olha se existem duplicados nos confirmados
duplicados <- is_duplicate(confirmados) %>%
  arrange(desc(not_excedentes))
