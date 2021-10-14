library(tidyverse)

esus <- read_csv2("R/esus.csv", col_types = cols(.default = "c"))
esus_confirmados <- read_csv2("R/confirmados.csv", col_types = cols(.default = "c"))

# Filtra a df para as colunas que gostaria de usar,além de substituir os dados NA para "Não preenchido"
esus_filtered <- esus %>%
  select(n_notificação = `Número da Notificação`, cpf = CPF, nome_completo = `Nome Completo`, 
         nome_mae = `Nome Completo da Mãe`, contains("Resultado"), 
         data_notificação = `Data da Notificação`, evolução = `Evolução Caso`,
         cnes = `CNES Notificação`, classificação_final = `Classificação Final`) %>%
  mutate(across(everything(), ~replace_na(.x, "Não preenchido"))) %>%
 #  mutate(cpf = gsub(pattern="\\D", "", cpf), nome = stri_trans_general(toupper(str_squish(str_trim(nome))), "Latin-ASCII")) %>%
  mutate(cpf = gsub(pattern="\\D", "", cpf), nome_completo = toupper(nome_completo)) %>%
  mutate(data_notificação = as.Date(data_notificação, "%d/%m/%Y")) %>%
  filter(evolução != "Cancelado")

############### Avalia as classificações não preenchidas, porém onde há testagem positiva ######################

# Separa os casos em que a classificação final não foi preenchida
classificacao_ignorada <- esus_filtered %>%
  filter(classificação_final == "Não preenchido")


# Substitui detectável/confirmado por TRUE e o oposto para FALSE e avalia se existe QUALQUER teste positivo
  # em cada linha da dataframe. Retorna para "testes_positivos" um vetor booleano
testes_positivos <- classificacao_ignorada %>%
  select(contains("Resultado")) %>%
  mutate(across(.cols = everything(), function(x) (x == "Detectável" | x == "Reagente"))) %>%
  transmute(resultado_final = map_lgl(pmap_dbl(.,sum), ~(.x>0)))

# Filtra com base no vetor acima e classifica esses pacientes como confirmados
# Excluir o IgG???
confirmados_sem_classificação <- classificacao_ignorada %>%
  select(-contains("Resultado")) %>%
  filter(testes_positivos) %>%
  mutate(classificação_final = "Confirmado Laboratorial*")

# Pega somente os casos confirmados, independente do tipo de confirmação
# Estou tirando as tabelas de resultado apenas para deixar mais limpo, 
  # mas elas podem ser úteis para análise depois
# IMPORTANTE LEMBRAR QUE O IGG ESTA INCLUÍDO NA TABELA, APESAR DE NÃO SE FAZER DIAG POR IGG
confirmados <- esus_filtered %>%
  select(-contains("Resultado")) %>%
  filter(grepl("Confirmado.*$", classificação_final), evolução != "Cancelado") %>%
  rbind(confirmados_sem_classificação)

####### Avalia duplicados ############
# Lembrar que antes eu classifiquei paciente anteriormente não classificados
notificacoes_duplicadas <- confirmados %>%
  filter(cpf != "") %>%
  group_by(cpf) %>%
  filter(n()>1) %>%
  arrange(cpf, data_notificação) %>%
  summarize(intervalo_not = diff(data_notificação)) %>%
  filter(intervalo_not <= 30) %>%
  count(name = "notificações_excedentes") %>%
  ungroup()

not_dup_data <- confirmados %>%
  select(cpf, data_notificação) %>%
  filter(cpf %in% notificacoes_duplicadas$cpf) %>%
  arrange(cpf, data_notificação)

############ Testes com gráfico ####################
# TODO
# Não vai funcionar porque eu alterei o código acima
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