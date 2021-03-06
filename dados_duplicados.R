library(tidyverse)

esus <- read_csv2("esus.csv", col_types = cols(.default = "c"))

# Filtra a df para as colunas que gostaria de usar,al�m de substituir os dados NA para "N�o preenchido"
esus_filtered <- esus %>%
  select(n_notifica��o = `N�mero da Notifica��o`, cpf = CPF, nome_completo = `Nome Completo`, 
         nome_mae = `Nome Completo da M�e`, contains("Resultado"), 
         data_notifica��o = `Data da Notifica��o`, evolu��o = `Evolu��o Caso`,
         cnes = `CNES Notifica��o`, classifica��o_final = `Classifica��o Final`) %>%
  mutate(across(everything(), ~replace_na(.x, "N�o preenchido"))) %>%
 #  mutate(cpf = gsub(pattern="\\D", "", cpf), nome = stri_trans_general(toupper(str_squish(str_trim(nome))), "Latin-ASCII")) %>%
  mutate(cpf = gsub(pattern="\\D", "", cpf), nome_completo = toupper(nome_completo)) %>%
  mutate(data_notifica��o = as.Date(data_notifica��o, "%d/%m/%Y")) %>%
  filter(evolu��o != "Cancelado")

############### Avalia as classifica��es n�o preenchidas, por�m onde h� testagem positiva ######################

# Separa os casos em que a classifica��o final n�o foi preenchida
classificacao_ignorada <- esus_filtered %>%
  filter(classifica��o_final == "N�o preenchido")


# Substitui detect�vel/confirmado por TRUE e o oposto para FALSE e avalia se existe QUALQUER teste positivo
  # em cada linha da dataframe. Retorna para "testes_positivos" um vetor booleano
testes_positivos <- classificacao_ignorada %>%
  select(contains("Resultado")) %>%
  mutate(across(.cols = everything(), function(x) (x == "Detect�vel" | x == "Reagente"))) %>%
  transmute(resultado_final = map_lgl(pmap_dbl(.,sum), ~(.x>0)))

# Filtra com base no vetor acima e classifica esses pacientes como confirmados
# Excluir o IgG???
confirmados_sem_classifica��o <- classificacao_ignorada %>%
  select(-contains("Resultado")) %>%
  filter(testes_positivos) %>%
  mutate(classifica��o_final = "Confirmado Laboratorial*")

# Pega somente os casos confirmados, independente do tipo de confirma��o
# Estou tirando as tabelas de resultado apenas para deixar mais limpo, 
  # mas elas podem ser �teis para an�lise depois
# IMPORTANTE LEMBRAR QUE O IGG ESTA INCLU�DO NA TABELA, APESAR DE N�O SE FAZER DIAG POR IGG
confirmados <- esus_filtered %>%
  select(-contains("Resultado")) %>%
  filter(grepl("Confirmado.*$", classifica��o_final), evolu��o != "Cancelado") %>%
  rbind(confirmados_sem_classifica��o)

####### Avalia duplicados ############
# Lembrar que antes eu classifiquei paciente anteriormente n�o classificados
notificacoes_duplicadas <- confirmados %>%
  filter(cpf != "") %>%
  group_by(cpf) %>%
  filter(n()>1) %>%
  arrange(cpf, data_notifica��o) %>%
  summarize(intervalo_not = diff(data_notifica��o)) %>%
  filter(intervalo_not <= 30) %>%
  count(name = "notifica��es_excedentes") %>%
  ungroup()

not_dup_data <- confirmados %>%
  select(cpf, data_notifica��o) %>%
  filter(cpf %in% notificacoes_duplicadas$cpf) %>%
  arrange(cpf, data_notifica��o)

############ Testes com gr�fico ####################
# TODO
# N�o vai funcionar porque eu alterei o c�digo acima
first_col_values <- colnames(testes_positivos)
second_col_values <- unname(sapply(testes_positivos, sum))

testes_quant <- as_tibble_col(first_col_values) %>%
  cbind(second_col_values) %>%
  filter(!grepl("Resultado Outros.*|.*IgA", value)) %>%
  mutate(value = c("PCR", "LAMP", "sor_IgM", "sor_IgG", "sor_total", "rap_IgM", "rap_IgG", "antigeno"))

# Cria gr�fico para teste
ggplot(testes_quant, aes(x=value, y=second_col_values)) +
  xlab("Testes") +
  ylab("Quantidade") +
  ggtitle("Testes positivos em pacientes n�o classificados") +
  geom_col()