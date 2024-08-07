### Passo 1: Instalação e Carregamento dos Pacotes


install.packages("FuzzyR")
install.packages("readxl")

library(readxl)
library(FuzzyR)


### importando os dados

dados_percurso <- read_excel("dados_percursos.xlsx", sheet = "Dados")
dados_percurso
dados_semaforos <- read_excel("dados_semaforo.xlsx", sheet = "Dados")
dados_semaforos


# Passo 2: Criar o Sistema Fuzzy

controle_verde <- newfis("Controle Inteligente Verde")

# Passo 3: Adicionar Variáveis de Entrada e Saída

# Adicionar variáveis de entrada
controle_verde <- addvar(controle_verde, "input", "Intensidade", c(0, 40))
controle_verde <- addvar(controle_verde, "input", "Tempo_Trajeto", c(0, 11))

# Adicionar variável de saída
controle_verde <- addvar(controle_verde, "output", "Verde", c(25, 60))


# Passo 4: Definir Funções de Pertinência (Membership Functions)

### Funções de pertinência para "Intensidade"

controle_verde <- addmf(controle_verde, "input", 1, "Baixo", "trimf", c(0, 10, 20))
controle_verde <- addmf(controle_verde, "input", 1, "Medio", "trimf", c(15, 25, 35))
controle_verde <- addmf(controle_verde, "input", 1, "Alto", "trimf", c(30, 40, 40))


######## Funções de pertinência para "Tempo_Trajeto"

controle_verde <- addmf(controle_verde, "input", 2, "Curto", "trimf", c(0, 6, 6.55)) # até 6,55 minutos
controle_verde <- addmf(controle_verde, "input", 2, "Medio", "trimf", c(7, 8, 8.5)) # de 7 a 8,5 minutos
controle_verde <- addmf(controle_verde, "input", 2, "Longo", "trimf", c(9, 10, 11)) # de 9 a 11 minutos

####### Funções de pertinência para "Verde"

controle_verde <- addmf(controle_verde, "output", 1, "Curto", "trimf", c(30, 35, 35))
controle_verde <- addmf(controle_verde, "output", 1, "Medio", "trimf", c(40, 50, 60))



# Passo 5: Definir a Base de Regras

ruleListVerde <- rbind(
  c(1, 1, 1, 1, 1), # Se Intensidade é Baixa e Tempo_Trajeto é Curto, então Verde é Curto
  c(1, 2, 1, 1, 1), # Se Intensidade é Baixa e Tempo_Trajeto é Médio, então Verde é Curto
  c(1, 3, 1, 1, 1), # Se Intensidade é Baixa e Tempo_Trajeto é Longo, então Verde é Curto
  c(2, 1, 2, 1, 1), # Se Intensidade é Média e Tempo_Trajeto é Curto, então Verde é Médio
  c(2, 2, 2, 1, 1), # Se Intensidade é Média e Tempo_Trajeto é Médio, então Verde é Médio
  c(2, 3, 2, 1, 1), # Se Intensidade é Média e Tempo_Trajeto é Longo, então Verde é Longo
  c(3, 3, 2, 1, 1)  # Se Intensidade é Alta e Tempo_Trajeto é Longo, então Verde é Médio
)

controle_verde <- addrule(controle_verde, ruleListVerde)
controle_verde

# Passo 6: Testar o Sistema Fuzzy

entrada_verde <- as.matrix(dados_percurso[, c("Intensidade", "Tempo")])
entrada_verde
str(entrada_verde)

# Avaliar o sistema fuzzy
saida_verde <- evalfis(entrada_verde, controle_verde)
saida_verde

# Arredondando

saida_verde <- round(saida_verde, 0)
saida_verde

##### Sinal Vermelho

# Passo 1: Criar o Sistema Fuzzy

controle_vermelho <- newfis("Controle Inteligente Vermelho")

### Passo 2: Adicionar Variáveis de Entrada e Saída (São as mesma do Sinal Verde)

# Adicionar variáveis de entrada
controle_vermelho <- addvar(controle_vermelho, "input", "Intensidade", c(0, 40))
controle_vermelho <- addvar(controle_vermelho, "input", "Tempo_Trajeto", c(0, 11))

# Adicionar variável de saída para Vermelho
controle_vermelho <- addvar(controle_vermelho, "output", "Vermelho", c(60, 70))

### Passo 3: Definir Funções de Pertinência (Membership Functions) 
# As Funções de pertinência para Intensidade e Tempo de Trajeto são os mesmos Valores de verde

# Funções de pertinência para "Intensidade"
controle_vermelho <- addmf(controle_vermelho, "input", 1, "Baixo", "trimf", c(0, 10, 20))
controle_vermelho <- addmf(controle_vermelho, "input", 1, "Medio", "trimf", c(15, 25, 35))
controle_vermelho <- addmf(controle_vermelho, "input", 1, "Alto", "trimf", c(30, 40, 40))

# Funções de pertinência para "Tempo_Trajeto"
controle_vermelho <- addmf(controle_vermelho, "input", 2, "Curto", "trimf", c(0, 6, 6.55)) # até 6,55 minutos
controle_vermelho <- addmf(controle_vermelho, "input", 2, "Medio", "trimf", c(7, 8, 8.5)) # de 7 a 8,5 minutos
controle_vermelho <- addmf(controle_vermelho, "input", 2, "Longo", "trimf", c(9, 10, 11)) # de 9 a 11 minutos

# Funções de pertinência para "Vermelho" 

controle_vermelho <- addmf(controle_vermelho, "output", 1, "Médio", "trimf", c(60, 65, 70))
controle_vermelho <- addmf(controle_vermelho, "output", 1, "Longo", "trimf", c(65, 70, 75))

### Passo 4: Definir a Base de Regras

# Regras para Vermelho
ruleListVermelho <- rbind(
  c(1, 1, 2, 1, 1), # Se Intensidade é Baixa e Tempo_Trajeto é Curto, então Vermelho é Longo
  c(1, 2, 1, 1, 1), # Se Intensidade é Baixa e Tempo_Trajeto é Médio, então Vermelho é Médio
  c(1, 3, 2, 1, 1), # Se Intensidade é Baixa e Tempo_Trajeto é Longo, então Vermelho é Longo
  c(2, 2, 1, 1, 1), # Se Intensidade é Média e Tempo_Trajeto é Médio, então Vermelho é Médio
  c(2, 3, 2, 1, 1), # Se Intensidade é Média e Tempo_Trajeto é Longo, então Vermelho é Longo
  c(3, 2, 1, 1, 1), # Se Intensidade é Alta e Tempo_Trajeto é Médio, então Vermelho é Médio
  c(3, 3, 2, 1, 1)  # Se Intensidade é Alta e Tempo_Trajeto é Longo, então Vermelho é Longo
)

# Adicionando as regras ajustadas para Vermelho
controle_vermelho <- addrule(controle_vermelho, ruleListVermelho)
controle_vermelho

### Passo 5: Testar o Sistema Fuzzy

entrada_vermelho <- as.matrix(dados_percurso[, c("Intensidade", "Tempo")])
entrada_vermelho
str(entrada_vermelho)

# Avaliar o sistema fuzzy
saida_vermelho <- evalfis(entrada_vermelho, controle_vermelho)
saida_vermelho

# Arredondando o Valor do sinal vermelho
saida_vermelho <- round(saida_vermelho, 0)
saida_vermelho

# Tabela Resumida
# Primeiro, adicionamos a saída à matriz de entrada
dados <- cbind(entrada_verde, Verde = saida_verde, Vermelho = saida_vermelho)
dados

dados_completos_df <- as.data.frame(dados)
colnames(dados_completos_df) <- c("Intensidade", "Tempo de Trajeto", "Verde Fuzzy", "Vermelho Fuzzy")
dados_completos_df
