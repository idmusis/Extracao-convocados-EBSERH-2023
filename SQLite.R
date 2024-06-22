library(DBI)
library(RSQLite)
library(rvest)
library(dplyr)
library(readxl)
library(openxlsx)

# Conectar ao banco de dados SQLite
db <- dbConnect(RSQLite::SQLite(), dbname = "editais.sqlite")

# Criar tabela se não existir
dbExecute(db, "
CREATE TABLE IF NOT EXISTS editais (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  hospital TEXT,
  edital TEXT,
  data_publicacao TEXT,
  indice TEXT,
  cargo TEXT,
  observacao_cargo TEXT,
  nomes TEXT,
  microrregiao TEXT
)")


# Função para adicionar editais ao banco se não existirem
add_edital_if_new <- function(hospital, edital, data_publicacao, indice, cargo, observacao_cargo, nomes,microrregiao) {
  # Verificar se a entrada já existe baseada em critérios únicos
  exists <- dbGetQuery(db, sprintf("SELECT 1 FROM editais WHERE edital = '%s' AND indice = '%s' AND nomes = '%s'", edital, indice, nomes))
  if (nrow(exists) == 0) {
    dbExecute(db, "INSERT INTO editais (hospital, edital, data_publicacao, indice, cargo, observacao_cargo, nomes,microrregiao) VALUES (?, ?, ?, ?, ?, ?, ?)", 
              params = list(hospital, edital, data_publicacao, indice, cargo, observacao_cargo, nomes,microrregiao))
    print(paste("Novo edital adicionado: ", edital, " de número: ", id))
  }
}

# Ler a página principal e extrair os links dos hospitais
hospital_page <- read_html("https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/concurso-no-01-2023-ebserh-nacional/convocacoes")


selected_hospital_links <- hospital_page %>% html_nodes(".card a") %>% html_attr("href")
selected_hospital_names <- hospital_page %>% html_nodes(".card a") %>% html_text() %>% gsub("\\s+", " ", .) %>% trimws()


#Estabelecendo como base para não atualizar de todos os hospitais
# microrregiao4_hospitais <- c("SEDE", "UnB", "UFG", "UFGD", "UFMS", "UFMT", "HDT-UFT")
# microrregiao4_index <- grep(paste(microrregiao4_hospitais, collapse = "|"), hospital_names, ignore.case = TRUE)
# 
# hospital_names <- hospital_names[microrregiao4_index]
# hospital_links <- hospital_links[microrregiao4_index]

# Dicionário para mapear hospitais para microrregiões
microrregiao_map <- list(
  "EBSERH-SEDE" = 4,
  "HUB-UnB" = 4,
  "HC-UFG" = 4,
  "HU-UFGD" = 4,
  "HUMAP-UFMS" = 4,
  "HUJM-UFMT" = 4,
  "HDT-UFT" = 4,
  "HUGV-UFAM" = 1,
  "HU-UNIFAP" = 1,
  "HU-UFMA" = 1,
  "CHU-UFPA" = 1,
  "HU-UFPI" = 1,
  "CH-UFC" = 2,
  "HUJB-UFCG" = 2,
  "HUAC-UFCG" = 2,
  "HULW-UFPB" = 2,
  "HUOL-UFRN" = 2,
  "MEJC-UFRN" = 2,
  "HUAB-UFRN" = 2,
  "HUPAA-UFAL" = 3,
  "MCO-UFBA" = 3,
  "HUPES-UFBA" = 3,
  "HC-UFPE" = 3,
  "HU-UNIVASF" = 3,
  "HU-UFS" = 3,
  "HUL-UFS" = 3,
  "HUCAM-UFES" = 5,
  "HC-UFMG" = 5,
  "HU-UFJF" = 5,
  "HC-UFTM" = 5,
  "HC-UFU" = 5,
  "HUAP-UFF" = 5,
  "HUGG-UNIRIO" = 5,
  "HU-UFSCAR" = 5,
  "CHC-UFPR" = 6,
  "HE-UFPEL" = 6,
  "HU-FURG" = 6,
  "HUSM-UFSM" = 6,
  "HU-UFSC" = 6
)

# Função para obter a microrregião baseada no hospital
get_microrregiao <- function(hospital) {
  return(microrregiao_map[[hospital]] %||% 0)  # Retorna 0 se o hospital não estiver mapeado
}

# Inicializar URL da página e número da página para cada hospital
for (j in 1:length(selected_hospital_links)) {
  hospital_name <- selected_hospital_names[j]
  microrregiao <- get_microrregiao(hospital_name)
  current_page_url <- selected_hospital_links[j]
  current_page_number <- 0
  
  while (!is.null(current_page_url)) {
    # Acessar a página do hospital
    hospital_page <- read_html(current_page_url)
    
    # Encontrar todos os editais do hospital
    edital_links <- hospital_page %>% html_nodes("a[href*='/view']") %>% html_attr("href")
    edital_texts <- hospital_page %>% html_nodes("a[href*='/view']") %>% html_text()
    
    # Filtrar links que contêm "/view" e começam com a URL do concurso
    filtered_indices <- which(grepl("/view", edital_links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", edital_links))
    
    # Obter os links e textos correspondentes aos índices filtrados
    edital_links <- edital_links[filtered_indices]
    edital_texts <- edital_texts[filtered_indices]
    
    #Filtrar apenas os que não foram processados
    editais_processados <- (dbGetQuery(db, "SELECT edital FROM editais"))$edital
    unprocessed_indices <- which(!edital_texts %in% editais_processados)
    edital_links <- edital_links[unprocessed_indices]
    edital_texts <- edital_texts[unprocessed_indices]
    
    # Verificar se os links dos editais foram encontrados corretamente
    print("Links dos editais:")
    print(head(edital_links))
    print("Textos dos editais:")
    print(head(edital_texts))
    
    # Loop para percorrer todos os editais do hospital
    for (i in 1:length(edital_links)) {
      edital_text <- edital_texts[i]
      edital_url <- edital_links[i]
      
      # Verificar URL do edital
      if (!is.na(edital_url) && nchar(edital_url) > 0) {
        print(paste("URL do edital:", edital_url))
        
        # Construir e verificar link do PDF
        pdf_link <- sub("/view$", "/@@download/file", edital_url)
        print(paste("Link do PDF:", pdf_link))
        
        if (!is.na(pdf_link) && nchar(pdf_link) > 0) {
          full_pdf_url <- pdf_link
          
          # Tentar extrair texto do PDF, capturando qualquer erro
          try({
            pdf_text <- extract_text_from_pdf(full_pdf_url)
            
            # Parsear o texto do PDF
            lines <- unlist(strsplit(pdf_text, "\n")) 
            
            #Verificando as linhas que contém convocados
            convocados <- FALSE
            last_index <- NULL
            for (line in lines) {
              # Obtendo data
              if (grepl(", DE \\d+ DE \\w+ DE \\d+", line)) {
                edital_data <- str_extract(line, "\\d+ DE \\w+ DE \\d+")
              }
              
              if (grepl("^1\\.1 ", line)) { # Inicio da lista de convocados: o primeiro subitem de 1 ("1.1")
                convocados <- TRUE
              }
              if (convocados) {
                if (grepl("^2\\.\\D", line)) { # Fim da lista de convocados: linhas com "2. "
                  convocados <- FALSE
                } else {
                  # Captura linhas do formato "1.1 Cargo Nome"
                  if (grepl("^\\d+\\.\\d+ ", line)) {
                    indice <- str_extract(line, "^\\d+\\.\\d+")
                    
                    cargo_completo <- str_trim(str_replace(line, "^\\d+\\.\\d+\\s+", "")) # Captura "Cargo" até a colocação do participante
                    cargo <- str_trim(str_extract(cargo_completo, "^[^\\(]+"))  # Captura o cargo até o primeiro parêntese
                    observacao <- str_extract(cargo_completo, "\\([^\\)]+\\)")  # Captura a observação dentro dos parênteses
                    nomes <- str_extract_all(line, "\\d+[ºª] [^;]+")  # Captura todos os nomes listados
                    nomes_concat <- paste(unlist(nomes), collapse = "; ")  # Concatena todos os nomes com ponto e vírgula
                    data <- append(data, list(data.frame(Hospital = hospital_name, Edital = edital_text, Data = edital_data, Índice = indice, Cargo = cargo, "Obs. Cargo" = observacao, Nomes = nomes_concat,Microrregião=microrregiao)))
                  } else {
                    # Captura linhas adicionais que continuam após a quebra
                    if (length(data) > 0) {
                      last_entry <- data[[length(data)]]
                      last_entry$Nomes <- paste(last_entry$Nomes, line)
                      data[[length(data)]] <- last_entry
                    }
                  }
                }
              }
            }
          }, silent = TRUE)  # Capturar e ignorar erros
        }
      }
    }
    
    # Atualizar o link para a próxima página
    current_page_number <- current_page_number + 20
    next_page_url <- paste0(current_page_url, "?b_start:int=", current_page_number)
    
    # Verificar se a próxima página contém links de editais válidos
    next_page_html <- try(read_html(next_page_url), silent = TRUE)
    if (inherits(next_page_html, "try-error") || !page_contains_valid_links(next_page_html)) {
      current_page_url <- NULL
    } else {
      current_page_url <- next_page_url
    }
  }
}
df <- bind_rows(data)
df2<-df
# Renomeando colunas para corresponder aos parâmetros da função
names(df) <- c("hospital", "edital", "data_publicacao", "indice", "cargo", "observacao_cargo", "nomes","microrregiao")

# Atualizando banco de dados:
apply(df, 1, function(row) {
  add_edital_if_new(row["hospital"], row["edital"], row["data_publicacao"], row["indice"], 
                    row["cargo"], row["observacao_cargo"], row["nomes"],row["microrregiao"])
})


tabela <- dbReadTable(db, "editais") #Lendo banco de dados

# Fechar a conexão com o banco de dados
dbDisconnect(db)

#Importando pro excel
atualizar_excel<-function(arquivo_excel="Editais.xlsx"){

if (file.exists(arquivo_excel)) {
  dados_excel <- readWorkbook(arquivo_excel, sheet = 1)
} else {
  dados_excel <- data.frame()  # Se o arquivo não existir, criar um DataFrame vazio
}
# Filtrar apenas os novos dados (não presentes no arquivo Excel)
if (nrow(dados_excel) > 0) {
  dados_novos <- anti_join(tabela, dados_excel, by = c("hospital", "edital", "data_publicacao", "indice", "cargo", "observacao_cargo", "nomes"))
} else {
  dados_novos <- tabela  # Se o arquivo Excel estava vazio, todos os dados são novos
}
if (nrow(dados_novos) > 0) {
  # Carregar ou criar um novo workbook
  if (file.exists(arquivo_excel)) {
    wb <- loadWorkbook(arquivo_excel)
  } else {
    wb <- createWorkbook()
    addWorksheet(wb, "Editais")
  }
  
  # Adicionar novos dados ao final do arquivo Excel
  writeData(wb, "Editais", dados_novos, startRow = ifelse(exists("dados_excel"), nrow(dados_excel) + 1, 1))
  
  # Salvar o workbook
  saveWorkbook(wb, arquivo_excel, overwrite = TRUE)
  cat("Novos dados adicionados ao arquivo 'Editais.xlsx'.\n")
} else {
  cat("Não há novos dados para adicionar.\n")
}
}

atualizar_excel()
