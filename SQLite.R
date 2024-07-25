library(stringr)
library(DBI)
library(RSQLite)
library(rvest)
library(dplyr)
library(readxl)
library(openxlsx)
library(pdftools)
library(data.table)

# Conectar ao banco de dados SQLite
db <- dbConnect(RSQLite::SQLite(), dbname = "editais.sqlite")

# Criar tabela se não existir
dbExecute(db, "
CREATE TABLE IF NOT EXISTS editais (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  microrregiao TEXT,
  hospital TEXT,
  edital_numero INTEGER,
  edital TEXT,
  data_publicacao TEXT,
  indice TEXT,
  cargo TEXT,
  observacao_cargo TEXT,
  nomes TEXT
)")


# Função para adicionar editais ao banco se não existirem
add_edital_if_new <- function(microrregiao,hospital, edital_numero, edital, data_publicacao, indice, cargo, observacao_cargo, nomes) {
  # # Verificar se a entrada já existe baseada em critérios únicos
  # exists <- dbGetQuery(db, sprintf("SELECT 1 FROM editais WHERE edital = '%s' AND indice = '%s' AND nomes = '%s'", edital, indice, nomes))
  # if (nrow(exists) == 0) {
  #   dbExecute(db, "INSERT INTO editais (microrregiao,hospital, edital, data_publicacao, indice, cargo, observacao_cargo, nomes) VALUES (?, ?, ?, ?, ?, ?, ?, ?)", 
  #             params = list(microrregiao,hospital, edital, data_publicacao, indice, cargo, observacao_cargo, nomes))
  #   print(paste("Novo edital adicionado: ", edital))
  # }
  # Verificar se a entrada já existe baseada em critérios únicos
  query <- "SELECT 1 FROM editais WHERE edital = ? AND indice = ? AND nomes = ?"
  exists <- dbGetQuery(db, query, params = list(edital, indice, nomes))
  
  if (nrow(exists) == 0) {
    insert_query <- "INSERT INTO editais (microrregiao, hospital, edital_numero, edital, data_publicacao, indice, cargo, observacao_cargo, nomes) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    dbExecute(db, insert_query, params = list(microrregiao, hospital, edital_numero, edital, data_publicacao, indice, cargo, observacao_cargo, nomes))
    return(paste("Novo edital adicionado: ", edital))
  } else {
    return(paste("Edital já existe:", edital))
  }
}

# Função para extrair texto de PDF
extract_text_from_pdf <- function(url) {
  pdf_text(url)
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
  "HUB-UNB" = 4,
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
  "HUGG" = 5,
  "CH-UFRJ"=5, #não está na lista
  "HU-UFSCAR" = 5,
  "CHC-UFPR" = 6,
  "HE-UFPEL" = 6,
  "HU-FURG" = 6,
  "HUSM-UFSM" = 6,
  "HU-UFSC" = 6
)

editais_processados <- unique((dbGetQuery(db, "SELECT edital FROM editais"))$edital)

# Função para obter a microrregião baseada no hospital
get_microrregiao <- function(hospital) {
  hospital_upper <- toupper(hospital)
  return(microrregiao_map[[hospital_upper]] %||% 0)  # Retorna 0 se o hospital não estiver mapeado
}

data <- list()

# inicio do loop ----
# Inicializar URL da página e número da página para cada hospital
for (j in 1:length(selected_hospital_links)) {
  hospital_name <- selected_hospital_names[j]
  microrregiao <- get_microrregiao(hospital_name)
  current_page_url <- selected_hospital_links[j]
  current_page_number <- 0
  consecutive_invalid_pages <-0
  
  while (!is.null(current_page_url)) {
    # Acessar a página do hospital
    #hospital_page <- read_html(current_page_url)
    # Acessar a página do hospital com tryCatch para capturar possíveis erros
    hospital_page <- tryCatch({
      read_html(current_page_url)
    }, error = function(e) {
      message(paste("Erro ao acessar a página do hospital:", hospital_name, "URL:", current_page_url))
      message(e)
      return(NULL)
    })
    
    if (is.null(hospital_page)) {
      # Se houve um erro ao ler a página, sair do loop while e continuar com o próximo hospital
      break
    }
    
    # Encontrar todos os editais do hospital
    edital_links <- hospital_page %>% html_nodes("a[href*='/view']") %>% html_attr("href")
    edital_texts <- hospital_page %>% html_nodes("a[href*='/view']") %>% html_text()
    
    # Filtrar links que contêm "/view" e começam com a URL do concurso
    filtered_indices <- which(grepl("/view", edital_links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", edital_links))
    
    # Obter os links e textos correspondentes aos índices filtrados
    edital_links <- edital_links[filtered_indices]
    edital_texts <- edital_texts[filtered_indices]
    
    #Filtrar apenas os que não foram processados
    #tabela1 <- dbReadTable(db, "editais") #Lendo banco de dados
    unprocessed_indices <- which(!edital_texts %in% editais_processados)
    edital_links <- edital_links[unprocessed_indices]
    edital_texts <- edital_texts[unprocessed_indices]
    
    # Verificar se os links dos editais foram encontrados corretamente
    print("Links dos editais:")
    print(head(edital_links))
    print("Textos dos editais:")
    print(head(edital_texts))
    
    if (length(edital_links) > 0){
      
    # Loop para percorrer todos os editais do hospital
    for (i in 1:length(edital_links)) {
      edital_text <- edital_texts[i]
      edital_url <- edital_links[i]
      # Verificar URL do edital
      if (length(edital_url) > 0 && (!is.na(edital_url) && nchar(edital_url) > 0)){
        # if (
        #   !is.na(edital_url)
        #   ) {
        print(paste("URL do edital:", edital_url))
        
        # Construir e verificar link do PDF
        pdf_link <- sub("/view$", "/@@download/file", edital_url)
        print(paste("Link do PDF:", pdf_link))
        
        if (!is.na(pdf_link) || nchar(pdf_link) > 0) {
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
              # Obtendo número do edital  
              
              if (grepl("^1\\.1 ", line)) { # Inicio da lista de convocados: o primeiro subitem de 1 ("1.1")
                convocados <- TRUE
              }
              if (convocados) {
                if (grepl("^2\\.", line)) { # Fim da lista de convocados: linhas com "2. "
                  convocados <- FALSE
                } else {
                  # Captura linhas do formato "1.1 Cargo Nome"
                  if (grepl("^\\d+\\.\\d+ ", line)) {
                    indice <- str_extract(line, "^\\d+\\.\\d+")
                    
                    cargo_completo <- str_trim(str_replace(line, "^\\d+\\.\\d+\\s+", "")) # Captura "Cargo" até a colocação do participante
                    edital_numero <- str_extract(edital_text, "\\d+")
                    cargo <- str_trim(str_extract(cargo_completo, "^[^\\(]+"))  # Captura o cargo até o primeiro parêntese
                    observacao <- str_extract(cargo_completo, "\\([^\\)]+\\)")  # Captura a observação dentro dos parênteses
                    nomes <- str_extract_all(line, "\\d+[ºª] [^;]+")  # Captura todos os nomes listados
                    nomes_concat <- paste(unlist(nomes), collapse = "; ")  # Concatena todos os nomes com ponto e vírgula
                    data <- append(data, list(data.table(Microrregião=microrregiao,Hospital = hospital_name, edital_numero = edital_numero, Edital = edital_text, Data = edital_data, Índice = indice, Cargo = cargo, "Obs. Cargo" = observacao, Nomes = nomes_concat)))
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
    }
    
    # Atualizar o link para a próxima página
    current_page_number <- current_page_number + 20
    #next_page_url <- paste0(current_page_url, "?b_start:int=", current_page_number)
    update_page_url <- function(current_page_url, current_page_number) {
      # Verifica se o URL já contém o parâmetro "?b_start:int="
      if (grepl("\\?b_start:int=", current_page_url)) {
        # Substitui o valor atual do parâmetro pelo novo valor
        new_url <- sub("\\?b_start:int=\\d+", paste0("?b_start:int=", current_page_number + 20), current_page_url)
      } else {
        # Adiciona o parâmetro ao URL
        new_url <- paste0(current_page_url, "?b_start:int=", current_page_number + 20)
      }
      return(new_url)
    }
    next_page_url <- update_page_url(current_page_url, current_page_number)
    
    # Verificar se a próxima página contém links de editais válidos
    next_page_html <- try(read_html(next_page_url), silent = TRUE)
    page_contains_valid_links <- function(page) {
      links <- page %>% html_nodes("a[href*='/view']") %>% html_attr("href")
      filtered_indices <- which(grepl("/view", links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", links))
      links <- links[filtered_indices]
      
      return(length(links) > 0)
    }
   #  
   #  if ((current_page_number >= 100) #||(inherits(next_page_html, "try-error") || !page_contains_valid_links(next_page_html))
   #      )
   #       {
   #    current_page_url <- NULL
   #  } else {
   #    current_page_url <- next_page_url
   #  
   # }
    while (!is.null(current_page_url) && consecutive_invalid_pages < 3) {
      next_page_url <- update_page_url(current_page_url, current_page_number)
      next_page_html <- try(read_html(next_page_url), silent = TRUE)
      
      if (inherits(next_page_html, "try-error") || !page_contains_valid_links(next_page_html)) {
        consecutive_invalid_pages <- consecutive_invalid_pages + 1
      } else {
        consecutive_invalid_pages <- 0
        current_page_url <- next_page_url
      }
      
      # Incrementa o número da página atual
      current_page_number <- current_page_number + 20
    }
    
    # Se três páginas consecutivas forem inválidas, current_page_url se torna NULL
    if (consecutive_invalid_pages >= 3) {
      current_page_url <- NULL
    }
  }
}
df <- bind_rows(data) 
df2<-df
# Renomeando colunas para corresponder aos parâmetros da função
names(df) <- c("microrregiao","hospital", "edital_numero","edital", "data_publicacao", "indice", "cargo", "observacao_cargo", "nomes")
df$edital_numero<-as.numeric(df$edital_numero)

df <- df[order(df$edital_numero, decreasing = FALSE), ]

# Atualizando banco de dados: ----
apply(df, 1, function(row) {
  add_edital_if_new(row["microrregiao"],row["hospital"],row["edital_numero"], row["edital"], row["data_publicacao"], row["indice"], 
                    row["cargo"], row["observacao_cargo"], row["nomes"])
})


tabela <- dbReadTable(db, "editais") #Lendo banco de dados

# Fechar a conexão com o banco de dados
dbDisconnect(db)

library(googlesheets4)

# Importando pro excel e google sheets ----
#tabela$edital_numero<-as.numeric(tabela$edital_numero)

atualizar_excel<-function(arquivo_excel="Editais.xlsx",excel=TRUE,sheets=TRUE,google_sheet_id="https://docs.google.com/spreadsheets/d/1LxCUSgQmXJKCzJFKEQHyxPcBbJcSOT52-ghsEV1mmJQ/") {
  
  tabela <- tabela[order(tabela$edital_numero, decreasing = FALSE), ]
  if(excel){
if (file.exists(arquivo_excel)) {
  dados_excel <- readWorkbook(arquivo_excel, sheet = 1)
} else {
  dados_excel <- data.frame()  # Se o arquivo não existir, criar um DataFrame vazio
}
# Filtrar apenas os novos dados (não presentes no arquivo Excel)
if (nrow(dados_excel) > 0 ) {
  dados_novos <- anti_join(tabela, dados_excel, by = join_by(edital,indice))
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
  if(sheets){
    dados_google_sheet <- read_sheet(google_sheet_id)
    if (nrow(dados_google_sheet) > 0) {
      dados_novos_google <- anti_join(tabela, dados_google_sheet, by = join_by(edital,indice))
    } else {
      dados_novos_google <- tabela  # Se o Google Sheet estava vazio, todos os dados são novos
    }
    
    if (nrow(dados_novos_google) > 0) {
      # Adicionar novos dados ao Google Sheet
      sheet_append(google_sheet_id, dados_novos_google)
      cat("Novos dados adicionados ao Google Sheet.\n")
    } else {
      cat("Não há novos dados para adicionar ao Google Sheet.\n")
    }
  } else {
    cat("Não há novos dados para adicionar.\n")
  }
}

atualizar_excel(sheets = FALSE)
atualizar_excel(excel=FALSE)
#atualizar_excel(excel=FALSE)
