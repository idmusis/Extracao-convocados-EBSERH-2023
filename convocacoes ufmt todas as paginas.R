library(stringr)
library(rvest)
library(dplyr)
library(pdftools)
library(openxlsx)

# URL inicial
base_url <- "https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/concurso-no-01-2023-ebserh-nacional/convocacoes"
page <- read_html(base_url)

# Função para extrair texto de PDF
extract_text_from_pdf <- function(url) {
  pdf_text(url)
}

# Lista para armazenar os dados
data <- list()

# Encontrar todos os hospitais
hospital_links <- page %>% html_nodes(".card a") %>% html_attr("href")
hospital_names <- page %>% html_nodes(".card a") %>% html_text() %>% gsub("\\s+", " ", .) %>% trimws()

# Verificar se os links foram encontrados corretamente
print("Links dos hospitais:")
print(head(hospital_links))
print("Nomes dos hospitais:")
print(head(hospital_names))

# Selecionar o hospital UFMT
ufmt_index <- grep("UFMT", hospital_names, ignore.case = TRUE)
hospital_name <- hospital_names[ufmt_index]
hospital_url <- hospital_links[ufmt_index]

# Verificar URL do hospital UFMT
print("URL do hospital UFMT:")
print(hospital_url)

# Inicializar URL da página e número da página
current_page_url <- hospital_url
current_page_number <- 0


# Função para verificar se uma página contém links de editais válidos
page_contains_valid_links <- function(page) {
  links <- page %>% html_nodes("a[href*='/view']") %>% html_attr("href")
  filtered_indices <- which(grepl("/view", links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", links))
  links <- links[filtered_indices]
  
  return(length(links) > 0)
}


while (!is.null(current_page_url)) {
  # Acessar a página do hospital UFMT
  hospital_page <- read_html(current_page_url)
  
  # Encontrar todos os editais do hospital UFMT
  edital_links <- hospital_page %>% html_nodes("a[href*='/view']") %>% html_attr("href")
  edital_texts <- hospital_page %>% html_nodes("a[href*='/view']") %>% html_text()
  
  # Filtrar links que contêm "/view" e começam com a URL do concurso
  filtered_indices <- which(grepl("/view", edital_links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", edital_links))
  
  # Obter os links e textos correspondentes aos índices filtrados
  edital_links <- edital_links[filtered_indices]
  edital_texts <- edital_texts[filtered_indices]
  
  # Verificar se os links dos editais foram encontrados corretamente
  print("Links dos editais:")
  print(head(edital_links))
  print("Textos dos editais:")
  print(head(edital_texts))
  
  # Loop para percorrer todos os editais do hospital UFMT
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
            #Obtendo data
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
                  data <- append(data, list(data.frame(Hospital = hospital_name, Edital = edital_text, Data=edital_data, Índice=indice,Cargo=cargo, "Obs. Cargo"=observacao,Nomes = nomes_concat)))
                 } 
                 else  {
                #   # Captura linhas adicionais que continuam após a quebra
                if (length(data) > 0 ) {
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
  next_page_url <- paste0(hospital_url, "?b_start:int=", current_page_number)
  
  # Verificar se a próxima página contém links de editais válidos
  next_page_html <- try(read_html(next_page_url), silent = TRUE)
  if (inherits(next_page_html, "try-error") || !page_contains_valid_links(next_page_html)) {
    current_page_url <- NULL
  } else {
    current_page_url <- next_page_url
  }
}

# Converter lista em dataframe
  
df <- bind_rows(data)
unique(df$Edital) # Checando o número de editais
View(df)
