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

# Acessar a página do hospital UFMT
hospital_page <- read_html(hospital_url)

# Encontrar todos os editais do hospital UFMT
edital_links <- hospital_page %>% html_nodes("a[href]") %>% html_attr("href")
edital_texts <- hospital_page %>% html_nodes("a[href]") %>% html_text()

# Filtrar links que contêm "/view" e começam com a URL especificada
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
  print(paste("URL do edital:", edital_url))
  
  # Acessar a página do edital
  edital_page <- read_html(edital_url)
  
  # Extrair o link do PDF da página do edital
  pdf_link <- sub("/view$", "/@@download/file", edital_url)
  
  # Verificar link do PDF
  print(paste("Link do PDF:", pdf_link))
  
  if (!is.na(pdf_link)) {
    full_pdf_url <- pdf_link
    
    # Extrair texto do PDF
    pdf_text <- extract_text_from_pdf(full_pdf_url)
    
    # Depuração: imprimir texto extraído do PDF
    #print(paste("Texto extraído do PDF (", edital_text, "):\n", head(pdf_text), "\n", sep=""))
    
    # Parsear o texto do PDF para encontrar as informações necessárias
    lines <- unlist(strsplit(pdf_text, "\n"))
    
    convocados <- FALSE
    for (line in lines) {
      if (grepl("Relação de candidatos convocados", line, ignore.case = TRUE)) {
        convocados <- TRUE
      }
      if (convocados) {
        if (grepl("Os candidatos relacionados neste edital deverão comparecer", line, ignore.case = TRUE)) {
          convocados <- FALSE
        } else {
          # Captura linhas do formato "1.1 Nome Cargo"
          if (grepl("^\\d+\\.\\d+ ", line)) {
            data <- append(data, list(data.frame(Hospital = hospital_name, Edital = edital_text, Convocado = line)))
          } else {
            # Captura linhas adicionais que continuam após a quebra
            previous <- tail(data, n = 1)
            if (length(previous) > 0) {
              data[[length(data)]] <- data.frame(Hospital = hospital_name, Edital = edital_text, Convocado = paste(previous[[1]]$Convocado, line))
            }
          }
        }
      }
    }
  }
}

# Converter lista em dataframe e salvar em Excel
df <- bind_rows(data)
#print("Dataframe final:")
#print(df)

