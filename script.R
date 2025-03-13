library(stringr) # Operações com strings
library(tidytable) # Organização de dados
library(DBI) # Integração com SQL
library(RSQLite) # Integração com SQL
library(rvest) # Scraping de páginas web
library(pdftools) # Leitura de PDFs
library(data.table) # Organização de dados
library(dplyr) # Organização de dados
library(openxlsx) # Integração com excel
library(googlesheets4) # Integração com google sheets
library(cli) # Customização de mensagens de console

# Conectar ao banco de dados SQLite
db <- dbConnect(RSQLite::SQLite(), dbname = "editais.sqlite")

# Garantir que existe a planilha

dbExecute(db, "
  CREATE TABLE IF NOT EXISTS editais (
    Microrregião TEXT,
    Hospital TEXT,
    `Número do edital` INTEGER,
    `Tipo de edital` TEXT,
    Edital TEXT,
    Data TEXT,
    Índice TEXT,
    Cargo TEXT,
    `Obs. Cargo` TEXT,
    `Posição` INTEGER,
    Nome TEXT,
    `Obs. Colocado 1` TEXT,
    `Obs. Colocado 2` TEXT,
    Ano INTEGER
  )
")

# Objetos --------------------------

# Ler a página principal e extrair os links dos hospitais
hospital_page <- read_html("https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/concurso-no-01-2023-ebserh-nacional/convocacoes")

selected_hospital_links <- hospital_page %>%
  html_nodes(".card a") %>%
  html_attr("href")
selected_hospital_names <- hospital_page %>%
  html_nodes(".card a") %>%
  html_text() %>%
  gsub("\\s+", " ", .) %>%
  trimws()

# Dicionário para mapear hospitais para microrregiões
microrregiao_map <- list(
  "EBSERH-SEDE" = 4,
  "HUB-UNB" = 4,
  "HC-UFG" = 4,
  "HU-UFGD" = 4,
  "HUMAP-UFMS" = 4,
  "HUJM-UFMT" = 4,
  "HDT-UFT" = 4,
  "HU-UFRR" = 1, # não está na lista
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
  "CH-UFRJ" = 5, # não está na lista
  "HU-UFSCAR" = 5,
  "CHC-UFPR" = 6,
  "HE-UFPEL" = 6,
  "HU-FURG" = 6,
  "HUSM-UFSM" = 6,
  "HU-UFSC" = 6
)

editais_processados <- unique((dbGetQuery(db, "SELECT Edital FROM editais"))$Edital)

# Funções -------------

# Função para adicionar editais ao banco se não existirem

add_edital_if_new <- function(Microrregião, Hospital, `Número do edital`, `Tipo de edital`, Edital, Data, Índice, Cargo, `Obs. Cargo`, `Posição`, Nome, `Obs. Colocado 1`, `Obs. Colocado 2`, Ano) {
  
  # Verificar se a entrada já existe
  query <- "SELECT 1 FROM editais WHERE Edital = ? AND Índice = ? AND Nome = ?"
  exists <- dbGetQuery(db, query, params = list(Edital, Índice, Nome))

  if (nrow(exists) == 0) {
    insert_query <- "INSERT INTO editais (Microrregião, Hospital, `Número do edital`, `Tipo de edital`, Edital, Data, Índice, Cargo, `Obs. Cargo`, `Posição`, Nome, `Obs. Colocado 1`, `Obs. Colocado 2`, Ano)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    dbExecute(db, insert_query, params = list(Microrregião, Hospital, `Número do edital`, `Tipo de edital`, Edital, Data, Índice, Cargo, `Obs. Cargo`, `Posição`, Nome, `Obs. Colocado 1`, `Obs. Colocado 2`, Ano))
    return(paste("Novo edital adicionado: ", Edital))
  } else {
    return(paste("Edital já existe:", Edital))
  }
}

# Função para obter a microrregião baseada no hospital
get_microrregiao <- function(hospital) {
  hospital_upper <- toupper(hospital)
  return(microrregiao_map[[hospital_upper]] %||% 0) # Retorna 0 se o hospital não estiver mapeado
}

get_next_page_url <- function(page) {
  next_page_node <- page %>% html_node("a.proximo")
  if (!is.null(next_page_node)) {
    return(next_page_node %>% html_attr("href"))
  } else {
    return(NULL) # Retorna NULL se não houver link para a próxima página
  }
}

page_contains_valid_links <- function(page) {
  links <- page %>%
    html_nodes("a[href*='/view']") %>%
    html_attr("href")
  filtered_indices <- which(grepl("/view", links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", links))
  links <- links[filtered_indices]

  return(length(links) > 0)
}

extrair_dados_edital <- function(pdf_link, hospital_name, microrregiao, edital_text) {
  # Lista para armazenar os dados extraídos
  dados_extraidos <- list()
  pdf_text <- pdftools::pdf_text(pdf_link)
  # Divide o texto em linhas
  lines <- unlist(strsplit(pdf_text, "\n"))

  # Inicializa variáveis
  convocados <- FALSE
  convocados_text <- ""

  # Processa cada linha do PDF
  for (line in lines) {
    # Extrai a data do edital
    if (grepl(", DE \\d+ DE \\w+ DE \\d+", line)) {
      edital_data <- stringr::str_extract(line, "\\d+ DE \\w+ DE \\d+") %>%
        stringr::str_to_sentence() %>%
        lubridate::dmy() %>%
        format("%Y/%m/%d")
    }

    # Início da lista de convocados: o primeiro subitem de 1 ("1.1")
    if (grepl("^\\s*1\\.1 ", line)) {
      convocados <- TRUE
    }

    # Coleta os convocados até encontrar um marcador de fim
    # Fim da lista de convocados: linhas com "2. "
    if (convocados) {
      if (grepl("^\\s*2\\.", line)) {
        convocados <- FALSE
      } else {
        convocados_text <- paste(convocados_text, line, sep = " ")
        convocados_text <- gsub("^ *|(?<= ) | *$", "", convocados_text, perl = TRUE)
      }
    }
  }

  # Extrai número e tipo do edital
  edital_numero <- stringr::str_extract(edital_text, "\\d+")
  edital_tipo <- stringr::str_extract(edital_text, "(?<=01-2023)\\s*[-–]*\\s*[A-Za-z].*") %>%
    stringr::str_remove_all(hospital_name) %>%
    stringr::str_remove_all("^\\s*[-_\\s]+|[-_\\s]+\\s*$") %>% # Remove traços e sublinhados do início e do fim
    stringr::str_squish()

  # Extrai os itens da lista de convocados
  itens <- stringr::str_extract_all(convocados_text, "\\d+\\.\\d+.*?(?=\\d+\\.\\d+|$)")[[1]]

  # Remove itens que contêm palavras-chave que indicam que não são nomes de convocados
  itens <- itens[!grepl("deverá|poderá|conforme|\\d+/\\d+/\\d+|Campus|localizado|av\\.|esocial", itens, ignore.case = TRUE)]


  # Se não houver itens convocados, adiciona entrada padrão
  if (length(itens) == 0) {
    dados_extraidos <- append(dados_extraidos, list(data.table::data.table(
      Microrregião = microrregiao,
      Hospital = hospital_name,
      "Número do edital" = edital_numero,
      "Tipo de edital" = edital_tipo,
      Edital = edital_text,
      Data = ifelse(exists("edital_data"), edital_data, NA),
      Índice = NA,
      Cargo = "N/A",
      "Obs. Cargo" = NA,
      "Posição" = NA,
      Nome = "N/A",
      "Obs. Colocado 1" = NA,
      "Obs. Colocado 2" = NA,
      Ano = ifelse(exists("edital_data"), lubridate::year(edital_data), NA)
    )))
  } else {
    # Processa cada item convocado
    for (item in itens) {
      indice <- stringr::str_extract(item, "^\\s*\\d+\\.\\d+")
      cargo_completo <- stringr::str_trim(stringr::str_remove(item, "^\\s*\\d+\\.\\d+\\.?\\s+"))
      cargo <- stringr::str_trim(stringr::str_extract(cargo_completo, "^[^\\(ºª\\d]+"))
      obs_cargo <- stringr::str_extract(cargo_completo, "\\([^\\)]+\\)") %>%
        stringr::str_remove_all("[\\(\\)]")

      # Extrai nomes e posições
      nomes <- stringr::str_extract_all(cargo_completo, "\\d+[ºª] [^;]+")[[1]]

      # Adiciona cada nome à tabela
      for (nome in nomes) {
        obs_colocados <- stringr::str_extract_all(nome, "\\(([^\\)]+)\\)")[[1]]
        posicao <- stringr::str_extract(nome, "\\d+\\s*[ºª]") %>%
          stringr::str_trim() %>%
          stringr::str_remove_all("[ºª]")
        nome <- stringr::str_trim(stringr::str_remove_all(nome, "\\d+\\s*[ºª]|\\([^\\)]+\\)"))

        # Define observações adicionais
        obs_colocado_1 <- if (length(obs_colocados) >= 1) stringr::str_remove_all(obs_colocados[1], "[\\(\\)]") else NA
        obs_colocado_2 <- if (length(obs_colocados) >= 2) {
          paste(stringr::str_remove_all(obs_colocados[2:length(obs_colocados)], "[\\(\\)]"), collapse = "; ")
        } else {
          NA
        }

        dados_extraidos <- append(dados_extraidos, list(data.table::data.table(
          Microrregião = microrregiao,
          Hospital = hospital_name,
          "Número do edital" = edital_numero,
          "Tipo de edital" = edital_tipo,
          Edital = edital_text,
          Data = ifelse(exists("edital_data"), edital_data, NA),
          Índice = indice,
          Cargo = cargo,
          "Obs. Cargo" = obs_cargo,
          "Posição" = posicao,
          Nome = nome,
          "Obs. Colocado 1" = obs_colocado_1,
          "Obs. Colocado 2" = obs_colocado_2,
          Ano = ifelse(exists("edital_data"), lubridate::year(edital_data), NA)
        )))
      }
    }
  }

  return(dados_extraidos)
}

# Filtrar hospitais específicos
# selected_hospital_names <- selected_hospital_names[grepl("ufpel|furg|ufsm|ufsc|unirio|ufrj|ufrr", selected_hospital_names, ignore.case = TRUE)]
# selected_hospital_links <- selected_hospital_links[grepl("ufpel|furg|ufsm|ufsc|unirio|ufrj|ufrr", selected_hospital_links, ignore.case = TRUE)]

# Início do loop ----------------

data <- list()

# Inicializar URL da página e número da página para cada hospital
for (j in 1:length(selected_hospital_links)) {
  
  hospital_name <- selected_hospital_names[j]
  microrregiao <- get_microrregiao(hospital_name)
  current_page_url <- selected_hospital_links[j]
  current_page_number <- 0

  cli_h2(paste0("Hospital: ", hospital_name)) # Print pro console

  while (!is.null(current_page_url)) {
    hospital_page <- tryCatch(
      {
        Sys.sleep(sample(10, 1) * 0.1) # Delay para evitar erros
        read_html(current_page_url)
      },
      error = function(e) {
        cli_alert_danger("Erro ao acessar a página do hospital: {hospital_name}, URL: {.url {current_page_url}}")
        message(e)
        return(NULL)
      }
    )

    if (is.null(hospital_page)) {
      # Se houve um erro ao ler a página, sair do loop while e continuar com o próximo hospital
      break
    }

    # Encontrar todos os editais do hospital na página
    edital_links <- hospital_page %>%
      html_nodes("a[href*='/view']") %>%
      html_attr("href")
    edital_texts <- hospital_page %>%
      html_nodes("a[href*='/view']") %>%
      html_text()

    # Filtrar links que contêm "/view" e começam com a URL do concurso
    filtered_indices <- which(grepl("/view", edital_links) & grepl("^https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/", edital_links))

    # Obter os links e textos correspondentes aos índices filtrados
    edital_links <- edital_links[filtered_indices]
    edital_texts <- edital_texts[filtered_indices]

    edital_texts <- str_remove(edital_texts, "\\.pdf$") %>%
      str_squish() # removendo .pdf e espaços extra

    # Filtrar apenas os que não foram processados
    unprocessed_indices <- which(!edital_texts %in% editais_processados)
    edital_links <- edital_links[unprocessed_indices]
    edital_texts <- edital_texts[unprocessed_indices]


    if (length(edital_links) > 0) {
      ## Loop para percorrer todos os editais do hospital ------

      for (i in 1:length(edital_links)) {
        edital_text <- edital_texts[i]
        edital_url <- edital_links[i]
        # Verificar URL do edital
        if (length(edital_url) > 0 && (!is.na(edital_url) && nchar(edital_url) > 0)) {
          # Construir e verificar link do PDF
          pdf_link <- sub("/view$", "/@@download/file", edital_url)
          cli_alert_success("Link do PDF: {.url {pdf_link}}") # Print pro console

          if (!is.na(pdf_link) && nchar(pdf_link) > 0) {
            # Tentar extrair texto do PDF, capturando erros
            try(
              {
                Sys.sleep(sample(10, 1) * 0.1) # Delay para evitar erros

                item_adicionado <- extrair_dados_edital(
                  pdf_link = pdf_link,
                  hospital_name = hospital_name,
                  microrregiao = microrregiao,
                  edital_text = edital_text
                )

                data <- append(data, item_adicionado)

                ## debug:
                print(as.data.table(item_adicionado))
              } # , silent = TRUE
            )
          }
        }
      }
    }

    ## Atualizar o link para a próxima página ----

    next_page_url <- get_next_page_url(hospital_page) # Atualiza o link para a próxima página usando o conteúdo HTML da página atual

    if (!is.null(next_page_url) && !is.na(next_page_url) && next_page_url != "") {
      current_page_url <- next_page_url
      cli_alert_info("Navegando para a próxima página: {.url {current_page_url}}")
    } else {
      cli_alert_info("Não há mais páginas para navegar.")
      break
    }
  }
  cli_progress_step("{j}/{length(selected_hospital_links)} hospitais processados.") # Atualizando progresso no console
}

# Fim do loop ----------

df <- bind_rows(data)
df2 <- df # Backup

df <- df %>%
  mutate(across(where(is.character), str_squish))

df$`Número do edital` <- as.numeric(df$`Número do edital`)

df <- df[order(df$`Número do edital`, decreasing = FALSE), ]

# Atualizando banco de dados: ----

apply(df, 1, function(row) {
  add_edital_if_new(
    row["Microrregião"],
    row["Hospital"],
    row["Número do edital"],
    row["Tipo de edital"],
    row["Edital"],
    row["Data"],
    row["Índice"],
    row["Cargo"],
    row["Obs. Cargo"],
    row["Posição"],
    row["Nome"],
    row["Obs. Colocado 1"],
    row["Obs. Colocado 2"],
    row["Ano"]
  )
})

tabela <- dbReadTable(db, "editais", check.names = FALSE) # Lendo banco de dados

# Fechar a conexão com o banco de dados
dbDisconnect(db)

# Importando pro excel e google sheets -----------------------

atualizar_planilha <- function(arquivo_excel = "Editais.xlsx",
                               excel = TRUE,
                               sheets = TRUE,
                               google_sheet_id = NULL,
                               planilha = "Convocados_interno") {
  
  # Depende da importação prévia da tabela com dbReadtable
  tabela <- tabela[order(tabela$`Número do edital`, decreasing = FALSE), ]
  
  if (excel) {
    if (file.exists(arquivo_excel)) {
      dados_excel <- readWorkbook(arquivo_excel, sheet = 1)
    } else {
      dados_excel <- data.frame() # Se o arquivo não existir, criar um DataFrame vazio
    }
    # Filtrar apenas os novos dados (não presentes no arquivo Excel)
    if (nrow(dados_excel) > 0) {
      names(dados_excel) <- names(tabela)
      dados_novos <- dplyr::anti_join(tabela, dados_excel, by = dplyr::join_by(Edital, Índice))
    } else {
      dados_novos <- tabela # Se o arquivo Excel estava vazio, todos os dados são novos
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
      writeData(wb,
        "Editais",
        dados_novos,
        startRow = ifelse(exists("dados_excel"), nrow(dados_excel) + 1, 1),
        colNames = FALSE
      )

      # Salvar o workbook
      saveWorkbook(wb, arquivo_excel, overwrite = TRUE)
      cat("Novos dados adicionados ao arquivo", arquivo_excel, ".\n")
    } else {
      cat("Não há novos dados para adicionar ao arquivo", arquivo_excel, ".\n")
    }
  }
  if (sheets) {
    dados_google_sheet <- read_sheet(google_sheet_id)
    if (nrow(dados_google_sheet) > 0) {
      dados_novos_google <- dplyr::anti_join(tabela, dados_google_sheet, by = dplyr::join_by(Edital, Índice))
    } else {
      dados_novos_google <- tabela # Se o Google Sheet estava vazio, todos os dados são novos
    }

    if (nrow(dados_novos_google) > 0) {
      # Adicionar novos dados ao Google Sheet
      sheet_append(google_sheet_id, dados_novos_google, sheet = planilha)
      cat("Novos dados adicionados ao Google Sheet.\n")
    } else {
      cat("Não há novos dados para adicionar ao Google Sheet.\n")
    }
  }
}

atualizar_planilha(sheets = FALSE) # Atualizar excel

atualizar_planilha(excel = FALSE, google_sheet_id = "https://docs.google.com/spreadsheets/d/1LxCUSgQmXJKCzJFKEQHyxPcBbJcSOT52-ghsEV1mmJQ/") # Atualizar google sheets
2
