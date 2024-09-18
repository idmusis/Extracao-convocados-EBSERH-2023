<h1 align="center">Extracao-convocados-EBSERH-2023</h1>

<p align="center">
	<img src="https://img.shields.io/badge/SQLite-%2307405e.svg?logo=sqlite&logoColor=white) alt="SQLite">
	<img src="https://img.shields.io/badge/R-276DC3.svg?style=default&logo=R&logoColor=white" alt="R">
</p>

Este repositório contém o código responsável pela extração e organização dos dados de convocações referentes ao [concurso nacional realizado pela EBSERH em 2023](https://www.gov.br/ebserh/pt-br/acesso-a-informacao/agentes-publicos/concursos-e-selecoes/concursos/2023/concurso-no-01-2023-ebserh-nacional). O processo inclui a coleta de informações a partir dos editais de convocação disponibilizados no site da EBSERH, a inserção desses dados em um banco de dados SQLite, e a exportação para planilhas Excel e Google Sheets.

---

## Planilha de Convocações

Uma planilha com os dados processados está disponível publicamente no seguinte link: [EBSERH 2023 - Editais de convocação](https://docs.google.com/spreadsheets/d/1LxCUSgQmXJKCzJFKEQHyxPcBbJcSOT52-ghsEV1mmJQ/)

---

## Detalhes do funcionamento

### 1. Conexão com o Banco de Dados

O script cria e conecta-se a um banco de dados SQLite chamado `editais.sqlite`.

### 2. Acesso aos PDFs de Convocação
O código acessa a página oficial da EBSERH, identifica os links relacionados às convocações de cada hospital e percorre todas as páginas disponíveis. Em cada página, são localizados os links dos PDFs contendo os editais de convocação. Serão processados apenas os editais que ainda não existirem em `editais.sqlite`.

### 3. Extração e Organização das Informações em Colunas
Após acessar os PDFs dos editais de convocação, o conteúdo é convertido em texto e segmentado em linhas para permitir a extração das informações.
O script varre cada linha do PDF e extrai as seguintes informações:

- **Microrregião**: Determinada com base no hospital de convocação.
- **Hospital**: Nome do hospital responsável pela convocação.
- **Número do Edital**: Número identificador do edital.
- **Tipo de Edital**: Categoria associada ao edital, como "Lista microrregional" ou "Convocação temporária". Extraído do nome do edital.
- **Edital**: O nome do arquivo do edital.
- **Data**: A data em que o edital foi publicado.
- **Índice**: Índice do cargo na convocação (ex.: `1.1`, `1.2`, etc.).
- **Cargo**: O cargo para o qual o convocado foi selecionado.
- **Obs. Cargo**: Informações adicionais sobre o cargo, caso estejam presentes.
- **Posição**: Colocação do candidato convocado conforme ordem de classificação.
- **Nome**: Nome do convocado.
- **Obs. Colocado 1 e 2**: Observações adicionais sobre a colocação do convocado (ex.: fim da fila, desistência, etc.).

As informações são extraídas utilizando padrões de texto e expressões regulares esperadas na formatação do edital.

### 4. Inserção no Banco de Dados
Após a extração e organização, o script verifica se os editais já foram processados anteriormente. Caso contrário, ele insere os novos dados no banco de dados SQLite, evitando a duplicação de registros. A conexão com o banco de dados é, então, fechada.

### 5. Exportação para Excel e Google Sheets
No final do script, temos a opção de exportação dos dados para uma planilha **Excel** ou uma planilha do **Google Sheets**. Caso queira utilizar sua própria planilha do Google, você pode configurar o **ID da Google Sheet** no script.

- **Atualizar a planilha Excel**: O script detecta novos dados e os adiciona à planilha `Editais.xlsx`.
    ```R
    atualizar_planilha(sheets = FALSE)  # Apenas exporta para Excel
    ```

- **Atualizar Google Sheets**: Se você quiser exportar os dados para sua própria Google Sheet, substitua o `google_sheet_id` no script conforme o seguinte comando:
    ```R
    atualizar_planilha(excel = FALSE, google_sheet_id = "SUA_GOOGLE_SHEET_ID")
    ```

---

## Dependências

Pacotes R utilizados: 
```R
install.packages(c("data.table", "DBI", "dplyr", "googlesheets4", "openxlsx", 
                   "pdftools", "RSQLite", "rvest", "stringr", "tidytable"))

```

---
##  Licença

Este projeto está licenciado sob a [Licença MIT](https://github.com/musiss/Extracao-convocados-EBSERH-2023/blob/main/LICENSE).

