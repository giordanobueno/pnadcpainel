# pnadcpainel: Painel Consolidado da PNAD Contínua (IBGE)

[![R-CMD-check](https://img.shields.io/badge/R-package-blue.svg)](https://github.com/giordanobueno/pnadcpainel)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

O pacote **`pnadcpainel`** automatiza o download de microdados da **PNAD Contínua (IBGE)** via pacote `PNADcIBGE`, aplica a metodologia de identificação longitudinal de domicílios e indivíduos do **Data Zoom (PUC-Rio)**, cruza a base trimestral com a base de Visita 1 (características domiciliares, programas sociais e renda domiciliar per capita) e realiza diagnóstico e balanceamento do painel.

---

## 📌 Também disponível em Python

Este pacote possui uma versão equivalente oficial em **Python**:
👉 **[giordanobueno/pnadcpainel-py (Versão Python)](https://github.com/giordanobueno/pnadcpainel-py)**

A sintaxe, os nomes de funções e parâmetros foram mantidos idênticos para que a transição entre R e Python seja simples e direta.

---

## 🚀 Instalação

Você pode instalar o **`pnadcpainel`** diretamente do GitHub usando o `remotes` ou `devtools`:

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("giordanobueno/pnadcpainel", force = TRUE)
```

---

## 💡 Exemplo de Uso Rápido

### 1. Painel Anual (Compatibilidade Retroativa)
```r
library(pnadcpainel)

# Gerar painel retangular e balanceado para o ano de 2023 (utilizando variaveis essenciais padrao)
painel_2023 <- gerar_painel_pnadc(ano = 2023)

# Visualizar as primeiras linhas
head(painel_2023)

# Inspecionar a tabela de diagnostico de preenchimento de colunas
diag_2023 <- diagnosticar_painel(painel_2023)
print(diag_2023)
```

### 2. Painel Mensalizado (Mês Exato + Pesos Calibrados)
Para obter os microdados identificando o **mês exato de referência** e os **pesos amostrais calibrados** (metodologia Hecksher & Barbosa, 2026 / `{PNADCperiods}`):

```r
# Gerar painel mensalizado para o ano de 2023
painel_mensal <- gerar_painel_pnadc_mensal(ano = 2023)

# Colunas criadas: mes_exato_aaaamm e peso_mensal
head(painel_mensal[, c("id_dom", "id_ind", "mes_exato_aaaamm", "peso_mensal")])
```

### 2. Painel Longitudinal Multi-Ano
Para obter um único painel longitudinal cobrindo uma sequência de anos (por exemplo, 2020 a 2025):

```r
# Gerar um único painel longitudinal com 24 trimestres consecutivos (2020 T1 até 2025 T4)
painel_multi <- gerar_painel_pnadc(anos = 2020:2025)

# Ou passando um vetor explícito de anos:
painel_multi <- gerar_painel_pnadc(anos = c(2020, 2021, 2022, 2023, 2024, 2025))
```

---

## 🗓️ Painel Longitudinal Multi-Ano

A extensão multi-ano permite acompanhar indivíduos e domicílios ao longo de extensas janelas temporais sem criar artificialmente "fronteiras de painel" na virada de ano:

- **Unidade de Observação**: A unidade de observação é **indivíduo × ano × trimestre** (`id_ind × Ano × Trimestre`).
- **Múltiplas Observações**: O mesmo indivíduo (`id_ind`) aparece em múltiplos trimestres em que esteve presente na amostra. **Não é realizada nenhuma deduplicação por `id_ind`**.
- **Continuidade T4 → T1**: A virada do 4º trimestre de um ano para o 1º trimestre do ano seguinte (`2020 T4 → 2021 T1`) é tratada como um par de períodos consecutivos sem quebrar o painel.
- **Painel Não Balanceado**: A ausência de observação em um trimestre não elimina o indivíduo. Indivíduos presentes em apenas 1, 2 ou alguns trimestres são totalmente preservados. Não são criadas linhas artificiais com `NA` para períodos ausentes.
- **Semântica do `balancear`**: O parâmetro `balancear` refere-se estritamente ao tratamento de dados ausentes nas variáveis de Visita 1 (habitação) e **não exige presença do indivíduo em todas as 5 entrevistas**.
- **Variável Auxiliar `periodo`**: É gerada a coluna `periodo` (ex.: `"2020_1"`, `"2020_4"`, `"2021_1"`) indicando a sequência temporal cronológica.

> ⚠️ **Nota Metodológica sobre Pesos Amostrais**:
> A quantidade de observações de uma pessoa ao longo do painel **não altera seu peso amostral**. Ter 5 observações da Pessoa A e 1 observação da Pessoa B **não significa** `peso A = 5 × peso B`. Estimativas populacionais ou inferências com amostragem complexa devem tratar os pesos amostrais e a estrutura longitudinal separadamente.

---

## 🎨 Customização de Variáveis

O usuário pode escolher exatamente quais colunas deseja importar através dos parâmetros `vars_tri` (base trimestral) e `vars_visita` (base de habitação/Visita 1):

### 1. Importar Apenas Variáveis Específicas (Recomendado para Economia de Memória)
As colunas de identificação necessárias para o painel (`id_dom`, `id_ind`, `UPA`, `V1008`, etc.) são incluídas automaticamente, mesmo que você especifique apenas as suas variáveis de interesse:

```r
painel_custom <- gerar_painel_pnadc(
  ano = 2023,
  vars_tri = c("V2009", "VD4002", "VD4020"),      # Idade, ocupacao e rendimento
  vars_visita = c("VD5002", "V5002A", "S01006")  # Renda per capita, Bolsa Familia e dormitorios
)
```

### 2. Importar TODAS as Variáveis da PNAD Contínua
Para importar todos os microdados sem nenhuma restrição de colunas, passe `"todas"` ou `"all"`:

```r
painel_completo <- gerar_painel_pnadc(
  ano = 2023,
  vars_tri = "todas",
  vars_visita = "todas"
)
```

---

## 🔬 Metodologia

A construção dos identificadores únicos de domicílio (`id_dom`) e indivíduo (`id_ind`) segue a metodologia desenvolvida pelo **Data Zoom (Departamento de Economia da PUC-Rio)** para acompanhamento longitudinal da PNAD Contínua:

- **`id_dom`**: Combinação de `UPA` + `V1008` (número do domicílio) + `V1014` (painel).
- **`id_ind`**: Combinação de `id_dom` + dia de nascimento (`V2008`) + mês de nascimento (`V20081`) + ano de nascimento (`V20082`) + sexo (`V2007`) + `UF`.

> **Nota de Isenção**: Este pacote é uma implementação própria em R inspirada na metodologia do Data Zoom para conveniência de montagem de painéis, e não um produto ou pacote oficial do projeto Data Zoom. Para a suíte oficial de ferramentas do Data Zoom em Stata, R e Python, acesse [datazoom.puc-rio.br](https://www.econ.puc-rio.br/datazoom/).

---

## ⚠️ Limitações Conhecidas & Descompasso Temporal

A PNAD Contínua acompanha cada domicílio em **5 entrevistas trimestrais consecutivas**. Contudo, existem diferenças de periodicidade entre os temas da pesquisa:

1. **Base Trimestral**: Coletada a cada trimestre (4 trimestres por ano) com dados de mercado de trabalho, ocupação, renda de todos os trabalhos e composição demográfica.
2. **Base de Visita 1 (Habitação/Anual)**: Coletada apenas na **primeira entrevista** do domicílio (Visita 1). Contém informações estruturais da casa (água, lixo, dormitórios), recebimento de programas sociais (Bolsa Família, BPC) e renda domiciliar per capita (`VD5002`).

### Consequências no Cruzamento:
- Um domicílio acompanhado na base trimestral durante um determinado trimestre pode ter realizado sua Visita 1 em um ano anterior ou período fora da janela baixada, resultando em valores ausentes (`NA`) após o `left_join`.
- A perda de dados **não é uniforme entre as variáveis** — algumas colunas da Visita 1 possuem maior taxa de emparelhamento do que outras.

### Estratégia de Balanceamento (`balancear`):
- Por padrão (`balancear = TRUE`), o pacote filtra o painel final para manter **apenas as observações completas em todas as variáveis de habitação/Visita 1**.
- Isso produz um **painel retangular sem `NA`s** nas variáveis de habitação, ao custo de uma redução amostral (reportada no console via `mensagem_diagnostico()`).
- Caso deseje manter a amostra total com valores `NA` para tratamento próprio (ex.: imputação ou análises específicas), defina `balancear = FALSE`:

```r
painel_bruto <- gerar_painel_pnadc(ano = 2023, balancear = FALSE)
```

---

## 💾 Gestão de Memória RAM

Microdados da PNAD Contínua ocupam volume significativo de memória. O pacote implementa boas práticas automáticas de otimização:

1. **Restrição de Variáveis**: Utilize apenas as colunas necessárias passando vetores customizados nos parâmetros `vars_tri` e `vars_visita`.
2. **Downcasting Automático**: Converte colunas numéricas categóricas para inteiros de 32-bits, reduzindo o uso de RAM pela metade.
3. **Modo `low_memory = TRUE`**: Para máquinas com memória RAM limitada (ex.: <= 8 GB), ative `low_memory = TRUE`. Isso processará trimestre a trimestre salvando dados intermediários em disco temporário (`tempdir()`) em vez de acumular em memória.

```r
painel_otimizado <- gerar_painel_pnadc(ano = 2023, low_memory = TRUE)
```

---

## 📄 Licença

Este projeto está licenciado sob a Licença MIT - veja o arquivo [LICENSE](LICENSE) para mais detalhes.
