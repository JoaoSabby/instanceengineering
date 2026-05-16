###
# Perfil global do R para machine learning preditivo
# Sistema alvo: Oracle Linux ou Red Hat compativel
# Objetivo: aplicar configuracoes leves e seguras em qualquer sessao R
###

# Define opcoes globais leves
# Objetivo: padronizar comportamento do R sem carregar pacotes pesados
options(
 
  # Repositorio padrao do CRAN
  # Objetivo: padronizar instalacao de pacotes
  repos = c(
    source = "https://cloud.r-project.org",
    CRAN = "https://cloud.r-project.org"
  ),
    
  # Codificacao padrao para leitura/escrita de arquivos
  # Evita problemas com caracteres especiais (acentos, cedilha)
  encoding = "UTF-8",
  
  # Limite de expressoes avaliadas por chamada/loop
  # Padrao: 5000L, aumentado para pipelines e recursoes profundas
  expressions = 50000L,
  
  # Instalacao de pacotes sempre via codigo-fonte
  # Garante compatibilidade em servidores Linux e desempenho da compilacao para cpu
  pkgType = "source",
  
  # Tamanho de pagina padrao para saidas PDF/LaTeX/knitr
  # Usa A4 em vez do padrao americano "letter"
  papersize = "a4",
  
  # Timezone padrao
  # Objetivo: padronizar logs, datas de referencia e execucoes
  TZ = "America/Sao_Paulo",
  
  # Fetch em lotes do banco 
  # Objetivo: Reduzir alocacao de uma so vez na RAM
  dbi.fetch.size = 50000L,
  
  # CPUs usadas na instalacao de pacotes
  # Objetivo: acelerar compilacao sem usar todos os cores do servidor
  Ncpus = 2L,
  
  # Plano preferencial do future
  # Objetivo: usar multicore em Oracle Linux, pois apresentou melhor desempenho no ambiente
  future.plan = "sequential",
  
  # Cores padrao para funcoes baseadas em fork
  # Objetivo: evitar uso automatico de todos os cores
  mc.cores = 2L,
  
  # Limite para objetos globais exportados pelo future
  # Objetivo: permitir pipelines com objetos grandes e manter limite explicito
  future.globals.maxSize = (50L * 1024L^3L),
  
  # Tratamento de RNG em future
  # Objetivo: reduzir avisos quando a semente e controlada explicitamente
  future.rng.onMisuse = "ignore",
  
  # Tratamento de referencias externas em future
  # Objetivo: impedir envio acidental de conexoes e objetos nao serializaveis
  future.globals.onReference = "error",
  
  # Evento binario padrao para yardstick
  # Objetivo: alinhar metricas com o primeiro nivel do factor como evento
  yardstick.event_first = TRUE,
  
  # Mensagens do tidymodels
  # Objetivo: reduzir logs repetitivos
  tidymodels.quiet = TRUE,
  
  # Mensagens do tidyverse
  # Objetivo: reduzir logs repetitivos
  tidyverse.quiet = TRUE,
  
  # Mensagens do dplyr summarise
  # Objetivo: reduzir informacoes repetitivas no console
  dplyr.summarise.inform = FALSE,
  
  # Mensagens do dbplyr summarise
  # Objetivo: reduzir informacoes repetitivas no console
  dbplyr.summarise.inform = FALSE,
  
  # Maximo de linhas impressas por tibble
  # Objetivo: evitar saidas longas
  tibble.print_max = 50L,
  
  # Minimo de linhas impressas por tibble
  # Objetivo: manter resumo suficiente
  tibble.print_min = 10L,
  
  # Mensagens do lubridate
  # Objetivo: reduzir logs de parse de datas
  lubridate.verbose = FALSE,
  
  # Fallback do duckplyr
  # Objetivo: evitar coleta inesperada de dados grandes para memoria
  duckplyr_fallback_collect = FALSE,
  
  # Notacao cientifica
  # Objetivo: melhorar exibicao de codigos, identificadores e metricas
  scipen = 999L,
  
  # Digitos de exibicao
  # Objetivo: padronizar impressao de numeros
  digits = 4L,
  
  # Reparse de decimais em I/O
  # Objetivo: evitra reparse de decimais em operacoes de I/O numerico 
  OutDec = ".",
  
  # Digitos de exibicao em tibble
  # Objetivo: melhorar legibilidade de tabelas
  pillar.sigfig = 4L,
  
  # Largura do console
  # Objetivo: melhorar exibicao no RStudio e terminal
  width = 160L,
  
  # Politica de conflitos
  # Objetivo: nao interromper sessoes por conflitos conhecidos
  conflicts.policy = list(
    error = FALSE,
    warn = FALSE
  ),
  
  # Nivel de compilacao do R 
  # Objetivo: Maxima otimizacao na compilacao em tempo de execucao
  jit.level = 3L,
  
  # Conversao automatica de character em factor
  # Objetivo evitar conversao desnecessaria de character para factor na leitura 
  stringsAsFactors = FALSE,
  
  # Exibicao de warnings
  # Objetivo: ignorar todos os warnings para eliminar overhead de coleta/exibicao
  warn = 0L,
  
  # Pasta padrao de armazenamento dos pipelines targets
  targets.store = "_targets",
  
  # Backend para crew/clustermq
  clustermq.scheduler = "multiprocess"  
)

# Datas e Horarios
# Finalidade: Define nomes de meses e dias da semana para o português
# Reflexo: Ao usar funcoes como format(Sys.Date(), "%B"), o R retornara "Maio" em vez de "May"
# E seguro porque nao altera a estrutura lógica do objeto de data, apenas sua representacao textual
invisible(Sys.setlocale("LC_TIME", "pt_BR.UTF-8"))

# Ordenacao de Texto
# Finalidade: Define a regra de comparacao de caracteres (alfabeto)
# Reflexo: Garante que palavras com acentos (ex: agua, Zebra, Maca) sejam ordenadas corretamente 
# seguindo a lógica da lingua portuguesa. Sem isso, caracteres acentuados podem ir para o fim da lista
invisible(Sys.setlocale("LC_COLLATE", "pt_BR.UTF-8"))

# Formatacao Monetaria
# Finalidade: Define o simbolo da moeda e separadores de milhar/centavos para valores financeiros
# Reflexo: Afeta funcoes de exibicao monetaria, preparando o ambiente para mostrar "R$" se necessario
# Nao interfere em calculos, pois o R nao armazena o simbolo da moeda dentro da variavel numerica
invisible(Sys.setlocale("LC_MONETARY", "pt_BR.UTF-8"))

# Tamanho de Papel
# Finalidade: Define o padrao de saida fisica ou virtual (PDF/Impressao)
# Reflexo: Garante que relatórios gerados via Quarto ou RMarkdown assumam o padrao A4 (Brasil)
# em vez do padrao Letter (EUA). Evita cortes indesejados em tabelas de modelos preditivos
invisible(Sys.setlocale("LC_PAPER", "pt_BR.UTF-8"))

# Unidades de Medida
# Finalidade: Define o sistema de medidas (Metrico vs Imperial)
# Reflexo: Util em parâmetros de graficos (plots) que definem margens em centimetros
invisible(Sys.setlocale("LC_MEASUREMENT", "pt_BR.UTF-8"))

# O MOTOR NUMERICO (A linha mais importante)
# Finalidade: Mantem o separador decimal como ponto (standard internacional "C")
# Reflexo: Garante que o R continue entendendo que 1.5 e um numero e meio. 
# Isso evita conflitos com os compiladores C++ e Fortran e com o Oracle 19c
# E o que impede a quebra de pacotes como Tidymodels e targets
invisible(Sys.setlocale("LC_NUMERIC", "C"))

# Indica que carregou o .Rprofile
options(loadedRprofile = TRUE)

####
## Fim
#



## Fim
# 
