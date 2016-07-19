# ShinyApps - Aplicações Shiny para Ensino de Estatística

Recursos interativos são uma ferramenta extraordinária para o Ensino de
qualquer disciplina. Na Estatística, em especial, muitos conceitos podem
ser transmitido de forma visual por meio de gráficos e animações. No
entanto, tais recursos ainda são pouco explorados apesar das muitas
funcionalidades que o R tem para a construção de aplicações que são
implementações disponíveis em vários pacotes: [`rpanel`], [`gWidgets`],
[`animation`], [`rgl`], [`googleVis`] e [`shiny`], por exemplo. Dentre
estes, o `shiny` tem a vantagem de permitir a criação e hospedagem de
aplicações na web, aumentando assim o acesso das pessoas, inclusive
daqueles que não sabem usar o R.

Esse repositório destina-se a organizar e disponibilizar o código fonte
de aplicações Shiny voltadas para o Ensino de Estatística, seja fazendo
a exibição de conceitos de uma forma iterativa ou fornecendo aplicações
que automatizem tarefas façam análise ou visualização de dados.

As aplicações aqui mantidas podem ser utilizadas acessando o endereço
`http://shiny.leg.ufpr.br/<1>/<2>`, em que `<1>` é o nome do usuário
(e.g. walmes) e `<2>` é o nome da aplicação.

<!-- TODO: Criar um usuário geral na servidora do Shiny. -->

## Como usar

Você pode usar as aplicações Shiny desse repositório de 3 formas:

  1. Clonando o repositório.
  2. Baixando um zip do repositório.
  3. Executando a aplicação a partir do fonte no GitHub.

Na tela inicial do repositório, que exibe este arquivo README, existe um
botão do lado direito escrito `Clone or download`. Clique nele para
escolher uma das opções.

Para clonar o repositório você precisa ter o Git instalado. Aí basta
clonar o projeto com:

```
# Clonando o repositório.
git clone https://github.com/leg-ufpr/ShinyApps.git
```

Caso não queira usar o Git, baixe o zip com o conteúdo do repositório e
descompacte para ter acesso às aplicações.

Para executar as aplicações da cópia local do repositório (por clone ou
download) faça em uma sessão R:

```r
# Carrega o pacote shiny.
library(shiny)

# Roda a aplicação density.
runApp("density")
```

Lembre-se de atribuir corretamente o diretório de trabalho para que a
aplicação seja encontrada pela função. Use `setwd()`. Você pode também
usar o caminho completo do diretório na aplicação na função `runApp()`.

Por último, caso não queira fazer uma cópia local, você pode executar as
aplicações a partir do código fonte no GitHub. O fragmento abaixo mostra
como executar a aplicação `density` sem precisar baixar (diretamente) os
arquivos.

```r
# Carrega o pacote shiny.
library(shiny)

# Roda a aplicação density acessando os fontes no GitHub.
runGitHub(repo = "ShinyApps",
          username = "leg-ufpr",
          subdir = "density/")
```

## Organização

Cada aplicação Shiny neste repositório é um diretório na raiz. Dentro
destes diretório deve existir o par de arquivos `server.R` e `ui.R` ou o
arquivo `app.Rmd`. Arquivos auxiliares, como `global.R`, ou diretórios,
como o `www/`, podem ser criados conforme a necessidade das aplicações.

As definições globais, por outro lado, devem estar no diretório
`GLOBAL/`, como os arquivos de estilo (`*.css`), cabeçalho
(`header.html`) e rodapé (`footer.html`) das aplicações.

Na criação dos arquivos, o guia de estilo padrão R descrito por
[Hadley](http://adv-r.had.co.nz/Style.html) e presente no
[lintr](https://github.com/jimhester/lintr) deve ser seguido. Para
verificar a consistência com o guia de estilo, execute em uma sessão R o
seguinte:

```r
library(lintr)

lint(filename = "ui.R",
     linters = with_defaults(commented_code_linter = NULL))

lint(filename = "server.R",
     linters = with_defaults(commented_code_linter = NULL))
```

Os arquivos neste repositório devem ter codificação UTF-8 (padrão no
Linux) para não haver problemas de codificação de caracteres.

Deve-se evitar arquivos que não sejam texto dentro do repositório, como
imagens, a menos que sejam indispensáveis para a aplicação. Arquivos que
são gerados pelas aplicações devem ser ignorados no `.gitignore`, como
os arquivos resultados de uma compilação LaTeX (.aux, .toc, etc).

<!------------------------------------------- -->

[`rpanel`]: https://cran.r-project.org/web/packages/rpanel/index.html
[`gWidgets`]: https://cran.r-project.org/web/packages/gWidgets/index.html
[`rgl`]: https://cran.r-project.org/web/packages/rgl/index.html
[`animation`]: https://cran.r-project.org/web/packages/animation/index.html
[`googleVis`]: https://cran.r-project.org/web/packages/googleVis/index.html
[`shiny`]: https://cran.r-project.org/web/packages/shiny/index.html
