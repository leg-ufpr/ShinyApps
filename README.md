# ShinyApps - Aplicações Shiny para Ensino de Estatística

Recursos interativos são uma ferramenta extraordinária para o Ensino de
qualquer disciplina. Na Estatística, em especial, muitos conceitos podem
ser transmitos de forma visual por meio de gráficos e animações. No
entanto, tais recursos ainda são pouco explorados apesar das muitas
funcionalidades que o R tem para a contrução de aplicações que são
implementações disponíveis em vários pacotes: [`rpanel`], [`gWidgets`],
[`animation`], [`rgl`], [`googleVis`] e [`shiny`], por exemplo. Dentre
estes, o `shiny` tem a vatagem de permitir a criação e hospedagem de
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

Para usar localmente as aplicações você pode ter uma cópia dos
diretórios. Você pode clonar o repositório (recomendado), baixar o zip
com todo o conteúdo dele (viável) ou apenas copiar os arquivos da
aplicação que deseja usar (risco de esqueçer algo). Depois de ter o
diretório com todos os arquivos, faça em uma sessão R:

```r
# Carrega o pacote shiny.
library(shiny)

# Roda a aplicação density.
runApp("density")
```

## Organização

Cada aplicação Shiny neste repositório é um diretório na raiz. Dentro
destes diretório deve existir o par de arquivos `server.R` e `ui.R` ou o
arquivo `app.Rmd`. Arquivos auxiliares, como `global.R`, ou diretórios,
como o `www/`, podem ser criados conforme a necessidade das aplições.

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
Linux) para não haver problemas de codifição de caracteres.

Deve-se evitar arquivos que não sejam texto dentro do repositório, como
imagens, a menos que sejam indispensaveis para a aplicação. Arquivos que
são gerados pelas aplicações devem ser ignorados no `.gitignore`, como
os arquivos resultados de uma compilação LaTeX (.aux, .toc, etc).

<!------------------------------------------- -->

[`rpanel`]: https://cran.r-project.org/web/packages/rpanel/index.html
[`gWidgets`]: https://cran.r-project.org/web/packages/gWidgets/index.html
[`rgl`]: https://cran.r-project.org/web/packages/rgl/index.html
[`animation`]: https://cran.r-project.org/web/packages/animation/index.html
[`googleVis`]: https://cran.r-project.org/web/packages/googleVis/index.html
[`shiny`]: https://cran.r-project.org/web/packages/shiny/index.html
