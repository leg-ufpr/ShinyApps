where_save <- function(name, file, append = FALSE) {
    # Faz arquivo ter o nome do objeto.
    if (missing(file)) {
        file <- sprintf("%s.R", name)
    }
    # Onde salvar.
    if (!is.na(file)) {
        if (file.exists("DESCRIPTION")) {
            file <- sprintf("R/%s", file)
        }
        if (!append & file.exists(file)) {
            stop(sprintf(paste("File `%s` already exists.",
                               "Use append = TRUE or remove it."),
                         file))
        }
    }
    return(c(file = file))
}

# where_save("fun")
# where_save("fun", file = "as.R")
# where_save("fun", file = "func.R", append = FALSE)
# where_save("fun", file = "func.R", append = TRUE)

#' @name roxy_data
#' @export
#' @title Gera o Esqueleto de Documentação de Datasets em
#'     \code{roxygen2}
#' @description Função que recebe uma conjunto de dados e produz o
#'     esquelo da documentação em \code{roxygen2}. Opções dessa função
#'     permitem escrever o esqueleto em arquivo, adicionar campos e
#'     abrir o arquivo gerado com algum editor de texto.
#' @param object Um objeto que armazena dados. Classes compreendidas são
#'     \code{data.frame} e vetores.
#' @param file Um nome de arquivo onde escrever a documentação do
#'     conjunto de dados. Quando não fornecido, o nome do objeto é usado
#'     como nome do arquivo. Se for usado \code{NA}, nenhum arquivo será
#'     criado e a documentação será exibida no console.
#' @param source String que é a referência bibliográfica do conjunto de
#'     dados.
#' @param keywords Um vetor com keywords para o conjunto de dados que
#'     serão escritas no campo \code{@keywords}.
#' @param author O autor do conjunto de dados. Este conteúdo será
#'     passado para o campo \code{@author}.
#' @param extra Vetor com o conteúdo de campos extras como
#'     \code{"@import lattice"}.
#' @param editor Nome do editor com o qual abrir o arquivo para fazer o
#'     preenchimento dos campos da documentação. Veja
#'     \code{\link[utils]{edit}}.
#' @param print Valor lógico que indica se deve imprimir no console o
#'     esqueleto de documentação gerado.
#' @param append Valor lógico que indica se deve escrever a documentação
#'     em arquivo que já existe.
#' @param find_file Valor lógico que indica se deve exibir no console o
#'     caminho do arquivo gerado com a documentação.
#' @return Essa função não retorna conteúdo mas cria/modifica arquivos.
#' @examples
#'
#' s <- "Smith; Sanders (1234)"
#' file.remove("iris.R")
#' roxy_data(iris,
#'          print = TRUE,
#'          source = s,
#'          editor = "emacs",
#'          keywords = c("BLA", "BLU"),
#'          find_file = TRUE,
#'          extra = c("@docType dataset",
#'                    "@details bla bla bla"))
#'
roxy_data <- function(object,
                      file,
                      source = NULL,
                      keywords,
                      author,
                      extra,
                      editor,
                      print = FALSE,
                      append = FALSE,
                      find_file = FALSE) {
    # Nome do objeto.
    name <- deparse(substitute(object))
    file <- where_save(name, file, append)
    # Determina a classe.
    cld <- class(object)[1]
    # Esqueleto do @format para cada classe.
    frmat <- switch(cld,
                    "data.frame" = {
                        f <- sprintf(paste(
                            "@format Um \\code{data.frame} com %d",
                            "observações e %d variáveis, em que"),
                            nrow(object), ncol(object))
                        f <- strwrap(f, width = 69)
                        f <- c(f[1], paste("    ", f[-1]))
                        c(f,
                          "", "\\describe{", "",
                          rbind(sprintf("\\item{\\code{%s}}{  }",
                                        names(object)), ""),
                          "}")
                    },
                    "numeric" = {
                        sprintf(
                            "@format Um vetor com %d elementos.",
                            length(object))
                    },
                    "matrix" = {
                        sprintf(
                            "@format Uma matriz de dimensão %d por %d.",
                            nrow(object), ncol(object))
                    },
                    stop(paste("`object` de classe não apropriada",
                               "para a função.")))
    # Conteúdo da documentação.
    ctnt <- c(sprintf("@name %s", name),
              if (!missing(author)) {
                  sprintf("@author %s", author)
              },
              "@title",
              "@description",
              frmat,
              if (!missing(extra)) {
                  extra
              },
              if (!missing(keywords)) {
                  paste(c("@keywords", keywords), collapse = " ")
              },
              if (!is.null(source)) {
                  s <- strwrap(paste("@source", source),
                               width = 69)
                  c(s[1], paste("    ", s[-1]))
              },
              "@examples")
    # Evitar espaço no final de linhas vazias.
    ctnt <- c(paste(ifelse(ctnt == "", "#\'", "#\' "),
                    ctnt, sep = ""), "NULL")
    if (is.na(file)) {
        cat(ctnt, sep = "\n")
    } else {
        # Exporta a documentação para o arquivo.
        cat(ctnt, sep = "\n", file = file, append = append)
        if (print) {
            cat(ctnt, sep = "\n")
        }
        # Abre arquivo no editor.
        if (!missing(editor)) {
            editor <- match.arg(arg = editor,
                                choices = c("vi",
                                            "emacs",
                                            "pico",
                                            "xemacs",
                                            "xedit"))
            do.call(editor, list(file = file))
        }
    }
    if (find_file) {
        cat(sprintf("(find-file \"%s\")",
                    paste(path.expand(getwd()), file, sep = "/")),
            "\n")
    }
    invisible()
}
