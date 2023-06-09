library(sloop)
library(glue)
library(lobstr)

get_info <- function(x) {
  get_obj_basics <- function(x) {
    x_chr <- deparse(substitute(x))
    x_class <- class(x)
    x_type <- typeof(x)
    x_mode <- mode(x)
    x_attr <- mode(x)
    return(list(
      "obj" = x_chr,
      "class" = x_class,
      "type" = x_type,
      "mode" = x_mode,
      "attr" = x_attr
    ))
  }
  obl <- get_obj_basics(x)
  
  get_obj_fun_info <- function(x) {
    if (is.function(x)) {
      x_fun <- sloop::ftype(x)
      if (length(x_fun) > 1) {
        obj_fun <- paste0(x_fun, collapse = ", ")
      } else {
        obj_fun <- x_fun
      }
      return(obj_fun)
    } else {
      NULL
    }
  }
  obj_fun <- get_obj_fun_info(x)

  get_attr <- function(x) {
    x_attr <- attributes(x)
    if (length(x_attr) > 1) {
      obj_attr <- paste0(names(x_attr), collapse = ", ")
    } else {
      obj_attr <- x_attr
    }
    return(obj_attr)
  }
  obj_attr <- get_attr(x)

  get_class <- function(obl) {
    if (length(obl[["class"]]) > 1) {
      obj_class <- paste0(obl[["class"]], collapse = ", ")
    } else {
      obj_class <- obl[["class"]]
    }
    return(obj_class)
  }
  obj_class <- get_class(obl = obl)

  get_type <- function(obl) {
    if (length(obl[["get_type"]]) > 1) {
      obj_type <- paste0(obl[["type"]], collapse = ", ")
    } else {
      obj_type <- obl[["type"]]
    }
    return(obj_type)
  }
  obj_type <- get_type(obl = obl)

  oi <- list(
    "obj" = deparse(substitute(x)),
    "obj_class" = obj_class,
    "obj_type" = obj_type,
    "obj_mode" = obl$mode,
    "obj_fun" = obj_fun,
    "obj_attr" = obj_attr
  )

  get_obj_info <- function(x) {
    oi <- x
    no_info <- purrr::map_lgl(oi, is.null)
    has_info <- oi[!no_info]
    # no obj_fun
    if (length(has_info[["obj_fun"]]) == 0) {
      glue::glue(
        "\n",
        "object: `{has_info$obj}`\n",
        "\tattributes => {has_info$obj_attr}\n",
        "\tclass => {has_info$obj_class}\n",
        "\ttype => {has_info$obj_type}\n",
        "\t*mode => {has_info$obj_mode}"
        
      )
      # no attributes
    } else if (length(has_info[["obj_attr"]]) == 0) {
      glue::glue(
        "\n",
        "object: {has_info$obj}\n",
        "\tfun type => {has_info$obj_fun}\n",
        "\tclass => {has_info$obj_class}\n",
        "\ttype => {has_info$obj_type}\n",
        "\t*mode => {has_info$obj_mode}\n"
      )
    }
  }
  get_obj_info(x = oi)
}