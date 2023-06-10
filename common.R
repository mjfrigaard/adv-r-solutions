# pkgs --------------------------------------------------------------------
library(sloop)
library(glue)
library(lobstr)
library(rlang)
library(stringr)

co_box <- function(color, num, content = "Question or answer") {
  class <- switch(color,
    Q = "note",
    A = "tip",
    sm2021 = "warning",
    sm2022 = "caution",
    red = "important",
    stop("Invalid `type`", call. = FALSE)
  )
  switch(color,
    Q = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='false'}", "\n\n",
      "#### Q:", num, "\n\n",
      "::: {style='font-size: 1.10em;'}\n\n",
      "*", content, "*",
      "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    A = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='false'}", "\n\n",
      "#### A", num, "\n\n",
      "::: {style='font-size: 1.10em;'}\n\n",
      "*", content, "*",
      "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    sm2021 = cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='true'}", "\n\n",
      "#### Answer advRs 2021 A:", num, "\n\n",
      "::: {style='font-size: 1.10em;'}\n\n",
      "*", content, "*",
      "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    sm2022 =  cat(paste0(
      "\n",
      ":::: {.callout-", class, " collapse='true'}", "\n\n",
      "#### Answer advRs 2022 A:", num, "\n\n",
      "::: {style='font-size: 1.10em;'}\n\n",
      "*", content, "*",
      "\n\n",
      "::: \n\n",
      "::::", "\n"
    )),
    stop("Invalid `type`", call. = FALSE)
  )
}

# objects -----------------------------------------------------------------
a <- 1:10
b <- letters[1:10]
c <- c(TRUE, FALSE)
class_list <- list(int = a, chr = b, log = c)
class_posixct <- Sys.time()
class_date <- Sys.Date()
class_fct <- factor(substring("statistics", 1:3, 1:3), levels = letters[1:3])

#' Get object classes
#'
#' @param x 
#'
#' @return classes in a pretty format
#'
#' obj_class_type(x = a) 
#' obj_class_type(x = class_list) 
#' obj_class_type(x = mean) 
#' obj_class_type(x = `[`)
obj_class_type <- function(x) {
    x_class <- class(x)
    x_type <- typeof(x)
    x_class_type <- unique(c(x_class, x_type))
    
    if (length(x_class_type) > 1 ) {
        x_class_types <- paste0(x_class_type, collapse = "/")
        glue::glue("class/type: {x_class_types}")
    } else {
      glue::glue("class/type: {x_class_type}")
    }
}


#' Object address (memory location)
#'
#' @param x 
#'
#' @return 'address of the value that `x` points to'
#' 
#' @importFrom lobstr obj_addr obj_addrs
#' 
#' @examples 
#' obj_address(x = class_date) 
#' obj_address(x = class_list) 
#' obj_address(x = class_posixct)
#' obj_address(x = mean)
obj_address <- function(x) {
  if (!is.list(x)) {
    addr <- lobstr::obj_addr(x)
  } else {
    addr <- lobstr::obj_addrs(x)
  }
  unique_addr <- unique(addr)
  if (length(unique_addr) > 1) {
    glue::glue("address for {x}: {unique_addr}", .sep = ", ")
  } else if (length(unique_addr) == 1) {
    glue::glue("address: {unique_addr}")
  } else {
    NULL
  }
}

#' Object function info
#'
#' @param x 
#'
#' @return if function, return 'function type'
#' 
#' @importFrom sloop ftype
#' 
#' @examples 
#' obj_fun_info(x = mean) 
#' obj_fun_info(x = `[`) 
#' obj_fun_info(x = `<-`)
#' obj_fun_info(x = 1:10) 
obj_fun_info <- function(x) {
    if (is.function(x)) {
      x_fun <- sloop::ftype(x)
      
      if (length(x_fun) > 1) {
        x_funs <- paste0(x_fun, collapse = ', ')
        glue::glue_collapse(
          glue::glue("function type: {x_funs}"), 
          sep = "\n")
      } else {
        glue::glue("
          function type: {x_fun}
          ")
      }
    } else {
      glue::glue("
        function type: not a function
        ")
    }
}



#' Object attributes 
#'
#' @param x 
#'
#' @return attributes in a pretty format
#'
#' @examples
#' obj_attr(class_fct)
#' obj_attr(class_posixct)
#' obj_attr(class_date)
#' obj_attr(b)
obj_attr <- function(x) {
# get attribute names 
attr_nms <- unlist(names(attributes(x)))
# get attribute values 
attr_vals <- unlist(unname(attributes(x)))
  if (is.null(attributes(x))) {
    glue::glue("No attributes")
  } else if (length(attr_nms) == length(attr_vals)) {
    glue::glue("attr name: {attr_nms}
                attr values: {attr_vals}")
  } else if (length(attr_nms) != length(attr_vals)) {
    all_attr_nms <- paste0(attr_nms, collapse = ", ")
    all_attr_vals <- paste0(attr_vals, collapse = ", ")
    glue::glue_collapse(
      glue::glue("attr name: {all_attr_nms}
                  attr values: {all_attr_vals}"))
  } else {
    NULL
  }
}

#' Object info
#'
#' @param x object 
#'
#' @return class, type, address, function type (if function), and attributes
#' (names and values)
#' 
#'
#' @examples 
#' obj_info(a)
#' obj_info(b)
#' obj_info(c)
#' obj_info(class_list)
#' obj_info(class_fct)
#' obj_info(class_posixct)
#' obj_info(class_date)
obj_info <- function(x) {
  obj <- deparse(substitute(x))
  glue::glue_collapse(
    glue::glue("OBJECT: [{obj}] 
      {obj_class_type(x)}
      {obj_attr(x)}
      {obj_address(x)}
      {obj_fun_info(x)}"),
    sep = "\n---------------------------------------\n")
}


  
  



