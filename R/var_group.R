#' Get / Set a variable group
#'
#' @param x a vector or a data.frame
#' @param value a character string or `NULL` to remove the group
#'  For data frames, it could also be a named list or a character vector
#'  of same length as the number of columns in `x`.
#' @param unlist for data frames, return a named vector instead of a list
#' @details
#'   For data frames, if `value` is a named list, only elements whose name will
#'   match a column of the data frame will be taken into account. If `value`
#'   is a character vector, groups should in the same order as the columns of the
#'   data.frame.
#' @examples
#' var_group(iris$Sepal.Length)
#' var_group(iris$Sepal.Length) <- 'length group'
#' \dontrun{
#'  View(iris)
#' }
#' # To remove a variable group
#' var_group(iris$Sepal.Length) <- NULL
#' # To change several variable groups at once
#' var_group(iris) <- c(
#'   "length group", "width group", "length group",
#'   "width group", "species"
#')
#' var_group(iris)
#' var_group(iris) <- list(
#'   Petal.Width = "width group",
#'   Petal.Length = "length group"
#' )
#' var_group(iris)
#' var_group(iris, unlist = TRUE)
#'
#' @export
var_group <- function(x) {
  UseMethod("var_group")
}

#' @export
var_group.default <- function(x, unlist = FALSE) {
  attr(x, "group", exact = TRUE)
}

#' @export
var_group.data.frame <- function(x, unlist = FALSE) {
  r <- lapply(x, var_group)
  if (unlist) {
    r <- lapply(r, function(x) {if (is.null(x)) "" else x})
    unlist(r, use.names = TRUE)
  } else
    r
}

#' @rdname var_group
#' @export
`var_group<-` <- function(x, value) {
  UseMethod("var_group<-")
}

#' @export
`var_group<-.default` <- function(x, value) {
  if ((!is.character(value) & !is.null(value)) | length(value) >
    1)
    stop("`value` should be a single character string or NULL",
      call. = FALSE)
  attr(x, "group") <- value
  x
}

#' @export
`var_group<-.data.frame` <- function(x, value) {
  if ((!is.character(value) & !is.null(value)) & !is.list(value) |
    (is.character(value) & length(value) > 1 & length(value) != ncol(x)))
    stop("`value` should be a named list, NULL, a single character string or a character vector of same length than the number of columns in `x`",
      call. = FALSE)
  if (is.character(value) & length(value) == 1) {
    value <- as.list(rep(value, ncol(x)))
    names(value) <- names(x)
  }
  if (is.character(value) & length(value) == ncol(x)) {
    value <- as.list(value)
    names(value) <- names(x)
  }
  if (is.null(value)) {
    value <- as.list(rep(1, ncol(x)))
    names(value) <- names(x)
    value <- lapply(value, function(x) {
      x <- NULL
    })
  }

  if (!all(names(value) %in% names(x)))
    stop("some variables not found in x")

  value <- value[names(value) %in% names(x)]
  for (var in names(value)) var_group(x[[var]]) <- value[[var]]
  x
}

#' @rdname var_group
#' @param .data a data frame
#' @param ... name-value pairs of variable groups (see examples)
#' @param .groups variable groups to be applied to the data.frame,
#'   using the same syntax as `value` in `var_group(df) <- value`.
#' @param .strict should an error be returned if some groups
#'   doesn't correspond to a column of `x`?
#' @note
#'   `set_variable_groups()` could be used with \pkg{dplyr} syntax.
#' @return
#'  `set_variable_groups()` will return an updated copy of `.data`.
#' @examples
#' if (require(dplyr)) {
#'   # adding some variable groups
#'   df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
#'     set_variable_groups(s1 = "demography", s2 = "group1")
#'   var_group(df)
#'
#'   # removing a variable group
#'   df <- df %>% set_variable_groups(s2 = NULL)
#'   var_group(df$s2)
#'
#'   # Set groups from dictionary, e.g. as read from external file
#'   # One description is missing, one has no match
#'   description = tibble(
#'     name = c(
#'       "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
#'       "Something"),
#'     group = c(
#'       "length group", "width group",  "length group", "width group",
#'       "something")
#'   )
#'   var_groups <- setNames(as.list(description$group), description$name)
#'   iris_groupled <- iris %>%
#'     set_variable_groups(.groups = var_groups, .strict = FALSE)
#'   var_group(iris_groupled)
#'
#'   # defining variable groups derived from variable names
#'   if (require(snakecase)) {
#'     iris <- iris %>%
#'       set_variable_groups(.groups = to_sentence_case(names(iris)))
#'     var_group(iris)
#'   }
#' }
#'
#' @export
set_variables_group <- function(.data, ..., .groups = NA, .strict = TRUE) {
  if (!identical(.groups, NA)) {
    if (!.strict)
      .groups <- .groups[intersect(names(.groups), names(.data))]
    var_group(.data) <- .groups
  }
  values <- rlang::dots_list(...)
  if (length(values) > 0) {
    if (.strict & !all(names(values) %in% names(.data)))
      stop("some variables not found in .data")

    for (v in intersect(names(values), names(.data)))
      var_group(.data[[v]]) <- values[[v]]
  }

  .data
}
