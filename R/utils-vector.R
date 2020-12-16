
vec_types <- c(
  "logical",
  "integer",
  "double",
  "complex",
  "character",
  "raw",
  "list"
)
as_vector_fn <- function(type) {
  if (!type %in% vec_types) {
    abort("`type` must be a vector type")
  }
  switch(type,
    logical = as.logical,
    integer = as.integer,
    double = as.double,
    complex = as.complex,
    # FIXME: explicit rlang::as_character() should serialise input
    character = as.character,
    raw = as.raw,
    # Rewrap lists - Workaround for #32
    list = list,
    abort("Internal error in `as_vector()`: unexpected type")
  )
}
as_vector <- function(x, type) {
  as_vector_fn(type)(x)
}

new_vector_fn <- function(type) {
  if (!type %in% vec_types) {
    abort("`type` must be a vector type")
  }
  switch(type,
    logical = new_logical,
    integer = new_integer,
    double = new_double,
    complex = new_complex,
    character = new_character,
    raw = new_raw,
    list = new_list,
    abort("Internal error in `new_vector()`: unexpected type")
  )
}
new_vector <- function(type, length = 0L) {
  new_vector_fn(type)(length)
}
