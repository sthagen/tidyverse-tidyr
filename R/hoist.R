#' @examples
#' df <- tibble(
#'   character = c("Toothless", "Dory"),
#'   metadata = list(
#'     list(
#'       species = "dragon",
#'       color = "black",
#'       films = c("How to Train Your Dragon", "How to Train Your Dragon 2")
#'     ),
#'     list(
#'       species = "clownfish",
#'       color = "blue",
#'       films = c("Finding Nemo", "Finding Dory")
#'     )
#'   )
#' )
#' df %>% hoist(metadata, "species")
#' df %>% hoist(metadata, "films")
#' df %>% hoist(metadata, c("films", "species", "color"))
#' @export hoist
hoist <- function(df, col, components, ptype = list()) {
  stopifnot(is.character(components))

  col <- tidyselect::vars_pull(names(df), !!enquo(col))
  x <- df[[col]]

  pull <- map(x, ~ .x[intersect(names(.x), components)])

  new_cols <- transpose(pull)
  for (component in components) {
    n <- map_int(new_cols[[component]], length)
    if (all(n == 1))
      new_cols[[component]] <- vec_c(!!!new_cols[[component]], .ptype = ptype[[component]])
  }

  keep <- map(x, ~ .x[setdiff(names(.x), components)])
  n_left <- map_int(keep, length)

  # Reconstruct
  out <- append_df(df, new_cols, after = col, remove = FALSE)
  out[[col]] <- if (all(n_left) == 0) NULL else keep
  out
}
