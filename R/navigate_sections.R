.section_lines <- function(contents) {
  grep(
    "^\\s*#.*(?:-{4,}|={4,}|#{4,})\\s*$",
    contents,
    perl = TRUE
  )
}


.block_lines <- function(contents) {
  grep(
    "^\\s*\\{\\s*(?:#.*)?$",
    contents,
    perl = TRUE
  )
}


.navigation_lines <- function(contents) {
  sort(unique(c(
    .section_lines(contents),
    .block_lines(contents)
  )))
}

#' Jump to next section or standalone block
#' @export
next_section <- function() {
  ctx <- rstudioapi::getSourceEditorContext()

  current_row <- as.integer(
    ctx$selection[[1]]$range$start[1]
  )

  targets <- .navigation_lines(ctx$contents)
  target <- targets[targets > current_row]

  if (length(target)) {
    rstudioapi::setCursorPosition(
      c(target[1], 1),
      id = ctx$id
    )
  }
}

#' Jump to previous section or standalone block
#' @export
previous_section <- function() {
  ctx <- rstudioapi::getSourceEditorContext()

  current_row <- as.integer(
    ctx$selection[[1]]$range$start[1]
  )

  targets <- .navigation_lines(ctx$contents)
  target <- targets[targets < current_row]

  if (length(target)) {
    rstudioapi::setCursorPosition(
      c(tail(target, 1), 1),
      id = ctx$id
    )
  }
}
