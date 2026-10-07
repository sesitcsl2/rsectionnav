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

.section_level <- function(x) {
  m <- regexec("^\\s*(#+)", x, perl = TRUE)
  hit <- regmatches(x, m)[[1]]

  if (length(hit) >= 2) {
    nchar(hit[2])
  } else {
    NA_integer_
  }
}

.strip_strings_and_comments <- function(x) {
  # Remove ordinary quoted strings/backticks first, so # or braces
  # occurring inside them do not affect our structural depth.
  x <- gsub(
    "\"(?:\\\\.|[^\"\\\\])*\"|'(?:\\\\.|[^'\\\\])*'|`(?:\\\\.|[^`\\\\])*`",
    "",
    x,
    perl = TRUE
  )

  sub("#.*$", "", x)
}

.brace_depth_before <- function(contents) {
  depth <- integer(length(contents))
  current <- 0L

  for (i in seq_along(contents)) {
    depth[i] <- current

    code <- .strip_strings_and_comments(contents[i])

    chars <- strsplit(code, "", fixed = TRUE)[[1]]

    if (length(chars)) {
      current <- current +
        sum(chars == "{") -
        sum(chars == "}")

      current <- max(current, 0L)
    }
  }

  depth
}

.navigation_targets <- function(contents) {
  section_lines <- .section_lines(contents)
  block_lines <- .block_lines(contents)

  brace_depth <- .brace_depth_before(contents)

  targets <- list()
  current_section_level <- 0L

  for (i in seq_along(contents)) {

    if (i %in% section_lines) {
      section_level <- .section_level(contents[i])
      current_section_level <- section_level

      targets[[length(targets) + 1L]] <- data.frame(
        line = i,
        level = brace_depth[i] + section_level,
        type = "section"
      )
    }

    if (i %in% block_lines) {
      targets[[length(targets) + 1L]] <- data.frame(
        line = i,
        level = brace_depth[i] + current_section_level + 1L,
        type = "block"
      )
    }
  }

  if (!length(targets)) {
    return(data.frame(
      line = integer(),
      level = integer(),
      type = character()
    ))
  }

  out <- do.call(rbind, targets)
  rownames(out) <- NULL
  out
}

.navigation_lines <- function(contents) {
  sort(unique(c(
    .section_lines(contents),
    .block_lines(contents)
  )))
}

.paragraph_starts <- function(contents) {
  nonblank <- nzchar(trimws(contents))

  which(
    nonblank &
      c(TRUE, !head(nonblank, -1L))
  )
}

.paragraph_ends <- function(contents) {
  nonblank <- nzchar(trimws(contents))

  which(
    nonblank &
      c(!tail(nonblank, -1L), TRUE)
  )
}

.paragraph_targets <- function(contents) {
  sort(unique(c(
    .paragraph_starts(contents),
    .paragraph_ends(contents)
  )))
}

.first_text_column <- function(line) {
  pos <- regexpr("[^[:space:]]", line, perl = TRUE)

  if (pos < 1L) {
    1L
  } else {
    as.integer(pos)
  }
}

.end_text_column <- function(line) {
  line <- sub("[[:space:]]+$", "", line, perl = TRUE)
  nchar(line) + 1L
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

#' Jump to next sibling, climbing when necessary
#' @export
next_sibling <- function() {
  ctx <- rstudioapi::getSourceEditorContext()

  current_row <- as.integer(
    ctx$selection[[1]]$range$start[1]
  )

  targets <- .navigation_targets(ctx$contents)

  if (!nrow(targets)) {
    return(invisible(NULL))
  }

  # Normally the cursor is exactly on a target because our navigation
  # commands leave it there. Otherwise use the most recent target.
  current <- which(targets$line <= current_row)

  if (!length(current)) {
    rstudioapi::setCursorPosition(
      c(targets$line[1], 1),
      id = ctx$id
    )
    return(invisible(NULL))
  }

  current <- tail(current, 1)
  current_level <- targets$level[current]

  later <- seq.int(current + 1L, nrow(targets))

  if (!length(later)) {
    return(invisible(NULL))
  }

  # Same level = sibling.
  # Lower numeric level = climb to a higher structural level.
  candidate <- later[
    targets$level[later] <= current_level
  ]

  if (length(candidate)) {
    rstudioapi::setCursorPosition(
      c(targets$line[candidate[1]], 1),
      id = ctx$id
    )
  }

  invisible(NULL)
}

#' Jump to previous sibling, climbing when necessary
#' @export
previous_sibling <- function() {
  ctx <- rstudioapi::getSourceEditorContext()

  current_row <- as.integer(
    ctx$selection[[1]]$range$start[1]
  )

  targets <- .navigation_targets(ctx$contents)

  if (!nrow(targets)) {
    return(invisible(NULL))
  }

  current <- which(targets$line <= current_row)

  if (!length(current)) {
    return(invisible(NULL))
  }

  current <- tail(current, 1)
  current_level <- targets$level[current]

  if (current <= 1L) {
    return(invisible(NULL))
  }

  earlier <- seq_len(current - 1L)

  candidate <- earlier[
    targets$level[earlier] <= current_level
  ]

  if (length(candidate)) {
    target <- tail(candidate, 1)

    rstudioapi::setCursorPosition(
      c(targets$line[target], 1),
      id = ctx$id
    )
  }

  invisible(NULL)
}

#' Jump to next paragraph boundary
#' @export
next_paragraph <- function() {
  ctx <- rstudioapi::getSourceEditorContext()

  current_row <- as.integer(
    ctx$selection[[1]]$range$start[1]
  )

  starts <- .paragraph_starts(ctx$contents)
  ends <- .paragraph_ends(ctx$contents)
  targets <- sort(unique(c(starts, ends)))

  target <- targets[targets > current_row]

  if (length(target)) {
    row <- target[1]

    # A one-line paragraph is both start and end.
    # When travelling downward, treat it as a start.
    if (row %in% starts) {
      column <- .first_text_column(ctx$contents[row])
    } else {
      column <- .end_text_column(ctx$contents[row])
    }

    rstudioapi::setCursorPosition(
      c(row, column),
      id = ctx$id
    )
  }

  invisible(NULL)
}

#' Jump to previous paragraph boundary
#' @export
previous_paragraph <- function() {
  ctx <- rstudioapi::getSourceEditorContext()

  current_row <- as.integer(
    ctx$selection[[1]]$range$start[1]
  )

  starts <- .paragraph_starts(ctx$contents)
  ends <- .paragraph_ends(ctx$contents)
  targets <- sort(unique(c(starts, ends)))

  target <- targets[targets < current_row]

  if (length(target)) {
    row <- tail(target, 1)

    # A one-line paragraph is both start and end.
    # When travelling upward, treat it as an end.
    if (row %in% ends) {
      column <- .end_text_column(ctx$contents[row])
    } else {
      column <- .first_text_column(ctx$contents[row])
    }

    rstudioapi::setCursorPosition(
      c(row, column),
      id = ctx$id
    )
  }

  invisible(NULL)
}
