# Splitting spec text into editable sections.
#
# The editor shows one block at a time (one measure, one latent, the
# screening block...). Rather than parse and re-serialize YAML, which would
# throw away comments, we find each block's lines and swap them out as text.

# Blocks whose children each get their own section.
container_blocks <- c("covariates", "measurement", "latents", "ema", "open_text")

# Returns a tibble with one row per section: id (like "measurement.phq9"),
# block, name, first and last line, and indent.
spec_sections <- function(lines) {
  is_key <- grepl("^ *[A-Za-z0-9_\"'.-]+ *:", lines) & !grepl("^ *#", lines)
  indent <- nchar(sub("^( *).*", "\\1", lines))
  key <- sub("^ *[\"']?([^\"':]+)[\"']? *:.*", "\\1", lines)

  # A key's lines run until the next key at the same or a smaller indent.
  section_end <- function(i) {
    later <- which(is_key & indent <= indent[i] & seq_along(lines) > i)
    last <- if (length(later) == 0) length(lines) else later[1] - 1
    while (last > i && grepl("^\\s*$", lines[last])) last <- last - 1
    last
  }

  top <- which(is_key & indent == 0)
  purrr::map_dfr(top, \(i) {
    end <- section_end(i)
    if (key[i] %in% container_blocks && end > i) {
      body <- (i + 1):end
      child_indent <- min(indent[body][is_key[body]], Inf)
      children <- body[is_key[body] & indent[body] == child_indent]
      if (length(children) > 0) {
        return(tibble::tibble(
          id = paste0(key[i], ".", key[children]), block = key[i], name = key[children],
          start = children, end = purrr::map_int(children, section_end), indent = child_indent
        ))
      }
    }
    tibble::tibble(id = key[i], block = key[i], name = key[i], start = i, end = end, indent = 0L)
  })
}

section_text <- function(lines, section) {
  part <- lines[section$start:section$end]
  paste(substring(part, pmin(section$indent + 1, nchar(part) + 1)), collapse = "\n")
}

replace_section <- function(lines, section, text) {
  new <- strsplit(text, "\n", fixed = TRUE)[[1]]
  new <- ifelse(new == "", "", paste0(strrep(" ", section$indent), new))
  c(lines[seq_len(section$start - 1)], new, lines[-seq_len(section$end)])
}
