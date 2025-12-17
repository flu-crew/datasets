files <- c("C:/Users/garrett.janzen/OneDrive - USDA/Projects/Celeste_Flu54/Phylogenetics/input_H1.fasta",
           "C:/Users/garrett.janzen/OneDrive - USDA/Projects/Celeste_Flu54/Phylogenetics/input_H3.fasta") 
all_lines <- unlist(lapply(files, readLines, warn = FALSE))
headers <- grep("^>", all_lines, value = TRUE)
parts <- strsplit(headers, "\\|", fixed = FALSE)
get_state <- function(x) {
  pos <- match("United_States", x)
  if (!is.na(pos) && length(x) >= pos + 1)
    x[pos + 1]
  else
    NA_character_
}

states <- sapply(parts, get_state, USE.NAMES = FALSE)
states <- gsub("_", " ", states, fixed = TRUE)    # North_Carolina → North Carolina
states <- states[nzchar(states) & states != "United_States"]  # drop blanks / mis‑hits
unique_states <- sort(unique(states));unique_states
length(unique_states)

tempdf <- unlist(parts)
tempdf <- tempdf[grep("1B", tempdf, invert = FALSE)];tempdf
table(tempdf)
