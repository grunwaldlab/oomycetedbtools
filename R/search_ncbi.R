parse_ft <- function(text) {
  text <- gsub(text, pattern = "\n\t\t\t", replacement = "\t", fixed = TRUE)
  parts <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
  part_data <- lapply(parts, function(x) {
    first_line <- sub(x, pattern = "\\n.+$", replacement = "")
    acc <- stringr::str_match(first_line, pattern = "\\|(.+)\\|")[,2]
    the_rest <- sub(x, pattern = "^>.+?\\n", replacement = "")
    output <- readr::read_tsv(the_rest, col_names = c("start", "end", "type", "info", "name"))
    output <- tibble::as_tibble(cbind(list(acc = acc), output, stringsAsFactors = FALSE))
  })
  output <- dplyr::bind_rows(part_data)
  output$complete <- ifelse(startsWith(output$start, "<") | startsWith(output$end, ">"), FALSE, TRUE)
  output$start <- as.integer(gsub(output$start, pattern = "<", replacement = ""))
  output$end <- as.integer(gsub(output$end, pattern = ">", replacement = ""))
  return(output)
}

parse_seqs <- function(text) {
  xml <- xml2::read_xml(text)
  tibble(acc = xml_text(xml_find_all(xml, "//TSeq_accver")),
         seq = xml_text(xml_find_all(xml, "//TSeq_sequence")),
         header = xml_text(xml_find_all(xml, "//TSeq_defline")),
         length = xml_text(xml_find_all(xml, "//TSeq_length")))
}


#' @export
get_isolate_seqs <- function(names, isolates, gene, must_be_complete = FALSE, pause = 0.5) {

  get_one <- function(name, islolate, gene) {

    # Wait a bit so NCBI doesnt get unhappy
    Sys.sleep(pause)

    # Search for sequences
    query <- paste0('"', name, '"[Organism] AND "', islolate, '"[Isolate] AND "', gene, '"[All Fields]')
    search <- entrez_search(db, term = query)
    if (length(search$ids) == 0) {
      return(NULL)
    }

    # Parse features
    features <- parse_ft(entrez_fetch(db, id = search$ids, rettype = "ft", retmode = "text"))
    features <- features[features$name == gene, ]
    if (! gene %in% features$name) {
      return(NULL)
    }
    if (must_be_complete) {
      features <- features[features$complete, ]
    }
    if (nrow(features) == 0) {
      return(NULL)
    }

    # Parse sequences
    sequences <- parse_seqs(entrez_fetch(db, id = search$ids, rettype = "fasta", retmode = "xml"))

    # Join feature and sequence data
    feature_data <- dplyr::left_join(features, sequences, by = "acc")

    # Subset sequences to fetures
    feature_data$seq <- substr(feature_data$seq, feature_data$start, feature_data$end)

    # Add query info
    feature_data <- tibble::as_tibble(cbind(list(species = name, islolate = islolate, gene = gene), feature_data, stringsAsFactors = FALSE))

    return(feature_data)
  }

  dplyr::bind_rows(purrr::pmap(list(names, isolates, gene), get_one))

}
