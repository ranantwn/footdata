#' Load, Process, and Merge Datasets with Footnotes
#'
#' This function loads and processes CSV and Excel files from a specified directory.
#'
#' @param dataset_code A prefix for file identification.
#' @param base_dir The base directory where the output CSV file will be saved.
#' @return A data frame of the merged dataset with footnotes.
#' @export
#' @examples
#' footdata("ef_m_pry", "C:/Users/ANTONOU/Downloads")

footdata <- function(dataset_code, base_dir) {
  library(data.table)
  library(eurostat)
  library(tidyverse)
  library(readxl)
  library(xml2)
  library(stringr)

  cat("Starting footdata()...\n")
  cat("Dataset Code:", dataset_code, "\n")
  cat("Base Dir:", base_dir, "\n")

  dataset_csv_path <- file.path(base_dir, paste0(dataset_code, ".csv"))
  zip_file_path <- file.path(base_dir, paste0(dataset_code, "_metadata.zip"))
  extract_path <- file.path(base_dir, paste0(dataset_code, "_unzipped"))
  output_path <- file.path(base_dir, paste0(dataset_code, "_footnotes.csv"))

  cat("Dataset CSV Path:", dataset_csv_path, "\n")
  cat("ZIP File Path:", zip_file_path, "\n")
  cat("Extract Path:", extract_path, "\n")
  cat("Output Path:", output_path, "\n")

  dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

  # ---------------------------
  # Download or Load CSV Data
  # ---------------------------
  if (!file.exists(dataset_csv_path)) {
    cat("Downloading data...\n")
    dataset_csv <- get_eurostat(dataset_code, keepFlags = TRUE, update_cache = FALSE)
    if (nrow(dataset_csv) == 0) stop("Downloaded dataset is empty!")
    if (!"TIME_PERIOD" %in% names(dataset_csv)) stop("TIME_PERIOD column missing!")

    dataset_csv <- dataset_csv %>% mutate(TIME_PERIOD = format(as.Date(TIME_PERIOD), "%Y"))
    write.csv(dataset_csv, dataset_csv_path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("Dataset saved to:", dataset_csv_path, "\n")
  } else {
    cat("Loading existing data from CSV...\n")
    dataset_csv <- fread(dataset_csv_path)
  }

  cat("Loaded Data Preview:\n")
  print(head(dataset_csv))

  # ---------------------------
  # XML Metadata - Find ZIP URL
  # ---------------------------
  cat("Downloading Metadata XML...\n")
  url <- "https://ec.europa.eu/eurostat/api/dissemination/catalogue/toc/xml"
  xml_file <- read_xml(url)
  leaves <- xml_find_all(xml_file, "//nt:leaf", ns = xml_ns(xml_file))
  specific_leaf <- leaves[xml_text(xml_find_all(leaves, "nt:code", ns = xml_ns(xml_file))) == dataset_code]

  if (length(specific_leaf) == 0) stop(paste("No dataset found with Code =", dataset_code))

  extract_specific_data <- function(element) {
    ns <- xml_ns(xml_file)
    title <- xml_text(xml_find_first(element, "nt:title", ns = ns))
    sdmx_links <- xml_text(xml_find_all(element, "nt:metadata[@format='sdmx']", ns = ns))
    return(tibble(title = title, sdmx = list(sdmx_links)))
  }

  toc <- extract_specific_data(specific_leaf)
  zip_api_url <- toc %>% filter(str_detect(sdmx[[1]], "https?://.*\\.zip")) %>% pull(sdmx)

  if (length(zip_api_url) == 0) stop("No ZIP file URL found in metadata.")
  zip_api_url <- zip_api_url[[1]][1]

  cat("ZIP API URL:", zip_api_url, "\n")

  # ---------------------------
  # Download & Extract ZIP
  # ---------------------------
  if (!file.exists(zip_file_path)) {
    cat("Downloading ZIP...\n")
    download.file(zip_api_url, zip_file_path, mode = "wb")
    cat("ZIP downloaded to:", zip_file_path, "\n")
  } else {
    cat("ZIP already exists at:", zip_file_path, "\n")
  }

  if (!dir.exists(extract_path)) {
    cat("Extracting ZIP...\n")
    unzip(zip_file_path, exdir = extract_path)
    cat("ZIP extracted to:", extract_path, "\n")
  } else {
    cat("ZIP already extracted to:", extract_path, "\n")
  }

  # ---------------------------
  # Search for Annexes Folder
  # ---------------------------
  annexes_path <- file.path(extract_path, "Annexes")
  if (!dir.exists(annexes_path)) {
    cat("Annexes folder not found directly, searching in subfolders...\n")
    annexes_candidates <- list.dirs(extract_path, recursive = TRUE)
    annexes_path <- annexes_candidates[grepl("Annexes", annexes_candidates)][1]
    if (is.na(annexes_path)) stop("No Annexes folder found in extracted contents.")
    cat("Using Annexes folder at:", annexes_path, "\n")
  } else {
    cat("Annexes folder found at:", annexes_path, "\n")
  }

  # ---------------------------
  # Load Excel File with Footnotes
  # ---------------------------
  matched_excel_files <- list.files(annexes_path, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(matched_excel_files) == 0) {
    cat("No Excel files found in Annexes folder. Contents of Annexes folder:\n")
    print(list.files(annexes_path))
    stop("No Excel files found in Annexes folder.")
  }

  cat("Excel files found:\n")
  print(matched_excel_files)

  footnotes_excel <- read_excel(matched_excel_files[1], skip = 4, col_types = "text")
  cat("Footnotes Excel loaded successfully. Preview:\n")
  print(head(footnotes_excel))

  # --------------------------------------------
  # Process and Join footnotes with dataset
  # --------------------------------------------
  preprocess_df <- function(df) df %>% mutate(across(where(is.character), trimws))
  dataset_csv_processed <- preprocess_df(dataset_csv)
  footnotes_processed <- preprocess_df(footnotes_excel)

  if ("TIME_PERIOD" %in% names(dataset_csv_processed)) {
    dataset_csv_processed <- dataset_csv_processed %>% mutate(TIME_PERIOD = as.character(TIME_PERIOD))
  }
  if ("TIME_PERIOD" %in% names(footnotes_processed)) {
    footnotes_processed <- footnotes_processed %>% mutate(TIME_PERIOD = as.character(TIME_PERIOD))
  }

  matching_columns <- intersect(names(dataset_csv_processed), names(footnotes_processed))
  matching_columns <- setdiff(matching_columns, c("Comment", "values", "flags"))

  dataset_with_footnotes <- left_join(dataset_csv_processed, footnotes_processed, by = matching_columns)

  footnotes_wildcards <- footnotes_processed %>% filter(if_any(all_of(matching_columns), ~ . == "*"))

  for (i in seq_len(nrow(footnotes_wildcards))) {
    wildcard_row <- footnotes_wildcards[i, ]
    row_condition <- rep(TRUE, nrow(dataset_with_footnotes))

    for (col in matching_columns) {
      if (wildcard_row[[col]] != "*") {
        row_condition <- row_condition & (dataset_with_footnotes[[col]] == wildcard_row[[col]])
      }
    }

    dataset_with_footnotes$Comment[row_condition] <- ifelse(
      is.na(dataset_with_footnotes$Comment[row_condition]),
      wildcard_row$Comment,
      paste0(dataset_with_footnotes$Comment[row_condition], " ", wildcard_row$Comment)
    )
  }

  # --- Save Final Merged Output ---
  fwrite(dataset_with_footnotes, output_path, bom = TRUE)
  # Reorder columns: values, flags, then the rest
column_order <- c(
  setdiff(names(dataset_with_footnotes), c("values", "flags")),
  "values",
  "flags"
)
dataset_with_footnotes <- dataset_with_footnotes[, column_order, with = FALSE]
  cat("Merging completed successfully! Output saved at:", output_path, "\n")
}
