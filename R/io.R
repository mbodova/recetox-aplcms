#' Write tibble to parquet file with metadata
#'
#' Writes a tibble to a parquet file, preserving all attributes as metadata.
#' Attributes are stored as key-value pairs in the arrow table schema.
#'
#' @param tibble A tibble to write
#' @param path Path to the output parquet file
#' @return Invisibly returns the result of [write_arrow()]
#' @export
#' @examples
#' \dontrun{
#' tb <- tibble::tibble(x = 1:5, y = letters[1:5])
#' write_tibble(tb, "output.parquet")
#' }
write_tibble <- function(tibble, path) {
  attrs <- attributes(tibble)
  attrs <- attrs[names(attrs) %notin% c("class", "row.names", "names")]
  metadata <- lapply(attrs, paste0, character(1), collapse = ",")
  write_arrow(tibble, path, metadata)
}

#' Write data frame with metadata to parquet file
#'
#' Creates an Arrow table from a data frame and optionally adds metadata
#' to the table schema before writing to parquet.
#'
#' @param data_frame A data frame to write
#' @param path Path to the output parquet file
#' @param metadata Optional named list of metadata key-value pairs to embed
#' @return Invisible NULL (side effect is writing the file)
#' @export
#' @importFrom arrow Table schema write_parquet
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' meta <- list(author = "test", date = "2024-01-01")
#' write_arrow(df, "output.parquet", meta)
#' }
write_arrow <- function(data_frame, path, metadata = NA) {
  tabular <- arrow::Table$create(data_frame)

  if (!any(is.na(metadata))) {
    table_schema <- arrow::schema(tabular)$WithMetadata(metadata)
    tabular <- tabular$cast(table_schema)
  }

  arrow::write_parquet(tabular, path)
}

#' Read data from parquet file
#'
#' Reads a parquet file and returns an Arrow table.
#'
#' @param filepath Path to the input parquet file
#' @return An Arrow table (can be converted to data frame with `as.data.frame()`)
#' @export
#' @importFrom arrow read_parquet
#' @examples
#' \dontrun{
#' tbl <- read_arrow("data.parquet")
#' df <- as.data.frame(tbl)
#' }
read_arrow <- function(filepath) {
  arrow::read_parquet(filepath, as_data_frame = FALSE)
}

#' Decode _xHHHH_ escapes in mzML IDs
#'
#' mzML producers (e.g. ProteoWizard/msconvert) encode characters that are
#' invalid at the start of an XML ID (such as digits) as `_xHHHH_`, where
#' HHHH is the Unicode code point in hexadecimal. This helper decodes such
#' sequences back to the original characters, e.g. `_x0032_` -> `2`.
#'
#' @param x A character vector of ID strings (possibly NA)
#' @return Character vector with escapes decoded
#' @keywords internal
unescape_mzml_id <- function(x) {
  stringr::str_replace_all(
    x, "_x[0-9a-fA-F]{4}_",
    function(m) intToUtf8(strtoi(substr(m, 3, 6), base = 16L), multiple=TRUE)
  )
}

#' Strip known mass spectrometry file extensions
#'
#' Iteratively removes trailing `.raw`, `.mzml`, `.mzXML`, `.xml`, `.netcdf`,
#' `.mzdata`, `.wiff` or `.wiff.scan` extensions (case insensitive), so that
#' compound extensions like `.raw.mzml` are handled as well.
#'
#' @param x Character vector of file names
#' @return Character vector with extensions stripped
#' @keywords internal
strip_ms_file_extensions <- function(x) {
  repeat {
    new_x <- gsub("\\.(raw|mzml|mzxml|xml|netcdf|mzdata|wiff|wiff.scan)$", "", x, ignore.case = TRUE)
    if (all(new_x == x)) break
    x <- new_x
  }
  x
}

#' Extract run ID from mass spectrometry file
#'
#' Reads the run identifier from mzML or Thermo RAW files.
#' For mzML files, checks `fileDescription/sourceFileList` for source file names.
#' If all source files (after stripping `.raw`, `.mzml`, `.mzXML`, `.netcdf`,
#' `.mzdata`, `.wiff` extensions, iteratively to handle compound extensions like `.raw.mzml`)
#' share the same base name, returns that name. Otherwise falls back to the
#' `id` attribute of the `run` node (decoding `_xHHHH_` character escapes used
#' by converters such as msconvert).
#' For RAW files, reads the sample ID from the file header.
#' As a last resort (no usable source file name or run ID in mzML, or any other
#' file type), returns the name derived from the filepath (with the same
#' extension stripping).
#'
#' @param filepath Path to an mzML or RAW file
#' @return Character string with run/sample ID
#' @export
#' @importFrom xml2 read_xml xml_ns xml_find_first xml_attr
#' @examples
#' \dontrun{
#' run_id <- read_run_id("sample.mzML")
#' sample_id <- read_run_id("sample.raw")
#' }
read_run_id <- function(filepath) {
  if(is_mzml(filepath)) {
    doc <- xml2::read_xml(filepath)
    ns <- xml2::xml_ns(doc)

    # First option - use <sourceFile> name attribute 
    # Check for fileDescription > sourceFileList
    source_file_nodes <- xml2::xml_find_all(doc, ".//d1:fileDescription/d1:sourceFileList/d1:sourceFile[@name]", ns)

    if (length(source_file_nodes) > 0) {
      # Get all names and strip known file type endings
      names <- strip_ms_file_extensions(xml2::xml_attr(source_file_nodes, "name"))
      # Check if all stripped names match
      if (length(unique(names)) == 1) {
        return(names[1])
      }
    }

    # Fallback to run node id attribute
    run_node <- xml2::xml_find_first(doc, ".//d1:run", ns)
    if (!inherits(run_node, "xml_missing")) {
      run_id <- xml2::xml_attr(run_node, "id")
      if (!is.na(run_id) && nzchar(run_id)) {
        return(unescape_mzml_id(run_id))
      }
    }

  } else if (tools::file_ext(filepath) == 'raw') {
    if (!requireNamespace("rawrr", quietly = TRUE)) {
      stop("The 'rawrr' package is required but not installed. Please install it with install.packages('rawrr').")
    }
    return(rawrr::readFileHeader(filepath)$'Sample id')
  }

  return(strip_ms_file_extensions(basename(filepath)))
}

#' Load multiple parquet files from testdata directory
#'
#' Convenience function for loading test data from the package's testdata folder.
#' Each filename is combined with a folder path and pattern suffix to construct
#' the full path.
#'
#' @param filename Character vector of base filenames (without extension)
#' @param folder Subdirectory within testdata
#' @param pattern File extension or suffix pattern (e.g., ".parquet")
#' @return List of tibbles, one per input file
#' @export
#' @importFrom arrow read_parquet
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' data_list <- read_parquet_files(c("features", "metadata"), "processed", ".parquet")
#' }
read_parquet_files <- function(filename, folder, pattern) {
  testdata <- file.path("..", "testdata")

  input <- lapply(filename, function(x) {
    tibble::as_tibble(arrow::read_parquet(file.path(testdata, folder, paste0(x, pattern))))
  })

  return(input)
}

#' Load aligned feature data from three parquet files
#'
#' Loads metadata, intensity, and retention time data from separate parquet files
#' into a structured list. This is typically used for aligned LC-MS feature tables.
#'
#' @param metadata_file Path to metadata parquet file
#' @param intensities_file Path to intensity values parquet file
#' @param rt_file Path to retention times parquet file
#' @return Named list with components:
#'   \describe{
#'     \item{metadata}{Tibble with sample/run metadata}
#'     \item{intensity}{Tibble with intensity values}
#'     \item{rt}{Tibble with retention times}
#'   }
#' @export
#' @importFrom arrow read_parquet
#' @examples
#' \dontrun{
#' features <- load_aligned_features(
#'   "metadata.parquet",
#'   "intensities.parquet",
#'   "retention_times.parquet"
#' )
#' }
load_aligned_features <- function(metadata_file, intensities_file, rt_file) {
  metadata <- arrow::read_parquet(metadata_file)
  intensities <- arrow::read_parquet(intensities_file)
  rt <- arrow::read_parquet(rt_file)

  result <- list()
  result$metadata <- as_tibble(metadata)
  result$intensity <- as_tibble(intensities)
  result$rt <- as_tibble(rt)
  return(result)
}
