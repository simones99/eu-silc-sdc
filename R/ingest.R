# Download and verify the EU-SILC public microdata for Italy.
#
# The file is pinned by SHA-256 in data/manifest.json, so every run works on
# exactly the bytes documented in the manifest. If Eurostat replaces the file,
# the run stops instead of silently producing different numbers.

read_manifest <- function(path = "data/manifest.json") {
  jsonlite::read_json(path)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

download_puf <- function(manifest = read_manifest(), dest_dir = "data/raw") {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, manifest$file)
  if (!file.exists(zip_path)) {
    message("Downloading ", manifest$url)
    utils::download.file(manifest$url, zip_path, mode = "wb", quiet = TRUE)
  }
  actual <- sha256_file(zip_path)
  if (!identical(actual, manifest$sha256)) {
    stop(sprintf(
      "SHA-256 mismatch for %s: expected %s, got %s. The source file has changed; review it and update data/manifest.json.",
      zip_path, manifest$sha256, actual
    ))
  }
  zip_path
}

# Read one of the four EU-SILC files (d, h, p, r) for one year from the zip.
# Empty strings and the literal "NA" are both missing values in the PUF.
read_silc <- function(zip_path, year, type = c("d", "h", "p", "r"), country = "IT") {
  type <- match.arg(type)
  member <- sprintf("%s_%d%s_EUSILC.csv", country, year, type)
  exdir <- file.path(tempdir(), "silc")
  utils::unzip(zip_path, files = member, exdir = exdir, overwrite = TRUE)
  data.table::fread(file.path(exdir, member), na.strings = c("", "NA"),
                    colClasses = "character")
}
