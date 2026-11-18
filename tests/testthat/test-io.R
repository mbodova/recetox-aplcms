data_frame <- data.frame(
  id = c(1, 2, 3),
  rt = c(100, 101, 102),
  mz = c(50, 51, 52)
)

local_parquet <- function(dir = tempdir(), env = parent.frame()) {
  outpath <- file.path(dir, "output.parquet")
  arrow::write_parquet(data_frame, outpath)
  withr::defer(fs::file_delete(outpath), envir = env)
  outpath
}

local_arrow <- function(metadata, dir = tempdir(), env = parent.frame()) {
  outpath <- file.path(dir, "output.parquet")
  write_arrow(data_frame, outpath, metadata)
  withr::defer(fs::file_delete(outpath), envir = env)
  outpath
}

test_that("write_arrow writes correct number of rows without metadata", {
  outpath <- file.path(tempdir(), "output.parquet")
  write_arrow(data_frame, outpath)

  actual <- arrow::read_parquet(outpath)
  expect_equal(as_tibble(data_frame), actual)
})

test_that("write_arrow writes simple metadata", {
  outpath <- file.path(tempdir(), "output.parquet")
  expected <- list("name" = "peter")

  write_arrow(data_frame, outpath, metadata = expected)
  actual <- arrow::read_parquet(outpath, as_data_frame = FALSE)

  expect_equal(expected, actual$metadata)
})

test_that('read_arrow can read file without metadata', {
  actual <- read_arrow(local_parquet())
  expected <- data_frame
  expect_equal(as.data.frame(actual), expected)
})

test_that('read_arrow can read file with metadata', {
  expected <- list('name' = 'peter')
  actual <- read_arrow(local_arrow(expected))$metadata
  expect_equal(actual, expected)
})

test_that('write_tibble writes all attributes', {
  t <- tibble::as_tibble(data_frame)
  attr(t, 'run_id') <- 'peter'

  outpath <- file.path(tempdir(), "output.parquet")

  write_tibble(t, outpath)
  actual <- read_arrow(outpath)

  expect_equal(actual$schema$metadata$run_id, attr(t, 'run_id'))
})

patrick::with_parameters_test_that(
  "read_run_id get's the run id",
  {
    expect_equal(read_run_id(filepath), expected)
  },
  patrick::cases(
    # Currently disabled due to not downloading large mzml files.
    # qc_no_dil_milliq = list(
    #   filepath = file.path("..", "testdata", "input", "8_qc_no_dil_milliq.mzml"),
    #   expected = '8_qc_no_dil_milliq'
    # ),
    mbr0 = list(
      filepath = file.path("..", "testdata", "input", 'mbr_test0.mzml'),
      expected = '2016_Jan_12_QE2_47'
    ),
    rawfile = list(
      filepath = file.path("..", "testdata", "input","8_qc_no_dil_milliq.raw"),
      expected = '8_qc_no_dil_milliq'
    ),
    mzxml = list(
      filepath = file.path("..", "testdata", "input", "test_file.mzXML"),
      expected = "test_file"
    ),
    mzdata = list(
      filepath = file.path("..", "testdata", "input", "alg3.mzdata"),
      expected = "alg3"
    ),
    processed_mzml = list(
      filepath = file.path("..", "testdata", "input", "Tribrid_201106_009-QC1_1_NEG_FISABIO_single_eic.raw.mzML"),
      expected = 'Tribrid_201106_009-QC1_1_NEG_FISABIO'
    ),
    escaped_run_id = list(
      filepath = file.path("..", "testdata", "input", "escaped_run_id.mzML"),
      expected = '2016_test'
    ),
    escaped_run_id2 = list(
      filepath = file.path("..", "testdata", "input", "tiny.pwiz.1.1.mzML"),
      expected = 'Experiment 1'
    ),
    file_path_fallback = list(
      filepath = file.path("..", "testdata", "input", "tiny2_SRM.mzML0.93.xml"),
      expected = 'tiny2_SRM.mzML0.93'
    ),
    no_run_id_no_source_name_mzml = list(
      filepath = file.path("..", "testdata", "input", "no_run_id_no_source_name.mzML"),
      expected = 'no_run_id_no_source_name'
    )
  )
)

patrick::with_parameters_test_that(
  "unescape_mzml_id works",
  {
    expect_equal(unescape_mzml_id(value), expected)
  },
  patrick::cases(
    mbr0 = list(
      value = "_x0032_016_Jan_12_QE2_47",
      expected = "2016_Jan_12_QE2_47"
    ),
    simple1 = list(
      value = "_x0041__x0042_",
      expected = "AB"
    ),
    simple2 = list(
      value = "_x0032_016_test",
      expected = "2016_test"
    ),
    blatt1_minimal = list(
      value = "LA_x0020_1777_x0020_Blatt_x0020_sauer1",
      expected = "LA 1777 Blatt sauer1"
    )
  )
)

patrick::with_parameters_test_that(
  "strip_ms_file_extensions works",
  {
    expect_equal(strip_ms_file_extensions(value), expected)
  },
  patrick::cases(
    simple = list(
      value = "sample.raw",
      expected = "sample"
    ),
    compound = list(
      value = "Tribrid_QC1.raw.mzML",
      expected = "Tribrid_QC1"
    ),
    wiff_scan = list(
      value = "sample.wiff.scan",
      expected = "sample"
    ),
    case_insensitive = list(
      value = "test_file.mzXML",
      expected = "test_file"
    ),
    vectorized = list(
      value = c("a.raw", "b.mzML"),
      expected = c("a", "b")
    ),
    no_extension = list(
      value = "plain_name",
      expected = "plain_name"
    )
  )
)


#TODO: test if fallback 2 in read_run_id works - DONE

#TODO test fallback 3 for basename - needs new test file - llm should generate - DONE
