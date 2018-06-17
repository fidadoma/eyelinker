context("SR research test files")

test_that("test_files_load",{
files <- c('bino1000.asc.gz','bino250.asc.gz','bino500.asc.gz','binoRemote250.asc.gz','binoRemote500.asc.gz','mono1000.asc.gz','mono2000.asc.gz','mono250.asc.gz','mono500.asc.gz','monoRemote250.asc.gz','monoRemote500.asc.gz')
for  (f in files)
{
    fpath <- system.file(paste0("extdata/",f),package="eyelinker")
    tst <- read.asc(fpath)
    expect_equal(sort(names(tst)),c("blinks","cal","fix","info","msg","raw", "sacc"  ))
}
})

test_that("test_calibration",{
  f <- 'bino1000.asc.gz'
  # avg calibration left eye - 0.35
  # avg calibration right eye - 0.30
  # max calibration left eye - 0.48
  # max calibration right eye - 0.91
  fpath <- system.file(paste0("extdata/",f),package="eyelinker")
  tst <- read.asc(fpath)
  cal <- tst$cal
  expect_length(cal$avg_error,2)
  expect_equal(cal$avg_error[cal$eye == "LEFT"], 0.35)
  expect_equal(cal$avg_error[cal$eye == "RIGHT"], 0.30)
  expect_equal(cal$max_error[cal$eye == "LEFT"], 0.48)
  expect_equal(cal$max_error[cal$eye == "RIGHT"], 0.91)
  
})

test_that("test_parse.calibration",{
  s <- "MSG\t6178510 !CAL VALIDATION HV13 LR LEFT  GOOD ERROR 0.33 avg. 0.71 max  OFFSET 0.14 deg. 3.8,3.3 pix."
  c <- parse.calibrations(s)
  expect_equal(sort(names(c)),c("avg_error","calResult","calType","eye","max_error","time"))
})