context("name value")

name.value.vec <- c(
  "  sampleType=monocyte   assayType=H3K27me3    cost=5",
  "sampleType=monocyte assayType=H3K27ac",
  " sampleType=Myeloidcell cost=30.5  assayType=H3K4me3")

name.value.pattern <- paste0(
  "(?<name>[^ ]+?)",
  "=",
  "(?<value>[^ ]+)")

test_that("name group used for rownames", {
  computed <- str_match_all_named(name.value.vec, name.value.pattern)
  expected <- list(
    cbind(value=c(sampleType="monocyte", assayType="H3K27me3", cost="5")),
    cbind(value=c(sampleType="monocyte", assayType="H3K27ac")),
    cbind(value=c(sampleType="Myeloidcell", cost="30.5", assayType="H3K4me3"))
    )
  expect_identical(computed, expected)
})
