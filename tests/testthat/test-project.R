test_that("XML and CSV readers give the same project", {
  from_xml <- read_redcap_project(xml = example_path("CalmMinds_metadata.xml"))
  from_csv <- read_redcap_project(
    dictionary = example_path("CalmMinds_DataDictionary.csv"),
    events = example_path("CalmMinds_Events.csv"),
    mappings = example_path("CalmMinds_InstrumentDesignations.csv"),
    repeating = example_path("CalmMinds_RepeatingInstrumentsEvents.csv")
  )

  expect_equal(from_xml$fields, from_csv$fields)
  expect_equal(from_xml$events, from_csv$events)
  expect_equal(from_xml$mappings, from_csv$mappings)
  expect_equal(from_xml$repeating, from_csv$repeating)
})

test_that("XML with records is refused", {
  expect_error(
    read_redcap_project(xml = test_path("fixtures/with_data.xml")),
    "ClinicalData"
  )
})

test_that("parse_choices splits codes and labels", {
  out <- parse_choices("0, Not at all | 1, Several days, or more")

  expect_equal(out$code, c("0", "1"))
  expect_equal(out$label, c("Not at all", "Several days, or more"))
})

test_that("parse_choices knows yesno", {
  out <- parse_choices(NA, "yesno")

  expect_equal(out$code, c("0", "1"))
  expect_equal(out$label, c("No", "Yes"))
})
