# Builds the invented "Calm Minds" example project in inst/examples/calm_minds/.
# Run from the package root: Rscript data-raw/calm_minds.R
library(tibble)
library(dplyr)
library(purrr)
library(stringr)

out_dir <- "inst/examples/calm_minds"

phq_choices <- "0, Not at all | 1, Several days | 2, More than half the days | 3, Nearly every day"
pss_choices <- "0, Never | 1, Almost never | 2, Sometimes | 3, Fairly often | 4, Very often"
ema_choices <- "1, Not at all | 2, A little | 3, Moderately | 4, Quite a bit | 5, Extremely"

phq_labels <- c(
  "Low interest in doing things", "Feeling down or hopeless",
  "Trouble with sleep", "Feeling tired", "Appetite changes",
  "Feeling bad about yourself", "Trouble concentrating",
  "Moving or speaking slowly, or being restless",
  "Thoughts of self-harm"
)
pss_labels <- c(
  "Upset by something unexpected", "Unable to control important things",
  "Felt nervous and stressed", "Confident handling personal problems",
  "Things were going your way", "Could not cope with everything",
  "Able to control irritations", "Felt on top of things",
  "Angered by things outside your control", "Difficulties piling up"
)

field <- function(name, form, type, label, choices = "", validation = "",
                  min = "", max = "", branching = "", matrix = "") {
  tibble(
    "Variable / Field Name" = name, "Form Name" = form, "Section Header" = "",
    "Field Type" = type, "Field Label" = label,
    "Choices, Calculations, OR Slider Labels" = choices, "Field Note" = "",
    "Text Validation Type OR Show Slider Number" = validation,
    "Text Validation Min" = min, "Text Validation Max" = max,
    "Identifier?" = "", "Branching Logic (Show field only if...)" = branching,
    "Required Field?" = "", "Custom Alignment" = "",
    "Question Number (surveys only)" = "", "Matrix Group Name" = matrix,
    "Matrix Ranking?" = "", "Field Annotation" = ""
  )
}

dictionary <- bind_rows(
  field("record_id", "demographics", "text", "Record ID"),
  field("age", "demographics", "text", "Age (years)", validation = "integer", min = "18", max = "80"),
  field("sex", "demographics", "radio", "Sex assigned at birth", "1, Female | 2, Male | 3, Intersex"),
  field("gender", "demographics", "radio", "Gender", "1, Woman | 2, Man | 3, Non-binary | 4, Prefer to self-describe"),
  field("gender_other", "demographics", "text", "Please describe", branching = "[gender] = '4'"),
  field("race", "demographics", "checkbox", "Race (check all that apply)",
        "1, American Indian or Alaska Native | 2, Asian | 3, Black or African American | 4, Native Hawaiian or Pacific Islander | 5, White | 6, Other"),
  field("education", "demographics", "dropdown", "Highest education",
        "1, High school or less | 2, Some college | 3, Bachelor's degree | 4, Graduate degree"),
  field("screen_date", "demographics", "text", "Screening date", validation = "date_ymd"),
  field("consent_given", "consent", "yesno", "Do you agree to take part in this study?"),
  field("consent_date", "consent", "text", "Consent date", validation = "date_ymd", branching = "[consent_given] = '1'"),
  map2_dfr(1:9, phq_labels, \(i, l) field(str_c("phq9_", i), "phq9", "radio", l, phq_choices, matrix = "phq9_matrix")),
  field("phq9_total", "phq9", "calc", "PHQ-9 total",
        str_c("sum(", str_c("[phq9_", 1:9, "]", collapse = ","), ")")),
  field("phq9_10", "phq9", "radio", "How difficult have these problems made things for you?",
        "0, Not difficult at all | 1, Somewhat difficult | 2, Very difficult | 3, Extremely difficult",
        branching = "[phq9_total] > 0"),
  field("rand_group", "randomization", "radio", "Randomized group", "1, Waitlist control | 2, Mindfulness"),
  map2_dfr(1:10, pss_labels, \(i, l) field(str_c("pss_", i), "pss", "radio", l, pss_choices, matrix = "pss_matrix")),
  field("ema_ts", "ema_mood", "text", "Prompt time", validation = "datetime_ymd"),
  field("ema_sad", "ema_mood", "radio", "Right now I feel sad", ema_choices),
  field("ema_anxious", "ema_mood", "radio", "Right now I feel anxious", ema_choices),
  field("ema_calm", "ema_mood", "radio", "Right now I feel calm", ema_choices),
  field("ema_happy", "ema_mood", "radio", "Right now I feel happy", ema_choices),
  field("ema_doing", "ema_mood", "notes", "What are you doing right now?"),
  field("exit_helpful", "exit_survey", "radio", "How helpful was taking part in this study?",
        "1, Not at all | 2, A little | 3, Somewhat | 4, Very | 5, Extremely"),
  field("exit_feedback", "exit_survey", "notes", "Anything you'd like to tell us about your experience?")
)

events <- tribble(
  ~event_name, ~arm_num, ~day_offset, ~offset_min, ~offset_max, ~unique_event_name, ~custom_event_label,
  "Screening", 1, -7, 0, 0, "screening_arm_1", "",
  "Baseline", 1, 0, 0, 0, "baseline_arm_1", "",
  "EMA burst", 1, 1, 0, 0, "ema_arm_1", "",
  "Week 4", 1, 28, 3, 3, "week4_arm_1", "",
  "Week 8", 1, 56, 3, 3, "week8_arm_1", "",
  "Follow-up", 1, 84, 7, 7, "followup_arm_1", ""
)

mappings <- tribble(
  ~unique_event_name, ~form,
  "screening_arm_1", "demographics", "screening_arm_1", "consent", "screening_arm_1", "phq9",
  "baseline_arm_1", "randomization", "baseline_arm_1", "phq9", "baseline_arm_1", "pss",
  "ema_arm_1", "ema_mood",
  "week4_arm_1", "phq9",
  "week8_arm_1", "phq9", "week8_arm_1", "pss",
  "followup_arm_1", "phq9", "followup_arm_1", "pss", "followup_arm_1", "exit_survey"
) |>
  mutate(arm_num = 1, .before = 1)

repeating <- tibble(event_name = "ema_arm_1", form_name = "ema_mood", custom_form_label = "[ema_ts]")

write_quoted <- function(x, file) readr::write_csv(x, file.path(out_dir, file), na = "", quote = "all")
write_quoted(dictionary, "CalmMinds_DataDictionary.csv")
write_quoted(events, "CalmMinds_Events.csv")
write_quoted(mappings, "CalmMinds_InstrumentDesignations.csv")
write_quoted(repeating, "CalmMinds_RepeatingInstrumentsEvents.csv")

# The same project as a metadata-only REDCap project XML (CDISC ODM + REDCap
# extensions). Element names follow REDCap's export as best we know it.
library(xml2)
doc <- xml_new_root("ODM",
  xmlns = "http://www.cdisc.org/ns/odm/v1.3",
  "xmlns:redcap" = "https://projectredcap.org",
  ODMVersion = "1.3.1", FileType = "Snapshot", FileOID = "000-00-0000"
)
study <- xml_add_child(doc, "Study", OID = "Project.CalmMinds")
gv <- xml_add_child(study, "GlobalVariables")
xml_add_child(gv, "StudyName", "Calm Minds")
xml_add_child(gv, "StudyDescription", "Invented example project for studysynth")
xml_add_child(gv, "ProtocolName", "Calm Minds")
rie <- xml_add_child(gv, "redcap:RepeatingInstrumentsAndEvents")
ri <- xml_add_child(rie, "redcap:RepeatingInstruments")
pwalk(repeating, \(event_name, form_name, custom_form_label) xml_add_child(ri, "redcap:RepeatingInstrument",
  "redcap:UniqueEventName" = event_name, "redcap:RepeatInstrument" = form_name,
  "redcap:CustomLabel" = custom_form_label))

mdv <- xml_add_child(study, "MetaDataVersion", OID = "Metadata.CalmMinds_1", Name = "Calm Minds")
protocol <- xml_add_child(mdv, "Protocol")
forms <- unique(dictionary$`Form Name`)
pwalk(events, \(event_name, arm_num, day_offset, offset_min, offset_max, unique_event_name, custom_event_label) {
  xml_add_child(protocol, "StudyEventRef", StudyEventOID = str_c("Event.", unique_event_name), Mandatory = "No")
})
pwalk(events, \(event_name, arm_num, day_offset, offset_min, offset_max, unique_event_name, custom_event_label) {
  ev <- xml_add_child(mdv, "StudyEventDef",
    OID = str_c("Event.", unique_event_name), Name = event_name, Type = "Common", Repeating = "No",
    "redcap:EventName" = event_name, "redcap:CustomEventLabel" = custom_event_label,
    "redcap:UniqueEventName" = unique_event_name, "redcap:ArmNum" = arm_num, "redcap:ArmName" = "Arm 1",
    "redcap:DayOffset" = day_offset, "redcap:OffsetMin" = offset_min, "redcap:OffsetMax" = offset_max)
  for (f in mappings$form[mappings$unique_event_name == unique_event_name]) {
    xml_add_child(ev, "FormRef", FormOID = str_c("Form.", f), Mandatory = "No", "redcap:FormName" = f)
  }
})

item_oids <- function(row) {
  if (row$`Field Type` == "checkbox") {
    codes <- str_trim(str_extract(str_split_1(row$`Choices, Calculations, OR Slider Labels`, "\\|"), "^[^,]+"))
    str_c(row$`Variable / Field Name`, "___", codes)
  } else {
    row$`Variable / Field Name`
  }
}

for (f in forms) {
  rows <- filter(dictionary, `Form Name` == f)
  fd <- xml_add_child(mdv, "FormDef", OID = str_c("Form.", f), Name = f, Repeating = "No", "redcap:FormName" = f)
  xml_add_child(fd, "ItemGroupRef", ItemGroupOID = str_c(f, ".", rows$`Variable / Field Name`[1]), Mandatory = "No")
}
for (f in forms) {
  rows <- filter(dictionary, `Form Name` == f)
  ig <- xml_add_child(mdv, "ItemGroupDef", OID = str_c(f, ".", rows$`Variable / Field Name`[1]), Name = f, Repeating = "No")
  for (i in seq_len(nrow(rows))) {
    for (oid in item_oids(rows[i, ])) xml_add_child(ig, "ItemRef", ItemOID = oid, Mandatory = "No", "redcap:Variable" = rows$`Variable / Field Name`[i])
  }
  xml_add_child(ig, "ItemRef", ItemOID = str_c(f, "_complete"), Mandatory = "No", "redcap:Variable" = str_c(f, "_complete"))
}

data_type <- function(type, validation) {
  case_when(
    validation == "integer" ~ "integer", validation == "number" ~ "float",
    validation == "date_ymd" ~ "date", validation == "datetime_ymd" ~ "datetime",
    type %in% c("radio", "dropdown", "yesno", "truefalse", "checkbox") ~ "text",
    type == "calc" ~ "float", .default = "text"
  )
}

code_lists <- list()
for (i in seq_len(nrow(dictionary))) {
  row <- dictionary[i, ]
  var <- row$`Variable / Field Name`
  type <- row$`Field Type`
  for (oid in item_oids(row)) {
    attrs <- list(OID = oid, Name = oid, DataType = data_type(type, row$`Text Validation Type OR Show Slider Number`),
      Length = "999", "redcap:Variable" = var, "redcap:FieldType" = type)
    if (row$`Text Validation Type OR Show Slider Number` != "") attrs[["redcap:TextValidationType"]] <- row$`Text Validation Type OR Show Slider Number`
    if (row$`Branching Logic (Show field only if...)` != "") attrs[["redcap:BranchingLogic"]] <- row$`Branching Logic (Show field only if...)`
    if (type == "calc") attrs[["redcap:Calculation"]] <- row$`Choices, Calculations, OR Slider Labels`
    if (row$`Matrix Group Name` != "") attrs[["redcap:MatrixGroupName"]] <- row$`Matrix Group Name`
    if (type == "checkbox") attrs[["redcap:CheckboxChoices"]] <- row$`Choices, Calculations, OR Slider Labels`
    item <- do.call(xml_add_child, c(list(mdv, "ItemDef"), attrs))
    q <- xml_add_child(item, "Question")
    xml_add_child(q, "TranslatedText", row$`Field Label`)
    if (type %in% c("radio", "dropdown", "yesno", "truefalse", "checkbox")) {
      cl <- str_c(oid, ".choices")
      xml_add_child(item, "CodeListRef", CodeListOID = cl)
      choices <- switch(type,
        yesno = "1, Yes | 0, No", truefalse = "1, True | 0, False",
        checkbox = "1, Checked | 0, Unchecked",
        row$`Choices, Calculations, OR Slider Labels`)
      code_lists[[cl]] <- list(var = var, choices = choices)
    }
    if (row$`Text Validation Min` != "") {
      rc <- xml_add_child(item, "RangeCheck", Comparator = "GE", SoftHard = "Soft")
      xml_add_child(rc, "CheckValue", row$`Text Validation Min`)
    }
    if (row$`Text Validation Max` != "") {
      rc <- xml_add_child(item, "RangeCheck", Comparator = "LE", SoftHard = "Soft")
      xml_add_child(rc, "CheckValue", row$`Text Validation Max`)
    }
  }
}
for (f in forms) {
  oid <- str_c(f, "_complete")
  item <- xml_add_child(mdv, "ItemDef", OID = oid, Name = oid, DataType = "text", Length = "1",
    "redcap:Variable" = oid, "redcap:FieldType" = "select")
  xml_add_child(xml_add_child(item, "Question"), "TranslatedText", "Complete?")
  xml_add_child(item, "CodeListRef", CodeListOID = str_c(oid, ".choices"))
  code_lists[[str_c(oid, ".choices")]] <- list(var = oid, choices = "0, Incomplete | 1, Unverified | 2, Complete")
}
iwalk(code_lists, \(cl, oid) {
  node <- xml_add_child(mdv, "CodeList", OID = oid, Name = cl$var, DataType = "text", "redcap:Variable" = cl$var)
  for (choice in str_trim(str_split_1(cl$choices, "\\|"))) {
    code <- str_trim(str_extract(choice, "^[^,]+"))
    label <- str_trim(str_remove(choice, "^[^,]+,"))
    cli <- xml_add_child(node, "CodeListItem", CodedValue = code)
    xml_add_child(xml_add_child(cli, "Decode"), "TranslatedText", label)
  }
})
write_xml(doc, file.path(out_dir, "CalmMinds_metadata.xml"))
