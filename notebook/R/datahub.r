if (!require(pacman)) install.packages("pacman")
# installing r packages needed to access the SNSF SQL server
pacman::p_load(DBI, odbc, dbplyr)
# and to tidy the data
pacman::p_load(rio, stringr, magrittr, plyr, tidyverse, lubridate, Hmisc)

# once you have a datahub account, you can ask connect to the server...
con <- DBI::dbConnect(
  odbc::odbc(),
  .connection_string = "Driver=SQL Server;Server=sqlprod04\\p04;Database=Dh;")

# R wrapping for SQL databases
# selecting the variables of interest
grants <- dplyr::tbl(con, in_schema("app", "Application")) %>%
  dplyr::select(
    ApplicationId,
    Number,
    CallTitle,
    CallEndDate,
    Type,
    StateDatahub,
    AdministrativeDivision,
    MainDisciplineLevel2,
    ResearchInstitutionId,
    ResponsibleApplicant_PersonBaseId,
    ResponsibleApplicantNumber,
    ResponsibleApplicantGender,
    ResponsibleApplicantBirthDate,
    IsBonusOfExcellence,
    IsLeadAgencySNF,
    IsLeadAgencyExtern) %>%
  dplyr::collect() %>%
  dplyr::filter(
    stringr::str_detect(AdministrativeDivision, "[1-3]"),
    stringr::str_detect(CallTitle, "Projektf.rderung"),
    stringr::str_detect(CallEndDate, "2009|201[0-6]"),
    stringr::str_detect(Type, "Grant App"),
    !(StateDatahub %in% c("withdrawal before ruling",
                          "formal non-consideration",
                          "unknown")),
    IsBonusOfExcellence == FALSE,
    IsLeadAgencySNF == FALSE,
    IsLeadAgencyExtern == FALSE) %>%
  dplyr::mutate(
    CallEndDate = ymd(CallEndDate),
    ResponsibleApplicantBirthDate = lubridate::ymd(ResponsibleApplicantBirthDate),
    # take the first 2 words from MainDisciplineLevel2:
    research_topic = stringr::str_trim(stringr::str_extract(MainDisciplineLevel2,
                                      "[[:alpha:][:space:]&]+")))

# add institute type ------------------------------------------------------
institutions <- dplyr::tbl(con, in_schema("app", "ResearchInstitution")) %>%
  dplyr::select(ResearchInstitutionId, Type) %>%
  dplyr::collect() %>%
  dplyr::rename(applicant_affiliation = Type)

# and responsible applicant nationality -----------------------------------
applicants <- dplyr::tbl(con, in_schema("app", "Person")) %>%
  dplyr::select(PersonId, NationalityIsoCode) %>%
  dplyr::filter(PersonId %in% local(grants$ResponsibleApplicant_PersonBaseId)) %>%
  dplyr::collect() %>%
  dplyr::rename(ApplicantNationality = NationalityIsoCode)

# get reviews for these grants --------------------------------------------
reviews <- dplyr::tbl(con, in_schema("app", "ReviewGaMySnf")) %>%
  dplyr::select(Reviewer_PersonId, ApplicationId, OverallGrade) %>%
  dplyr::filter(ApplicationId %in% local(grants$ApplicationId)) %>%
  dplyr::collect() %>%
  dplyr::mutate(
    OverallGrade = str_replace_all(OverallGrade, "^A$|outstanding", "6"),
    OverallGrade = str_replace_all(OverallGrade, "^AB$|excellent", "5"),
    OverallGrade = str_replace_all(OverallGrade, "^B$|very good", "4"),
    OverallGrade = str_replace_all(OverallGrade, "^BC$|good", "3"),
    OverallGrade = str_replace_all(OverallGrade, "^C$|average", "2"),
    OverallGrade = str_replace_all(OverallGrade, "^D$|poor", "1"),
    OverallGrade = ifelse(str_detect(OverallGrade, "[1-6]"), OverallGrade, NA),
    OverallGrade = as.integer(OverallGrade)) %>%
  dplyr::rename(review_score = OverallGrade)


# combine grant information -----------------------------------------------
grants <- grants %>%
  dplyr::left_join(institutions, by = "ResearchInstitutionId") %>%
  dplyr::left_join(
    applicants,
    by = c("ResponsibleApplicant_PersonBaseId" = "PersonId")) %>%
  # remove grants without reviews
  dplyr::semi_join(reviews, by = "ApplicationId")


# RH: distinct grades -----------------------------------------------------
distinct_grade <- FALSE
if (distinct_grade){
dist_grades <- dplyr::tbl(con, in_schema('app', 'ReviewItem')) %>% 
  dplyr::select(ApplicationId, ReviewerId, ReviewId, EvaluationCriterion,
                Grade) %>% 
  dplyr::filter(ApplicationId %in% grants$ApplicationId) %>% 
  dplyr::filter(!is.na(Grade), !is.na(EvaluationCriterion)) %>% 
  dplyr::collect() %>% 
  dplyr::mutate(
    EvaluationCriterion = as.factor(EvaluationCriterion),
    EvaluationCriterion = fct_recode(EvaluationCriterion, 
                                     track_record = "Applicants' scientific track record and expertise",
                                     relevance_originality_topicality = "Scientific relevance, originality and topicality ",
                                     feasibility = "Suitability of methods and feasibility ")) %>% 
  spread(EvaluationCriterion, Grade)
  
# get reviewer person id: 
reviewer_person_id <- dplyr::tbl(con, in_schema('app', 'Reviewer')) %>% 
  dplyr::select(ReviewerId, PersonId) %>% 
  dplyr::filter(ReviewerId %in% dist_grades$ReviewerId) %>% 
  dplyr::collect() 

which_statecorrect <- dplyr::tbl(con, in_schema('app', 'Review')) %>% 
  dplyr::select(ReviewId, StateSortNumber) %>% 
  dplyr::filter(ReviewId %in% dist_grades$ReviewId) %>% 
  collect()

dist_grades <- dist_grades %>% 
  dplyr::left_join(reviewer_person_id, by = 'ReviewerId') %>% 
  dplyr::left_join(which_statecorrect, by = 'ReviewId') %>% 
  dplyr::filter(!StateSortNumber %in% c(6, 11, 10),
                !track_record %in% c(-1, 0), 
                !feasibility %in% c(-1, 0),
                !relevance_originality_topicality %in% c(-1, 0))
}

# reviewers data ----------------------------------------------------------

reviewer_id <- unique(reviews$Reviewer_PersonId)

reviewers <- dplyr::tbl(con, in_schema("app", "Person")) %>%
  dplyr::select(PersonId, Gender) %>%
  dplyr::filter(PersonId %in% reviewer_id) %>%
  dplyr::collect() %>%
  dplyr::rename(reviewer_gender = Gender)

# add reviewers' Country
address <- dplyr::tbl(con, in_schema("app", "Address")) %>%
  dplyr::select(PersonBaseId, CountryIsoCode, IsCurrent) %>%
  dplyr::filter(PersonBaseId %in% reviewer_id) %>%
  dplyr::collect() %>%
  dplyr::filter(IsCurrent == TRUE) %>%
  dplyr::select(-IsCurrent) %>%
  dplyr::mutate(CountryIsoCode = str_to_lower(CountryIsoCode)) %>%
  dplyr::rename(reviewer_country = CountryIsoCode)

# add reviewers' email contact
contacts <- dplyr::tbl(con, in_schema("app", "Contact")) %>%
  dplyr::select(PersonBaseId, TYPE, Contact, IsCurrent) %>%
  dplyr::filter(PersonBaseId %in% reviewer_id,
                TYPE == "email", IsCurrent == 1) %>%
  dplyr::collect() %>%
  dplyr::select(-TYPE, -IsCurrent) %>%
  dplyr::rename(reviewer_email = Contact)


# add source list of the reviewer
reviewer_source <- dplyr::tbl(con, in_schema("app", "Application_Reviewer")) %>%
  dplyr::select(ApplicationId, Reviewer_PersonId, ReviewerSource) %>%
  dplyr::filter(ApplicationId %in% local(grants$ApplicationId)) %>%
  dplyr::collect() %>%
  dplyr::filter(!is.na(ReviewerSource)) %>%
  dplyr::distinct() %>%
  dplyr::rename(reviewer_source = ReviewerSource) %>%
  # includes reviews & invitations
  dplyr::mutate(
    reviewer_source = str_replace_all(
      reviewer_source, "Gesch.ftsstelle", "Administrative Offices"),
    reviewer_source = str_replace_all(
      reviewer_source, "Gesuchstellervorschlag", "Applicant"),
    reviewer_source = str_replace_all(
      reviewer_source, "Experte", "Reviewer"),
    reviewer_source = str_replace_all(
      reviewer_source, "Referent", "Referee"))

reviews <- reviews %>%
  dplyr::left_join(reviewers, by = c("Reviewer_PersonId" = "PersonId")) %>%
  dplyr::left_join(address, by = c("Reviewer_PersonId" = "PersonBaseId")) %>%
  dplyr::left_join(contacts, by = c("Reviewer_PersonId" = "PersonBaseId")) %>%
  dplyr::left_join(reviewer_source, 
                   by = c("Reviewer_PersonId", "ApplicationId")) %>%
  dplyr::left_join(grants, by = "ApplicationId") 

if (distinct_grade){
reviews_2016only <- reviews %>%
  dplyr::inner_join(dist_grades, by = c('ApplicationId',
                                        'Reviewer_PersonId' = 'PersonId')) 
}
readr::write_csv(reviews, path = bzfile("data/reviews_original.csv.bz2"))

# warning: only about half of reviewers' contacts "app.Reviewer"
dbDisconnect(con)

# clean and anonymize
set.seed(5)# to make everything reproducible
anonymize_person <- tibble(
  ga_person_id = unique(c(reviews$Reviewer_PersonId,
                          reviews$ResponsibleApplicant_PersonBaseId)),
  ppl_id = sample(length(ga_person_id)))

anonymize_application <- tibble(
  ga_grant_id = unique(reviews$ApplicationId, reviews$Number),
  project_id = sample(length(ga_grant_id)))

saveRDS(list(anonymize_person = anonymize_person,
             anonymize_application = anonymize_application),
        file = "data/lookup_tabs.Rds")

#nrow(reviews) # 42419
reviews <- reviews %>%
  dplyr::left_join(anonymize_application, by = c("ApplicationId" = "ga_grant_id")) %>%
  dplyr::left_join(anonymize_person, by = c("Reviewer_PersonId" = "ga_person_id")) %>%
  dplyr::rename(reviewer_id = ppl_id) %>%
  dplyr::left_join(
    anonymize_person,
    by = c("ResponsibleApplicant_PersonBaseId" = "ga_person_id")) %>%
  dplyr::rename(applicant_id = ppl_id) %>%
  dplyr::mutate(
    reviewer_email_ending = str_extract(reviewer_email, "[.][[:alpha:]]+$"),
    reviewer_email_ending = str_replace_all(reviewer_email_ending, "[.]", ""),
    reviewer_country = ifelse(is.na(reviewer_country),
                              reviewer_email_ending,
                              reviewer_country),
    reviewer_origin = ifelse(reviewer_country == "ch", reviewer_country, "int"),
    reviewer_iso = str_to_lower(reviewer_country),
    applicant_iso = str_to_lower(ApplicantNationality),
    applicant_nationality = str_to_lower(ApplicantNationality),
    applicant_nationality = ifelse(applicant_nationality == "ch", "ch", "int"),
    applicant_birth_year = as.integer(year(ResponsibleApplicantBirthDate))) %>%
  dplyr::rename(
    call_end_date = CallEndDate,
    applicant_gender = ResponsibleApplicantGender) %>%
  dplyr::select(-reviewer_email,
                -reviewer_email_ending,
                -reviewer_country) %>%
  dplyr::select(sort(current_vars())) %>%
  dplyr::select(matches("^[a-z]", ignore.case = FALSE)) %>%
  dplyr::filter(!is.na(reviewer_source),
                !is.na(review_score),
                !is.na(project_id),
                !is.na(reviewer_id)) %>%
  dplyr::select(review_score, call_end_date, project_id, everything()) %>%
  dplyr::arrange(call_end_date, project_id)

nrow(reviews) #40978 (-1441)

# recode variables --------------------------------------------------------
reviews <- reviews %>%
  dplyr::mutate(
    applicant_affiliation = str_trim(applicant_affiliation),
    applicant_affiliation = str_replace_all(
      applicant_affiliation, "^Uni(.*)", "UAS/UTE"),
    applicant_affiliation = str_replace_all(
      applicant_affiliation, "^Canton(.*)", "Universities"),
    applicant_affiliation = str_replace_all(
      applicant_affiliation, "^Hospital(.*)|^[Oo](.*)", "Other"))

# reduce the number of research topics
# reviews <- readRDS("reviews_raw.rds.bz2")
reviews <- reviews %>%
  dplyr::mutate(
    research_topic = as.factor(research_topic),
    research_topic = recode_factor(
      research_topic,
      `Art studies` = "Architecture",
      `Basic Biological Research` = "Biology",
      `General Biology` = "Biology",
      `Ethnology` = "Sociology",
      `Astronomy` = "Math./Physics",
      `Physics` = "Math./Physics",
      `Mathematics` = "Math./Physics",
      `Social Medicine` = "Medicine",
      `Basic Medical Sciences` = "Medicine",
      `Experimental Medicine` = "Medicine",
      `Clinical Medicine` = "Medicine",
      `Preventive Medicine` = "Medicine",
      `Environmental Sciences` = "Chemistry",
      `Earth Sciences` = "Geology",
      `Theology & religious studies` = "History",
      `Linguistics and literature` = "Linguistics",
      `Engineering Sciences` = "Engineering"))


# final polishing ----------------------------------------------------------
reviews <- reviews %>%
  dplyr::filter(call_end_date < "2016-10-01") %>%
  dplyr::rename(
    overall_score = review_score,
    review_origin = reviewer_origin,
    referral_type = reviewer_source) %>%
  dplyr::mutate(
    overall_score = as.numeric(overall_score),
    applicant_gender = as.factor(applicant_gender),
    applicant_nationality = as.factor(applicant_nationality),
    applicant_age = year(call_end_date) - applicant_birth_year,
    applicant_age = round(applicant_age / 10, 1),
  #  applicant_age = round_any(applicant_age, .5),
    applicant_affiliation = as.factor(applicant_affiliation),
    applicant_affiliation = recode_factor(applicant_affiliation,
                                          `UAS/UTE` = "Other"),
    review_origin = as.factor(review_origin),
    review_origin = recode_factor(review_origin,
                                  `ch` = "national",
                                  `int` = "international"),
    reviewer_gender = as.factor(reviewer_gender),
    reviewer_source = referral_type, 
                # added by RH to see all of the categories, for descr.stats
    referral_type = as.factor(referral_type),
    referral_type = recode_factor(
      referral_type,
      `Applicant` = "applicant",
      .default = "other"),
    call_cut = ifelse(call_end_date < "2011-10-01", "older", "newer"),
    call_cut = as.factor(call_cut),
    call_end_date = as.factor(call_end_date)) %>%
  dplyr::select(-applicant_birth_year)



#readr::write_csv(reviews, bzfile("../data/reviews.csv.bz2"))
readr::write_csv(reviews, path = bzfile("data/reviews_AGEupdate.csv.bz2"))


### ---- 
if (distinct_grade){
theIDs <- anonymize_application$ga_grant_id[which(anonymize_application$project_id %in% reviews$project_id)]
reviews_2016only <- reviews_2016only %>%
  dplyr::mutate(
    review_score = as.numeric(review_score),
    track_record = as.numeric(track_record),
    feasibility = as.numeric(feasibility),
    relevance_originality_topicality =
      as.numeric(relevance_originality_topicality)) %>% 
  dplyr::filter(ApplicationId %in% theIDs)


readr::write_csv(reviews_2016only, path = "data/reviews_AGEupdate2016only.csv")
}
