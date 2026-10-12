# MAUDE Entities ----

# MAUDE records the manufacturing entity as whatever the reporter typed, and
# neither the manufacturer nor the brand field is a controlled vocabulary. This
# file holds the curated facts that turn those strings into companies, and the
# lookups that read them in both directions.
#
# # Why the facts live here rather than in a dataset
#
# There are 61 entities and about 110 facts. Carrying them as CSVs under
# `data-raw/`, rebuilt into `.rda` files by a script, meant that adding one
# company was an edit, a sourced script, a regenerated dataset and a package
# rebuild. As `tribble()`s in a registry function it is an edit and
# `load_all()`, the diff is still one line per fact, and there is one file to
# read instead of six. `data-raw/complications.R` is the precedent.
#
# # Aliases, not regular expressions
#
# `aliases` holds literal search terms, pipe-separated, and that is what makes
# the table work in both directions. Searching openFDA uses the terms as they
# stand; matching a returned string escapes each one and wraps it in `\b`.
# Three characters are read rather than escaped. A leading `^` and a trailing
# `$` anchor a term that is only safe anchored -- `MPRI` is a Medtronic plant,
# `MPRIME` is not. A trailing `*` lets the term match into a longer word, which
# MAUDE needs constantly: "POLARXFIT", "VARIPULSETM", "MEDTRONICS". And a space
# matches any run of punctuation or none, since one catheter is filed as
# "THERMO COOL", "THERMO-COOL" and "THERMOCOOL".
#
# A regular expression could not be searched on, which is why the reverse
# direction did not exist before: `maude_search_terms()` has to hand openFDA
# something it will accept.
#
# # One row per entity per ownership interval
#
# `parent`, `from` and `to` are dated corporate ownership, and only that. A
# relationship with no date -- a plant, a contract manufacturer -- is an alias
# of the entity whose product it is, with `note` saying why. Most entities are
# one row. An entity that was sold twice takes one row per interval, which is
# how a divestiture is expressed: Physio-Control belongs to Medtronic until
# 2012, to nobody in the table until 2016, and to Stryker after that.
#
# Three rules keep the ownership columns from becoming a corporate database:
#
#   1. An entity earns a row only if an alias names it, or if it sits on a chain
#      between one and its parent. Nothing speculative.
#   2. A row needs a date that can be stated. Where the closing date could not be
#      established the row says so in `note` and leaves `source` empty, so the
#      gap stays visible rather than looking settled.
#   3. The chain stops at the operating company the field would name. ZOLL has
#      been an Asahi Kasei subsidiary since 2012, and nobody calls it an Asahi
#      Kasei defibrillator.
#
# # Precedence
#
# There is no priority column. When a string matches more than one entity the
# deeper one in the ownership graph wins, so "MEDTRONIC CRYOCATH LP" resolves to
# CryoCath because Medtronic is its parent. Unrelated entities matching the same
# string fall back to the longest matched alias, and a test asserts that no such
# tie occurs on the labelled fixture.

#' The MAUDE Manufacturing Entity Registry
#'
#' @description Curated table of the manufacturing entities that appear in
#'   MAUDE, the strings each is known by, and the dated corporate ownership
#'   above it. Read by [maude_normalize_manufacturer()], [maude_resolve_owner()]
#'   and [maude_search_terms()].
#'
#' @details
#' `aliases` are literal search terms separated by `|`, matched
#' case-insensitively on word boundaries. A leading `^` or trailing `$` anchors
#' the term, which a few plant codes need. A trailing `*` lets a term match into
#' a longer word, as "MEDTRONICS" and "POLARXFIT" require, and a space matches
#' any run of punctuation or none, since MAUDE writes a compound name every way.
#'
#' One row per entity per ownership interval. `parent` is the company that owned
#' the entity between `from` and `to`, either of which may be `NA` for an
#' unbounded interval, and an entity with no row covering a date owned itself on
#' that date. `note` records why a row reads as it does, and `source` cites the
#' closing date. A row without a `source` has a date that could not be traced to
#' a primary announcement, and its `note` says how approximate the date is.
#'
#' @return A `tbl_df` with columns `entity`, `aliases`, `parent`, `from`, `to`,
#'   `note` and `source`. `from` and `to` are `Date`.
#'
#' @seealso [maude_platforms()] for the ablation brands,
#'   [maude_normalize_manufacturer()] and [maude_search_terms()] for the lookups
#'   over this table.
#'
#' @examples
#' # Every entity that Medtronic has acquired
#' entities <- maude_entities()
#' entities[!is.na(entities$parent) & entities$parent == "Medtronic", ]
#'
#' @export
maude_entities <- function() {
  out <- tibble::tribble(
    ~entity,                       ~aliases,                                                             ~parent,             ~from,         ~to,           ~note,                                                                                                  ~source,

    # Medtronic and the entities beneath it
    "Medtronic",                   "medtronic*|^mpri$|ipg mfg switzerland|^perfusion systems|plexus",      NA,                  NA,            NA,            "MPRI, IPG Manufacturing Switzerland and Perfusion Systems are plants; Plexus builds under contract",   NA,
    "CryoCath",                    "cryocath",                                                            "Medtronic",         "2008-11-12",  NA,            "remaining shares acquired 2008-12-22",                                                                 "https://www.dicardiology.com/content/medtronic-acquires-remaining-shares-cryocath-technologies-inc",
    "Affera",                      "affera",                                                              "Medtronic",         "2022-08-30",  NA,            NA,                                                                                                     "https://news.medtronic.com/2022-08-30-Medtronic-completes-acquisition-of-Affera-Inc",
    "EPiX Therapeutics",           "epix therapeutics",                                                   "Medtronic",         "2019-01-24",  NA,            "agreement announced 2019-01-24; the closing was not separately announced and fell in Medtronic's fiscal Q4 ending 2019-04-26", "https://news.medtronic.com/2019-01-24-Medtronic-to-Acquire-EPIX-Therapeutics-Expanding-Its-Cardiac-Ablation-Portfolio",
    "Covidien",                    "covidien",                                                            "Medtronic",         "2015-01-26",  NA,            NA,                                                                                                     "https://www.sec.gov/Archives/edgar/data/0001613103/000119312515021837/d859367dex991.htm",
    "ev3",                         "ev3|micro therapeutics",                                              "Covidien",          "2010-07-12",  NA,            "tender offer expired 2010-07-09; merger completed 2010-07-12",                                         "https://www.biospace.com/covidien-successfully-completes-tender-offer-for-shares-of-ev3-inc",

    # Boston Scientific and the entities beneath it
    "Boston Scientific",           "boston scientific*|stellartech",                                       NA,                  NA,            NA,            "Stellartech builds the Maestro and RF3000 generators under contract",                                  NA,
    "Farapulse",                   "farapulse",                                                           "Boston Scientific", "2021-08-02",  NA,            NA,                                                                                                     "https://news.bostonscientific.com/2021-08-02-Boston-Scientific-Completes-Acquisition-of-Farapulse-Inc",
    "Guidant",                     "guidant",                                                             "Boston Scientific", "2006-04-21",  NA,            "cardiac rhythm management business; the vascular business went to Abbott",                              "https://www.ftc.gov/news-events/news/press-releases/2006/04/ftc-requires-asset-divestitures-allowing-boston-scientifics-27-billion-acquisition-guidant",
    "Cardiac Pacemakers",          "cardiac pacemakers|^cpi",                                             "Guidant",           NA,            NA,            "Guidant's cardiac rhythm management subsidiary; the relationship predates the period MAUDE covers",    NA,
    "Cameron Health",              "cameron health",                                                      "Boston Scientific", "2012-06-08",  NA,            NA,                                                                                                     "https://news.bostonscientific.com/2012-06-08-Boston-Scientific-Closes-Cameron-Health-Acquisition",
    "Baylis Medical",              "baylis medical*",                                                      "Boston Scientific", "2022-02-14",  NA,            "announced as closed on 2022-02-15",                                                                    "https://news.bostonscientific.com/2022-02-15-Boston-Scientific-Closes-Acquisition-of-Baylis-Medical-Company-Inc",
    "Preventice Solutions",        "preventice",                                                          "Boston Scientific", "2021-03-01",  NA,            NA,                                                                                                     "https://www.sec.gov/Archives/edgar/data/885725/000088572521000014/bsx-20210331.htm",
    "EP Technologies",             "ep technologies|ep technology|ept",                                   "Boston Scientific", "1996-01-01",  NA,            "merger consummated in early 1996; closing date approximate to the year",                               NA,
    "BTG",                         "btg",                                                                 "Boston Scientific", "2019-08-19",  NA,            NA,                                                                                                     "https://news.bostonscientific.com/2019-08-19-Boston-Scientific-Completes-Acquisition-of-BTG-plc",
    "Galil Medical",               "galil medical",                                                       "BTG",               "2016-06-15",  NA,            "agreement announced 2016-05-06 to close before the end of June 2016; closing date approximate",         NA,

    # Abbott and the entities beneath it
    "Abbott",                      "abbott|venusa|av-temecula|medfact",                                   NA,                  NA,            NA,            "Venusa, the Temecula plant and Medfact build for Abbott; none is an ownership relationship",           NA,
    "St. Jude Medical",            "st. jude|st jude",                                             "Abbott",            "2017-01-04",  NA,            NA,                                                                                                     "https://abbott.mediaroom.com/2017-01-04-Abbott-Completes-Acquisition-of-St-Jude-Medical",
    "Telectronics Pacing Systems", "telectronics",                                                        "St. Jude Medical",  "1996-11-29",  NA,            "cardiac rhythm management assets acquired from Pacific Dunlop, effective 1996-11-29",                   "https://www.sec.gov/Archives/edgar/data/0000203077/000020307796000019/0000203077-96-000019.txt",
    "Thoratec",                    "thoratec",                                                            "St. Jude Medical",  "2015-10-08",  NA,            NA,                                                                                                     "https://www.businesswire.com/news/home/20151008006482/en/St.-Jude-Medical-Completes-Acquisition-Thoratec",
    "CardioMEMS",                  "cardiomems",                                                          "St. Jude Medical",  "2014-05-30",  NA,            NA,                                                                                                     "https://www.dicardiology.com/article/st-jude-medical-completes-acquisition-cardiomems",
    "Irvine Biomedical",           "irvine biomedical",                                                   "St. Jude Medical",  "2004-10-07",  NA,            "remaining 86% of the stock acquired 2004-10-07",                                                       "https://www.sec.gov/Archives/edgar/data/203077/000089710105000696/stjude051052_10k.htm",

    # Johnson & Johnson
    "Biosense Webster",            "biosense|stockert|^hei",                                              "Johnson & Johnson", "1998-08-01",  NA,            "formed in August 1998 from Biosense, bought by J&J in September 1997, and Cordis Webster; Stockert builds the generators and HEI the irrigation pump under contract", "https://insights.citeline.com/MT083258/Johnson-and-Johnson-combines-Biosense-with-Cordis-subsidiary/",
    "Abiomed",                     "abiomed",                                                             "Johnson & Johnson", "2022-12-22",  NA,            NA,                                                                                                     "https://www.jnj.com/media-center/press-releases/johnson-johnson-completes-acquisition-of-abiomed",
    "Cordis",                      "cordis",                                                              "Johnson & Johnson", NA,            "2015-10-02",  "divested to Cardinal Health",                                                                          "https://www.jnj.com/media-center/press-releases/johnson-johnson-announces-completion-of-cordis-divestiture-to-cardinal-health",
    "Cordis",                      "cordis",                                                              "Cardinal Health",   "2015-10-02",  "2021-08-02",  "divested to Hellman & Friedman",                                                                        "https://www.prnewswire.com/news-releases/cardinal-health-completes-acquisition-of-cordis-300153418.html",
    "Cordis",                      "cordis",                                                              "Hellman & Friedman", "2021-08-02", NA,            NA,                                                                                                     "https://www.beckershospitalreview.com/supply-chain/cardinal-completes-sale-of-cordis-medical-device-business/",

    # Philips
    "Philips",                     "philips",                                                             NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Spectranetics",               "spectranetics",                                                       "Philips",           "2017-08-09",  NA,            "tender offer expired and the merger completed on 2017-08-09",                                          "https://www.dicardiology.com/article/philips-acquires-spectranetics-expanding-its-image-guided-therapy-device-offerings",
    "Volcano",                     "volcano",                                                             "Philips",           "2015-02-17",  NA,            NA,                                                                                                     "https://www.sec.gov/Archives/edgar/data/313216/000119312515052556/d875094d6k.htm",
    "BioTelemetry",                "biotelemetry",                                                        "Philips",           "2021-02-09",  NA,            NA,                                                                                                     "https://www.philips.com/a-w/about/news/archive/standard/news/press/2021/20210209-philips-completes-the-acquisition-of-biotelemetry-inc.html",
    "Remote Diagnostic Technologies", "remote diagnostic",                                                "Philips",           "2018-06-13",  NA,            NA,                                                                                                     "https://www.philips.com/a-w/about/news/archive/standard/news/press/2018/20180613-philips-expands-its-therapeutic-care-business-with-the-acquisition-of-remote-diagnostic-technologies.html",

    # Stryker, and the entity that changed hands twice
    "Stryker",                     "stryker",                                                             NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Physio-Control",              "physio control",                         "Medtronic",         NA,            "2012-01-30",  "divested to Bain Capital in January 2012",                                                              "https://www.massdevice.com/stryker-closes-1-3b-physio-control-deal/",
    "Physio-Control",              "physio control",                         "Stryker",           "2016-04-05",  NA,            NA,                                                                                                     "https://investors.stryker.com/press-releases/news-details/2016/Stryker-completes-acquisition-of-Physio-Control-International-Inc/default.aspx",
    "HeartSine Technologies",      "heartsine",                                                           "Physio-Control",    "2015-09-14",  NA,            "announced 2015-09-14 and completed that September; closing date approximate to the month",              NA,

    # Becton Dickinson
    "Becton Dickinson",            "becton|^bd",                                                          NA,                  NA,            NA,            NA,                                                                                                     NA,
    "C. R. Bard",                  "bard",                                                                "Becton Dickinson",  "2017-12-29",  NA,            NA,                                                                                                     "https://www.sec.gov/Archives/edgar/data/0000010795/000119312517383523/d517273dex991.htm",
    "Medivance",                   "medivance",                                                           "C. R. Bard",        "2011-11-01",  NA,            "agreement announced 2011-10-25 to close in the fourth quarter of 2011; closing date approximate",       "https://www.businesswire.com/news/home/20111025006977/en/Bard-Announces-Agreement-to-Acquire-Medivance-Inc.",

    # Getinge
    "Getinge",                     "getinge",                                                             NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Datascope",                   "datascope",                                                           "Getinge",           "2009-01-30",  NA,            "tender offer completed 2009-01-30",                                                                    "https://www.sec.gov/Archives/edgar/data/0000027096/000095014409000652/g17518e8vk.htm",
    "Maquet",                      "maquet",                                                              "Getinge",           "2000-11-17",  NA,            "acquired from RWE; part of the Getinge Group from 2001-01-01",                                          "https://www.getinge.com/us/company/news/press-releases/2000/30636-Getinge-acquires-Maquet---global-leader-in-surgical/",

    # LivaNova
    "LivaNova",                    "livanova",                                                            NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Sorin",                       "sorin",                                                               "LivaNova",          "2015-10-19",  NA,            "merged with Cyberonics to form LivaNova",                                                              "https://www.sec.gov/Archives/edgar/data/0001639691/000163969116000042/livn-20160331x10q.htm",
    "Cyberonics",                  "cyberonics",                                                          "LivaNova",          "2015-10-19",  NA,            "merged with Sorin to form LivaNova",                                                                   "https://www.sec.gov/Archives/edgar/data/0001639691/000163969116000042/livn-20160331x10q.htm",

    # ZOLL, and the owner between Cardiac Science's two homes
    "ZOLL",                        "zoll",                                                                NA,                  NA,            NA,            "terminal by rule: ZOLL has been an Asahi Kasei subsidiary since 2012, but the field names ZOLL",        NA,
    "Cardiac Science",             "cardiac science",                                                     "Opto Circuits",     "2010-12-01",  "2019-08-26",  "acquisition announced 2010-10-19; closing date approximate to the quarter",                             "https://www.dicardiology.com/content/opto-circuits-acquire-cardiac-science",
    "Cardiac Science",             "cardiac science",                                                     "ZOLL",              "2019-08-26",  NA,            NA,                                                                                                     "https://www.zoll.com/en-us/about/news-and-events/press-releases/press-release/2019/08/27/zoll-medical-completes-acquisition-of-cardiac-science",

    # Teleflex
    "Teleflex",                    "teleflex",                                                            NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Arrow International",         "arrow int*",                                                 "Teleflex",          "2007-10-01",  NA,            NA,                                                                                                     "https://investors.teleflex.com/news/news-details/2007/Teleflex-Completes-Arrow-International-Acquisition/default.aspx",

    # AtriCure
    "AtriCure",                    "atricure",                                                            NA,                  NA,            NA,            NA,                                                                                                     NA,
    "nContact Surgical",           "ncontact",                                                            "AtriCure",          "2015-10-13",  NA,            "merger agreement effective 2015-10-04, completed 2015-10-13",                                           "https://ir.atricure.com/news-releases/news-release-details/atricure-completes-acquisition-ncontact",

    # Entities that own themselves, in the order they are most often met
    "Biotronik",                   "biotronik",                                                           NA,                  NA,            NA,            NA,                                                                                                     NA,
    "CardioFocus",                 "cardiofocus",                                                         NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Acutus Medical",              "acutus medical",                                                      NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Adagio Medical",              "adagio",                                                              NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Cook Medical",                "cook",                                                                NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Defibtech",                   "defibtech",                                                           NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Edwards Lifesciences",        "edwards",                                                             NA,                  NA,            NA,            NA,                                                                                                     NA,
    "W. L. Gore",                  "gore",                                                                NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Masimo",                      "masimo",                                                              NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Merit Medical",               "merit medical",                                                       NA,                  NA,            NA,            NA,                                                                                                     NA,
    "MicroPort",                   "microport",                                                           NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Nihon Kohden",                "nihon kohden",                                                        NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Penumbra",                    "penumbra",                                                            NA,                  NA,            NA,            NA,                                                                                                     NA,
    "Terumo",                      "terumo",                                                              NA,                  NA,            NA,            NA,                                                                                                     NA,
  )

  out$from <- as.Date(out$from)
  out$to <- as.Date(out$to)
  out
}

#' The MAUDE Cardiac Ablation Platform Registry
#'
#' @description Curated table mapping the brand strings that appear in MAUDE
#'   onto the ablation platform they belong to, the energy that platform
#'   delivers, and the entity that makes it. Read by
#'   [maude_normalize_ablation()].
#'
#' @details
#' `aliases` work as they do in [maude_entities()]: literal terms separated by
#' `|`, matched case-insensitively on word boundaries.
#'
#' `modality` is `NA` for a device that is matched but delivers no energy, which
#' is deliberate. A mapping catheter, a sheath and an irrigation pump all carry
#' the ablation product codes and all appear in an ablation query, and sweeping
#' them into an energy arm would count an access device as an ablation.
#'
#' `entity` names the company that makes the platform, never its corporate
#' parent, so the table does not have to be edited when a company is acquired.
#' Pass it to [maude_resolve_owner()] for the parent.
#'
#' @return A `tbl_df` with columns `platform`, `aliases`, `modality`, `entity`,
#'   `note` and `source`.
#'
#' @seealso [maude_normalize_ablation()], [maude_entities()]
#'
#' @examples
#' # Which platforms deliver pulsed field energy, and who makes them
#' platforms <- maude_platforms()
#' platforms[
#'   !is.na(platforms$modality) & platforms$modality == "pulsed field",
#'   c("platform", "entity")
#' ]
#'
#' @export
maude_platforms <- function() {
  tibble::tribble(
    ~platform,               ~aliases,                                            ~modality,                        ~entity,             ~note,                                                                                     ~source,

    # Pulsed field
    "Farapulse",             "farapulse*|farawave*|farastar*|farapoint*",             "pulsed field",                   "Farapulse",         NA,                                                                                        NA,
    "Faradrive",             "faradrive",                                         NA,                               "Farapulse",         "the steerable sheath that delivers the Farawave catheter, and not itself an ablation",     NA,
    "PulseSelect",           "pulse select",                          "pulsed field",                   "Medtronic",         NA,                                                                                        NA,
    "Varipulse",             "varipulse*|trupulse*",                                "pulsed field",                   "Biosense Webster",  "TruPulse is the generator of the Varipulse platform",                                     "https://www.jnj.com/media-center/press-releases/biosense-webster-announces-ce-mark-approval-in-europe-for-varipulse-pulsed-field-ablation-pfa-platform",
    "Volt",                  "volt",                                              "pulsed field",                   "Abbott",            NA,                                                                                        "https://abbott.mediaroom.com/2025-03-27-Abbott-Receives-CE-Mark-for-its-Volt-TM-Pulsed-Field-Ablation-System-to-Treat-Patients-with-Abnormal-Heart-Rhythms",
    "Affera Sphere-9",       "affera|sphere 9",                          "pulsed field or radiofrequency", "Affera",            "delivers both energies from one catheter; the narrative decides which was used",           NA,

    # Cryoablation
    "Arctic Front",          "arctic front*",                                      "cryoablation",                   "CryoCath",          NA,                                                                                        NA,
    "Freezor",               "freezor*",                                           "cryoablation",                   "CryoCath",          "focal cryoablation catheter",                                                             NA,
    "Medtronic CryoAblation", "cryoconsole|cryoflex",                             "cryoablation",                   "CryoCath",          "console and accessories",                                                                 NA,
    "PolarX",                "polarx*",                                            "cryoablation",                   "Boston Scientific", NA,                                                                                        "https://www.bostonscientific.com/en-EU/medical-specialties/electrophysiology/arrhythmias/single-shot-ablation/polarx-cryoballoon.html",
    "SmartFreeze",           "smartfreeze",                                       "cryoablation",                   "Boston Scientific", "console for the PolarX cryoballoon",                                                      "https://www.bostonscientific.com/content/dam/bostonscientific/ep/portfolio-group/single-shot/in-depth/documents/EP-955001%20SmartFreeze%20Console%20Operator%20Guide.pdf",

    # Laser
    "HeartLight",            "heartlight",                                        "laser",                          "CardioFocus",       NA,                                                                                        NA,

    # Radiofrequency, Biosense Webster
    "Thermocool",            "thermo cool*|thmcl|celsius*|navi star*|ez steer*|smart touch*|smtch", "radiofrequency", "Biosense Webster", "one platform under many brand spellings",                    NA,
    "QDOT Micro",            "qdot*",                                              "radiofrequency",                 "Biosense Webster",  NA,                                                                                        NA,
    "SmartAblate",           "smartablate*",                                       "radiofrequency",                 "Biosense Webster",  "radiofrequency generator",                                                                NA,
    "Stockert",              "stockert|ep shuttle",                    "radiofrequency",                 "Biosense Webster",  "radiofrequency generator",                                                                NA,
    "nMARQ",                 "nmarq",                                             "radiofrequency",                 "Biosense Webster",  "withdrawn from the market",                                                               NA,

    # Radiofrequency, St. Jude Medical
    "TactiCath",             "tacticath*|tactiflex*|tactisys*",                      "radiofrequency",                 "St. Jude Medical",  "acquired with Endosense in 2013, and marketed by Abbott since 2017",                      NA,
    "FlexAbility",           "flexability*",                                       "radiofrequency",                 "St. Jude Medical",  NA,                                                                                        NA,
    "Cool Path",             "cool path",                                         "radiofrequency",                 "St. Jude Medical",  NA,                                                                                        NA,
    "Livewire",              "livewire",                                          "radiofrequency",                 "St. Jude Medical",  NA,                                                                                        NA,
    "Safire",                "safire",                                            "radiofrequency",                 "St. Jude Medical",  NA,                                                                                        NA,
    "Therapy",               "therapy cool*|therapy ablation*",                     "radiofrequency",                 "St. Jude Medical",  NA,                                                                                        NA,
    "Ampere",                "ampere",                                            "radiofrequency",                 "St. Jude Medical",  "radiofrequency generator",                                                                NA,

    # Radiofrequency, Boston Scientific
    "Blazer",                "blazer",                                            "radiofrequency",                 "Boston Scientific", NA,                                                                                        NA,
    "IntellaNav",            "intellanav|intellatip",                             "radiofrequency",                 "Boston Scientific", NA,                                                                                        NA,
    "Chilli II",             "chilli",                                            "radiofrequency",                 "Boston Scientific", "cooled-tip ablation catheter",                                                            NA,
    "Maestro",               "maestro",                                           "radiofrequency",                 "Boston Scientific", "radiofrequency generator",                                                                NA,

    # Radiofrequency, Medtronic
    "Atakr",                 "atakr",                                             "radiofrequency",                 "Medtronic",         "radiofrequency generator",                                                                NA,
    "Marinr",                "^marinr|^rf marinr",                                "radiofrequency",                 "Medtronic",         NA,                                                                                        NA,
    "Cardioblate",           "cardioblate",                                       "radiofrequency",                 "Medtronic",         "surgical ablation",                                                                       NA,
    "DiamondTemp",           "diamond temp",                          "radiofrequency",                 "EPiX Therapeutics", NA,                                                                                        NA,

    # Surgical, AtriCure
    "EPi-Sense",             "epi sense",                                "radiofrequency",                 "AtriCure",          "epicardial ablation",                                                                     NA,
    "Cobra Fusion",          "cobra fusion*",                                      "radiofrequency",                 "AtriCure",          "surgical ablation",                                                                       NA,
    "AtriCure Bipolar",      "atricure bipolar|isolator",                         "radiofrequency",                 "AtriCure",          "surgical bipolar clamp",                                                                  NA,
    "CoolRail",              "coolrail",                                          "radiofrequency",                 "AtriCure",          "surgical linear ablation pen",                                                            NA,

    # Biotronik
    "Alcath",                "alcath",                                            "radiofrequency",                 "Biotronik",         NA,                                                                                        NA,

    # Matched, but not ablation: these share the ablation product codes
    "Pentaray",              "pentaray*",                                          NA,                               "Biosense Webster",  "mapping catheter",                                                                        NA,
    "Octaray",               "octaray*",                                           NA,                               "Biosense Webster",  "mapping catheter",                                                                        NA,
    "Advisor HD Grid",       "advisor hd grid",                     NA,                               "St. Jude Medical",  "mapping catheter",                                                                        NA,
    "EnSite",                "ensite",                                            NA,                               "St. Jude Medical",  "mapping system",                                                                          NA,
    "MetriQ",                "metriq",                                            NA,                               "St. Jude Medical",  "irrigation tubing",                                                                       NA,
    "Rhythmia",              "rhythmia",                                          NA,                               "Boston Scientific", "mapping system",                                                                          NA,
  )
}

# Matching ----

#' Turn an alias string into one regular expression
#'
#' @description Splits `aliases` on `|`, escapes each term so that punctuation
#'   is literal, and wraps it in word boundaries. A leading `^` and a trailing
#'   `$` survive as anchors, which is what a plant code such as `MPRI` needs to
#'   avoid matching every string that merely contains it.
#'
#' @param aliases One pipe-separated alias string.
#'
#' @return A single regular expression.
#'
#' @noRd
alias_pattern <- function(aliases) {
  terms <- strsplit(aliases, "|", fixed = TRUE)[[1]]
  terms <- trimws(terms)
  terms <- terms[nzchar(terms)]

  built <- vapply(terms, function(term) {
    starts <- startsWith(term, "^")
    ends <- endsWith(term, "$")
    prefix <- endsWith(term, "*")
    body <- term
    if (starts) body <- substring(body, 2L)
    if (ends || prefix) body <- substring(body, 1L, nchar(body) - 1L)

    body <- gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", body, perl = TRUE)

    # A space stands for any run of punctuation, or none at all. MAUDE writes
    # one catheter as "THERMO COOL", "THERMO-COOL" and "THERMOCOOL", and one
    # brand as "THERAPY<?> ABLATION CATHETER", where the mark is a mangled
    # trademark symbol.
    body <- gsub(" ", "[^A-Za-z0-9]*", body, fixed = TRUE)

    # The leading boundary is what stops a short alias matching inside a longer
    # word: `bard` must not claim "LOMBARD". It is written as a lookbehind
    # rather than as `\\b` because `_` is a word character to a regular
    # expression and is not one to MAUDE, which files brands as "UNK_QDOT".
    paste0(
      if (starts) "^" else "(?<![A-Za-z0-9])",
      body,
      if (ends) "$" else if (prefix) "" else "(?![A-Za-z0-9])"
    )
  }, character(1), USE.NAMES = FALSE)

  paste(built, collapse = "|")
}

#' Order entities so that a subsidiary is tested before its parent
#'
#' @description Depth in the ownership graph, counted by following `parent`
#'   upward. Testing deeper entities first is what makes
#'   "MEDTRONIC CRYOCATH LP" resolve to CryoCath, and it replaces the priority
#'   column the CSVs carried, which said the same thing by hand.
#'
#' @param registry A registry as returned by [maude_entities()].
#'
#' @return An integer vector of depths, one per row.
#'
#' @noRd
entity_depth <- function(registry) {
  # One parent per entity is enough to rank it, so an entity sold twice is
  # represented by whichever row names a parent first.
  named <- registry[!is.na(registry$parent), c("entity", "parent")]
  named <- named[!duplicated(named$entity), ]

  depth_of <- function(entity) {
    depth <- 0L
    seen <- character(0)
    current <- entity
    while (depth < 20L) {
      hit <- match(current, named$entity)
      # A parent that is not itself an entity ends the chain, which is what
      # a holding company named only as an owner looks like.
      if (is.na(hit) || named$parent[[hit]] %in% seen) {
        break
      }
      seen <- c(seen, current)
      current <- named$parent[[hit]]
      depth <- depth + 1L
    }
    depth
  }

  vapply(registry$entity, depth_of, integer(1), USE.NAMES = FALSE)
}

#' Assign each string the entity or platform whose alias matches it
#'
#' @description Walks the registry in precedence order and gives each input the
#'   first row that matches it. Precedence is depth in the ownership graph where
#'   there is one, then the length of the matched alias, so a specific name is
#'   never claimed by the general one above it.
#'
#' @param x Character vector of raw MAUDE strings.
#' @param registry A registry with an `aliases` column.
#' @param depth Optional integer vector of depths, one per row.
#'
#' @return An integer vector the same length as `x`, giving the matching row, or
#'   `NA_integer_` where nothing matched.
#'
#' @noRd
match_maude_alias <- function(x, registry, depth = NULL) {
  x <- as.character(x)
  out <- rep(NA_integer_, length(x))
  if (!nrow(registry)) {
    return(out)
  }

  patterns <- vapply(registry$aliases, alias_pattern, character(1), USE.NAMES = FALSE)
  if (is.null(depth)) {
    depth <- rep(0L, nrow(registry))
  }
  longest <- vapply(
    strsplit(registry$aliases, "|", fixed = TRUE),
    function(terms) max(nchar(terms)),
    numeric(1)
  )
  order_of <- order(-depth, -longest, seq_len(nrow(registry)))

  for (i in order_of) {
    pending <- which(is.na(out) & !is.na(x))
    if (!length(pending)) {
      break
    }
    hit <- grepl(patterns[[i]], x[pending], ignore.case = TRUE, perl = TRUE)
    out[pending[hit]] <- i
  }

  out
}

#' Find each entity's parent on a given date
#'
#' @description One hop up the ownership chain, taking the row whose interval
#'   contains `when`. An entity with no row matching that date has no parent
#'   then, which is how an entity that owns itself is expressed.
#'
#' @param child Character vector of entity names.
#' @param when Date vector the same length as `child`.
#' @param registry A registry as returned by [maude_entities()].
#'
#' @return A character vector of parents, `NA_character_` where there is none.
#'
#' @noRd
parent_of <- function(child, when, registry) {
  out <- rep(NA_character_, length(child))

  for (i in seq_len(nrow(registry))) {
    if (is.na(registry$parent[[i]])) {
      next
    }
    hit <- is.na(out) &
      !is.na(child) & child == registry$entity[[i]] &
      (is.na(registry$from[[i]]) | when >= registry$from[[i]]) &
      (is.na(registry$to[[i]]) | when < registry$to[[i]])
    out[which(hit)] <- registry$parent[[i]]
  }

  out
}

# Lookups ----

#' Normalise MAUDE Manufacturer and Device Strings
#'
#' @description
#' `maude_normalize_manufacturer()` maps the `manufacturer_name` returned by
#' [maude_query()] onto the entity that made the device, collapsing plants,
#' spelling variants, and contract manufacturers onto one name.
#'
#' `maude_normalize_ablation()` maps a cardiac ablation `device_brand_name` onto
#' its platform, the energy modality it delivers, and the entity that makes it.
#'
#' `maude_resolve_owner()` follows an entity up its chain of acquisitions to the
#' company that owned it on a given date.
#'
#' `maude_search_terms()` runs the other way, from a company to every name its
#' reports have been filed under, which is what a query has to search on.
#'
#' @details
#' All four read [maude_entities()] or [maude_platforms()], and all return `NA`
#' rather than a guess for a value the registry does not cover. On the result of
#' [maude_query()] the usual pipeline is:
#'
#' ```r
#' events |>
#'   dplyr::mutate(
#'     entity  = maude_normalize_manufacturer(manufacturer_name),
#'     company = maude_resolve_owner(entity, as_of = Sys.Date()),
#'     at_time = maude_resolve_owner(entity, as_of = date_received)
#'   )
#' ```
#'
#' # Why normalising and resolving are separate
#'
#' Which company a string names does not change with time. Who owns that company
#' does. Keeping them apart is what lets one acquisition be one row rather than a
#' rewrite of every alias beneath it, and it is why `maude_resolve_owner()`
#' requires `as_of` while `maude_normalize_manufacturer()` takes no date at all.
#'
#' In the pipeline above, `company` counts every St. Jude report as Abbott, which is what a question
#' about today's market wants. `at_time` counts a 2010 report as St. Jude, which
#' is what a question about who was shipping the device then wants. Neither is a
#' default, because picking one silently would answer a question the caller did
#' not ask.
#'
#' # Searching for a company that has acquired others
#'
#' Normalising happens after the reports are in hand, so it cannot help with the
#' reports a query never asked for. A filter on `"Abbott"` does not reach the
#' St. Jude reports of 2016, and nothing about the result says so.
#' `maude_search_terms()` is the direction that fixes it:
#'
#' ```r
#' abbott <- maude_search_terms("Abbott")
#' maude_query(manufacturer_name = abbott$term, limit = 1000)
#' ```
#'
#' With `as_of`, the set is restricted to what the company owned on that date;
#' without it, every name the company's entities have ever filed under is
#' returned, which is what a retrospective query wants.
#'
#' # Why the modality needs the brand
#'
#' Product code does not determine ablation modality. `OAE` covers cryoablation
#' and radiofrequency alike, so a rule that resolves it from the product code and
#' falls back to radiofrequency will put cryoballoon, pulsed-field, and laser
#' devices into a radiofrequency arm. Deriving the modality from the brand is what
#' separates them, and a matched device that delivers no energy, such as a
#' mapping catheter or a sheath, carries `NA` rather than being swept into an arm.
#'
#' @param manufacturer_name Character vector of raw MAUDE manufacturer strings,
#'   as returned in the `manufacturer_name` column of [maude_query()].
#'
#' @param device_brand_name Character vector of raw MAUDE brand strings, as
#'   returned in the `device_brand_name` column of [maude_query()].
#'
#' @param entity Character vector of entity names, as returned by
#'   `maude_normalize_manufacturer()` or in the `entity` column of
#'   `maude_normalize_ablation()`.
#'
#' @param company The company to search from, named as the registry names it.
#'
#' @param as_of For `maude_resolve_owner()`, the date to resolve ownership at,
#'   recycled against `entity`. There is deliberately no default: the answer
#'   changes with it, and a `Sys.Date()` default would make the same code return
#'   different results after a future acquisition without anything in the call
#'   having changed. Pass `Sys.Date()` for the company that owns the entity now,
#'   or a report's `date_received` for the company that owned it when the report
#'   was filed. For `maude_search_terms()` it is optional, and restricts the
#'   result to the entities the company owned on that date.
#'
#' @param entities The entity registry to read, defaulting to [maude_entities()].
#'   Supply your own to extend or override it; it must carry the same columns.
#'
#' @param platforms The platform registry to read, defaulting to
#'   [maude_platforms()].
#'
#' @return `maude_normalize_manufacturer()` returns a character vector the same
#'   length as `manufacturer_name`, holding the entity name or `NA` where the
#'   registry does not cover the string.
#'
#'   `maude_resolve_owner()` returns a character vector the same length as
#'   `entity`, holding the company that owned it on `as_of`. An entity nobody
#'   owned on that date resolves to itself, which is how a divestiture is
#'   expressed: Physio-Control resolves to Medtronic in 2005, to itself in 2014,
#'   and to Stryker in 2020.
#'
#'   `maude_search_terms()` returns a `tbl_df` with one row per searchable term:
#'   - `entity` — the entity the term names.
#'   - `term` — the string to search openFDA on, anchors removed.
#'   - `from`, `to` — the window during which `company` owned that entity, `NA`
#'     where the window is unbounded. The company's own terms carry `NA` for
#'     both.
#'
#'   `maude_normalize_ablation()` returns a `tbl_df` with one row per input:
#'   - `device_brand_name` — the input string, unchanged.
#'   - `platform` — the device platform, such as `"Arctic Front"`.
#'   - `modality` — one of `"cryoablation"`, `"radiofrequency"`,
#'     `"pulsed field"`, `"pulsed field or radiofrequency"`, or `"laser"`. `NA`
#'     both for an unmatched brand and for a matched device that delivers no
#'     energy, so check `platform` to tell the two apart.
#'   - `entity` — the entity that makes the platform. Pass it to
#'     `maude_resolve_owner()` for the corporate parent.
#'
#' @seealso [maude_entities()] and [maude_platforms()] for the registries these
#'   read, and [maude_query()] for the columns they are applied to.
#'
#' @examples
#' # Plants, spelling variants, and a contract manufacturer collapse onto one
#' # entity, without anyone having to say when
#' maude_normalize_manufacturer(c(
#'   "BOSTON SCIENTIFIC CORPORATION",
#'   "FARAPULSE, INC.",
#'   "BIOSENSE WEBSTER, INC. (JUAREZ)",
#'   "VENUSA DE MEXICO S.A. DE C.V.",
#'   "SOME COMPANY NOT IN THE REGISTRY"
#' ))
#'
#' # Ownership is the part that needs a date
#' maude_resolve_owner(
#'   "St. Jude Medical",
#'   as_of = as.Date(c("2010-01-01", "2024-01-01"))
#' )
#'
#' # A divestiture, which a single-parent table cannot express
#' maude_resolve_owner(
#'   "Physio-Control",
#'   as_of = as.Date(c("2005-01-01", "2014-01-01", "2020-01-01"))
#' )
#'
#' # The other direction: what a query for Abbott has to search on
#' maude_search_terms("Abbott")
#'
#' # Modality comes from the brand, not the product code
#' maude_normalize_ablation(c(
#'   "ARCTIC FRONT ADVANCE PRO CARDIAC CRYOABLATION CATHETER",
#'   "FARAWAVE PULSED FIELD ABLATION CATHETER",
#'   "PENTARAY NAV"
#' ))
#'
#' @name maude_normalization
NULL

#' @rdname maude_normalization
#' @export
maude_normalize_manufacturer <- function(
  manufacturer_name,
  entities = maude_entities()
) {
  if (!is.character(manufacturer_name) && !all(is.na(manufacturer_name))) {
    stop("'manufacturer_name' must be a character vector", call. = FALSE)
  }
  validate_maude_registry(entities, c("entity", "aliases", "parent"), "entities")

  entities$entity[
    match_maude_alias(manufacturer_name, entities, entity_depth(entities))
  ]
}

#' @rdname maude_normalization
#' @export
maude_normalize_ablation <- function(
  device_brand_name,
  platforms = maude_platforms()
) {
  if (!is.character(device_brand_name) && !all(is.na(device_brand_name))) {
    stop("'device_brand_name' must be a character vector", call. = FALSE)
  }
  validate_maude_registry(
    platforms,
    c("platform", "aliases", "modality", "entity"),
    "platforms"
  )

  matched <- match_maude_alias(device_brand_name, platforms)

  tibble::tibble(
    device_brand_name = as.character(device_brand_name),
    platform = platforms$platform[matched],
    modality = platforms$modality[matched],
    entity = platforms$entity[matched]
  )
}

#' @rdname maude_normalization
#' @export
maude_resolve_owner <- function(entity, as_of, entities = maude_entities()) {
  # The message names the two things a caller is actually choosing between,
  # because the cost of a required argument is someone who does not know what
  # to pass, and "must be a Date" would not tell them.
  if (missing(as_of)) {
    stop(
      "'as_of' must be given, as a Date. There is no default because the ",
      "answer changes with it: use 'as_of = Sys.Date()' for the company that ",
      "owns the entity now, or 'as_of = <the report's date_received>' for the ",
      "company that owned it when the report was filed.",
      call. = FALSE
    )
  }
  if (!inherits(as_of, "Date")) {
    as_of <- tryCatch(
      as.Date(as_of),
      error = function(e) {
        stop(
          "'as_of' must be a Date, or something 'as.Date()' accepts",
          call. = FALSE
        )
      }
    )
  }
  validate_maude_registry(
    entities,
    c("entity", "parent", "from", "to"),
    "entities"
  )

  entity <- as.character(entity)
  n <- max(length(entity), length(as_of))
  if (n > 0 && (length(entity) == 0 || length(as_of) == 0)) {
    return(character(0))
  }
  if (length(entity) != n) {
    entity <- rep_len(entity, n)
  }
  if (length(as_of) != n) {
    as_of <- rep_len(as_of, n)
  }

  # Walk the whole vector one hop at a time rather than one entity at a time:
  # chains are short and the lookup is vectorised, so this costs a handful of
  # passes over the registry.
  max_depth <- 20L
  current <- entity
  for (step in seq_len(max_depth)) {
    parent <- parent_of(current, as_of, entities)
    if (!any(!is.na(parent))) {
      return(current)
    }
    current[!is.na(parent)] <- parent[!is.na(parent)]
  }

  # Still moving after 20 hops means the registry contains a cycle. Name the
  # entities it caught, since a chain that long is otherwise invisible.
  stuck <- unique(current[!is.na(parent_of(current, as_of, entities))])
  stop(
    "'entities' does not resolve within ", max_depth, " steps, which means it ",
    "contains a cycle. Still resolving: ", paste(stuck, collapse = ", "),
    call. = FALSE
  )
}

#' @rdname maude_normalization
#' @export
maude_search_terms <- function(company, as_of = NULL, entities = maude_entities()) {
  if (!is.character(company) || length(company) != 1 || is.na(company) ||
      !nzchar(company)) {
    stop("'company' must be a single company name", call. = FALSE)
  }
  validate_maude_registry(
    entities,
    c("entity", "aliases", "parent", "from", "to"),
    "entities"
  )

  known <- unique(c(entities$entity, stats::na.omit(entities$parent)))
  hit <- match(tolower(company), tolower(known))
  if (is.na(hit)) {
    close <- utils::head(agrep(company, known, value = TRUE, ignore.case = TRUE), 5)
    stop(
      "'company' was given as ", company,
      ", which the registry does not name.",
      if (length(close)) {
        paste0(" Names it does carry that look close: ", paste(close, collapse = ", "))
      },
      call. = FALSE
    )
  }
  company <- known[[hit]]

  # Walk down from the company rather than up from each entity, carrying the
  # window each step was owned within. The window of a chain is the intersection
  # of its steps: Telectronics has only been Abbott's since Abbott bought
  # St. Jude, not since St. Jude bought Telectronics.
  reached <- list(list(entity = company, from = NA, to = NA))
  frontier <- company
  seen <- company

  while (length(frontier)) {
    rows <- which(!is.na(entities$parent) & entities$parent %in% frontier)
    rows <- rows[!entities$entity[rows] %in% seen]
    if (!length(rows)) {
      break
    }

    added <- character(0)
    for (i in rows) {
      above <- Filter(function(x) x$entity == entities$parent[[i]], reached)[[1]]
      from <- suppressWarnings(max(c(above$from, entities$from[[i]]), na.rm = TRUE))
      to <- suppressWarnings(min(c(above$to, entities$to[[i]]), na.rm = TRUE))

      # An empty window means the two ownerships never overlapped: Medtronic
      # sold Physio-Control in 2012 and Physio-Control bought HeartSine in
      # 2015, so HeartSine was never Medtronic's.
      if (!is.infinite(from) && !is.infinite(to) && from >= to) {
        next
      }

      reached <- c(reached, list(list(
        entity = entities$entity[[i]],
        from = from,
        to = to
      )))
      added <- c(added, entities$entity[[i]])
    }

    frontier <- unique(added)
    seen <- c(seen, frontier)
  }

  out <- lapply(reached, function(node) {
    aliases <- unique(entities$aliases[entities$entity == node$entity])
    terms <- unlist(strsplit(aliases, "|", fixed = TRUE), use.names = FALSE)
    terms <- gsub("^\\^|\\$$", "", trimws(terms))
    tibble::tibble(
      entity = node$entity,
      term = terms,
      from = as_bounded_date(node$from),
      to = as_bounded_date(node$to)
    )
  })

  out <- dplyr::bind_rows(out)
  out <- out[!duplicated(out[, c("entity", "term")]), ]

  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
    keep <- (is.na(out$from) | out$from <= as_of) &
      (is.na(out$to) | out$to > as_of)
    out <- out[keep, ]
  }

  out
}

# Internal helpers ----

#' Refuse a registry that is missing the columns a lookup reads
#'
#' @description One message naming the argument, the columns wanted, and the
#'   ones that are actually there, since a caller supplying their own registry
#'   is usually one column short rather than wholly wrong.
#'
#' @param registry The supplied registry.
#' @param required Character vector of required column names.
#' @param arg The argument name to quote in the message.
#'
#' @return `NULL`, invisibly. Called for the error.
#'
#' @noRd
validate_maude_registry <- function(registry, required, arg) {
  if (!is.data.frame(registry) || !all(required %in% names(registry))) {
    stop(
      "'", arg, "' must be a data frame with the columns ",
      paste(required, collapse = ", "),
      if (is.data.frame(registry)) {
        paste0("; it carries ", paste(names(registry), collapse = ", "))
      },
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Keep an unbounded interval bound as `NA` rather than as an infinity
#'
#' @description `max()` and `min()` over an all-`NA` interval return `-Inf` and
#'   `Inf` with a warning, which would then print as a date nobody meant.
#'
#' @param x One date, or an infinite value standing for an unbounded window.
#'
#' @return A `Date` of length one, `NA` where the window is unbounded.
#'
#' @noRd
as_bounded_date <- function(x) {
  if (length(x) != 1 || is.na(x) || is.infinite(x)) {
    return(as.Date(NA))
  }
  as.Date(x, origin = "1970-01-01")
}

#' Warn when a manufacturer filter leaves a company's other names behind
#'
#' @description A filter on `"Abbott"` reaches the reports Abbott filed and not
#'   the ones St. Jude Medical filed before 2017, and the result looks the same
#'   either way. This says which names were left out, without changing the query
#'   that was written.
#'
#' @param manufacturer_name The strings the caller filtered on.
#' @param entities The entity registry to read.
#'
#' @return `NULL`, invisibly. Called for the warning.
#'
#' @noRd
warn_maude_family <- function(manufacturer_name, entities = maude_entities()) {
  named <- unique(stats::na.omit(
    maude_normalize_manufacturer(manufacturer_name, entities)
  ))
  if (!length(named)) {
    return(invisible(NULL))
  }

  absent <- lapply(named, function(company) {
    family <- maude_search_terms(company, entities = entities)
    beneath <- setdiff(unique(family$entity), named)
    if (!length(beneath)) {
      return(NULL)
    }

    since <- family$from[match(beneath, family$entity)]
    paste0(
      beneath,
      ifelse(is.na(since), "", paste0(" (since ", format(since), ")"))
    )
  })
  absent <- unlist(absent, use.names = FALSE)
  if (!length(absent)) {
    return(invisible(NULL))
  }

  warning(
    "The manufacturer filter names ", paste(named, collapse = ", "),
    ", which also owns ", paste(absent, collapse = ", "),
    ". Reports filed under those names are not in this result. ",
    "'maude_search_terms()' returns every name to search on.",
    immediate. = TRUE,
    call. = FALSE
  )
  invisible(NULL)
}

# Coverage ----

#' Measure How Much of MAUDE the Entity Registry Covers
#'
#' @description Counts how many MAUDE mentions the registry can name, by asking
#'   the openFDA `count` endpoint for the most common manufacturer or brand
#'   strings and matching them against [maude_entities()] or [maude_platforms()].
#'
#' @details
#' Measured rather than stored, because a coverage figure written into
#' documentation goes stale the next time a row is added and nothing says so.
#'
#' The count endpoint returns at most 999 terms, which is the head of a much
#' longer tail, so the fraction is over the mentions it can see. The ablation
#' specialists sit far below that cut-off in a cardiovascular-wide count, which
#' is why the ablation stream is measured on its own.
#'
#' @param stream Which stream to measure: `"manufacturer"` for cardiovascular
#'   manufacturer strings, `"ablation"` for manufacturer strings within the
#'   ablation product codes, or `"brand"` for ablation brand strings.
#' @param api_key An openFDA API key, passed through to [maude_fda_api_call()].
#'
#' @return A `tbl_df` of one row per unmatched string, ordered by how many
#'   mentions it carries, with the matched fraction attached as a `"matched"`
#'   attribute and the total as `"mentions"`.
#'
#' @seealso [maude_entities()], [maude_platforms()]
#'
#' @examples
#' \dontrun{
#' missed <- maude_entity_coverage("manufacturer")
#' attr(missed, "matched")
#' head(missed)
#' }
#'
#' @export
maude_entity_coverage <- function(
  stream = c("manufacturer", "ablation", "brand"),
  api_key = NULL
) {
  stream <- match.arg(stream)

  ablation_query <- paste0(
    "device.device_report_product_code:",
    "(QZI OR OAE OR LPB OR OAD OR MTD OR NIY OR OCL)"
  )
  spec <- switch(
    stream,
    manufacturer = list(
      search = "device.openfda.medical_specialty_description:Cardiovascular",
      count = "device.manufacturer_d_name.exact",
      registry = maude_entities()
    ),
    ablation = list(
      search = ablation_query,
      count = "device.manufacturer_d_name.exact",
      registry = maude_entities()
    ),
    brand = list(
      search = ablation_query,
      count = "device.brand_name.exact",
      registry = maude_platforms()
    )
  )

  counts <- maude_fda_api_call(
    search_query = spec$search,
    count = spec$count,
    limit = 999,
    api_key = api_key
  )

  depth <- if (identical(spec$count, "device.manufacturer_d_name.exact")) {
    entity_depth(spec$registry)
  } else {
    NULL
  }
  matched <- match_maude_alias(counts$term, spec$registry, depth)

  out <- counts[is.na(matched), ]
  attr(out, "mentions") <- sum(counts$count)
  attr(out, "matched") <- sum(counts$count[!is.na(matched)]) / sum(counts$count)
  out
}
