# MAUDE entity table coverage

Measured against the openFDA `count` endpoint on 2026-09-05. Regenerate with `source("data-raw/maude-entities.R")`.

| stream | mentions | matched |
|---|---:|---:|
| manufacturers, cardiovascular | 1,472,849 | 92.9% |
| manufacturers, cardiac ablation | 45,412 | 99.7% |
| ablation brands | 44,509 | 97.0% |

## Patterns that never fired

A pattern matching none of the 999 terms the count endpoint returns per
stream is a typo, an entity that does not report, or one whose strings
sit below the cut-off: `CAMERON HEALTH` names 2,770 reports and still
lands here. Check with a direct search before dropping a row.

### Manufacturers

- `AFFERA`
- `CAMERON HEALTH`
- `PREVENTICE`
- `CARDIOMEMS`
- `CYBERONICS`
- `BIOTELEMETRY`

### Ablation brands

- `NMARQ`

## Ownership rows nothing references

- none

## Ownership rows without a source

The closing dates below were recorded from general knowledge rather than from a citation, and their `note` says how precise each one is. A row here is not wrong, only unverified.

- `Abiomed`
- `Arrow International`
- `Baylis Medical`
- `BioTelemetry`
- `C. R. Bard`
- `Cameron Health`
- `Cardiac Pacemakers`
- `Cardiac Science`
- `CardioMEMS`
- `Cordis`
- `Covidien`
- `CryoCath`
- `Cyberonics`
- `Datascope`
- `EP Technologies`
- `EPiX Therapeutics`
- `ev3`
- `Galil Medical`
- `Guidant`
- `HeartSine Technologies`
- `Irvine Biomedical`
- `Maquet`
- `nContact Surgical`
- `Physio-Control`
- `Preventice Solutions`
- `Sorin`
- `Spectranetics`
- `Telectronics Pacing Systems`
- `Thoratec`
- `Volcano`

## Largest unmatched strings

Where the next rows should come from, if anywhere.

### Manufacturers, cardiovascular

- `MEDOS INTERNATIONAL SARL` (4,508)
- `` (3,726)
- `ASAHI INTECC CO., LTD.` (2,347)
- `WELCH ALLYN PROTOCOL, INC.` (2,201)
- `MINDRAY DS USA, INC.` (2,089)
- `MICROVENTION, INC.` (2,042)
- `OSCOR INC.` (1,815)
- `GREATBATCH MEDICAL` (1,751)
- `BIO-DETEK INCORPORATED` (1,646)
- `IRHYTHM TECHNOLOGIES, INC` (1,531)

### Manufacturers, cardiac ablation

- `` (24)
- `ARDEN HILLS, MN` (21)
- `UNKNOWN` (8)
- `UNK` (7)
- `*` (5)
- `NELLCOR PURITAN BENNETT IRELAND` (5)
- `CARDIAC PATHWAYS CORP.` (4)
- `RICE CREEK MFG` (4)
- `TOPERA INC.` (4)
- `MDT ABLATION FRONTIERS MFG` (3)

### Ablation brands

- `` (258)
- `NGEN GENERATOR` (39)
- `ADVISOR¿ HD GRID X¿ MAPPING CATHETER, SENSOR ENABLED¿` (26)
- `CURRENT¿ PFA GENERATOR` (25)
- `TRUPULSE¿ GENERATOR` (25)
- `NGEN PUMP` (23)
- `CANNULA ACCESSORIES` (22)
- `ADVISOR¿ HD GRID MAPPING CATHETER, SENSOR ENABLED¿` (21)
- `COOL POINT¿ IRRIGATION PUMP` (19)
- `COOLFLOW® IRRIGATION PUMP` (19)
