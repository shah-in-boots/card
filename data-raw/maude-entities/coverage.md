# MAUDE entity table coverage

Measured against the openFDA `count` endpoint on 2026-08-08. Regenerate with `source("data-raw/maude-entities.R")`.

| stream | mentions | matched |
|---|---:|---:|
| manufacturers, cardiovascular | 1,463,645 | 89.4% |
| manufacturers, cardiac ablation | 44,640 | 99.2% |
| ablation brands | 43,777 | 97.1% |

## Patterns that never fired

A pattern matching nothing in either stream is either a typo or an
entity that does not report. Both are worth knowing; neither shows up
in the percentages above.

### Manufacturers

- `AFFERA`
- `CAMERON HEALTH`
- `PREVENTICE`
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

- `MEDIVANCE, INC.  ¿ 1725056` (21,093)
- `AV-TEMECULA-CT` (17,171)
- `PLEXUS MANUFACTURING SDN. BHD` (4,754)
- `MEDOS INTERNATIONAL SARL` (4,486)
- `PERFUSION SYSTEMS` (4,342)
- `` (3,707)
- `REMOTE DIAGNOSTIC TECHNOLOGIES LTD.` (2,685)
- `ASAHI INTECC CO., LTD.` (2,321)
- `WELCH ALLYN PROTOCOL, INC.` (2,201)
- `MINDRAY DS USA, INC.` (2,089)

### Manufacturers, cardiac ablation

- `PERFUSION SYSTEMS` (147)
- `` (23)
- `ARDEN HILLS, MN` (21)
- `HEI, INC.` (16)
- `MEDFACT ENGINEERING GMBH` (8)
- `UNKNOWN` (8)
- `STELLARTECH RESEARCH CORPORATION` (7)
- `UNK` (7)
- `*` (5)
- `NELLCOR PURITAN BENNETT IRELAND` (5)

### Ablation brands

- `` (230)
- `NGEN GENERATOR` (38)
- `ADVISOR¿ HD GRID X¿ MAPPING CATHETER, SENSOR ENABLED¿` (26)
- `CURRENT¿ PFA GENERATOR` (23)
- `CANNULA ACCESSORIES` (22)
- `NGEN PUMP` (22)
- `ADVISOR¿ HD GRID MAPPING CATHETER, SENSOR ENABLED¿` (21)
- `COOLFLOW® IRRIGATION PUMP` (19)
- `TRUPULSE¿ GENERATOR` (19)
- `7F ES STEER DS BI-DIRECTIONAL NAV CATHETER` (18)
