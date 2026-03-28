# data-raw/complications.R
# Run this script to regenerate data/complication_definitions.rda,
# data/maude_code_index.rda, and data/maude_code_ontology.rda
#
# source("data-raw/complications.R")

# Ablation ----
complication_definitions <- list(
  # Pericardial Effusion / Cardiac Tamponade / Pericarditis ----
  pericardial = list(
    title = "Pericardial Effusion / Cardiac Tamponade / Pericarditis",

    definition = "Any pericardial complication occurring during or after catheter ablation of atrial fibrillation. Includes accumulation of fluid (blood or serous) in the pericardial space, frank cardiac tamponade, and post-ablation pericarditis with or without associated effusion. Mechanisms include direct mechanical perforation of the atrial wall by catheter or transseptal needle, thermal injury with delayed rupture, excessive anticoagulation, and post-procedural inflammatory pericarditis. The narrative may describe hypotension, pulsus paradoxus, pleuritic chest pain, pericardial friction rub, echocardiographic findings of effusion, pericardiocentesis, surgical drainage, or treatment with NSAIDs or colchicine. Includes both acute intraprocedural events and delayed presentations hours to days after ablation.",

    severity = c(
      pericarditis_without_effusion = "Post-ablation pericarditis presenting with pleuritic chest pain and possibly a friction rub, without a clinically significant pericardial effusion. Managed with NSAIDs, colchicine, or observation.",
      trivial_effusion = "Small or trace pericardial effusion identified at the end of the case or on post-procedure imaging. No hemodynamic compromise. No drainage required. Managed with observation, with or without anti-inflammatory therapy.",
      moderate_no_intervention = "Moderate pericardial effusion causing symptoms such as chest pain, dyspnea, or tachycardia, but managed conservatively without pericardiocentesis or surgical drainage. May include holding or reversing anticoagulation and medical therapy.",
      tamponade_pericardiocentesis = "Hemodynamically significant pericardial effusion or frank cardiac tamponade requiring percutaneous pericardiocentesis. Includes cases with hypotension, pulsus paradoxus, or echocardiographic evidence of chamber collapse that prompted emergent drainage.",
      tamponade_surgical = "Cardiac tamponade or perforation requiring surgical intervention, including pericardial window, thoracotomy, sternotomy, or surgical repair of the atrial wall.",
      insufficient_info = "The narrative mentions pericardial effusion, tamponade, pericarditis, or related terms but does not provide enough detail to determine whether intervention was required or what management was performed."
    )
  ),

  # Cerebrovascular Event ----
  stroke = list(
    title = "Cerebrovascular Event",

    definition = "Any ischemic stroke, hemorrhagic stroke, transient ischemic attack (TIA), or systemic thromboembolism occurring during or after catheter ablation of atrial fibrillation. Mechanisms include thrombus formation on catheters or sheaths, air embolism during transseptal puncture or catheter exchange, char embolism from overheated tissue, and dislodgement of pre-existing left atrial thrombus. The narrative may describe focal neurologic deficits (weakness, speech difficulty, visual changes), altered mental status, cerebral imaging findings (CT or MRI), or systemic embolism to other vascular beds. Includes both clinically apparent events and silent cerebral lesions detected on post-procedure MRI.",

    severity = c(
      tia = "Transient neurologic deficit resolving completely within 24 hours with no evidence of cerebral infarction on imaging. Full recovery to neurologic baseline.",
      minor_stroke = "Ischemic or hemorrhagic stroke with mild residual neurologic deficit at discharge or last follow-up. Includes events described as 'minor stroke' or with NIHSS score of 3 or less. Also includes silent cerebral lesions detected on post- procedure MRI if reported.",
      major_stroke = "Stroke with significant persistent neurologic deficit requiring ICU-level care, resulting in lasting functional disability, or requiring neurosurgical intervention. Includes large-territory infarction and symptomatic intracranial hemorrhage.",
      fatal_stroke = "Cerebrovascular event that directly caused or substantially contributed to the patient's death.",
      systemic_embolism = "Thromboembolism to a non-cerebral vascular territory (e.g., mesenteric, renal, or peripheral arterial embolism) occurring in the peri-procedural period.",
      insufficient_info = "The narrative describes a cerebrovascular or embolic event but does not provide enough detail to determine the severity of deficit, duration of symptoms, or clinical outcome."
    )
  ),

  # Vascular Access Complication ----
  vascular = list(
    title = "Vascular Access Complication",

    definition = "Complications arising from percutaneous vascular access for catheter ablation typically at the femoral venous or arterial puncture site. The narrative may describe groin hematoma, swelling, pain at the access site, pseudoaneurysm, arteriovenous (AV) fistula, retroperitoneal hemorrhage, or significant bleeding requiring transfusion. Also includes venous thrombosis (DVT) or pulmonary embolism related to venous access and vascular injury from catheter or sheath manipulation. Does NOT include bleeding complications at non-access sites.",

    severity = c(
      minor_hematoma = "Groin hematoma or minor bleeding at the access site managed with manual compression, observation, or prolonged bed rest only. No transfusion or procedural intervention required.",
      major_hematoma_transfusion = "Access-site hematoma or bleeding requiring blood transfusion but not surgical or interventional repair. Includes significant hematomas causing hemoglobin drop of 2 g/dL or more.",
      pseudoaneurysm = "Femoral pseudoaneurysm at the access site requiring intervention such as ultrasound-guided thrombin injection, compression, or surgical repair.",
      av_fistula = "Arteriovenous fistula at the access site. May be managed with observation, compression, or surgical repair depending on size and symptoms.",
      retroperitoneal = "Retroperitoneal hemorrhage from femoral vessel injury. Typically presents with flank or abdominal pain, hemodynamic instability, and hemoglobin drop. May require transfusion, IR embolization, or surgical repair.",
      vte = "Venous thromboembolism (deep vein thrombosis or pulmonary embolism) related to venous access or catheter manipulation.",
      insufficient_info = "The narrative describes a vascular access complication but lacks enough detail to classify the specific type or severity."
    )
  ),

  # Pulmonary Vein Stenosis ----
  pv_stenosis = list(
    title = "Pulmonary Vein Stenosis",

    definition = "Narrowing of one or more pulmonary veins resulting from catheter ablation within or at the ostium of the pulmonary veins. The incidence has decreased substantially with the shift from ostial to antral isolation strategies. Primarily associated with radiofrequency and cryoballoon ablation; not typically seen with pulsed field ablation due to tissue selectivity. The narrative may describe dyspnea, hemoptysis, recurrent pulmonary infections, or CT/MRI findings of PV narrowing. Symptoms may present weeks to months after ablation.",

    severity = c(
      asymptomatic = "Pulmonary vein narrowing detected on follow-up imaging (CT, MRI, or TEE) but the patient is asymptomatic. Includes mild stenosis (less than 50% luminal reduction) and moderate stenosis (50-70%) without symptoms.",
      symptomatic_medical = "Symptomatic PV stenosis (dyspnea, hemoptysis, recurrent pneumonia, or reduced exercise tolerance) managed with medical therapy or observation without procedural intervention.",
      intervention_required = "PV stenosis requiring procedural intervention such as balloon angioplasty or stent placement. Typically involves severe stenosis (greater than 70% luminal reduction) with limiting symptoms.",
      complete_occlusion = "Complete occlusion of one or more pulmonary veins documented on imaging. May be asymptomatic if collateral drainage is adequate but represents the most severe anatomic finding.",
      insufficient_info = "The narrative mentions PV stenosis or related symptoms but does not provide enough detail on imaging findings, symptom severity, or management to determine the degree of stenosis."
    )
  ),

  # Esophageal Injury ----
  esophageal = list(
    title = "Esophageal Injury",

    definition = "Injury to the esophagus caused by energy delivery on the posterior left atrial wall, which lies in close anatomic proximity to the anterior esophageal wall. Ranges from superficial mucosal injury to the catastrophic and often fatal atrioesophageal fistula (AEF). Applies to all energy modalities, though PFA appears to have lower risk of direct esophageal thermal injury due to tissue selectivity. The narrative may describe chest pain radiating to the back, dysphagia, odynophagia, fever, endoscopic findings of erythema or ulceration, CT findings of mediastinal air, or neurologic symptoms from air embolism through a fistula. Also includes gastroparesis from injury to the periesophageal vagal plexus, which manifests as nausea, vomiting, early satiety, and bloating.",

    severity = c(
      mucosal = "Superficial esophageal injury limited to mucosal erythema, erosion, or shallow ulceration found on endoscopy. Managed conservatively with proton pump inhibitors and dietary modification. No perforation or fistula.",
      deep_ulceration = "Deep esophageal ulceration extending beyond the mucosa without fistula formation. Requires extended medical management, possibly including sucralfate, IV PPI, and close surveillance imaging.",
      atrioesophageal_fistula = "Atrioesophageal fistula (AEF) confirmed on imaging (CT with air in the mediastinum or left atrium) or at surgery. A life- threatening complication typically presenting 2-5 weeks post- ablation with fever, neurologic symptoms from air or septic embolism, hematemesis, or sepsis.",
      gastroparesis = "Symptomatic gastroparesis (nausea, vomiting, early satiety, abdominal bloating, delayed gastric emptying) resulting from injury to the periesophageal vagal plexus during posterior wall ablation. Severity ranges from self-limited to debilitating.",
      insufficient_info = "The narrative describes esophageal symptoms, findings, or concern for esophageal injury but does not provide enough detail to classify the depth of injury or clinical outcome."
    )
  ),

  # Phrenic Nerve Injury ----
  phrenic = list(
    title = "Phrenic Nerve Injury",

    definition = "Injury to the right or left phrenic nerve during catheter ablation resulting in diaphragmatic paresis or paralysis. The right phrenic nerve is most commonly affected due to its proximity to the right superior pulmonary vein and superior vena cava. Most frequently associated with cryoballoon ablation of the right superior PV but can also occur with RFA and has been reported rarely with PFA. The narrative may describe loss of diaphragmatic excursion during the procedure (fluoroscopic or pacing-monitored), post-procedure dyspnea, elevated hemidiaphragm on chest X-ray, or reduced inspiratory effort.",

    severity = c(
      intraprocedural_only = "Phrenic nerve capture was lost or diminished diaphragmatic excursion was noted during the procedure, prompting immediate cessation of ablation. Phrenic function recovered before the end of the procedure or by the time of discharge.",
      transient = "Phrenic nerve palsy persisting beyond the procedure but recovering fully within 12 months. Includes patients with elevated hemidiaphragm on chest X-ray at discharge who subsequently recovered.",
      persistent = "Phrenic nerve palsy still present at 12 months or later, or described as permanent. Patient may have chronic dyspnea on exertion and persistent hemidiaphragm elevation.",
      insufficient_info = "The narrative describes phrenic nerve injury or elevated hemidiaphragm but does not provide enough follow-up information to determine whether recovery occurred."
    )
  ),

  # Procedure-Related Arrhythmia ----
  arrhythmia = list(
    title = "Procedure-Related Arrhythmia",

    definition = "A new arrhythmia caused by the ablation procedure itself distinct from recurrence of the patient's original atrial fibrillation. Includes iatrogenic left atrial macro-reentrant tachycardia or atypical flutter from gaps in linear lesion sets, new-onset AV block from septal ablation or catheter trauma to the conduction system, inappropriate sinus tachycardia from autonomic modulation, and proarrhythmia (new ventricular arrhythmia or organized atrial arrhythmia not present before ablation). Does NOT include recurrence of the patient's pre- existing AF or early reconnection arrhythmias within the blanking period that are expected.",

    severity = c(
      self_terminating = "Iatrogenic arrhythmia that terminated spontaneously or with brief pacing maneuvers during the procedure, with no recurrence and no additional intervention required.",
      cardioversion_or_medical = "Iatrogenic arrhythmia requiring electrical cardioversion, antiarrhythmic drug treatment, or rate-control medication. Includes persistent atrial tachycardia or flutter managed medically without repeat ablation.",
      repeat_ablation = "Iatrogenic arrhythmia requiring a repeat ablation procedure to address (e.g., mapping and ablation of a gap-related macro-reentrant atrial tachycardia or flutter circuit).",
      device_implant = "Arrhythmia or conduction disturbance requiring implantation of a permanent pacemaker or ICD. Includes complete AV block from septal ablation or catheter trauma requiring permanent pacing.",
      insufficient_info = "The narrative describes a new arrhythmia related to the ablation but does not provide enough detail on the type of arrhythmia, management, or outcome."
    )
  ),

  # Coronary Artery Injury / Spasm ----
  coronary = list(
    title = "Coronary Artery Injury / Spasm",

    definition = "Coronary artery spasm, occlusion, or direct vascular injury caused by ablation energy delivery in proximity to the coronary arteries. Most commonly reported with pulsed field ablation (PFA), where the high-voltage electric field can stimulate vascular smooth muscle contraction. Focal spasm of the left circumflex artery during mitral isthmus ablation and of the right coronary artery during cavotricuspid isthmus ablation are the most recognized patterns. Delayed diffuse coronary spasm possibly mediated by hemolysis-related nitric oxide depletion has also been described with PFA. The narrative may describe ST-segment changes, chest pain, hemodynamic instability, coronary angiography findings, or nitroglycerin administration during the procedure. Can also occur rarely with RFA particularly during epicardial ablation.",

    severity = c(
      transient_spasm = "Coronary artery spasm during ablation that resolved promptly with cessation of energy delivery and/or administration of intracoronary or intravenous nitroglycerin. No evidence of myocardial injury (normal troponin). No lasting ischemic consequence.",
      sustained_spasm = "Coronary spasm requiring prolonged vasodilator therapy, occurring remotely from energy delivery (delayed spasm), or recurring after initial resolution. May include troponin elevation without meeting criteria for myocardial infarction.",
      myocardial_infarction = "Acute myocardial infarction from coronary occlusion or sustained spasm. Includes ST-elevation MI, non-ST-elevation MI with significant troponin rise and ischemic symptoms or ECG changes, and coronary occlusion requiring emergent PCI.",
      coronary_stenosis = "Coronary artery narrowing or vascular remodeling detected on follow-up angiography or OCT, attributed to ablation-induced vascular injury. A recently described finding with PFA near coronary vessels.",
      insufficient_info = "The narrative describes coronary symptoms, ST changes, or concern for coronary injury but does not provide enough detail to determine the mechanism, severity, or outcome."
    )
  ),

  # Hemolysis / Acute Kidney Injury ----
  hemolysis = list(
    title = "Hemolysis / Acute Kidney Injury",

    definition = "Intravascular hemolysis caused by electroporation of red blood cells predominantly associated with pulsed field ablation (PFA). High-voltage pulsed electric fields generate a transmembrane potential in erythrocytes, leading to membrane pore formation, colloid osmotic swelling, and cell rupture. Manifests as elevated plasma free hemoglobin, elevated LDH, hemoglobinuria (dark or discolored urine), and in severe cases, acute kidney injury (AKI) from heme-mediated proximal tubular damage. Free hemoglobin also scavenges nitric oxide, which may contribute to smooth muscle dysfunction (urinary retention, coronary spasm, hypertension). Risk correlates with the total number of PFA applications and catheter-tissue contact quality.",

    severity = c(
      laboratory_only = "Evidence of hemolysis on laboratory testing (elevated free hemoglobin, elevated LDH, hemoglobinuria) without clinically significant renal dysfunction or other end-organ effects. Self-resolving within 24-48 hours.",
      aki_no_dialysis = "Acute kidney injury from hemolysis-induced tubular damage, defined as a significant rise in serum creatinine, managed conservatively with IV hydration without requiring renal replacement therapy.",
      aki_dialysis = "Severe hemolysis-induced AKI requiring temporary or sustained renal replacement therapy (hemodialysis).",
      insufficient_info = "The narrative mentions hemolysis, dark urine, elevated LDH, or renal dysfunction potentially related to hemolysis but does not provide enough detail to characterize the severity."
    )
  ),

  # Respiratory / Pulmonary Complication ----
  respiratory = list(
    title = "Respiratory / Pulmonary Complication",

    definition = "Pulmonary or thoracic complications of catheter ablation NOT related to pulmonary vein stenosis (see pv_stenosis) or phrenic nerve injury (see phrenic). Includes pneumothorax (from subclavian or internal jugular access, or epicardial access), hemothorax, pulmonary hemorrhage or hemoptysis (reported with PFA), bronchial injury, post-procedure pulmonary edema, and pulmonary infection. The narrative may describe dyspnea, chest pain, reduced breath sounds, chest tube placement, or chest imaging findings.",

    severity = c(
      mild = "Minor respiratory complication managed conservatively, such as small pneumothorax on imaging that resolved without chest tube, transient hemoptysis, or mild pulmonary edema treated with diuretics.",
      moderate_intervention = "Respiratory complication requiring procedural intervention, such as chest tube placement for pneumothorax or hemothorax, or significant pulmonary hemorrhage requiring bronchoscopy.",
      severe = "Respiratory complication requiring ICU care, mechanical ventilation, surgical intervention, or resulting in prolonged hospitalization.",
      insufficient_info = "The narrative describes a respiratory or pulmonary complication but does not provide enough detail to grade severity."
    )
  ),

  # Procedure-Related Infection ----
  infection = list(
    title = "Procedure-Related Infection",

    definition = "Infection attributable to the catheter ablation procedure. Includes vascular access site infection (cellulitis, abscess), endocarditis from catheter-introduced organisms, bacteremia or sepsis from intravascular instrumentation, mediastinitis (which may be associated with esophageal injury), and post-procedure pneumonia. The narrative may describe fever, elevated white blood cell count, positive blood cultures, wound erythema or drainage, or antibiotic administration for a suspected procedure-related source. Does NOT include infections unrelated to the procedure.",

    severity = c(
      local = "Localized infection at the access site (cellulitis, superficial wound infection) managed with oral antibiotics without hospitalization.",
      systemic_iv_antibiotics = "Systemic infection (bacteremia, pneumonia, UTI) requiring IV antibiotics or hospitalization but without hemodynamic instability or end-organ damage.",
      sepsis = "Sepsis or septic shock from a procedure-related source. Includes endocarditis, mediastinitis, or septic embolism. Requires ICU-level care, vasopressors, or surgical source control.",
      insufficient_info = "The narrative describes infection or related findings but does not provide enough detail to determine the source, extent, or severity."
    )
  ),

  # Procedure-Related Death ----
  death = list(
    title = "Procedure-Related Death",

    definition = "Death occurring during or after catheter ablation of atrial fibrillation that is judged to be related to the procedure or one of its complications. Death from AF ablation is multifactorial and may result from cardiac tamponade, atrioesophageal fistula, massive stroke, anesthesia-related events, coronary occlusion, or other procedural causes. The overall incidence is approximately 0.05-0.1%. The narrative may explicitly state that the patient died or may describe a clinical course leading to death (e.g., refractory cardiac arrest, withdrawal of care). This category captures death as the final outcome; the underlying mechanism may also warrant assignment to another complication category (e.g., pericardial, stroke, esophageal).",

    severity = c(
      intraprocedural = "Death occurring during the ablation procedure itself, in the electrophysiology laboratory or operating room.",
      periprocedural = "Death occurring within 30 days of the ablation procedure, outside the procedural setting but attributed to a procedural complication.",
      delayed = "Death occurring more than 30 days after ablation but attributed to a procedural complication (e.g., late atrioesophageal fistula or late PV stenosis with pulmonary consequences).",
      insufficient_info = "The narrative indicates the patient died but does not provide enough detail to determine the timing or cause of death relative to the procedure."
    )
  ),

  # Device / Equipment Malfunction ----
  device_malfunction = list(
    title = "Device / Equipment Malfunction",

    definition = "Failure or malfunction of the ablation catheter, energy generator, mapping system, or ancillary procedural equipment (sheaths, transseptal needles, irrigation pump, recording system) that is reported as part of the adverse event. This category captures problems with the device itself, regardless of whether the malfunction led to patient injury. In MAUDE, many reports are filed under the 'Malfunction' event type with Annex A device problem codes. The narrative may describe catheter fracture, tip detachment, generator error codes, impedance faults, software or firmware failures, irrigation failures, mapping system crashes, or inability to deliver energy. If the malfunction also caused a clinical complication, both this category and the relevant clinical complication category may be assigned.",

    severity = c(
      no_procedure_impact = "Device malfunction that was identified and resolved without impact on the ablation procedure. The procedure was completed as planned using the same or replacement equipment.",
      procedure_altered = "Device malfunction that required a change in procedural strategy, use of backup equipment, or premature termination of the procedure, but did not result in direct patient injury.",
      patient_injury = "Device malfunction that directly caused or contributed to a patient injury. The specific injury should also be classified under the appropriate clinical complication category.",
      insufficient_info = "The narrative describes a device malfunction but does not provide enough detail to determine whether the procedure or patient was affected."
    )
  ),

  # No Patient Harm ----
  no_harm = list(
    title = "No Patient Harm",

    definition = "The adverse event report describes a device problem, procedural deviation, near-miss event, or other reportable occurrence, but explicitly states that no patient injury, adverse clinical outcome, or harm occurred. This category exists to account for reports that are filed to satisfy regulatory reporting obligations but do not represent a clinical complication. The narrative may state 'no patient harm,' 'no adverse outcome,' 'no clinical consequence,' 'no impact to the patient,' or similar language. This category should only be assigned when the narrative affirmatively indicates absence of harm, not merely when harm is not mentioned.",

    severity = c(
      confirmed_no_harm = "The narrative explicitly and clearly states that no patient harm occurred as a result of the reported event.",
      probable_no_harm = "The narrative strongly suggests no patient harm occurred based on the described circumstances, but does not contain an explicit statement confirming absence of harm.",
      insufficient_info = "The narrative does not provide enough information to determine whether patient harm occurred or not."
    )
  ),

  # Other Complication ----
  other = list(
    title = "Other Complication",

    definition = "A procedure-related adverse event that does not fit any of the preceding complication categories. This is a residual category for clinically significant events that are real complications but are uncommon enough to not warrant a dedicated category. Examples include: cardiac valve injury (mitral or tricuspid valve damage from catheter manipulation, chordal entanglement, new or worsened valvular regurgitation), vasovagal or autonomic responses (profound bradycardia, asystole, or hypotension during energy delivery, particularly with PFA near ganglionated plexi), atrial septal defect from transseptal puncture requiring closure, stiff left atrium syndrome, radiation-related skin injury, contrast or dye allergy or anaphylaxis, anesthesia- related complications (aspiration, airway injury, medication reaction), cardiogenic shock not from tamponade, acute heart failure exacerbation, urinary retention, skin burns, and musculoskeletal injury from patient positioning or PFA-induced skeletal muscle stimulation. The adjudicator should use this category only when the event clearly does not fit a more specific category above.",

    severity = c(
      minor = "Complication that was self-limited or managed with minimal intervention, did not prolong hospitalization, and resolved without lasting sequelae.",
      moderate = "Complication requiring additional treatment, prolonged hospitalization, or a secondary procedure, but without lasting disability or life-threatening consequence.",
      severe = "Complication that was life-threatening, caused lasting disability, required major intervention, or significantly altered the patient's clinical course.",
      insufficient_info = "The narrative describes a complication in this category but does not provide enough detail to grade its severity."
    )
  )
)

# Data saving ----
usethis::use_data(
  complication_definitions,
  overwrite = TRUE
)
