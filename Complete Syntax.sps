* Encoding: UTF-8.

DATASET ACTIVATE DataSet2.
SORT CASES  BY YNmissingoutcome.
SPLIT FILE LAYERED BY YNmissingoutcome.
* Data file "ABI outcomes for SPSS"
*code missing nominal variables=
* Under variable view code nominal predictors as missing values by putiing a single blank space under missing- 'discrete missing values' 
*Explore continuous data for districtibution- normal distribution skewness and kurtosis < x2 SE. View data with bar charts

FREQUENCIES VARIABLES=simd_rank age_at_injury_weeks age_at_admission_weeks 
    days_between_injury_admission hads_a hads_d baseline_mpai_total baseline_mpai_abilties 
    baseline_mpai_adjustment baseline_mpai_participation wais_vci wais_pri wais_wmi wais_psi wais_fsiq 
    topf_vci topf_pri topf_wmi topf_psi topf_fsiq memory
  /NTILES=4
  /STATISTICS=STDDEV SEMEAN MEAN MEDIAN SKEWNESS SESKEW KURTOSIS SEKURT
  /ORDER=ANALYSIS.

*compare groups- stats for spread of data between groups

SORT CASES  BY YNmissingoutcome.
SPLIT FILE SEPARATE BY YNmissingoutcome.

FREQUENCIES VARIABLES=simd_rank age_at_injury_weeks age_at_admission_weeks 
    days_between_injury_admission hads_a hads_d baseline_mpai_total baseline_mpai_abilties 
    baseline_mpai_adjustment baseline_mpai_participation wais_vci wais_pri wais_wmi wais_psi wais_fsiq 
    topf_vci topf_pri topf_wmi topf_psi topf_fsiq memory
  /NTILES=4
  /STATISTICS=STDDEV SEMEAN MEAN MEDIAN SKEWNESS SESKEW KURTOSIS SEKURT
  /BARCHART FREQ
  /ORDER=ANALYSIS.



SPLIT FILE OFF.

* Mann Whitney U tests for variables that aren't normally distributed 

*Nonparametric Tests: Independent Samples. 
NPTESTS 
  /INDEPENDENT TEST (memory wais_psi wais_wmi baseline_mpai_abilties baseline_mpai_adjustment 
    baseline_mpai_participation hads_a hads_d simd_rank) GROUP (YNmissingoutcome) MANN_WHITNEY 
    MEDIAN(TESTVALUE=SAMPLE COMPARE=PAIRWISE) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE
  /CRITERIA ALPHA=0.05  CILEVEL=95.

* T tests for normally distributed variables 

T-TEST GROUPS=YNmissingoutcome('Y' 'N')
  /MISSING=ANALYSIS
  /VARIABLES=topf_vci topf_pri topf_wmi topf_psi topf_fsiq wais_fsiq wais_pri wais_vci 
    baseline_mpai_total age_at_injury_weeks age_at_admission_weeks days_between_injury_admission
  /CRITERIA=CI(.95).

*Run chi squared analyses for differences between groups with and without the outcome for nominal data

CROSSTABS
  /TABLES=gender diagnosis preinjury_psychosis drug_dependence alcohol_abuse multiple_trauma 
    other_medical_condition neuro_administered BY YNmissingoutcome
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT ROW 
  /COUNT ROUND CELL
  /BARCHART.
