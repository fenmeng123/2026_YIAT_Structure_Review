# Revisiting Y-IAT factor structure

Author: Kunru Song

This project examines the factor structure of the 20-item Young's Internet Addiction Test (Y-IAT) through a systematic review and computational semantic analyses. The review comprises 64 reports, 72 studies and 115 analyses.

Reports are included source documents; studies are distinguished by the sample(s) and factor solution retained for synthesis; analyses are separately coded exploratory or confirmatory procedures. This terminology does not imply that all 72 studies are independent samples. Quality assessment covers sample size and validation design at the study level, and methodological support and reporting completeness at the analysis level.

This initial release includes analysis code, coding manuals, the preregistration, selected tabular results and figure data. Source analysis inputs are deferred to a later update. The supplied result RDS retain study- and analysis-level information used to prepare the figures.

## Repository Layout

```text
./                                  # Contains the study's analysis and reporting materials.
├── Materials_1_CodingInstruction/   # Provides coding manuals and the formal preregistration.
├── Step_1_SysRev/                   # Contains systematic-review analyses and the EndNote reference library.
├── Step_2_SemanticAnalysis/         # Prepares semantic embeddings and analyses their similarities.
├── Step_3_Visualization/            # Generates the manuscript and supplementary figures.
├── Res_2_Results/                   # Stores scientific tables and figure data.
└── Res_3_Report/                    # Provides an initially empty destination for reproduced figures.
```

The systematic-review workflow starts with harmonized study and analysis data. The semantic workflow compares 20 item wordings with 19 benchmark definitions covering six addiction components, nine DSM-5-TR IGD criteria and four ICD-11 GD criteria. The script tables below describe the full analysis sequence and the figures supported by the supplied result objects.

The reference library is [YIAT_FacStruc.enl](Step_1_SysRev/YIAT_FacStruc.enl); keep its companion YIAT_FacStruc.Data database directory beside it when opening it in EndNote.

## Environment

The workflow uses R 4.5.3 on Windows. Install dplyr, tidyr, stringr, readxl, openxlsx, cluster, ggplot2, gtable, scales and jsonlite, together with their dependencies. Regression analyses also use brglm2, clubSandwich and enrichwith. Install Arial and Microsoft YaHei for the English and Chinese figure text.

Embedding generation uses Python 3.12 and the package versions in [requirements.txt](Step_2_SemanticAnalysis/requirements.txt). Install its CPU PyTorch build from the [official CPU wheel index](https://download.pytorch.org/whl/cpu), then the remaining listed dependencies. S2_2 calls R/jsonlite to save RDS; set its Rscript executable path to your installed R before generating embeddings.

## Four encoders

For optional embedding generation, download the required model snapshot from its source, retaining the specified revision and its weights, tokenizer, module and pooling files. The four encoders support the five conditions listed below.

| Encoder and download page                                                                                                                                                                                           | Revision                                 | Use                                                            |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------- | -------------------------------------------------------------- |
| [Generic MPNet: sentence-transformers/all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2/tree/e8c3b32edf5434bc2275fc9bab85f82640a19130)                                              | e8c3b32edf5434bc2275fc9bab85f82640a19130 | Revised English; Figure S1                                     |
| [Personality MPNet: dwulff/mpnet-personality](https://huggingface.co/dwulff/mpnet-personality/tree/da377389cdf92ed0e28ffbc608912bcbcd97d162)                                                                        | da377389cdf92ed0e28ffbc608912bcbcd97d162 | Revised English, primary Figure 4; original English, Figure S3 |
| [Harmony: harmonydata/mental_health_harmonisation_1](https://huggingface.co/harmonydata/mental_health_harmonisation_1/tree/13115a1ad333e14b2804bb7eba60e91ce6c42c73)                                                | 13115a1ad333e14b2804bb7eba60e91ce6c42c73 | Revised English; Figure S2                                     |
| [Multilingual MPNet: sentence-transformers/paraphrase-multilingual-mpnet-base-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-mpnet-base-v2/tree/4328cf26390c98c5e3c738b4460a05b95f4911f5) | 4328cf26390c98c5e3c738b4460a05b95f4911f5 | Chinese; Figure S4                                             |

To prepare optional encoder inference, create the corresponding snapshot directory and place the downloaded files using this layout:

```text
Res_1_Data_for_analyses/Frozen_Encoders/models--<owner>--<model>/snapshots/<revision>/
```

For example, the personality snapshot belongs under models--dwulff--mpnet-personality/snapshots/da377389cdf92ed0e28ffbc608912bcbcd97d162/. Replace the model ID's slash with two hyphens after the models-- prefix. Preserve each snapshot's internal subdirectories for offline loading by S2_2.

## Reproduce the analyses

The full workflow requires the source analysis inputs. Once those inputs are available, run the scripts from the repository root in the Step 1 → Step 2 → Step 3 order below. Set the R executable to your installation and invoke each R script using the following pattern:

```powershell
$env:LC_ALL = 'English_United States.utf8'
$rscript = 'D:\Program Files\R\R-4.5.3\bin\Rscript.exe'
& $rscript 'Step_1_SysRev/S1_1_Generate_R_DataFrame.R'
if ($LASTEXITCODE -ne 0) { throw 'S1_1 failed' }
```

## Step 1 Narrative Synthesis and Meta-analysis of Symptom Structure

Run S1_1–S1_5 in numerical order. Prepare the geographic inputs for S1_2 using the links in Notes. S1_6 provides the exploratory regression analysis.

| Script                                              | Function                                                                                                                 |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| S1_1_Generate_R_DataFrame.R                         | Converts the harmonized workbook into study-level data and linked analyses.                                       |
| S1_2_StudyCharacteristicsDescriptiveVisualization.R | Summarizes study characteristics for Table 1 and prepares descriptive and geographic data for Figure 3.                  |
| S1_3_FactorStructureAndAnalysisStages.R             | Organizes factor allocations and analyses for structural synthesis and Figure 2.                                  |
| S1_4_QualityAssessmentAnalysis.R                    | Summarizes methodological practices for Table 2 and prepares criterion-level quality assessment data for Figure 3.                  |
| S1_5_FactorCooccurenceAnalysis.R                    | Estimates item cooccurrence across studies and evaluates clustering, stability and sensitivity.                          |
| S1_6_ExploratoryRegression.R                        | Fits adjacent-category ordinal and multiple linear models of reported factor count and exports their coefficient tables. |

## Step 2 LLM-based Semantic Embeddings and Similarity Analysis

Run S2_1, then S2_3–S2_5 with the formal embeddings in place. S2_4 also uses the literature cooccurrence data produced by S1_5.

| Script                               | Function                                                                                                               |
| ------------------------------------ | ---------------------------------------------------------------------------------------------------------------------- |
| S2_1_PrepareSemanticInputs.R         | Prepares item wording and benchmark definitions for embedding generation and semantic analysis.                        |
| S2_2_GenerateEmbeddings.py           | Generates normalized item and definition embeddings for five conditions using four fixed encoders.                     |
| S2_3_EncoderAgreement.R              | Estimates agreement among the three encoders applied to the same revised-English texts.                                |
| S2_4_SemanticAlignment.R             | Analyses semantic similarity and its correspondence with literature-based item cooccurrence, and prepares figure data. |
| S2_5_CorrelationOfCosineSimilarity.R | Compares item–criterion cosine similarities across the primary and four supplementary conditions.                      |

## Step 3 Visualization

The supplied figure data support Figure 2, Figure 4 and Figures S1–S4 directly. Figure 3 also requires the geographic preparation described in Notes. The figure scripts export PDFs to Res_3_Report, supplied with only an empty .gitkeep placeholder as the reproduction output destination. Scientific tables are written to their corresponding modules in Res_2_Results.

| Script         | Function                                                                                    |
| -------------- | ------------------------------------------------------------------------------------------- |
| Figure_2.R     | Generates Figure 2, Overview of factor structures and analysis methods across the included studies, from S1_3. |
| Figure_3.R     | Generates Figure 3, Geographical and language coverage, reported factor numbers, and quality assessment, from S1_2/S1_4. |
| Figure_4.R     | Visualizes the primary personality-MPNet revised-English results from S2_4.                 |
| Figure_S1-S4.R | Visualizes the four supplementary encoder and wording conditions from S2_4.                 |

Figure 1 is a manually made PRISMA 2020 flowchart. Figure 2 presents the study-level structural and methodological overview, followed by the descriptive and quality assessment synthesis in Figure 3. Figure 4 presents the primary semantic analysis.

## License

Original content by Kunru Song in this repository—including R and Python scripts, the extraction-data compilation, documentation, and original tables and figures—is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE). Copyright © 2026 Kunru Song.

The license permits sharing and adaptation, including commercial use, with appropriate attribution, a [license link](https://creativecommons.org/licenses/by/4.0/), and an indication of changes. Third-party materials, including reproduced questionnaire wording, source-publication content and cartographic elements, retain their respective rights and license terms.

## Notes

Figure 3 uses cartographic geometry from the Ministry of Natural Resources [Standard Map Service](http://bzdt.ch.mnr.gov.cn/) and its [world-map data service](http://bzdt.ch.mnr.gov.cn/iserver/services/map-1E_WorldMap_0825/rest/maps/1E_WorldMap_0825), at a scale of 1:100,000,000. Download the geographic inputs before running S1_2. Once S1_2 and S1_4 have prepared the descriptive and quality assessment data, run Figure_3.R to assemble the figure.
