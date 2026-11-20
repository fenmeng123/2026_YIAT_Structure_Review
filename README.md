# Revisiting Y-IAT factor structure

Author: Kunru Song

This project examines the factor structure of the 20-item Young's Internet Addiction Test (Y-IAT) through a systematic review and computational semantic analyses.

## Repository Layout

```text
├── Materials_1_CodingInstruction/   # Provides coding manuals and the formal preregistration.
├── Res_1_Data_for_analyses/          # Stores the analysis workbooks, fixed embeddings and encoder configuration.
│   └── Frozen_Encoders/             # Provides an empty destination for optional encoder downloads.
├── Step_1_SysRev/                   # Contains systematic-review analyses and the EndNote reference library.
├── Step_2_SemanticAnalysis/         # Prepares semantic embeddings and analyses their similarities.
├── Step_3_Visualization/            # Generates the manuscript and supplementary figures.
├── Res_2_Results/                   # Stores scientific tables and figure data.
└── Res_3_Report/                    # Provides an initially empty destination for reproduced figures.
```

The [harmonized extraction workbook](Res_1_Data_for_analyses/Coding_Results_Harmonized_Prepared_for_analysis_Double-Checked.xlsx) contains the study data (`Data`) and linked analysis data (`Stages`). Step 2 uses `Wordings_Long` from the [item wording workbook](Res_1_Data_for_analyses/YIAT_20_Item_Wording_Four_Versions_v01.xlsx), `Benchmark_Dictionary` from the [semantic benchmark workbook](Res_1_Data_for_analyses/YIAT_Domain_Neutral_Benchmark_Dictionary_v02.xlsx), and five supplied embedding files. The [Chinese display translations](Res_1_Data_for_analyses/chinese_display_translations.csv) provide labels for Figure S4.

The reference library is [YIAT_FacStruc.enl](Step_1_SysRev/YIAT_FacStruc.enl); keep its companion YIAT_FacStruc.Data database directory beside it when opening it in EndNote.

## Environment

The workflow uses R 4.5.3 on Windows. Install dplyr, tidyr, stringr, readxl, openxlsx, cluster, ggplot2, gtable, scales and jsonlite, together with their dependencies. Regression analyses also use brglm2, clubSandwich and enrichwith. Install Arial and Microsoft YaHei for the English and Chinese figure text.

If you regenerate embeddings, use Python 3.12 and the package versions in [requirements.txt](Step_2_SemanticAnalysis/requirements.txt). Install the CPU build of PyTorch from the [official CPU wheel index](https://download.pytorch.org/whl/cpu), then install the remaining dependencies. S2_2 calls R with jsonlite to save RDS files. Set the Rscript path in S2_2 to your local installation.

## Four encoders

The four encoders and their revisions are specified in the [encoder configuration](Res_1_Data_for_analyses/YIAT_Encoder_Configuration.json). To regenerate embeddings, download the required model snapshots, including their weights, tokenizers, module files and pooling configurations.

| Encoder and download page                                                                                                                                                                                           | Revision                                 |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------- |
| [Generic MPNet: sentence-transformers/all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2/tree/e8c3b32edf5434bc2275fc9bab85f82640a19130)                                              | e8c3b32edf5434bc2275fc9bab85f82640a19130 |
| [Personality MPNet: dwulff/mpnet-personality](https://huggingface.co/dwulff/mpnet-personality/tree/da377389cdf92ed0e28ffbc608912bcbcd97d162)                                                                        | da377389cdf92ed0e28ffbc608912bcbcd97d162 |
| [Harmony: harmonydata/mental_health_harmonisation_1](https://huggingface.co/harmonydata/mental_health_harmonisation_1/tree/13115a1ad333e14b2804bb7eba60e91ce6c42c73)                                                | 13115a1ad333e14b2804bb7eba60e91ce6c42c73 |
| [Multilingual MPNet: sentence-transformers/paraphrase-multilingual-mpnet-base-v2](https://huggingface.co/sentence-transformers/paraphrase-multilingual-mpnet-base-v2/tree/4328cf26390c98c5e3c738b4460a05b95f4911f5) | 4328cf26390c98c5e3c738b4460a05b95f4911f5 |

Place the downloaded snapshots in the `Frozen_Encoders` directory included in the repository. Use the layout below.

```text
Res_1_Data_for_analyses/Frozen_Encoders/models--<owner>--<model>/snapshots/<revision>/
```

For example, the personality snapshot belongs under models--dwulff--mpnet-personality/snapshots/da377389cdf92ed0e28ffbc608912bcbcd97d162/. Replace the model ID's slash with two hyphens after the models-- prefix. Preserve each snapshot's internal subdirectories for offline loading by S2_2.

## Reproduce the analyses

Run the scripts from the repository root in Step 1, Step 2 and Step 3 order. Set `$rscript` to the path to your local R installation and run each R script as shown below.

```powershell
$env:LC_ALL = 'English_United States.utf8'
$rscript = 'C:\path\to\R-4.5.3\bin\Rscript.exe'
& $rscript 'Step_1_SysRev/S1_1_Generate_R_DataFrame.R'
if ($LASTEXITCODE -ne 0) { throw 'S1_1 failed' }
```

## Step 1 Narrative Synthesis and Meta-analysis of Symptom Structure

Run S1_1–S1_5 in numerical order. Prepare the geographic inputs for S1_2 using the links in Notes. S1_6 provides the exploratory regression analysis.

| Script                                              | Function                                                                                                                 |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| S1_1_Generate_R_DataFrame.R                         | Converts the harmonized workbook into study-level data and linked analyses.                                              |
| S1_2_StudyCharacteristicsDescriptiveVisualization.R | Summarizes study characteristics for Table 1 and prepares descriptive and geographic data for Figure 3.                  |
| S1_3_FactorStructureAndAnalysisStages.R             | Organizes factor allocations and analyses for structural synthesis and Figure 2.                                         |
| S1_4_QualityAssessmentAnalysis.R                    | Summarizes methodological practices for Table 2 and prepares criterion-level quality assessment data for Figure 3.       |
| S1_5_FactorCooccurenceAnalysis.R                    | Estimates item cooccurrence across studies and evaluates clustering, stability and sensitivity.                          |
| S1_6_ExploratoryRegression.R                        | Fits adjacent-category ordinal and multiple linear models of reported factor count and exports their coefficient tables. |

## Step 2 LLM-based Semantic Embeddings and Similarity Analysis

Run S2_1, then S2_3–S2_5 with the supplied embeddings. S2_4 also uses the literature cooccurrence data produced by S1_5. To regenerate the embeddings, run S2_2 after S2_1 with the model snapshots in place. Use `--overwrite` to replace the supplied embedding files.

| Script                               | Function                                                                                                               |
| ------------------------------------ | ---------------------------------------------------------------------------------------------------------------------- |
| S2_1_PrepareSemanticInputs.R         | Prepares item wording and benchmark definitions for embedding generation and semantic analysis.                        |
| S2_2_GenerateEmbeddings.py           | Generates normalized item and definition embeddings for five conditions using four fixed encoders.                     |
| S2_3_EncoderAgreement.R              | Estimates agreement among the three encoders applied to the same revised-English texts.                                |
| S2_4_SemanticAlignment.R             | Analyses semantic similarity and its correspondence with literature-based item cooccurrence, and prepares figure data. |
| S2_5_CorrelationOfCosineSimilarity.R | Compares item–criterion cosine similarities across the primary and four supplementary conditions.                      |

## Step 3 Visualization

Use the supplied data in `Res_2_Results` to redraw Figures 2 and 4 and Figures S1–S4. To redraw Figure 3, obtain the map inputs described in Notes and run S1_1 and S1_2 first. The figure scripts write PDFs to `Res_3_Report`. The analysis scripts write tables to their corresponding subdirectories in `Res_2_Results`.

| Script         | Function                                                                                                                 |
| -------------- | ------------------------------------------------------------------------------------------------------------------------ |
| Figure_2.R     | Generates Figure 2, Overview of factor structures and analysis methods across the included studies, from S1_3.           |
| Figure_3.R     | Generates Figure 3, Geographical and language coverage, reported factor numbers, and quality assessment, from S1_2/S1_4. |
| Figure_4.R     | Visualizes the primary personality-MPNet revised-English results from S2_4.                                              |
| Figure_S1-S4.R | Visualizes the four supplementary encoder and wording conditions from S2_4.                                              |

Figure 1 is a manually made PRISMA 2020 flowchart. Figure 2 presents the study-level structural and methodological overview, followed by the descriptive and quality assessment synthesis in Figure 3. Figure 4 presents the primary semantic analysis.

## License

This repository is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE). Copyright © 2026 Kunru Song. Third-party materials, including reproduced questionnaire wording, source-publication content and cartographic elements, retain their respective rights and license terms.

## Notes

Figure 3 uses cartographic geometry at a scale of 1:100,000,000 from the Ministry of Natural Resources [Standard Map Service](http://bzdt.ch.mnr.gov.cn/) and its [world-map data service](http://bzdt.ch.mnr.gov.cn/iserver/services/map-1E_WorldMap_0825/rest/maps/1E_WorldMap_0825). Obtain the map layers before running S1_2 and use the input location and file names specified in the script.
