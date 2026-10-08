# Economic Participation of Persons with Disabilities in India

An R-based analysis of economic participation among persons with disabilities in India, examining differences across age, sex, rural–urban residence, disability type, and State/Union Territory.

## Research Question

How does economic participation among persons with disabilities vary by age, sex, disability type, and rural–urban residence in India?

## About the Project

This project uses Census of India 2011 data to examine the economic participation of persons with disabilities at the national and State/Union Territory levels.

The analysis focuses on differences in participation across demographic and geographic characteristics rather than treating the Census table as individual-level survey data. The project was conducted in R, with the workflow covering data cleaning, validation, descriptive analysis, statistical testing, and visualisation.

## Data Source

**Office of the Registrar General & Census Commissioner, India (ORGI), Population Census 2011**

**Table C-23:** *Disabled population among main workers, marginal workers, non-workers by type of disability, age and sex (India & States/UTs) – 2011*

Official Census catalogue:  
https://censusindia.gov.in/nada/index.php/catalog/2251

Reference ID: **PC11_C23**

The table contains population counts classified by:

- State/Union Territory
- Rural/Urban residence
- Type of disability
- Age group
- Sex
- Main worker status
- Marginal worker status
- Non-worker status

The original Census file is not included in this repository. It is excluded through `.gitignore`; the repository contains the R workflow and derived results needed to document the analysis.

## Unit of Analysis

The source data consist of aggregated Census counts representing persons with disabilities across different demographic and geographic categories.

The analysis therefore does **not** treat each row as an independent individual observation.

Where comparisons are made across States/UTs, India-level aggregate rows are excluded from State/UT summaries to avoid mixing national totals with subnational observations.

## Economic Participation Measure

Economic participation is measured as the proportion of persons with disabilities classified as workers:

**Economic Participation Rate = (Main Workers + Marginal Workers) / Disabled Population × 100**

The two marginal-worker categories in the Census table are combined:

- Marginal workers working for less than 3 months
- Marginal workers working for 3–6 months

Non-worker percentages are also calculated to provide a complementary measure of economic participation.

## Analytical Approach

### 1. Descriptive analysis

The project first describes the disabled population by:

- Sex
- Age group
- Disability type
- Rural–urban residence
- State/Union Territory

Economic participation rates are then compared across these dimensions.

### 2. Age comparison

Economic participation is examined for:

- 0–14 years
- 15–59 years
- 60+ years

State/UT-level variation is also examined to identify geographic differences in participation across age groups.

### 3. Sex comparison

Male and female economic participation rates are compared at the national level, with the percentage-point difference between the two groups reported.

### 4. Rural–urban comparison

Economic participation is compared between rural and urban areas at the national level and across States/UTs.

### 5. Disability-type comparison

Economic participation is compared across the eight specific disability categories available in the C-23 table:

- Seeing
- Hearing
- Speech
- Movement
- Mental retardation
- Mental illness
- Any other disability
- Multiple disabilities

### 6. Statistical association

Chi-square tests are used to examine the association between worker status and:

- Sex
- Age group
- Rural–urban residence
- Disability type

Cramér's V is reported as an effect-size measure alongside the chi-square statistics.

Because the Census represents a very large population rather than a sample survey, interpretation places greater emphasis on the magnitude of differences and effect sizes than on statistical significance alone.

## Key Findings

The analysis shows substantial differences in economic participation across demographic and disability characteristics.

- **Age:** Economic participation is highest among persons aged 15–59 years (50.5%), followed by those aged 60+ (28.3%) and 0–14 years (4.1%).
- **Sex:** Male participation is 47.2%, compared with 22.6% among females, representing a **24.6 percentage-point difference**.
- **Rural–urban residence:** Participation is 37.6% in rural areas and 33.5% in urban areas, a **4.1 percentage-point difference**.
- **Disability type:** Participation ranges from 18.5% among persons with multiple disabilities to 42.0% among persons with speech disabilities at the national level.
- **Geographic variation:** Economic participation varies considerably across States and Union Territories.
- **Statistical associations:** Age group shows the largest Cramér's V among the factors examined (V = 0.39), followed by sex (V = 0.25), disability type (V = 0.15), and rural–urban residence (V = 0.04).

These findings describe observed population-level differences and associations in the 2011 Census data; they should not be interpreted as evidence of causal relationships.

## Results

### Tables

The analysis produces six main results tables:

1. National profile of the disabled population
2. Economic participation by age group
3. Economic participation by sex
4. Economic participation by rural–urban residence
5. Economic participation by disability type
6. Chi-square tests and Cramér's V

### Figures

The analysis produces five figures:

1. Economic participation by age group
2. Economic participation by sex
3. Economic participation by rural–urban residence
4. Economic participation by disability type
5. State/Union Territory variation in economic participation

All tables and figures are generated from the R workflow and saved in the `results/` directory.

## Reproducibility

The project uses a reproducible R script for data cleaning, analysis, and visualisation.

To reproduce the analysis:

1. Obtain the C-23 Census 2011 source file from the official Census catalogue.
2. Place the source file in `data/raw/`.
3. Open `Disability_economic_participation.Rproj` in RStudio.
4. Run `scripts/01_data_cleaning.R`.
5. The analysis outputs are generated in `results/tables/` and `results/figures/`.

## Repository Structure

```text
Disability_economic_participation/
│
├── data/
│   └── raw/
│       └── [Census source file — not included in GitHub]
│
├── scripts/
│   └── 01_data_cleaning.R
│
├── results/
│   ├── figures/
│   └── tables/
│
├── Disability_economic_participation.Rproj
├── .gitignore
└── README.md