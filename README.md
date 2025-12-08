# NBA Analytics for STAT107

This is the repository for the NBA Analytics group in STAT107 F25.

For this project, we seek to answer the following two questions
1. **What contributes best to an aggregate metric of offensive/defensive efficiency relevant to
winning games?**
2. **What wins more games, defense or offense?**

To do so, we will be pulling from these two public data sets
> [NBA Data 2010-2024 by NocturneBear (GitHub)](https://github.com/NocturneBear/NBA-Data-2010-2024)
> 
> [Offical NBA Team & Player Statistics](https://www.nba.com/stats)

## Repository File Description:
1. [FinalReport.Rmd](FinalReport.Rmd) - Rmd file containing report
2. [FinalReport.pdf](FinalReport.pdf) - Report in PDF format
3. [FinalReport.tex](FinalReport.tex) - Knitted Report PDF containing report + displayed code
4. [Team22_NBA-Analysis.RData](Team22_NBA-Analysis.RData) - RData jar containing all environment variables
5. [STAT107-NBAAnalytics.Rproj](STAT107-NBAAnalytics.Rproj) - Contains all project data
6. [img/util/NocturneBear-NBA_Analytics-blue.png](img/util/NocturneBear-NBA_Analytics-blue.png) - Citation badge for source data
7. [data/nba_processed.csv](data/nba_processed.csv) - Processed Data
8. [data/nba_clean.csv](data/nba_clean.csv) - Cleaned Data
9. [data/nba_raw.csv](data/nba_raw.csv) - Raw Data
10. [util/00_requirements.R](util/00_requirements.R) - Required libraries list
11. [util/01_functions.R](util/01_functions.R) - Custom functions
12. [img/plots](img/plots) - All plots
13. [img/util](img/util) - DOI badges for citation
14. [util/11_DataCleaning.Rmd](util/11_DataCleaning.Rmd) - Data Cleaning & Saving File
15. [util/21_DataProcessing.Rmd](util/21_DataProcessing.Rmd) - Data Processing File
16. [util/31_DataVisualization.Rmd](util/31_DataVisualization.Rmd) - Data Visualization File
17. [util/41_Offensive_Model.Rmd](util/41_Offensive_Model.Rmd) - Offensive Model
18. [util/42_Defensive_Model.Rmd](util/42_Defensive_Model.Rmd) - Defensive Model
19. [util/43_Full_Model.Rmd](util/43_Full_Model.Rmd) - Full Model (Offensive + Defensive)
20. [util/51_LikelihoodTest.Rmd](util/51_LikelihoodTest.Rmd) - Nested $\chi^2$-test on smaller models against composite
21. [data/](data/) - REnv variables stored at each stage (51 data jar not made)
