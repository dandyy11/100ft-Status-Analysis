# 100ft Status Analysis - Capstone Project for Helmreich and Payne Company
Analysis of 100ft status using R. This includes data loading, cleaning, transformation, and statistical analysis using multiple datasets. The project involves EDA, visualization, and linear regression modeling to uncover insights and performance metrics across rigs and oil samples.

### Dataset
1. **TVI Data (All Rigs):** This dataset contains information related to rig performance for various rigs. Key variables include rig identification, performance metrics, and TVI measurements.
2. **100ft Data (All Rigs):** This dataset provides detailed information on 100ft measurements for multiple rigs.
3. **Oil Sample Data (All Rigs):** This dataset includes oil sample data across all rigs, with attributes related to oil quality and performance indicators.
4. **TopDrive Rig List:** This dataset contains a list of top-drive rigs used in the analysis.

### Objectives
1. **Data Cleaning and Transformation:** Remove missing data, perform necessary transformations, and prepare the datasets for analysis.
2. **Exploratory Data Analysis (EDA):** Visualize the data through scatter plots and summaries to understand relationships between key variables.
3. **Statistical Modeling:** Develop and evaluate linear regression models to predict the relationships between variables and explain performance metrics.
4. **Conclusion:** Summarize findings and provide recommendations based on the analysis.

### Repository Structure
- `100ft_Status_Final.Rmd` - R Markdown file containing the full analysis, including data loading, transformation, EDA, and statistical modeling.
- `100ft_Status_Final.R` - The original R script for the analysis.
- `oil_tests_final.qmd` - A Quarto Markdown file for oil sample analysis.
- `TVI Data (All Rigs).xlsx` - Dataset containing TVI data for all rigs.
- `100ft Data (All Rigs).xlsx` - Dataset containing 100ft data for all rigs.
- `Oil Sample Data (All Rigs).xlsx` - Dataset containing oil sample data for all rigs.
- `TopDrive Rig List.xlsx` - List of TopDrive rigs used in the analysis.
- `Capstone Project.pptx` - Project Description in detail.
- `README.md` - This file, providing an overview of the analysis, objectives, and key findings.


### Findings
1. **Data Cleaning and Transformation:** The datasets were successfully cleaned by filtering out missing data and performing transformations to prepare the variables for analysis.
2. **Exploratory Data Analysis (EDA):** Scatter plots and summaries revealed trends between key variables, providing insights into relationships between rig performance, oil samples, and 100ft measurements.
3. **Statistical Modeling:** Linear regression models were developed to predict relationships between variables. The models provided valuable insights, with performance evaluated using metrics such as R-squared and p-values.

### Conclusion
The analysis of the 100ft status dataset provided valuable insights into the relationships between key variables across rigs and oil samples. Through the use of exploratory data analysis and statistical modeling, the project revealed important trends that can guide decision-making and performance evaluations.

How to Use
To reproduce the analyses, clone this repository and open the R Markdown files (.Rmd). You can knit these files in RStudio to generate the HTML or Markdown outputs.

# Clone the repository
https://github.com/dandyy11/100ft-Status-Analysis.git

### Contact
For questions or suggestions, please contact Salman Imtiaz at salman.imtiaz414@gmail.com
