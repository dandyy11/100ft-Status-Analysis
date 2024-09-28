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

### Analysis
orque and Performance Metrics:

Max Torque: 43.33 k ft-lbs
Mean Torque: 20.87 k ft-lbs
Median Torque: 20.48 k ft-lbs
The torque values indicate that while the majority of operations experience torque levels around 20.5 k ft-lbs, there are instances of significantly higher torque (up to 43.33 k ft-lbs), which could stress equipment and lead to failures. This suggests a need to monitor operations where torque exceeds normal ranges.

Failure Rates:

Active Status: 7,078 records (47.6%)
Failed Status: 7,792 records (52.4%)
The slightly higher percentage of failures compared to active statuses indicates that more than half of the monitored equipment has experienced failures at some point. This suggests potential systemic issues in maintenance or operational procedures that need to be addressed to improve equipment longevity.

Drilling Depths:

Max Depth: 27,500 feet
Mean Depth: 10,439 feet
Median Depth: 10,200 feet
The majority of drilling occurs around 10,000 feet, but some operations reach depths as high as 27,500 feet. These deeper operations likely place more strain on equipment, increasing the risk of failure.

Rate of Penetration (ROP):

Max ROP: 1,787 ft/hr
Mean ROP: 239 ft/hr
Median ROP: 172 ft/hr
Most drilling progresses at moderate speeds (around 172-239 ft/hr), but extreme outliers (up to 1,787 ft/hr) could indicate operational inefficiencies or instances where equipment is pushed beyond its optimal performance range.

Iron Content in Oil Samples:

Max Iron Content: 7,699 ppm
Mean Iron Content: 130.5 ppm
Median Iron Content: 5 ppm
While the majority of samples show low levels of iron (median of 5 ppm), some outliers (max 7,699 ppm) suggest significant wear in specific components. The elevated iron levels indicate that certain rigs may be at risk for mechanical failure due to excessive wear.

Lubrication and Fluid Changes:

Mean Fluid Added: 0.03 units
Median Fluid Added: 0 units
Most rigs did not require significant lubrication or fluid changes, as reflected in the median of 0 units added. However, the mean value shows that a small number of rigs did require maintenance, signaling that ongoing monitoring is necessary.


### Findings
1. **Data Cleaning and Transformation:** The datasets were successfully cleaned by filtering out missing data and performing transformations to prepare the variables for analysis.
2. **Exploratory Data Analysis (EDA):** Scatter plots and summaries revealed trends between key variables, providing insights into relationships between rig performance, oil samples, and 100ft measurements.
3. **Statistical Modeling:** Linear regression models were developed to predict relationships between variables. The models provided valuable insights, with performance evaluated using metrics such as R-squared and p-values.

### Conclusion
Failure Rate: With 52.4% of rigs experiencing failures, this is a significant concern that highlights the need for improved maintenance practices and possibly enhanced operational training or procedural adjustments. Addressing this could reduce downtime and associated costs.

Torque and Operational Stress: The fact that some rigs experience torque levels significantly higher than the median (43.33 k ft-lbs vs. 20.48 k ft-lbs) indicates that certain operations may be putting undue stress on machinery. Monitoring and adjusting operations for rigs experiencing high torque can help prevent mechanical failures.

Drilling Depth and Performance: The deep drilling operations (max 27,500 feet) require careful attention as they pose more risk to equipment due to increased strain. While most drilling is concentrated around 10,000 feet, deeper operations should be closely monitored for wear and potential failure risks.

Wear and Tear Indicators: The analysis of oil samples, particularly the iron content, provides valuable insights into machinery wear. Although the median iron content is low (5 ppm), the presence of outliers with extremely high iron levels (7,699 ppm) signals that some rigs are experiencing severe wear. These rigs need immediate attention to prevent breakdowns.

Maintenance and Fluid Usage: The lack of significant fluid changes in most rigs suggests that lubrication issues are not widespread. However, the few instances where fluids were added indicate that periodic maintenance is still crucial to prevent future failures.

How to Use
To reproduce the analyses, clone this repository and open the R Markdown files (.Rmd). You can knit these files in RStudio to generate the HTML or Markdown outputs.

# Clone the repository
https://github.com/dandyy11/100ft-Status-Analysis.git

### Contact
For questions or suggestions, please contact Salman Imtiaz at salman.imtiaz414@gmail.com
