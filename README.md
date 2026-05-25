# NYC-Parks

Data cleaning, feature engineering, and exploratory analysis in R on NYC public open spaces. Classifies every park as Active-Dominant, Passive-Dominant, or Equal based on the percentage of space allocated to each recreation type. Exports a cleaned, labeled dataset as a final deliverable.
What it does

Cleans raw column names using tolower() + gsub() and converts percentage columns to numeric
Creates a derived recreation_type classification column using nested ifelse() comparing active vs passive percentage per park
Counts parks in each category using sum() on logical vectors and prints a CrossTable() frequency summary
Reshapes the data from wide to long format using pivot_longer() to enable a side-by-side boxplot comparing active and passive distributions
Produces four ggplot2 visualizations: histograms of active % and passive %, a side-by-side boxplot, and a bar chart of recreation type counts
Renames columns to human-readable labels using which()-based safe renaming and exports the cleaned dataset to Cleaned_NYC_Parks.csv
