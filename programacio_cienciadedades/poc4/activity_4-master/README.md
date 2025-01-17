# POC4
## Files
main.py: The main script that coordinates the data processing by calling functions from the supporting modules.

setup.py: Contains the datapull function to load and display the dataset.

anomize.py: Contains functions to anonymize data, specifically the name_surname function to anonymize names.

minutagg.py: Contains functions to group times, specifically the gruptimes function to group times and plot a histogram.

fix_club.py: Contains functions to clean and depurate club names in the dataset.

team_analisis.py: Contains functions to analyze team data, focusing on specific teams.

## Description of Scripts
main.py: Imports functions from setup.py, anomize.py, minutagg.py, fix_club.py, and team_analisis.py and executes them in sequence to process the dataset.

setup.py: Loads the dataset and prints its basic details.

anomize.py: Anonymizes the biker column in the dataset and filters out entries with a time value of "00:00:00".

minutagg.py: Groups times in the dataset into 00, 20, or 40-minute intervals and plots a histogram of the grouped times.

fix_club.py: Cleans and depurates the club column in the dataset to standardize the club names.

team_analisis.py: Analyzes the team data, identifying the best performer in a specific team and calculating their position relative to the entire dataset.

## Usage
Ensure all required packages are installed. You can use the following command to install necessary packages:

bash
pip install -r requirements.txt
Place the dataset in the data directory with the name dataset.csv.

## Run the main script:

bash
python main.py

## Example
When you run main.py, it will:

Load the dataset and display the first 5 entries.
Anonymize the biker names and display the first 5 entries after anonymization.
Group the times and display the first 15 entries of the dataset with grouped times.
Clean and depurate club names and display the first 15 entries of the dataset with clean club names.
Analyze the team data for the "UCSC" team, identifying the best performer and their position relative to the entire dataset.
