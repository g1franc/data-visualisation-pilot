# visualisation-pilot

Gapminder visualisation 
* launch R script 
	cd Data cleaning Visualisation 1-V2
	Scripts/DataPrep-Gapminder.R
* A csv file will be generated 
	Data cleaning Visualisation 1-V2\Output\CountryInformation.csv
* Copy the concent of the csv in 
	https://docs.google.com/spreadsheets/d/1Ex4EEEDeT8lfRWSi5nCwYnZBNskl9bMO-nmDX3mMszk/edit?ts=58385836 
* If you want to add column
	select the column you want on https://developers.google.com/chart/interactive/docs/querylanguage#setting-the-query-in-the-data-source-url 
	copy the generated URL and update the variable dataURL in "visualisation\Gapminder.html"

Country Network visualisation
* launch R script 
	cd Data cleaning Visualisation 1-V2
	Scripts/DataPrep-Link.R
* Two csv files will be generated 
	Data cleaning Visualisation 2\Output\CountryInformation.csv
	Data cleaning Visualisation 2\Output\NbLink.csv
* launch python to transform the csv in json 
		Scripts/GenerateJSONEU28.py Output/NbLink.csv Output/CountryInformation.csv
		Scripts/GenerateJSON.py Output/NbLink.csv Output/CountryInformation.csv
* Six json files will be generated 
	Data cleaning Visualisation 2\OutputJS\FP6.json
	Data cleaning Visualisation 2\OutputJS\FP7.json
	Data cleaning Visualisation 2\OutputJS\H2020.json
	Data cleaning Visualisation 2\OutputJS\FP6EU28.json
	Data cleaning Visualisation 2\OutputJS\FP7EU28.json
	Data cleaning Visualisation 2\OutputJS\H2020EU28.json
* copy data 
* change js 
