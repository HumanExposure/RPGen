# Residential Population Generator.

This repository contains scripts, input files, and some example 
output files for the Residential Population Generator, an R-based tool to generate U.S. synthetic human residental populations to use in making estimates of near-field chemical exposures. This tool is most readily adapted for using in the workflow for CHEM, the Combined Human Exposure Model, avaialable in two other GitHub repositories in the HumanExposure project, including ProductUseScheduler and source2dose. CHEM is currently best suited to estimating exposure to product use. Outputs from RPGen are translated into ProductUseScheduler, which with subsequent outputs used in source2dose. Please see documentation for those modules in the appropriate repositories.  

Detailed information about this module is included in the following manuscript, currently in review
East et al. 2021. The Residential Population Generator(RPGen): A tool to parameterize residential, demographic, and physiological data to model intraindividual exposure, dose, and risk.

RPGen requires information from three data sources, including the American Housing Survey(AHS), the Residential Energy Consumption Survey(RECS) and the Person Public Use Microdata Survey (PUMS). This information must currently be manually downloaded from the following sources:

RECS: https://www.eia.gov/consumption/residential/data/2015
AHS: https://census.gov/programs-surveys/ahs/data.html
PUMS: http://www.census.gov/programs-surveys/acs/data/pums.html

RPGen was originally written by GG at ICF, and titled the PopulationHousingGenerator. Code for this version is stored in a GitHub repository of the same name at https://github.com/HumanExposure/PopulationHousingGenerator
RPGen includes the majority of elements from the original repository, with updates submitted to EPA in August 2019 from ICF. Please see the PopulationHousingGenerator
for a full history of RPGen. 

To run RPGen, open the 'Control.R' file, load the RPGen.run() function, and call it. Inputs may also be called using a .txt file.
Further information on running RPGen, inputs, and methods is provided in the technical manual. 
