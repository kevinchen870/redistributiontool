#Redistribution Tool
Build Electorates from SA1 

#Stakeholders
Individiuals building submissions for Redistributions
#Inputs
Population projection from AEC
Shapefiles from ABS geography

#Outputs 
RShiny 

#Code 
The Code is split into three components
1. Global: Code which is run at the start of each session to import files and set up the data file
2. ui: The user interface - how the Shiny App looks
    Uses the styles.css file for the look fo the app
3. server: Functionality of the App - what happens when people interact with the functions. 

A standalone code exists 'leaflet' which was built for prototyping