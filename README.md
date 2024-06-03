#Redistribution Tool
Tool to assist people in Building Australian COmmonwealth electorates from SA1 

#Stakeholders
Individuals building submissions for Redistributions

#Inputs
Population projection from AEC

#Outputs 
RShiny 

#Code 
The Code is split into three components
1. Global: Code which is run at the start of each session to import files and set up the data file
2. ui: The user interface - how the Shiny App looks
    Uses the styles.css file for the look fo the app
3. server: Functionality of the App - what happens when people interact with the functions. 

A standalone code exists 'editmap' which was built for prototyping

#To Do
add SA1 to user created division template - DONE

Build UI to upload user file
change to tab format
Download template
User upload section
Leaflet map
User saves html


Build Server functions
Download Template

Merge uploaded SA1 file with SA1s 
dissolve by new group
create leaflet
centre on divisions midpoint
label contains new total pop and deviation from quota
SA2, current, proposed turned off by default

save as html 
