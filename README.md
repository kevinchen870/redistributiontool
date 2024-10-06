# Redistribution Tool

Tool to assist people in Building Australian Commonwealth electorates from SA1 

## Stakeholders
Individuals building submissions for Redistributions

## Inputs
Population projection from AEC
User generated mapping file from SA1 to divisions

## Outputs 
RShiny Dashboards

## Code 
The Code is split into three components
1. Global: Code which is run at the start of each session to import files and set up the data file
2. ui: The user interface - how the Shiny App looks
    Uses the styles.css file for the look fo the app
3. server: Functionality of the App - what happens when people interact with the functions. 

Prototyping codes are located in ad hoc testing

## To Do
- add SA1 to user created division template - DONE

- Build UI to upload user file - DONE
- change to tab format - DONE
- Download template - DONE
- User upload section - DONE
- Leaflet map - DONE
- User saves html - Doesn't seem doable
- Add SA2 to CSV template
- selector by state for other states
- Add SA2/SA1 clickable
- Add selector for Division ->SA1


