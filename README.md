# **Mallee Ecosystems State-and-Transition Model (STM) App**

This interactive Shiny application implements a State-and-Transition Model (STM) designed specifically for the Mallee ecosystems in Australia. The STM framework allows users to explore and define vegetation state transitions under different scenarios of degradation and regeneration, across two future time horizons: 25 years and 100 years.

## **Purpose**

The primary goal of this tool is to facilitate the elicitation of expert knowledge. Experts can interactively:

- Visualise vegetation states and potential transitions.
- Assign transition probabilities for each state change.
- Select ecological drivers that influence each transition.
- Specify different values for degradation and regeneration pathways.
- Set probabilities independently for near-term (25 years) and long-term (100 years) projections.

The data entered can be downloaded or submitted, enabling structured capture of expert input for future analysis or model calibration.

## **Features**

- Fully interactive network diagram of vegetation states.
- Dynamic edge selection and highlighting.
- Custom driver selection grouped by ecological themes (e.g., climate, fire, grazing, weeds).
- Scenario and time-period toggles.
- CSV download and export of all expert responses including probabilities and selected drivers.

## **Structure**

The app is composed of:

- A predefined list of vegetation states (nodes).
- Defined transitions (edges) for both degradation and regeneration scenarios.
- A detailed driver database with over 150 ecological preconditions grouped under 13 broad themes.

## **Usage**

Launch the Shiny app.

1. Select a scenario (Degradation or Regeneration).
2. Choose a time horizon (25 or 100 years).
3. Click on any transition arrow to:
4. Enter a transition probability.
    - Select ecological drivers.
    - Repeat for all transitions.
5. Click Submit All Probabilities or download the data for further processing.

## **Notes**

- Transitions without assigned probabilities are displayed as thin grey arrows.
- Completed transitions are coloured:
  - Teal for degradation
  - Sky blue for regeneration

