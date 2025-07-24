# **Mallee Ecosystems State-and-Transition Model (STM) App**

This interactive [STM shiny app](https://rco-anderson.shinyapps.io/stm_app/) implements a State-and-Transition Model (STM) designed specifically for the Mallee ecosystems in Australia. The STM framework allows users to explore and define vegetation state transitions under different scenarios of degradation and recovery pathways.

## **Purpose**

The primary goal of this tool is to facilitate the elicitation of expert knowledge. Experts can interactively:

- Visualise vegetation states and potential transitions.
- Assign transition probabilities for each state change.
- Select ecological drivers that influence each transition.
- Specify different values for degradation and regeneration pathway for each drivers, including their interactions.

## **Features**

- Interactive network diagram of vegetation states and transitions.
- Transition-specific driver selection.
- Scenario and time-period toggles.
- CSV download of responses including transition probabilities for drivers (including interactions).

## **Structure**

The app is composed of:

- A predefined list of vegetation states (nodes).
- Defined transitions (edges) for both degradation and regeneration scenarios.

## **Usage**

Launch the [STM shiny app](https://rco-anderson.shinyapps.io/stm_app/).

1. Enter Name and email
2. Select a scenario (Degradation or Regeneration).
3. Click on any transition arrow to.
4. Click a transition arrow to:
    - Enter a transition probability for each driver.
    - Optionally add interactions between drivers.
    - Optionally add comments.
5. Repeat for other transitions.
6. Click Submit All Probabilities and download the data.

## **Notes**

- Transitions without assigned probabilities are displayed as thin grey arrows.
- Completed transitions are coloured:
  - Red for degradation
  - Sky blue for regeneration

