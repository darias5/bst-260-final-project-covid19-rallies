## Shiny app guideline

This shiny app is designed to help users to give a detailed visualization on the data. In general, the information with rallies is more focused.

- The first tab is designed to visualize COVID-19 data for each rally. 
  - You can choose any county that once had a rally, and the rolling 7-day mean data 3-week before and after would be plotted in the format of line plot. With the day of rally lies in the middle, users can clearly see how the trend goes before and after the selected rally.
  - Users can also choose up to 5 counties with rallies to plot their data together, so that it would be easier to compare between 2 rallies, for example, to see if they share the same trend after rallies. 
  - Also, a radio button is provided for users to choose the shown data type: rolling 7-day mean new cases, or rolling 7-day mean new deaths.
- The second tab is designed to provide a more general information:
  - Users can specifically select a county with rally with the first selection input. A dashed line on the rally's date would be plotted so that users can now exactly when the rally happened. If there are 2 rallies in same rally, the first/second date would be shown in a red/blue dash line.
  - The second selection input allows users to freely choose counties, and see how their data changed over time. This also can be used when users want to compare the selected rally-county with other controls, for example, counties in same state.
  - The radio button for data type, similar as that in the first tab, is also provided.
- The third tab is designed to spatially plot the data on a US map, as another descriptive visualization :
  - In general, users can see how the pandemic goes over time in United States by selecting date. A heatmap baesd on the real geographic map would be presented.
  - The radio button for data type, similar as that in the first tab, is also provided.

