# Making Human Readable Visualizations

Descriptions and goals

We want to ensure both researchers and practioners alike will understand the results of our post-hoc analysis. Because the output of the analysis is rather extensive, visualizing results past those seen previously can be a massive undertaking. Consequently, it is a challenge to make code for the figures easy to follow and to choose the proper results to visualize. Our goal for this walkthrough is to extend our work in the post-hoc analysis page by highlighting various results through different types of human-readable figures.

## Shiny Apps

R has the ability to run dynamic web-based apps, called Shiny apps. Shiny apps may be hosted on a server, or they may be simply ran on a local machine. The apps are great for allowing viewers to choose their own different options or formats to visualize the data you want to present. Obviously, the primary downside of Shiny apps is the issue of hosting. Nonetheless, we have a couple apps hosted through RStudio that serve to visualize different aspects of the post-hoc analysis.

The first Shiny app is one which allows users to choose several different hybrids and plot their Yield distributions via violin plots. There is also an option to split the hybrids into their respective groups in order to see how hybrids that are associated with more than one weather profile differ across post-hoc groups. We won't provide all the code to produce the app in this walkthrough because it is extensive and may be found at this [link](https://github.com/TACC/EnviroTyping/blob/Tutorial_Additions/sandbox/posthoc_group_analysis/2016/violin_app/app.R). If you desire to run the app on your local machine, you must download the whole folder entitled `violin_app` and ensure any directory references match those in your own R workspace. __*However, you may also see the app hosted online [here](https://bno5761.shinyapps.io/violin_app/)*__. 

Weather profiles and location of code

## Maps

Examples
Code
