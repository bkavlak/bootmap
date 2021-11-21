# bootmap <a href='https://github.com/bkavlak/bootmap/blob/main/functions/bootmap-hexagon.R'><img src='visualizations/bootmap-15-hexagon.png' align="right" height="139" /></a>

Bootmap is an R framework for calculating uncertainty metrics from a crop map.
We are now working on a paper to elaborate on our procedure and results.
A detailed documentation and a R package structure will be organized in this repository.

Hopefully, you will be able to create graphics and tables for your map accuracy assessments:


### Bootstrap Accuracy & Area Plots

These graphs displays the uncertainty of your model for each class and for each adjusted crop area.

Accuracy Plot              |  Area Plot
:-------------------------:|:-------------------------:
![](visualizations/bootmap_accuracyhistogram_rep1500_allclasses.png)  |  ![](visualizations/bootmap_areahistogram_rep1500_allclasses.png)


As you increase **n** in bootstrap, the crops with enough sample converges to normal distribution. They grow just as plants!
<div align="center"><img src="visualizations/bootmap_accuracyhistogram_differentreps_tomato.png"></div>

### Bootstrap Accuracy & Area Tables

The tables are a translation of graphs to another human-created language. 95% confidence is an estimate to give some advice for your uncertain life. 

Accuracy Table              |  Area Table
:-------------------------:|:-------------------------:
![](visualizations/BootTable_Accuracy_1500.png)  |  ![](visualizations/bootmap_areatable_rep1500_allclasses.png)

### Crop Area Adjustment Uncertainty Plot

This is a zoom to an accuracy plot of the Tomato class.

<div align="center"><img src="visualizations/bootmap_accuracyhistogram_rep1500_Tomato.png"></div>
