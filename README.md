Detecting Fraud in Turkish Election by Statistical Method
================

Introduction
------------

This project focuses on the interdisciplinary field of data science and political science, and explores the application of statistical methods to the detection of fraud in 2014 Turkish Presidential Election.

The open-source data describes the number of votes for all parties in Ankara. The number of votes is categorized into three factors: number of registered votes, number of people who are observed to go to the office, and number of valid votes in the end.

The concept of "turnout rate" is defined as following: number of valid votes over number of observed people in the voting office. Thus, if the turnout rate is larger than one, then there might be some extra votes that are not supposed to exist.

Methods
-------

This fraud detection mainly concerns two major parties in Turkey: AKP and CHP, and the election resulted in the victory of AKP. However, the AKP votes share shows a significant positive relationship with the turnout rate from a linear regression.

Furthermore, the dataset is divided into two subgroups by any certain turnout rate from 0.73 to 1.0. A negative slope results from turnout rates less than the threshold value, while a postive slope stems from turnout rates larger than the threshold value.

Result
------

Based on hypothesis tests and confidence intervals, the two groups of slopes are from different distribution, which suggest a high probability that AKP committed a fraud.

Download the data
-----------------

<https://github.com/prismsus/election/blob/master/ankara.csv.zip>

Read the full report
--------------------

<https://github.com/prismsus/election/blob/master/presentation.md>
