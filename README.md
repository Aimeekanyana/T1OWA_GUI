# Type-1 OWA Operators for Aggregating Fuzzy Sets
This graphical user interface (GUI) found in the "T1OWA.r" aims to provide researchers without computing background with a tool to impliment the type-1 ordered weighting average (OWA) aggregation process.
For more information on this process please visit https://en.wikipedia.org/wiki/Type-1_OWA_operators

The GUI is split over three different tabs all with different functionality
It's important that these files are kept in the same directory as the GUI is dependent on the files containing the functions.
### Tab 1
This tab allows the user to set up the aggregated objects. 
- This entails selecting the domain range using the slider range input, selecting the membership function (MF) type, and inputting the corresponding parameter values. 
- Once these have been added, the user needs to "Generate plot" which will give a plot output containing the membership function curves.
### Tab 2
The user can set up the fuzzy weights in this tab
- Once the user has selected the type of weight they wish to use, and domain partition (where applicable), the user can "Plot" the results. 
### Tab 3 
- In this tab, the user will generate a plot to aggregate the results. The plot will show the aggregated fuzzy sets with red dashed line indicating the aggregated result for the Type-1 operator selected

*Please refer to "T1OWA PLOT RESULT EXAMPLES.pdf" for visual examples of what to expect when generating the plots using the GUI 
*Research papers written by Shangming Zhou et al.(2008; 2011; 2021) are also found in this repo. for more knowledge on this topic.

- S.-M. Zhou, F. Chiclana, R. I. John, J. M. Garibaldi, and L. Huo, “Type-1 OWA Operators in Aggregating Multiple Sources of Uncertain Information: Properties and Real World Application in Integrated Diagnosis,” IEEE Transactions on Fuzzy Systems, vol.29, no.8, pp. 2112-2121, 2021 (https://doi.org/10.1109/TFUZZ.2020.2992909)
- S.-M. Zhou, F. Chiclana, R. I. John and J. M. Garibaldi, “Alpha-level aggregation: a practical approach to type-1 OWA operation for aggregating uncertain information with applications to breast cancer treatments,” IEEE Transactions on Knowledge and Data Engineering, Vol.23, No.10, pp. 1455-1468, 2011. (http://dx.doi.org/10.1109/TKDE.2010.191)
- S.-M. Zhou, F. Chiclana, R. I. John and J. M. Garibaldi, “Type-1 OWA operators for aggregating uncertain information with uncertain weights induced by type-2 linguistic quantifiers,” Fuzzy Sets and Systems, Vol.159, No.24, pp.3281-3296, December, 2008. (http://dx.doi.org/10.1016/j.fss.2008.06.018)
