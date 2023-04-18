# Sales_forecasting-in-R
Implementing Machine learning models for sales forecasting in R

I examined Random forest and XGBoost machine learning techniques in R to create a suitable model to predict sales for a Retails company.


I performed Random Forest and XGBoost to predict company ‘s sales. The rank of important variables for both models was different. Random 
Forest put VMB(VDMA Machine Building), IBS(ifo Business Situation) and GDP and  PTM(Production in total manufacturing Index ), PTI (Production of total industry Index ) at the 
top. And XGBoost put PTM(Production in total manufacturing Index ), GDP, ER( Employment Rate), VMB(VDMA Machine Building), COP(Crude oil prices), and IBS (ifo Business Situation) 
at the top. We saw Both methods give us good accuracy. Since XGBoost gave us better RMSE and it fits actual sales better than Random Forest, I suggest the XGBoost machine learning 
mode to the Company. 
