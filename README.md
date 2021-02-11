# Wikipedia-Web-Traffic-Forecasting
Forecasting future webpage hits for public figures on Wikipedia

Data set from Kaggle:
https://www.kaggle.com/c/web-traffic-time-series-forecasting/data

The data is obtained from Kaggle, where the competition was to use the provided data to predict the view count of a specific date range. Our model will not be designed to predict the same specific date range in particular, but will utilize the data all the same. The dataset records daily traffic from a date range of July 2015 to the end of September 2017. For the analysis, the date range of July 2015 to December 2016 will be used to create the model, and the remaining data will stand as a reference point for the model’s forecasting. The public figures in particular that this report will analyze are two actors, Emma Stone and Leonardo DiCaprio, as well as two music artists, Kendrick Lamar and Bruno Mars. 
The original dataset contained information for many different public figures. This dataset was thoroughly trimmed down to contain only people whose Wikipedia traffic can be used in a more  business-oriented way. In addition, other elements that were not relevant to the report were removed, such as the type of web browser, because this part of the data did not affect the analysis. For the people of interest, the excess data was trimmed down to just their Wikipedia page traffic, which was recorded on a daily view basis. Each of the datasets have 550 entries, one for each day, from a date range of July 2015 to the end of September 2017.
The data was then partitioned into a training set with a size of 385 entries, and the remaining 165 entries were the validation set.
Multiple methods of forecasting are performed and compared.
