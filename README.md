This Project was done as part of datascience specialization capstone course in Mar 2019. 

Objective:

The objective of the project was to create a shiny app which predicts the next few words based on the input user provides.

The project required the following steps to be finished -

1. get the huge text corpus data consisting of blogs, news and tweets. 
2. clean the data and create n grams from the text.
3. create the n gram frequency tables (1 to 4 grams).
4. Use the n grams tables as an input to create a shiny app which asks user for input phrase and predicts the next few words.

The basic logic used in the project is to check for most occurring highest ngram (in our case 4 gram) based on the user's last 3 input words and select the top 3 n grams. The last words of these phrases could be the most probable next few words. If we can't find the relevant n-gram we back off to the lower n-gram and use only last two words and so on until we find relevant next words. 