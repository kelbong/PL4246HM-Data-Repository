# PL4246HM-Data-Repository

# Main aim of project 
The main aim of this project was find out whether ethnic homophily exerts as much singnificance 
on the interpersonal connections that are made in the context of a multiracial society like Singapore. 
Additionally, gender homophily is also examined as it expected to be more salient on an Asian context like Singapore. 

'FinalProject.R' contains all the code for all the statistical calculations and plots used in the network analysis mentioned in my paper, 

'Processed Data.R' contains all the processed data that are utilised in the network analysis. 
It consists of various data.frames and igraph objects, as well as summary tables generated for some values. 

# Raw .csv files
'class-names-filtered.csv' contains a pre-processed set of data for names of students, 
where names that are filtered out for weight, and mutuality are also removed from this dataset. Furthermore, 
names that have reponses that are "NA" are also removed. 
There were originally 76 names. After filtering, 54 names remain. Hence, a total of 22 names were removed. 

'class-responses-filtered.csv' contains a pre-processed set of data for the responses of students,
after filtering out and excluding the weights that are less than 2. 
