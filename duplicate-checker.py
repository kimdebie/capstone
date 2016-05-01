'''
Script to check if tweets are partial duplicates of each other.
Based on: http://chairnerd.seatgeek.com/fuzzywuzzy-fuzzy-string-matching-in-python/
The fuzzywuzzy library, based on https://en.wikipedia.org/wiki/Levenshtein_distance

Kim de Bie
Leiden University College The Hague
Created: 10-04-2016
Last edited: 11-04-2016
'''

from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import csv
import os

finisheddata = []

# iterate over files

for i in os.listdir(os.getcwd()):
    if i.endswith(".csv"):

        #### reading CSV ####

        # open csv file
        csvfile = open( i, "rb" )

        # read csv file according to dialect
        reader = csv.reader(csvfile)

        # read header
        header = reader.next()

        # read data into memory
        data = [row for row in reader]

        # close input file
        csvfile.close()

        #### analyzing a disaster ####

        print 'Analyzing', i

        # loop over every tweet
        for i, tweet in enumerate(data):

            # check if tweet has not already been considered a duplicate of another
            # if that is the case, we can skip it
            if data[i][13] == 'FALSE':

                # if it is not: we will be checking this tweet
                checked_tweet = data[i][7]

                # give the tweet a different status to avoid it from being compared to itself
                # and to avoid unnecessary comparisons later down the line (comparing 1 to 3 AND 3 to 1)
                data[i][13] = 'watched'

                # compare the tweet we are checking to every other tweet in the dataset
                for j, tweet in enumerate(data):

                    if data[j][13] == 'FALSE':

                        # determine the Token Sort ratio of the two tweets
                        ratio = fuzz.token_sort_ratio(checked_tweet, data[j][7])

                        if ratio > 70:

                            # the tweet is indeed a duplicate of another tweet
                            data[i][13] = True
                            data[j][13] = True
                            # the ID number of the duplicated tweet is added to both tweets (easy for grouping)
                            data[i][14] = data[i][0]
                            data[j][14] = data[i][0]

        # add all new & updated rows to the array
        for row in data:
            finisheddata.append(row)

#### Saving output to CSV ####
writer = csv.writer(open('finaldata.csv', 'wb'))
writer.writerow(header)
for row in finisheddata:
    if row[0] != '':
	       writer.writerow(row)
