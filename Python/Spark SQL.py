from pyspark.sql import SparkSession
from pyspark.sql import Row
from pyspark.sql.functions import *


def loadMovieNames():
    movieNames = {}
    with open("ml-100k/u.item") as f:
        for line in f:
            fields = line.split('|')
            movieNames[int(fields[0])] = fields[1]
    return movieNames

def parseInput(line):
    fields = line.split()
    return Row(movieID = int(fields[1]), rating = float(fields[2]))

if __name__ == "__main__":
    # Create a SparkSession (the config bit is only for Windows!)
    spark = SparkSession.builder.config("spark.sql.warehouse.dir", "file:///C:/temp").appName("Spark SQL").getOrCreate()

    # Load up movie ID and convert to a name dictionary
    nameDict = loadMovieNames()
    
    # load the u.data file and convert it to a RDD (movieID, ratings)
    lines = spark.sparkContext.textFile("file:///Users/Melvin/Documents/William and Mary/Courses/Spring 2021/BUAD 5132 Machine Learning II/Module 6/ml-100k/u.data")
    movies = lines.map(parseInput)
    
    # Convert RDD to a DataFrame
    movieDataset = spark.createDataFrame(movies)

    # Compute the average rating for each movieID
    averageRatings = movieDataset.groupBy("movieID").avg("rating")

    # Compute count of ratings for each movieID
    counts = movieDataset.groupBy("movieID").count()

    # Join the two together (We now have movieID, avg(rating), and count columns)
    averagesAndCounts = counts.join(averageRatings, "movieID")

    # Filter movies rated 100 or more times
    popularAveragesAndCounts = averagesAndCounts.filter("count > 100").cache()

    # Order the movies by Average Ratings smallest to highest
    moviefinal = popularAveragesAndCounts.orderBy("avg(rating)").cache()
    moviefinal.show()
    
    # Print them out, converting movie ID's to names as we go.
    print("Which movie—remember, it must have at least 100 ratings—has the highest average rating? What is that average?  (In case of a tie, list all.)")
        
    for movie in moviefinal.collect():
        print(nameDict[movie[0]], movie[1], movie[2])
    
    #Find out which movie is the max 
    print(moviefinal.agg({"avg(rating)": "max"}).collect())
    print("The movie with the highest average rating is A Close Shave (1995) with an average rating of 4.491071428571429")
    
    # Stop the session
    spark.stop()
    
# !spark-submit M6_Spark_SQL_Assignment.py > M6_Spark_Final2.txt
