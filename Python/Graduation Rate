from pyspark import SparkConf, SparkContext

def parseLine(line):
    fields = line.split(",")
    schoolname = fields[0]
    apps = int(fields[2])
    gradrate = int(fields[18])
    return (schoolname, (apps, gradrate))

conf = SparkConf().setMaster("local").setAppName("GraduationRate")
sc = SparkContext(conf = conf)

#Removed the first row of data that was as stated: ,Private,Apps,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad,Outstate,Room.Board,
#Books,Personal,PhD,Terminal,S.F.Ratio,perc.alumni,Expend,Grad.Rate

lines = sc.textFile("file:///Users\Melvin\Documents\William and Mary\Courses\Spring 2021\BUAD 5132 Machine Learning II\Module 5\M5_Data_Update.csv")

#Create a map filter over the entire dataset
schools = lines.map(parseLine)

#Set the values to be greater than or equal to 10000 applicants
schools_over10 = schools.filter(lambda x: x[1][0] >= 10000)

flipped_schools_over10 = schools_over10.map(lambda x: (x[1][1], x[0]))

sortedschools = flipped_schools_over10.sortByKey(False).collect()

print("The highest graduation rate to the lowest graduation with at least 10,000 applicants is shown in the following list below: ")

for schools in sortedschools:
    print(schools)


#!spark-submit M5_Spark_School_Acceptance_Graduation_Rates.py > M5_Graduation_Rate.txt
