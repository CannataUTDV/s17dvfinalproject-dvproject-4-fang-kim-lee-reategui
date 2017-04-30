require(data.world)
conn <- data.world()
collegescorecard <- query(conn, dataset="jlee/s-17-dv-final-project", type = "sql",query="SELECT *
FROM `CollegeScorecard.csv/CollegeScorecard`
LIMIT 10")

require(data.world)
conn <- data.world()
census <- query(conn, dataset = "jlee/s-17-dv-final-project", type = "sql", query = "SELECT *
FROM `us_education_census.csv/us_education_census`")



print(summary(collegescorecard))
print(summary(census))
print(head(collegescorecard))
print(head(census))
