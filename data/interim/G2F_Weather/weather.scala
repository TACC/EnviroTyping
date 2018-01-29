val df = spark.read.format("csv").option("header",true).load("g2f_2014_weather_clean.csv")

df.printSchema()

df.createOrReplaceTempView("weather")

val df2= spark.sql("SELECT `Station ID` as StationID,`Month [Local]` as Month,`Year [Local]` as Year,  cast(`Temperature [C]` as double) as Temperature FROM weather")

df2.printSchema

df2.createOrReplaceTempView("weather")


val sqlDF = spark.sql(" SELECT StationID, Month, Year, avg(Temperature) as temp_mean, percentile_approx(Temperature, 0.5 )  as temp_median,  min(Temperature)   as temp_min, max(Temperature) as temp_max, avg(Dewpoin) as Dewpoin_mean, percentile_approx(Dewpoin, 0.5 )  as Dewpoin_median,  min(Dewpoin)   as Dewpoin_min, max(Dewpoin) as Dewpoin_max, avg(RelativeHumidity) as RelativeHumidity_mean, percentile_approx(RelativeHumidity, 0.5 )  as RelativeHumidity_median,  min(RelativeHumidity)   as RelativeHumidity_min, max(RelativeHumidity) as RelativeHumidity_max, avg(SolarRadiation) as SolarRadiation_mean, percentile_approx(SolarRadiation, 0.5 )  as SolarRadiation_median,  min(SolarRadiation)   as SolarRadiation_min, max(SolarRadiation) as SolarRadiation_max, avg(Rainfall) as Rainfall_mean, percentile_approx(Rainfall, 0.5 )  as Rainfall_median,  min(Rainfall)   as Rainfall_min, max(Rainfall) as Rainfall_max,avg(WindSpeed) as WindSpeed_mean, percentile_approx(WindSpeed, 0.5 )  as WindSpeed_median,  min(WindSpeed)   as WindSpeed_min, max(WindSpeed) as WindSpeed_max,avg(WindGust) as WindGust_mean, percentile_approx(WindGust, 0.5 )  as WindGust_median,  min(WindGust)   as WindGust_min, max(WindGust) as WindGust_max   FROM weather GROUP BY  StationID, Month,Year") 



sqlDF.show

sqlDF.printSchema

sqlDF.coalesce(1).write.format("csv").option("header", true).save("temperature.csv")

