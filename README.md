<h2>Shiny app link</h2>
https://sf8nhp-malika-yelyubayeva.shinyapps.io/CS424_Project1/ 

<h2> About the app. </h2>
The application was created to visualize on the bar charts the data about the Chicago stations, more precisely about UIC-Halsted, O’Hare and Indiana stations. Data depicts the number of rides that was taken on those stations from 2001 to 2021 (November). You can see that the dashboard is divided on three columns and each column represent different station data. First row shows the overall load on certain stations throughout the years. Then, you can see the load on that station in certain month, then weekdays and then days.
The menu consists of three tabs and three menu options. The tabs show graphs, tables and about the app page. Tables tab shows tables of the same data as was shown on the graphs. And the menu options allow user to choose between years that they want to see for a certain station. By choosing other year for one station, other stations do not change.

 
<h2>Where the data was taken from?</h2>
The dataset was taken from the official Chicago Data Portal website which has data about all stations in Chicago area. This list shows daily totals of ridership, by station entry, for each 'L' station dating back to 2001. Dataset shows entries at all turnstiles, combined, for each station. Ridership statistics are provided on a system-wide and bus route/station-level basis. Ridership is primarily counted as boarding, that is, customers boarding a transit vehicle (bus or rail).  On the rail system, there is a distinction between station entries and total rides, or boarding. Datasets indicate such in their file name and description. 
In order to analyze the data in R, I read the file and took only the stations I needed in my future analysis. I did filter out selected stations by their ID, which is consistent in the whole document. Then, parsed it in order to get year, month and day separately, I also kept the full date in ‘Date’ format in order to manipulate it on the graphs. I used several format functions to get names of months, days and weekdays. 
I used dashboard and put all graphs in there using ggplot2 library. For the tables I used DT library. In order to calculate the total amount for each of the months, days and weekdays I used aggregate function. The color of the dashboard is blue which I took from the CTA’s logo. I also used text formatting to make y axis Italic. For the graphs showing the month, day and weekday data, I used coord_flip() in order to show it in horizontal way. The reason for this was that Days were overlayed on each other, so I decided to flip all of the station related dynamic graphs.

 
<h2>Interesting trends</h2>
The interesting thing about O’Hare airport station is that its peak was in 2019 in January (with the start of COVID-19 pandemic). It can be correlated with the fact that everyone was returning home for quarantine and the airport station was loaded. 
At the same time, the load on UIC-Halsted has significantly dropped from 2019 to 2020. Because UIC has a lot of international students and commuters the station had significantly less rides as the quarantine started. The cancelation of on-campus classes has led to this drop.
The same has happened to Indiana station where the drop in rides happened in 2020 with the quarantine start. Interestingly, that the public transport and CTA were open during the pandemic, but because the majority of people started working/studying online and remotely, there was a significant decrease in public transport usage. This also leads to the global statistical facts that CO2 has significantly dropped in 2020 because of reduced usage of public/private transport and airplanes. Does this mean that the quarantine is good for our planet?


