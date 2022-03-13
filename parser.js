const fs = require("fs");

/*
function getYear(strDate) {
    return parseInt(strDate.split("/")[2]);
}

let rawdata = fs.readFileSync('allData.json');
let allData = JSON.parse(rawdata);
allData.pop()

let zero_stations = [];

for (const entry of allData) {
    if(parseInt(entry.rides) === 0) {
        zero_stations.push(entry);
    }
}

console.log(zero_stations.length);
*/
/*
let separatedData = {};
let allYears = [];

console.log(allData.length);

// read all years
for(entry of allData) {
    // allYears.push(parseInt(entry.date.split("/")[2]))
    allYears.push(parseInt(entry.date.split("/")[2]));
}

allYears = new Set(allYears);

// separate by years
for(year of allYears) {
    let currYearData = [];
    for(entry of allData) {
        getYear(entry.date) === year && currYearData.push(entry);
    }

    separatedData[year] = currYearData;
}

// scan for zero rides



// // write to files
// for (year of Object.keys(separatedData)) {
//     let currYearStr = ""
//     // get columns to be first
//     currYearStr += Object.keys(entry).join('\t').concat("\n")
//     for(entry of separatedData[year]) {
//         // convert to tsv format
//         const raw = Object.values(entry).join('\t').concat("\n");
//         currYearStr += raw;
//     }
//     // once whole year is done
//     // write to sep file
//     fs.writeFileSync(`data_${year}.tsv`, currYearStr);
// }
*/

const csv = require("csv-parser");
let allData = [];
let locations = [];
let cnt = 0;

function ReadAllData() {
  return new Promise((resolve, reject) => {
    fs.createReadStream("data.csv")
      .pipe(csv())
      .on("data", function (row) {
        allData.push(row);
        if (parseInt(row.rides) === 0) cnt++;
      })
      .on("end", function () {
        // console.log(allData, cnt);
        resolve();
      })
      .on("error", reject);
  });
}

function ReadLocationData() {
  return new Promise((resolve, reject) => {
    fs.createReadStream("locations.csv")
      .pipe(csv())
      .on("data", function (row) {
        locations.push(row);
      })
      .on("end", function () {
        // console.log(locations);
        resolve();
      })
      .on("error", reject);
  });
}

function getLocation(stationName) {
  let found = locations.find((el) => el.STATION_NAME == stationName);
  return found !== undefined ? found.Location : null;
}

(async function () {
  // Read all data
  await ReadAllData();
  // read all location
  await ReadLocationData();

  // join locations
  for (let i = 0; i < allData.length; i++) {
    allData[i]["location"] = getLocation(allData[i].stationname);
  }

  let noLocations = [];
  for (entry of allData) {
    if (entry.location == null) noLocations.push(entry.stationname);
  }

  console.log(new Set(noLocations).size);


})();
