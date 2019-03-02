#!/usr/bin/env node

const fs = require('fs');
const https = require("https");
const outputDir= "./output";
let total = 0;
let done = 0;

function fetch(url,filename){
    console.log(`Fetching: ${url}`);
    let urlSplit= url.split("/");
    let urlShort = urlSplit[urlSplit.length - 1];
    let filesize = 0;
    let dlsize = 0;
    let writeStream = fs.createWriteStream(filename);
    let req = https.get(url, (res) => {
        if(res.statusCode!=200){
            console.log("ERR: status code ", res.statusCode);
            return;
        }

        res.on('data', (data) => {
            writeStream.write(data);
            dlsize += data.length;
            console.log(`${urlShort}: Fetched ${dlsize} of ${filesize} - ${
                (dlsize / filesize * 100).toFixed(2)
            }%`);
        });

        res.on('end', function() {
            console.log(`Finished fetching ${filename}`);
            done += 1;
            console.log(`Completed ${done}/${total}`);
        });
    });

    req.on('error', (e) => {
        console.error(`ERR: Could not fetch ${url}`);
        console.error(e);
    });

    req.on('response', function (data) {
        filesize = data.headers['content-length'];
    });
}

function createDir(dirName){
    fs.existsSync(dirName) || fs.mkdirSync(dirName);
}

function scrapeRange(prefix,first,last){
    createDir(outputDir);
    let dirPath =`${outputDir}/${prefix}`;
    createDir(dirPath);
    total = last - first;
    for(let i = first;i<last;i++){
        let fileName = `${prefix}_${i}.zip`;
        let fullpath = `${dirPath}/${fileName}`;
        fetch(`https://aqs.epa.gov/aqsweb/airdata/${fileName}`,fullpath);
    }
}

scrapeRange("hourly_WIND",1990,2018);
