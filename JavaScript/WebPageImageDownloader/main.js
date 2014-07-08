'use strict';

var fs = require('fs');
var path = require('path');
var url = require('url');
var http = require('http');
var util = require('util');

var jsdom = require('jsdom');
//------------------------------
function downloadUrl(dir, fileUrl, id) {
    if (!fs.existsSync(dir)) fs.mkdirSync(dir);

    http.get(fileUrl, function(res) {

        var chunks = [];
        res.on('data', function(chunk) {
            chunks.push(chunk);
        });
        res.on('end', function() {
            var localPath = path.join(dir, id + '_' + path.basename(url.parse(fileUrl).pathname));
            console.log(util.format('@ [%s] downloaded: %s => %s', new Date().toLocaleTimeString(), fileUrl, localPath));

            fs.writeFileSync(localPath, Buffer.concat(chunks), 'binary');
        });

    }).on('error', function(e) {
        console.error(e);
    });
}

jsdom.env(
        process.argv[2],
        ['http://code.jquery.com/jquery-1.11.1.min.js'],
        function (errors, window) {

            var suffix = ['.png', '.jpg'];
            var url2Id = {};
            var id = 0;
            for (var i = 0; i < suffix.length; ++i) {
                var elems = window.$(util.format('[src$="%s"]', suffix[i]));
                for (var j = 0; j < elems.length; ++j) {
                    if (!url2Id.hasOwnProperty(elems[j].src)) {
                        url2Id[elems[j].src] = id++;
                    }
                }
            }

            var urlArray = [];
            for (var url in url2Id) {
                urlArray.push({url:url, id:url2Id[url]});
            }
            urlArray.sort(function(a, b){ return a.id - b.id; });

            for (var i in urlArray) {
                downloadUrl('files', urlArray[i].url, urlArray[i].id);
            }
            console.log('@ finish...');
        }
        );
