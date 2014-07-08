'use strict';

var fs = require('fs');
var path = require('path');
var url = require('url');
var http = require('http');
var util = require('util');

var jsdom = require('jsdom');
//------------------------------

function downloadUrl(dir, fileUrl) {
    if (!fs.existsSync(dir)) fs.mkdirSync(dir);

    http.get(fileUrl, function(res) {

        var data = '';
        res.on('data', function(chunk) {
            data += chunk;
        });
        res.on('end', function() {
            var localPath = path.join(dir, path.basename(url.parse(fileUrl).pathname));
            console.log(util.format('@ [%s] downloaded: %s => %s', new Date().toLocaleTimeString(), fileUrl, localPath));

            fs.writeFileSync(localPath, data);
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
            var urls = {};
            for (var i = 0; i < suffix.length; ++i) {
                var elems = window.$(util.format('[src$="%s"]', suffix[i]));
                for (var j = 0; j < elems.length; ++j) urls[elems[j].src] = true;
            }

            for (var url in urls) {
                downloadUrl('files', url);
            }
            console.log('@ finish...');
        }
        );
