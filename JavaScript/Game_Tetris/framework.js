'use strict';

var framework = (function() {
    var gFramework = {};

    var gTimerID = null;

    function installTimer() {
        clearTimeout(gTimerID);

        var lastDate = new Date();
        gTimerID = setTimeout(function() {

            if (typeof this.onUpdate == 'function') {
                this.onUpdate((new Date() - lastDate) / 1000);
            }

            if (typeof this.onRender == 'function') {
                this.onRender(this.canvas.getContext('2d'));
            }

            installTimer();

        }.bind(gFramework), 1000 / gFramework.fps);
    };

    gFramework.setup = function(canvas) {
        this.canvas = canvas;
        this.width = canvas.width;
        this.height = canvas.height;

        var keyEventTimerID = null;
        document.onkeydown = function(event) {
            clearInterval(keyEventTimerID);

            var triggerEvent = function() {
                if (typeof gFramework.onKeyPress == 'function') {
                    gFramework.onKeyPress(event.keyCode);
                }
            };

            keyEventTimerID = setInterval(triggerEvent, 1000 * gFramework.keyPressTime);
            triggerEvent();
        };
        document.onkeyup = function(event) {
            clearInterval(keyEventTimerID);
            keyEventTimerID = null;
        };

        if (typeof this.onSetup == 'function') {
            this.onSetup();
        }
    };

    gFramework.start = function() {
        if (typeof this.onStart == 'function') {
            this.onStart();
        }

        installTimer();
    };

    gFramework.pause = function() {
        if (gTimerID == null) {
            installTimer();
        } else {
            clearTimeout(gTimerID);
            gTimerID = null;
        }
    };

    gFramework.fps = 60;
    gFramework.canvas = null;
    gFramework.width = 0;
    gFramework.height = 0;
    gFramework.keyPressTime = 0.20;
    gFramework.onSetup = null;
    gFramework.onStart = null;
    gFramework.onKeyPress = null;
    gFramework.onUpdate = null;
    gFramework.onRender = null;

    gFramework.KEYCODE = {
        up:38, down: 40, left: 37, right: 39, space: 32,
    };

    return gFramework;
})();
