'use strict';

var gameplay = (function() {

    var gGameplay = {
        dropTime: 1,
        onLineClear: null,
        onGameOver: null,
    };

    var ROWS = 20;
    var COLS = 10;
    var GRID_W = 0;
    var GRID_H = 0;
    //------------------------------
    function Board(width, height) {
        this.mGrids = new Array(height);
        for (var i = this.mGrids.length - 1; i >= 0; --i) {
            this.mGrids[i] = new Array(width);
        }
    }

    Board.prototype.testCollide = function(x, y) {
        if (y >= 0 && y < this.mGrids.length) {
            if (x >= 0 && x < this.mGrids[y].length) {
                return typeof this.mGrids[y][x] != "undefined";
            }
        }
        return true;
    };

    Board.prototype.fill = function(x, y, color) {
        this.mGrids[y][x] = color;
    };

    Board.prototype.clearLines = function() {
        var lines = 0;
        for (var y = this.mGrids.length - 1; y >= 0; ) {
            if (this.mGrids[y].all(function(v){return v;})) {
                ++lines;
                this.mGrids.splice(y, 1);
                this.mGrids.splice(0, 0, new Array(this.mGrids[0].length));
            } else {
                --y;
            }
        }

        return lines;
    };

    Board.prototype.getGrids = function() {
        var grids = [];
        for (var y = this.mGrids.length - 1; y >= 0; --y) {
            var row = this.mGrids[y];
            for (var x = row.length - 1; x >= 0; --x) {
                if (row[x]) grids.push({x:x, y:y, color:row[x]});
            }
        }
        return grids;
    }

    //------------------------------
    function Shape(color) {
        this.mColor = color;
        this.mFrames = [];
    }

    Shape.prototype.addFrame = function(anchorX, anchorY, matrix) {
        this.mFrames.push({anchorX: anchorX, anchorY: anchorY, matrix: matrix});
        return this;
    };

    Shape.prototype.frameCount = function() {
        return this.mFrames.length;
    };

    Shape.prototype.getFrameAnchor = function(frameID) {
        var frame = this.mFrames[frameID];
        return {x:frame.anchorX, y:frame.anchorY};
    };

    Shape.prototype.getFrameWidth = function(frameID) {
        var frame = this.mFrames[frameID];
        return frame.matrix[0].length
    };

    Shape.prototype._foreach = function(x, y, frameID, callback) {
        var frame = this.mFrames[frameID];
        var matrix = frame.matrix;
        for (var ri = matrix.length - 1; ri >= 0; --ri) {
            var row = matrix[ri];
            for (var ci = row.length - 1; ci >= 0; --ci) {
                if (row[ci] == 1) {
                    callback.call(this, x - frame.anchorX + ci, y - frame.anchorY + ri);
                }
            }
        }
    }

    Shape.prototype.testCollide = function(board, x, y, frameID) {
        var collide = false;
        this._foreach(x, y, frameID, function(x, y) {
            collide = collide || board.testCollide(x, y);
        });
        return collide;
    };

    Shape.prototype.putIntoBoard = function(board, x, y, frameID) {
        this._foreach(x, y, frameID, function(x, y) {
            board.fill(x, y, this.mColor);
        });
    };

    Shape.prototype.getGrids = function(x, y, frameID) {
        var grids = [];
        this._foreach(x, y, frameID, function(x, y) {
            grids.push({x:x, y:y, color:this.mColor});
        });
        return grids;
    }
    //------------------------------
    var SHAPES = [
        new Shape("#F20C0E")
        .addFrame(0, 0, [
                [1,1],
                [1,1]]),

        new Shape("#F8FD1A")
            .addFrame(1, 0, [
                    [1,1,1],
                    [0,1,0],])
            .addFrame(1, 1, [
                    [0,1,0],
                    [0,1,1],
                    [0,1,0], ])
            .addFrame(1, 1, [
                    [0,1,0],
                    [1,1,1],])
            .addFrame(1, 1, [
                    [0,1,],
                    [1,1,],
                    [0,1,], ]),

        new Shape("#FE6300")
            .addFrame(1, 0, [
                    [1,1,1,1],])
            .addFrame(1, 1, [
                    [0,1,],
                    [0,1,],
                    [0,1,],
                    [0,1,], ]),

        new Shape("#0201F3")
            .addFrame(1, 1, [
                    [1,0,0],
                    [1,1,1]])
            .addFrame(1, 1, [
                    [0,1,],
                    [0,1,],
                    [1,1,],])
            .addFrame(1, 1, [
                    [1,1,1],
                    [0,0,1]])
            .addFrame(1, 1, [
                    [1,1,],
                    [1,0,],
                    [1,0,],]),

        new Shape("#7003CA")
            .addFrame(1, 1, [
                    [0,0,1],
                    [1,1,1]])
            .addFrame(1, 1, [
                    [1,1,],
                    [0,1,],
                    [0,1,],])
            .addFrame(1, 1, [
                    [1,1,1],
                    [1,0,0]])
            .addFrame(1, 1, [
                    [1,0,],
                    [1,0,],
                    [1,1,],]),

        new Shape("#6BC5F7")
            .addFrame(1, 1, [
                    [1,1,0],
                    [0,1,1]])
            .addFrame(1, 1, [
                    [0,1,],
                    [1,1,],
                    [1,0,],]),

        new Shape("#0EF511")
            .addFrame(1, 1, [
                    [0,1,1],
                    [1,1,0]])
            .addFrame(1, 1, [
                    [1,0,],
                    [1,1,],
                    [0,1,],]), 
        ];

    //------------------------------
    var gBoard = null;

    var gShape = null;
    var gX = null; 
    var gY = null;
    var gFrameID = null;

    var gAccumDropTime = 0;

    function emitShape() {
        gShape = Math.randomChoice(SHAPES);
        gFrameID = 0;

        var frameWidth = gShape.getFrameWidth(gFrameID);
        var frameAnchor = gShape.getFrameAnchor(gFrameID);
        gX = frameAnchor.x + Math.floor((COLS - frameWidth) / 2);
        gY = frameAnchor.y;

        gAccumDropTime = 0;

        if (gShape.testCollide(gBoard, gX, gY, gFrameID)) {
            if (typeof gGameplay.onGameOver == 'function') {
                gGameplay.onGameOver();
            } 
        }
    }

    function onSetup() {
        GRID_W = framework.width / COLS;
        GRID_H = framework.height / ROWS;
    }

    function onStart() {
        gBoard = new Board(COLS, ROWS);

        emitShape();
    }

    function onUpdate(dt) {
        gAccumDropTime += dt;

        if (gAccumDropTime >= gGameplay.dropTime) {
            gAccumDropTime -= gGameplay.dropTime;

            if (gShape.testCollide(gBoard, gX, gY + 1, gFrameID)) {
                gShape.putIntoBoard(gBoard, gX, gY, gFrameID);

                
                var lines = gBoard.clearLines();
                if (lines > 0) {
                    if (typeof gGameplay.onLineClear == 'function') {
                        gGameplay.onLineClear(lines);
                    }
                }

                emitShape();
            } else {
                ++gY;
            }

            framework.refresh = true;
        }
    }

    function onRender(ctx) {
        // background
        var gradient = ctx.createLinearGradient(0, 0, this.width, this.height);
        gradient.addColorStop(0,"rgba(128,128,128, 1)");
        gradient.addColorStop(1,"rgba(96,96,96, 1)");
        ctx.fillStyle = gradient;
        ctx.fillRect(0, 0, this.width, this.height);

        // grids
        var grids = gBoard.getGrids();
        grids = grids.concat(gShape.getGrids(gX, gY, gFrameID));

        grids.sort(function(a, b) {
            return (a.y * ROWS + a.x) - (b.y * ROWS + b.x);
        });

        ctx.shadowBlur = 16;
        ctx.shadowColor = "black";
        ctx.lineWidth = 1;
        ctx.lineJoin = 'round';
        ctx.strokeStyle = '#FFFFFF';

        for (var i = grids.length - 1; i >= 0; --i) {
            var x = grids[i].x * GRID_W, y = grids[i].y * GRID_H;
            var color = grids[i].color;

            var midX = x + GRID_W / 2;
            var midY = y + GRID_H / 2;

            var gradient = ctx.createRadialGradient(midX, midY, 0, midX, midY, 60);
            gradient.addColorStop(0, color);
            gradient.addColorStop(1, "white");
            ctx.fillStyle = gradient;
            ctx.fillRect(x, y, GRID_W, GRID_H);

            ctx.strokeRect(x, y, GRID_W, GRID_H);
        }
    }

    function onKeyPress(keyCode) {
        switch (keyCode) {
            case null:
                break;
            case framework.KEYCODE.up:
                {
                    var nextFrameID = (gFrameID + 1) % gShape.frameCount();
                    if (!gShape.testCollide(gBoard, gX, gY, nextFrameID)) {
                        gFrameID = nextFrameID;
                    } 
                }
                break;
            case framework.KEYCODE.down:
                if (!gShape.testCollide(gBoard, gX, gY + 1, gFrameID)) {
                    ++gY;
                }
                break;
            case framework.KEYCODE.left:
                if (!gShape.testCollide(gBoard, gX - 1, gY, gFrameID)) {
                    --gX;
                }
                break;
            case framework.KEYCODE.right:
                if (!gShape.testCollide(gBoard, gX + 1, gY, gFrameID)) {
                    ++gX;
                }
                break;
            case framework.KEYCODE.space:
                while (!gShape.testCollide(gBoard, gX, gY + 1, gFrameID)) {
                    ++gY;
                }
                break;
            default:
                break;
        }

        framework.refresh = true;
    }

    //------------------------------
    framework.onSetup = onSetup;
    framework.onStart = onStart;
    framework.onUpdate = onUpdate;
    framework.onRender = onRender;
    framework.onKeyPress = onKeyPress;

    return gGameplay;

})();
