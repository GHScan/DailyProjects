"use strict";

function Point(x, y) {
    this.x = x;
    this.y = y;
}
//-------------------------------------------------------------------------
function randomItem(barrier) {
    var item;
    do {
        item = new Point(Math.randomInt(0, ROWS), Math.randomInt(0, COLS));
    } while (barrier.findIf(function (v) { return v.equals(item) }) != -1);
    return item;
}

function calcItemScore(item, food, barrier) {
    if (item.x < 0 || item.x >= ROWS) return 0;
    if (item.y < 0 || item.y >= COLS) return 0;

    if (barrier.findIf(function (v) { return v.equals(item); }) != -1) return 0;

    return 1 /  (1 + (Math.abs(item.x - food.x) + Math.abs(item.y - food.y)));
}

function findNextItem(food, snake) {
    var head = snake[snake.length - 1];

    var items = [new Point(head.x, head.y + 1), new Point(head.x, head.y - 1), new Point(head.x + 1, head.y), new Point(head.x - 1, head.y)];
    items = items.map(function (item) { return { item: item, score: calcItemScore(item, food, snake) }; });

    var item = items.reduce(function (a, b) { return a.score > b.score ? a : b });
    return item.score > 0 ? item.item : null;
}

//-------------------------------------------------------------------------
var GRID_SIZE = 10;
var ROWS = 0;
var COLS = 0;

var gSnake = null;
var gFood = null;

function onStart() {
    ROWS = WIDTH / GRID_SIZE;
    COLS = HEIGHT / GRID_SIZE;

    gSnake = [new Point(ROWS / 2, COLS / 2)];
    gFood = randomItem(gSnake);
}

function onUpdate(dt) {
    var head = gSnake[gSnake.length - 1];

    var item = findNextItem(gFood, gSnake);
    if (item == null) {
        gSnake = [new Point(ROWS / 2, COLS / 2)];
        gFood = randomItem(gSnake);
    } else if (item.equals(gFood)) {
        gSnake.push(gFood);
        gFood = randomItem(gSnake);
    } else {
        gSnake.push(item);
        gSnake.splice(0, 1);
    }
}

function onRender(ctx) {
    ctx.strokeStyle = "rgba(0,0,0,1)";
    ctx.lineWidth = 0.5;
    ctx.lineJoin = "round";

    ctx.fillStyle = "rgba(234,83,71,1)";
    ctx.fillRect(gFood.x * GRID_SIZE + 1, gFood.y * GRID_SIZE + 1, GRID_SIZE - 2, GRID_SIZE - 2);
    ctx.strokeRect(gFood.x * GRID_SIZE, gFood.y * GRID_SIZE, GRID_SIZE, GRID_SIZE);

    ctx.fillStyle = "rgba(104,33,122,1)";
    for (var i in gSnake) {
        var item = gSnake[i];
        ctx.fillRect(item.x * GRID_SIZE + 1, item.y * GRID_SIZE + 1, GRID_SIZE - 2, GRID_SIZE - 2);
        ctx.strokeRect(item.x * GRID_SIZE, item.y * GRID_SIZE, GRID_SIZE, GRID_SIZE);
    }
}

//-------------------------------------------------------------------------
var FPS = 30;
var WIDTH = 0;
var HEIGHT = 0;

//-------------------------------------------------------------------------
var gTimeID = null;
var gLastDate = null;
var gCanvas = null;

function installTimer() {
    gLastDate = new Date();
    gTimeID = setTimeout(function () {

        var dt = new Date() - gLastDate;

        onUpdate(dt);

        var ctx = gCanvas.getContext("2d");
        ctx.clearRect(0, 0, WIDTH, HEIGHT);

        ctx.strokeStype = "rgba(0,0,0,1)";
        ctx.strokeRect(0, 0, WIDTH, HEIGHT);

        onRender(ctx);

        installTimer();

    }, 1000 / FPS);
}

function start(canvas, fps) {
    clearTimeout(gTimeID);
    gTimeID = null;

    FPS = fps;

    gCanvas = canvas;
    WIDTH = gCanvas.width;
    HEIGHT = gCanvas.height;

    onStart();

    installTimer();
}

function pause() {
    if (gTimeID != null) {
        clearTimeout(gTimeID);
        gTimeID = null;
    } else {
        installTimer();
    }
}
