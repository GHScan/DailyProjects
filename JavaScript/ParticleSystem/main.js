//------------------------------
function Vector2(x, y) {
    this.x = x;
    this.y = y;
}

Vector2.prototype.add = function(o) { return new Vector2(this.x + o.x, this.y + o.y); }
Vector2.prototype.sub = function(o) { return new Vector2(this.x - o.x, this.y - o.y); }
Vector2.prototype.mul = function(o) { return new Vector2(this.x * o.x, this.y * o.y); }
Vector2.prototype.scale = function(s) { return new Vector2(this.x * s, this.y * s); }
Vector2.prototype.dot = function(o) { return this.x * o.x + this.y * o.y; }
Vector2.prototype.clone = function(o) { return new Vector2(this.x, this.y); }

function Color(r, g, b, a) {
    this.r = r;
    this.g = g;
    this.b = b;
    this.a = a;
}

Color.prototype.toRGBAString = function() {
    return "rgba(" + this.r + "," + this.g + "," + this.b + "," + this.a + ")"
}

//------------------------------
function Particle(life, size, position, velocity, color) {
    this.age = 0;
    this.size = size;
    this.life = life;
    this.position = position;
    this.velocity = velocity;
    this.color = color;
}

function ParticleSystem() {
    this.mParticles = [];
    this.mEffector = [];
}

ParticleSystem.prototype.update = function(dt) {
    var effectors = this.mEffector;
    var particles = this.mParticles;

    for (var ei in effectors) {
        var effector = effectors[ei];
        for (var pi in particles) {
            effector(particles[pi], dt);
        }
    }

    for (var i in particles) {
        var p = particles[i];
        p.position = p.position.add(p.velocity.scale(dt));
    }
}

ParticleSystem.prototype.grow = function(dt) {

    var particles = this.mParticles;
    for (var i = 0; i < particles.length; ) {
        particles[i].age += dt;

        if (particles[i].age < particles[i].life) {
            ++i
        } else {
            particles[i] = particles[particles.length - 1];
            particles.pop();
        }
    }
}

ParticleSystem.prototype.render = function(ctx) {
    ctx.strokeStype = "#000000";

    var particles = this.mParticles;
    for (var i in particles) {
        var p = particles[i];

        var size = p.size * (1 - p.age / p.life);
        p.color.a = 1 - p.age / p.life;

        ctx.fillStyle = p.color.toRGBAString();
        ctx.beginPath();
        ctx.arc(p.position.x, p.position.y, p.size, 0, 2 * Math.PI);
        ctx.closePath();
        ctx.fill();
    }
}

ParticleSystem.prototype.addEffector = function(effector) {
    this.mEffector.push(effector);
}

ParticleSystem.prototype.add = function(particle) {
    this.mParticles.push(particle);
}
//------------------------------
function VelocityEffector_Force(a) {
    return function(p, dt) {
        p.velocity = p.velocity.add(a.scale(dt));
    };
}

function VelocityEffector_BoxClamper(x, y, width, height) {
    return function(p, dt) {
        if (p.position.x < x || p.position.x > x + width) p.velocity.x = -p.velocity.x / 2;
        if (p.position.y < y || p.position.y > y + height) p.velocity.y = -p.velocity.y / 2;
    };
}

//------------------------------
var EMIT_PER_SEC = 1;
var LIFE_SCALE = 1;
var SIZE_SCALE = 1;
var VELOCITY_SCALE = 1;

var gPS = null;
var gColors = [new Color(117,168,237,1), new Color(255,114,58,1), new Color(204,0,0,1), new Color(89,214,14,1)];
var gMousePos = new Vector2(0, 0);
var gLastMousePos = new Vector2(0, 0);
var gMouseVelocity = new Vector2(0, 0);

function emitParticle() {
    var angle = Math.randomNumber(0, 2 * Math.PI);
    var life = Math.randomNumber(2, 4) * LIFE_SCALE; 
    var size = Math.randomNumber(3, 5) * SIZE_SCALE;
    var velocity = new Vector2(Math.cos(angle), Math.sin(angle)).scale(75 * VELOCITY_SCALE);

    var p = new Particle(
            life,
            size,
            gMousePos,
            velocity.add(gMouseVelocity), 
            Math.randomChoice(gColors));

    gPS.add(p);
}

function onStart() {
    gPS = new ParticleSystem();
    gPS.addEffector(VelocityEffector_Force(new Vector2(0, 100)));
    gPS.addEffector(VelocityEffector_BoxClamper(0, 0, WIDTH, HEIGHT));
}

function onUpdate(dt) {
    gMouseVelocity = gMousePos.sub(gLastMousePos).scale(1 / dt);
    gMouseVelocity = gMouseVelocity.scale(0.1);
    gLastMousePos = gMousePos;

    for (var i = 0; i < EMIT_PER_SEC; ++i) {
        emitParticle();
    }

    gPS.update(dt);
    gPS.grow(dt);
}

function onRender(ctx) {
    gPS.render(ctx);
}

function onMouseMove(pos) {
    gMousePos = pos;
}

//------------------------------
var FPS = 0;
var WIDTH = 0;
var HEIGHT = 0;

var gCanvas = null;
var gTimerID = null;
var gLastDate = null;

function installTimer() {
    clearTimeout(gTimerID);

    gLastDate = new Date();
    gTimerID = setTimeout(function() {

        var ctx = gCanvas.getContext("2d");
        ctx.fillStyle = "rgba(0, 0, 0, 0.1)";
        ctx.fillRect(0, 0, WIDTH, HEIGHT);

        onUpdate((new Date() - gLastDate) / 1000.0);
        onRender(ctx);

        installTimer();

    }, 1000 / FPS);
}

function start(canvas) {
    window.onerror = function(message) {
        alert(message);
    }

    WIDTH = canvas.width;
    HEIGHT = canvas.height;

    gCanvas = canvas;

    gCanvas.onmousemove = function(event) {
        onMouseMove(new Vector2(event.x, event.y));
    };

    onStart();

    installTimer();
}
