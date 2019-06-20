var system;
var startTime = Date.now();

function setup() {
  createCanvas(720, 400);
  system = new ParticleSystem(createVector(width/2, 50));
}

function draw() {
  background(51);

  const time = Date.now() - startTime;

  system.addParticle(
    createVector(
      width/2 + 100 * Math.cos(2 * time),
      height/2 + 100 * Math.sin(3 * time)
    )
  );
  system.run();
}

// A simple Particle class
class Particle {
  constructor(position) {
    this.acceleration = createVector(0, 0.05);
    this.velocity = createVector(random(-1, 1), random(-1, 0));
    this.position = position.copy();
    this.lifespan = 255.0;

    const colors = [
      [255,255,178],
      [254,217,118],
      [254,178,76],
      [253,141,60],
      [240,59,32],
      [189,0,38],
    ];
    const index = Math.floor(Math.random() * colors.length);
    this.color = colors[index];
  }

  run() {
    this.update();
    this.display();
  }

  update() {
    this.velocity.add(this.acceleration);
    this.position.add(this.velocity);
    this.lifespan -= 2;
  }

  display() {
    stroke(200, this.lifespan);
    strokeWeight(2);
    fill(this.color[0], this.color[1], this.color[2]);
    ellipse(this.position.x, this.position.y, 12, 12);
  }

  isDead() {
    if (this.lifespan < 0) {
      return true;
    } else {
      return false;
    }
  }
}

class ParticleSystem {
  constructor(position) {
    this.origin = position.copy();
    this.particles = [];
  }

  addParticle(origin) {
    this.particles.push(new Particle(origin || this.origin));
  }

  run() {
    for (var i = this.particles.length-1; i >= 0; i--) {
      var p = this.particles[i];
      p.run();
      if (p.isDead()) {
        this.particles.splice(i, 1);
      }
    }
  }

}
