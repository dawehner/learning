use std::num::Float;

use std::fmt;
use std::fmt::Formatter;
use std::fmt::Display;

use std::io::prelude::*;
use std::fs::File;

struct NBody {
    positions: Vec<f64>,
    momentums: Vec<f64>,
}

impl Clone for NBody {
    fn clone(&self) -> NBody {
        let nbody_new = NBody { positions: self.positions.clone(), momentums: self.momentums.clone() };
        return nbody_new;
    }
}

impl Display for NBody {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for p in self.positions.iter() {
            write!(f, "{}, ", p);
        }

        for i in 0 .. self.positions.len() {
            if i == (self.positions.len() - 1) {
              write!(f, "{}", self.positions[i]);
            }
            else {
                write!(f, "{}, ", self.positions[i]);
            }
        }
        Ok(())
    }
}


struct GravityMomentumCalc {
    masses: Vec<f64>,
}

trait MomentumCalc {
    fn apply(&self, arg: & Vec<f64>) -> Vec<f64>;
}

trait Integrator {
    fn apply(&self, x: & Vec<f64>, dx: & Vec<f64>, dt: f64) -> Vec<f64>;
}

struct<T> IntegratorEuler {
    grav: <T>,
}

impl Integrator for IntegratorEuler {
    fn apply(&self, x: & Vec<f64>, dxdt: & Vec<f64>, dt: f64) -> Vec<f64> {
        let mut x2 = x.clone();

        for i in 0 .. x.len() {
            x2[i] = x[i] + dt * dxdt[i]
        }

        return x2;
    }
}

impl MomentumCalc for GravityMomentumCalc {
   fn apply(&self, pos: & Vec<f64>) -> Vec<f64> {
        assert_eq!(pos.len() %2, 0);
        assert_eq!(pos.len() %6, 0);
        assert_eq!(pos.len() / 6, self.masses.len());

        let g = 1.0;

        let mut momentum_new = Vec::with_capacity(pos.len());

        for i in 0 .. pos.len()/2 {
            momentum_new.push(pos[pos.len()/2 + i]);
        }

        // Iterate over the particles.
        for i in 0 .. pos.len()/6 {
            for j in 0 .. pos.len()/6 {
                if i == j {
                    continue;
                }

                let mut r_2 = 0.0;
                for k in 0 .. 3 {
                    let r_d = pos[i*3 + k] - pos[j*3 + k];
                    r_2 += r_d * r_d;
                }
                let r = r_2.sqrt();
                let r_3 = r*r*r;

                for k in 0 .. 3 {
                    let momentum = -g * self.masses[j] * (pos[i*3 + k] - pos[j*3 + k]) / r_3;
                    momentum_new.push(momentum);
                }
            }

        }

        assert_eq!(momentum_new.len(), pos.len());

        return momentum_new;
    }
}

impl<F> MomentumCalc for F where F: Fn(&Vec<f64>) -> Vec<f64> {
    fn apply(&self, arg: &Vec<f64>) -> Vec<f64> { self(arg) }
}

fn advance(nbody: NBody) -> NBody {
    assert_eq!(nbody.positions.len(), nbody.momentums.len());

    let dt = 0.01;

    let mut pos_new: Vec<f64> = nbody.positions.clone();

    for i in 0 .. nbody.positions.len() {
        pos_new[i] += dt * nbody.momentums[i];
    }

    let new_nbody = NBody { positions: pos_new, momentums: nbody.momentums.clone() };

    return new_nbody;
}


fn calc_momentum(f: &MomentumCalc, nbody: NBody) -> NBody {
    let dt = 0.01;

    let mut x_dot_dot = f.apply(&nbody.positions);
    for i in 0 .. x_dot_dot.len() {
        x_dot_dot[i] = nbody.momentums[i] + dt * x_dot_dot[i];
    }

    let new_nbody = NBody { positions: nbody.positions.clone(), momentums: x_dot_dot };

    return new_nbody;
}

fn main() {
    let masses = vec![1.0, 1.0];

    let nbody = NBody {
        positions: vec![1.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        momentums: vec![0.0, 0.5, 0.0, 0.0, -0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    };

    let grav = GravityMomentumCalc { masses: masses.clone() };
    let integrator = IntegratorEuler { grav: grav };

    let mut nbody_t = nbody;

    let dt = 0.1;
    for t in 0 .. 1000 {
        nbody_t = calc_momentum(&grav, nbody_t);
        nbody_t.positions = integrator.apply(&nbody_t.positions, &nbody_t.positions, dt);
        nbody_t = advance(nbody_t);

        println!("{}", nbody_t);
    }
}
