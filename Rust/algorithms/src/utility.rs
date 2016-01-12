
use super::time;

pub trait Concat<T> {
    fn concat(&self, rhs : &[T]) -> Self;
}

impl<T : Clone> Concat<T> for Vec<T> {
    fn concat(&self, rhs : &[T]) -> Self {
        let mut vec = self.clone();
        for v in rhs {
            vec.push(v.clone());
        }
        vec
    }
}

pub fn time_it<F : FnMut()>(times : i32, mut f : F) -> f64 {
    if times > 1 { f() }

    let start = time::precise_time_ns();
    for _ in 0..times {
        f();
    }
    (time::precise_time_ns() - start) as f64 / times as f64 / 1000_000_000.0
}
