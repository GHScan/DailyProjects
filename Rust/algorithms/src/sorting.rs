
use std::cmp::Ord;
use std::cmp::Ordering;

pub fn bubble_sort<T : Ord>(slice : &mut [T]) {
    for i in (1..slice.len()).rev() {
        for j in 1..i + 1 {
            match slice[j].cmp(&slice[j - 1]) {
                Ordering::Less => slice.swap(j - 1, j),
                _ => ()
            }
        }
    }
}

pub fn selection_sort<T : Ord>(slice : &mut[T]) {
    if slice.len() < 2 { return }

    for i in 0..slice.len() - 1 {
        for j in i + 1..slice.len() {
            match slice[j].cmp(&slice[i]) {
                Ordering::Less => slice.swap(i, j),
                _ => (),
            }
        }
    }
}

pub fn insertion_sort<T : Ord>(slice : &mut[T]) {
    for i in 1..slice.len() {
        for j in (1..i + 1).rev() {
            match slice[j].cmp(&slice[j - 1]) {
                Ordering::Less => slice.swap(j, j - 1),
                _ => break,
            }
        }
    }
}

pub fn shell_sort<T : Ord>(slice : &mut[T]) {
    let mut span = 1;
    while span < slice.len() { span *= 3 }

    loop {
        span /= 3;
        if span == 0 { break }

        for i in span..slice.len() {
            let mut j = i;

            while j >= span {
                match slice[j].cmp(&slice[j - span]) {
                    Ordering::Less => { 
                        slice.swap(j, j - span);
                        j -= span;
                    },
                    _ => break,
                }
            }
        }
    }
}

pub fn quick_sort<T : Ord>(slice : &mut[T]) {
    let len = slice.len();
    if len < 2 { return }

    let mut p = 0;
    for i in 1..len {
        match slice[i].cmp(&slice[0]) {
            Ordering::Less | Ordering::Equal => {
                p += 1;
                slice.swap(i, p);
            },
            _ => (),
        }
    }
    slice.swap(p, 0);

    quick_sort(&mut slice[0..p]);
    quick_sort(&mut slice[p + 1..len]);
}

pub fn quick_sort_2<T : Ord>(slice : &mut[T]) {
    let len = slice.len();
    if len < 2 { return }

    slice.swap(0, len / 2);

    let mut p = 0;
    for i in 1..len {
        match slice[i].cmp(&slice[0]) {
            Ordering::Less | Ordering::Equal => {
                p += 1;
                slice.swap(i, p);
            },
            _ => (),
        }
    }
    slice.swap(p, 0);

    quick_sort(&mut slice[0..p]);
    quick_sort(&mut slice[p + 1..len]);
}

pub mod benchs {
    use super::*;
    use std::cmp::Ord;
    use rand;
    use rand::Rng;
    use super::super::utility::Concat;
    use super::super::utility::time_it;

    pub fn get_sorting_funcs<T : Ord + 'static>() -> Vec<(&'static str, Box<Fn(&mut [T])>, usize)> {
        vec! [
            ("bubble_sort", Box::new(bubble_sort), 4 * 1024),
            ("selection_sort", Box::new(selection_sort), 4 * 1024),
            ("insertion_sort", Box::new(insertion_sort), 4 * 1024),
            ("shell_sort", Box::new(shell_sort), 1024 * 1024),
            ("quick_sort", Box::new(quick_sort), 32 * 1024),
            ("quick_sort_2", Box::new(quick_sort), 32 * 1024),
        ]
    }

    fn gen_rand_vec(len : usize) -> Vec<i32> {
        let mut rng = rand::thread_rng();
        let mut vec = Vec::with_capacity(len);
        for _ in 0..len {
            vec.push(rng.gen_range(0, len as i32));
        }
        vec
    }

    pub fn bench_sorting_funcs() {
        let vecs = [
            ("1k_rand",  gen_rand_vec(1024)),
            ("1k_ordered", (0..1024).collect()),
            ("1k_halfdup", gen_rand_vec(512).concat(&[256;512])),
            ("1k_fulldup", vec![256;1024]),
            ("2k_rand",  gen_rand_vec(2 * 1024)),
            ("2k_ordered", (0..2 * 1024).collect()),
            ("2k_halfdup", gen_rand_vec(1024).concat(&[256;1024])),
            ("2k_fulldup", vec![256;1024]),
            ("16k_rand",  gen_rand_vec(16 * 1024)),
            ("16k_ordered", (0..16 * 1024).collect()),
            ("16k_halfdup", gen_rand_vec(8 * 1024).concat(&[256;8 * 1024])),
            ("16k_fulldup", vec![256;16 * 1024]),
            ("32k_rand",  gen_rand_vec(32 * 1024)),
            ("32k_ordered", (0..32 * 1024).collect()),
            ("32k_halfdup", gen_rand_vec(16 * 1024).concat(&[256;16 * 1024])),
            ("32k_fulldup", vec![256;32 * 1024]),
            ("1M_rand",  gen_rand_vec(1024 * 1024)),
            ("1M_ordered", (0..1024 * 1024).collect()),
            ("1M_halfdup", gen_rand_vec(512 * 1024).concat(&[256;512 * 1024])),
            ("1M_fulldup", vec![256;1024 * 1024]),
        ];
        for &(name, ref vec) in &vecs {
            println!("{}:", name);
            let baseline_elapse = time_it(3, ||{vec.clone();});

            for (name, func, limit) in get_sorting_funcs() {
                if limit < vec.len() { continue }

                let elapse = time_it(3, ||{func(&mut vec.clone());}) - baseline_elapse;
                println!("\t{:32}: {:.6}s", name, elapse);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::benchs::get_sorting_funcs;

    #[test]
    fn test_sorting_funcs() {
        for (_, func, _) in get_sorting_funcs() {
            let vecs = [
                vec![],
                vec![1],
                vec![1, 0],
                vec![1, 0, 2],
                vec![3, 1, 0, 2],
                vec![5, 3, 0, 0, 1],
                vec![0, 8, 3, 2, 3, 4],
                vec![0, 8, 5, 3, 2, 3, 4],
                vec![0, 8, 3, 9, 4, 2, 3, 4],
            ];
            for vec in &vecs {
                let mut vec1 = vec.clone();
                let mut vec2 = vec.clone();
                vec1.sort();
                func(&mut vec2);
                assert_eq!(vec1, vec2);
            }
        }
    }
}
