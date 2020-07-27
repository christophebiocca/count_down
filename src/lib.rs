pub mod count_down {
    use itertools::{Either, Itertools};
    use std::fmt;
    use std::rc::Rc;
    use std::iter;

    type Int = i64;

    #[derive(Copy, Clone)]
    pub enum Op {
        Add,
        Sub,
        Mul,
        Div,
    }

    #[derive(Clone)]
    pub enum Expr {
        Val(Int),
        App(Op, Rc<Expr>, Rc<Expr>),
    }

    use Expr::*;
    use Op::*;

    fn valid(op: Op, x: Int, y: Int) -> bool {
        match op {
            Add => x <= y,
            Sub => x > y,
            Mul => x != 1 && y != 1 && x <= y,
            Div => y > 1 && ((x % y) == 0),
        }
    }

    fn apply(op: Op, a: Int, b: Int) -> Int {
        match op {
            Add => a + b,
            Sub => a - b,
            Mul => a * b,
            Div => a / b,
        }
    }

    fn split<T>(xs: &[T]) -> impl Iterator<Item=(&[T], &[T])> {
        (1..xs.len())
            .map(move |i| xs.split_at(i))
    }

    fn sub_bags<T: Clone>(xs: Vec<T>) -> impl Iterator<Item=Vec<T>> {
        (0..xs.len() + 1)
            .flat_map(move |i| xs.clone().into_iter().permutations(i))
    }

    type Result = (Rc<Expr>, Int);

    fn combine((l, x): Result, (r, y): Result) -> impl Iterator<Item=Result> {
        [Add, Sub, Mul, Div].iter()
            .filter(move |&&op| valid(op, x, y))
            .map(move |&op|
                (Rc::new(App(op, l.clone(), r.clone())),
                 apply(op, x, y)))
    }

    fn results<'a>(ns: &'a [Int]) -> impl Iterator<Item=Result> +'a {
        match ns {
            &[] => Either::Left(Either::Left(iter::empty())),
            &[n] => Either::Left(Either::Right(iter::once((Rc::new(Val(n)), n)))),
            _ => Either::Right(Box::new(_results(ns)) as Box<dyn Iterator<Item=Result>>),
        }
    }

    fn _results<'a>(ns: &'a [Int]) -> impl Iterator<Item=Result> + 'a {
        split(ns)
            .flat_map(|(ls, rs)| results(ls)
                .flat_map(move |lx| {
                     iter::repeat(lx).zip(results(rs)).flat_map(|(lx, ry)| combine(lx, ry))
                 }))
    }

    pub fn solutions(ns: Vec<Int>, n: Int) -> Vec<Expr> {
        sub_bags(ns)
            .flat_map(|bag|
                results(&bag).into_iter()
                    .filter(|(_, m)| *m == n)
                    .map(|(e, _)| Rc::try_unwrap(e).unwrap_or_else(|rc| rc.as_ref().clone()))
                    .collect::<Vec<_>>()
            )
            .collect()
    }

    // Utilities for displaying expressions
    fn get_str(expr: &Expr) -> String {
        match &*expr {
            Val(v) => v.to_string(),
            App(op, l, r) => format!("({} {:?} {})", get_str(&l), op, get_str(&r)),
        }
    }

    impl fmt::Debug for Op {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Add => write!(f, "+"),
                Sub => write!(f, "-"),
                Mul => write!(f, "*"),
                Div => write!(f, "/"),
            }
        }
    }

    impl fmt::Debug for Expr {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", get_str(self))
        }
    }
}
