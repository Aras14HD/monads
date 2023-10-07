trait Run<W> {
    type Wrapper<T>;
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, f: F) -> Self::Wrapper<T>;
}

trait RunTrivial<W>: Run<W> {
    fn run_triv<T, F: FnOnce(W) -> T>(self, f: F) -> Self::Wrapper<T>;
    fn run_inner<T, U, F: FnOnce(U) -> W::Wrapper<T>>(self, f: F) -> Self::Wrapper<W::Wrapper<T>>
    where
        W: Run<U>,
        Self: Sized,
    {
        self.run_triv(|x| x.run(f))
    }
}

impl<W> Run<W> for Option<W> {
    type Wrapper<T> = Option<T>;
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, f: F) -> Option<T> {
        match self {
            None => None,
            Some(x) => f(x),
        }
    }
}

impl<W> RunTrivial<W> for Option<W> {
    fn run_triv<T, F: FnOnce(W) -> T>(self, f: F) -> Self::Wrapper<T> {
        self.run(|x| Some(f(x)))
    }
}

impl<W, E> Run<W> for Result<W, E> {
    type Wrapper<T> = Result<T, E>;
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, f: F) -> Result<T, E> {
        match self {
            Err(e) => Err(e),
            Ok(x) => f(x),
        }
    }
}

impl<W, E> RunTrivial<W> for Result<W, E> {
    fn run_triv<T, F: FnOnce(W) -> T>(self, f: F) -> Self::Wrapper<T> {
        self.run(|x| Ok(f(x)))
    }
}

#[derive(Debug)]
struct WithLog<T> {
    v: T,
    log: String,
}

impl<T> WithLog<T> {
    fn new(v: T) -> Self {
        Self {
            v,
            log: "".to_owned(),
        }
    }
}

impl<W> Run<W> for WithLog<W> {
    type Wrapper<T> = WithLog<T>;
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, f: F) -> Self::Wrapper<T> {
        let res = f(self.v);
        WithLog {
            v: res.v,
            log: self.log + &res.log,
        }
    }
}

fn maybe<T>(x: T) -> Option<T> {
    if rand::random() {
        Some(x)
    } else {
        None
    }
}

fn incr_log(x: i32) -> WithLog<i32> {
    WithLog {
        v: x + 1,
        log: format!("Incremented {:?} ", x),
    }
}

fn run_both<
    T,
    X,
    Y,
    A: Run<X>,
    B: Run<Y, Wrapper<T> = A::Wrapper<T>>,
    F: Fn(X, Y) -> B::Wrapper<T>,
>(
    a: A,
    b: B,
    f: F,
) -> A::Wrapper<T> {
    a.run(|x| b.run::<T, _>(|y| f(x, y)))
}

fn run_both_triv<
    T,
    X,
    Y,
    A: Run<X>,
    B: Run<Y, Wrapper<T> = A::Wrapper<T>> + RunTrivial<Y>,
    F: Fn(X, Y) -> T,
>(
    a: A,
    b: B,
    f: F,
) -> A::Wrapper<T> {
    a.run(|x| b.run_triv::<T, _>(|y| f(x, y)))
}

fn main() {
    let x = maybe(5).run(maybe);
    let y = maybe(WithLog::new(5))
        .run_inner(incr_log)
        .run_inner(incr_log);
    let add = |x, y: u16| x + y as i32;
    let a = Some(3);
    let b = Some(5);
    let res = run_both_triv(a, b, add);
    println!("x: {:?}", x);
    println!("y: {:?}", y);
    println!("res: {:?}", res);
}
