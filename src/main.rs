trait Run<W> {
    type Wrapper<T>;
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, f: F) -> Self::Wrapper<T>;
    fn run_lazy<T, F: FnOnce(W) -> Self::Wrapper<T> + 'static>(
        self,
        f: F,
    ) -> Box<dyn FnOnce() -> Self::Wrapper<T>>
    where
        Self: Sized + 'static,
    {
        Box::new(|| self.run(f))
    }
}

impl<T: Run<W>, W> Run<W> for Box<dyn FnOnce() -> T> {
    type Wrapper<U> = T::Wrapper<U>;
    fn run<U, F: FnOnce(W) -> Self::Wrapper<U>>(self, f: F) -> Self::Wrapper<U> {
        self().run(f)
    }
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

trait RunTwo<W, V, U> {
    type Wrapper<T>;
    fn run<F: FnOnce(W, V) -> Self::Wrapper<U>>(self, f: F) -> Self::Wrapper<U>;
    fn run_lazy<F: FnOnce(W, V) -> Self::Wrapper<U> + 'static>(
        self,
        f: F,
    ) -> Box<dyn FnOnce() -> Self::Wrapper<U>>
    where
        Self: Sized + 'static,
    {
        Box::new(|| self.run(f))
    }
}

// trait RunTwoTrivial<W, V, U>: RunTwo<W, V, U> {
//     fn run_triv<F: FnOnce(W, V) -> U>(self, f: F) -> Self::Wrapper<U>;
//     fn run_inner<T, Q, R, F: FnOnce(W, V) -> <(W, V) as RunTwo<W, V, R>>::Wrapper<R>>(
//         self,
//         f: F,
//     ) -> Self::Wrapper<<(W, V) as RunTwo<W, V, R>>::Wrapper<R>>
//     where
//         (W, V): RunTwo<T, Q, R>,
//         U = (W, V),
//         W: Run<W>,
//         V: Run<V, Wrapper<R> = W::Wrapper<R>>,
//         Self: Sized,
//     {
//         self.run_triv(|x, y| (x, y).run(f))
//     }
// }

impl<U, A, B, X: Run<A>, Y: Run<B, Wrapper<U> = X::Wrapper<U>>> RunTwo<A, B, U> for (X, Y) {
    type Wrapper<T> = X::Wrapper<T>;
    fn run<F: FnOnce(A, B) -> Self::Wrapper<U>>(self, f: F) -> Self::Wrapper<U> {
        let (x, y) = self;
        x.run(|a| y.run::<U, _>(|b| f(a, b)))
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
    F: FnOnce(X, Y) -> B::Wrapper<T>,
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
    F: FnOnce(X, Y) -> T,
>(
    a: A,
    b: B,
    f: F,
) -> A::Wrapper<T> {
    a.run(|x| b.run_triv::<T, _>(|y| f(x, y)))
}

fn lazy<T, U, F: FnOnce(T) -> U>(v: T, f: F) -> impl FnOnce() -> U {
    || f(v)
}

fn main() {
    let x = maybe(5).run(maybe);
    let y = maybe(WithLog::new(5))
        .run_inner(incr_log)
        .run_inner(incr_log);
    let add = |x: i32, y: u16| x + y as i32;
    let a = Some(3);
    // let a = None;
    let b = Some(5);
    let res = (a, b).run(|a, b| Some(add(a, b)));
    let z = res.run_lazy(maybe).run_lazy(maybe)();
    println!("x: {:?}", x);
    println!("y: {:?}", y);
    println!("z: {:?}", z);
    println!("res: {:?}", res);
}
