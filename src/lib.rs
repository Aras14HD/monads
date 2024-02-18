//! This Crate implements a generic monad trait Run, that basically does .then/flatmap, a trait RunTrivial that implements .map, and a trait RunTuple that extends these to function with multiple arguments using tuples.
//!
//! ```
//! use monads::*;
//! assert!((Some(5 as i32), Some(3 as i32))
//!     .run(|x,y| x.checked_add(y))                                // Some(8)
//!     .run_triv(|x| if x == 8 {Ok(x)} else {Err("wrong answer")}) // Some(Ok(8))
//!     .run_inner(|x| x.checked_sub(1).ok_or("Underflow!"))        // Some(Ok(7))
//!     .unwrap().run_lazy(|x| Ok(x+1))()                           // Ok(8)
//!     == Ok(8));
//! ```
#![feature(unboxed_closures, tuple_trait)]
use std::marker::Tuple;
use monad_macro::{impl_run_tuple, impl_run_tuple_trivial};

/// The main Monad trait
///
/// ```
/// use monads::Run;
/// assert!(Some(9).run(|x| if x > 5 {Some(x+1)} else {None}) == Some(10));
/// ```
pub trait Run<W> {
    type Wrapper<T>;
    /// Possibly runs f with the inner type and/or returns some wrapped value.
    ///
    /// ```
    /// use monads::Run;
    /// assert!(Some(5 as usize).run(|x| x.checked_sub(1)) == Some(4));
    /// assert!(Some(0 as usize).run(|x| x.checked_sub(1)) == None);
    /// ```
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, f: F) -> Self::Wrapper<T>;
    /// Returns a closure, that calls run when called. These closures can also be run.
    ///
    /// ```ignore
    /// x.run_lazy(f).run_lazy(g)()
    /// ```
    fn run_lazy<T, F: FnOnce(W) -> Self::Wrapper<T> + 'static>(
        self,
        f: F,
    ) -> impl FnOnce() -> Self::Wrapper<T>
    where
        Self: Sized + 'static,
    {
        || self.run(f)
    }
}

/// Implement Run for all closures that just return a Run (eg run_lazy)
impl<T: Run<W>, Func: FnOnce() -> T, W> Run<W> for Func {
    type Wrapper<U> = T::Wrapper<U>;
    fn run<U, F: FnOnce(W) -> Self::Wrapper<U>>(self, f: F) -> Self::Wrapper<U> {
        self().run(f)
    }
}

/// A trait that allows simpler functions to be called with Run
pub trait RunTrivial<W>: Run<W> {
    /// Like run, but automatically wrapps the result (like map)
    ///
    /// ```
    /// use monads::RunTrivial;
    /// assert!(Some(5).run_triv(|y| y+1) == Some(6));
    /// ```
    fn run_triv<T, F: FnOnce(W) -> T>(self, f: F) -> Self::Wrapper<T>;
    /// Run nested monads (where the outer implements RunTrivial)
    ///
    /// ```
    /// use monads::RunTrivial;
    /// assert!(Some(Ok(5 as i32)).run_inner(|x| x.checked_sub(1).ok_or("Underflow!")) == Some(Ok(4)));
    /// ``` 
    fn run_inner<T, U, F: FnOnce(U) -> W::Wrapper<T>>(self, f: F) -> Self::Wrapper<W::Wrapper<T>>
    where
        W: Run<U>,
        Self: Sized,
    {
        self.run_triv(|x| x.run(f))
    }
}

// pub trait RunTwo<W, V, U> {
//     type Wrapper<T>;
//     fn run<F: FnOnce(W, V) -> Self::Wrapper<U>>(self, f: F) -> Self::Wrapper<U>;
//     fn run_lazy<F: FnOnce(W, V) -> Self::Wrapper<U> + 'static>(
//         self,
//         f: F,
//     ) -> Box<dyn FnOnce() -> Self::Wrapper<U>>
//     where
//         Self: Sized + 'static,
//     {
//         Box::new(|| self.run(f))
//     }
// }

/// Run monads with multiple compatible arguments (eg the same Wrapper)
pub trait RunTuple<W: Tuple, U> {
    type Wrapper<T>;
    /// Calls f with the inner types of self's field in order and/or returns some wrapped value
    ///
    /// ```
    /// use monads::RunTuple;
    /// assert!((Some(5 as i32), Some(6)).run(|x,y| x.checked_sub(y)) == Some(-1));
    /// ```
    fn run<F: FnOnce<W, Output = Self::Wrapper<U>>>(self, f: F) -> Self::Wrapper<U>;
    /// Like normal run_lazy but for functions with multiple arguments
    ///
    /// ```ignore
    /// (x,y).run_lazy(f).run_lazy(g)()
    /// ```
    fn run_lazy<F: FnOnce<W, Output = Self::Wrapper<U>> + 'static>(
        self,
        f: F,
    ) -> Box<dyn FnOnce() -> Self::Wrapper<U>>
    where
        Self: Sized + 'static,
    {
        Box::new(|| self.run(f))
    }
}

// Macro needed to generate impls
impl_run_tuple!();
impl_run_tuple!(B);
impl_run_tuple!(B, C);
impl_run_tuple!(B, C, D);
impl_run_tuple!(B, C, D, E);
impl_run_tuple!(B, C, D, E, F);
impl_run_tuple!(B, C, D, E, F, G);
impl_run_tuple!(B, C, D, E, F, G, H);
impl_run_tuple!(B, C, D, E, F, G, H, I);
impl_run_tuple!(B, C, D, E, F, G, H, I, J);
impl_run_tuple!(B, C, D, E, F, G, H, I, J, K);
impl_run_tuple!(B, C, D, E, F, G, H, I, J, K, L);

/// RunTriv but with multiple args
pub trait RunTupleTrivial<W: Tuple, U>: RunTuple<W, U> {
    /// Run a fuction with multiple args and automatically wrap it
    ///
    /// ```
    /// use monads::RunTupleTrivial;
    /// assert!((Some(5), Some(3)).run_triv(|x,y| x+y) == Some(8));
    /// ```
    fn run_triv<F: FnOnce<W, Output =  U>>(self, f: F) -> Self::Wrapper<U>;
}

// Implementations for RunTupleTriv
impl_run_tuple_trivial!();
impl_run_tuple_trivial!(B);
impl_run_tuple_trivial!(B, C);
impl_run_tuple_trivial!(B, C, D);
impl_run_tuple_trivial!(B, C, D, E);
impl_run_tuple_trivial!(B, C, D, E, F);
impl_run_tuple_trivial!(B, C, D, E, F, G);
impl_run_tuple_trivial!(B, C, D, E, F, G, H);
impl_run_tuple_trivial!(B, C, D, E, F, G, H, I);
impl_run_tuple_trivial!(B, C, D, E, F, G, H, I, J);
impl_run_tuple_trivial!(B, C, D, E, F, G, H, I, J, K);
impl_run_tuple_trivial!(B, C, D, E, F, G, H, I, J, K, L);

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

// impl<U, A, B, X: Run<A>, Y: Run<B, Wrapper<U> = X::Wrapper<U>>> RunTwo<A, B, U> for (X, Y) {
//     type Wrapper<T> = X::Wrapper<T>;
//     fn run<F: FnOnce(A, B) -> Self::Wrapper<U>>(self, f: F) -> Self::Wrapper<U> {
//         let (x, y) = self;
//         x.run(|a| y.run::<U, _>(|b| f(a, b)))
//     }
// }

impl<W> Run<W> for () {
    type Wrapper<T> = ();
    fn run<T, F: FnOnce(W) -> Self::Wrapper<T>>(self, _f: F) -> Self::Wrapper<T> {
        ()
    }
}

impl<W> RunTrivial<W> for () {
    fn run_triv<T, F: FnOnce(W) -> T>(self, _f: F) -> Self::Wrapper<T> {
        ()
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
pub struct WithLog<T> {
    v: T,
    log: String,
}

impl<T> WithLog<T> {
    pub fn new(v: T) -> Self {
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


pub fn incr_log(x: i32) -> WithLog<i32> {
    WithLog {
        v: x + 1,
        log: format!("Incremented {:?} ", x),
    }
}

/// Legacy implementation that led to RunTuple
pub fn run_both<
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

/// Hopefully some day a legacy implementation to run_triv with multiple args
pub fn run_both_triv<
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

/// Crates a closure that when called calls f(v)
pub fn lazy<T, U, F: FnOnce(T) -> U>(v: T, f: F) -> impl FnOnce() -> U {
    || f(v)
}
