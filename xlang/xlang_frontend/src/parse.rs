use xlang::abi::ops::{ControlFlow, Try};

use crate::iter::{IntoRewinder, PeekMoreIterator};

pub fn take_left<R>(left: R, _: R) -> R {
    left
}

pub fn take_right<R>(_: R, right: R) -> R {
    right
}

pub fn do_alternation<C, I: Iterator, F: IntoIterator, R, S>(
    parse: &mut PeekMoreIterator<I>,
    alternates: F,
    mut combiner: R,
    state: &S,
) -> C
where
    C: Try,
    F::Item: FnOnce(&mut PeekMoreIterator<I>, &S) -> C,
    R: FnMut(C::Residual, C::Residual) -> C::Residual,
{
    crate::iter::with_rewinder_accept_on_continue(parse, move |tree| {
        let mut last_res = None;
        for alt in alternates {
            match alt(tree, state).branch() {
                ControlFlow::Continue(val) => return C::from_output(val),
                ControlFlow::Break(res) => {
                    match &mut last_res {
                        Some(last_res) => {
                            // Manual replace_with impl
                            struct AbortGuard;
                            impl Drop for AbortGuard {
                                fn drop(&mut self) {
                                    let x = AbortGuard;
                                    panic!("AbortGuard dropped")
                                }
                            }
                            let abort_guard = AbortGuard;
                            let val = unsafe { core::ptr::read(last_res) };
                            let res = combiner(val, res);
                            unsafe {
                                core::ptr::write(last_res, res);
                            }
                            core::mem::forget(abort_guard)
                        }
                        x @ None => {
                            *x = Some(res);
                        }
                    }
                }
            }
        }

        C::from_residual(last_res.expect("alternates must not be empty"))
    })
}
