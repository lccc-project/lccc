use core::cell::RefCell;
use core::hash::siphash::{SipHashState, SipHasher};

thread_local! {
    static RAND_GEN: RefCell<Option<(u64,SipHashState)>> = const {RefCell::new(None)};
}

#[track_caller]
pub(crate) fn gen_keys() -> (u64, u64) {
    RAND_GEN.with_borrow_mut(|x| {
        let (step, state) = x.get_or_insert_with(|| {
            let mut buf = [[0u8; 8], [0u8; 8], [0u8; 8]];

            match crate::os::rand::seed_rand_buffer(buf.flatten_mut()) {
                Ok(()) => {
                    let [a, b, k] = buf.map(u64::from_ne_bytes);

                    (k, SipHashState::new_with_keys(a, b))
                }
                Err(e) if e.kind() == crate::io::ErrorKind::Unsupported => (
                    0xa5a5a5a5a5a5a5a5,
                    SipHashState::new_with_keys(0x0123456789abcdef, 0xfecba9876543210),
                ),
                Err(e) => panic!("{e}"),
            }
        });
        state.update_and_round::<2>(*step);
        let k0 = state.update_and_final::<2>();
        let k1 = state.update_and_final::<2>();
        state.update_and_round::<2>(*step);

        (k0, k1)
    })
}

pub struct RandomState(SipHasher<2, 4>);
