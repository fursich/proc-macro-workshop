// So far our macro has repeated the entire loop body. This is not sufficient
// for some use cases because there are restrictions on the syntactic position
// that macro invocations can appear in. For example the Rust grammar would not
// allow a caller to write:
//
//     enum Interrupt {
//         seq!(N in 0..16 {
//             Irq~N,
//         });
//     }
//
// because this is just not a legal place to put a macro call.
//
// Instead we will implement a way for the caller to designate a specific part
// of the macro input to be repeated, so that anything outside that part does
// not get repeated. The repeated part will be written surrounded by #(...)*.
//
// The invocation below should expand to:
//
//     #[derive(Copy, Clone, PartialEq, Debug)]
//     enum Interrupt {
//         Irq0,
//         ...
//         Irq15,
//     }
//
// Optionally, allow for there to be multiple separate #(...)* sections,
// although the test suite does not exercise this case. The #(...)* sections
// will each need to be repeated according to the same loop bounds.

use seq::seq;

seq!(N in 0..16 {
    #[derive(Copy, Clone, PartialEq, Debug)]
    enum Interrupt {
        #(
            Irq~N,
        )*
    }
});

// additional test 1
// check if each #()* is expanded independntly
seq!(N in 0..4 {
    #[derive(Copy, Clone, PartialEq, Debug)]
    enum ProductCode {
        #(
            ItemTypeA~N,
        )*
        #(
            ItemTypeB~N,
        )*
    }
});

// additional test 2
// make sure 'global' loop can be parsed as well
seq!(N in 1..4 {
    fn f~N () -> u64 {
        N * 2
    }
});

fn f0() -> u64 {
    100
}

fn main() {
    let interrupt = Interrupt::Irq8;

    assert_eq!(interrupt as u8, 8);
    assert_eq!(interrupt, Interrupt::Irq8);

    let product_code1 = ProductCode::ItemTypeA2;
    let product_code2 = ProductCode::ItemTypeB3;

    assert_eq!(product_code1 as u8, 2);
    assert_eq!(product_code1, ProductCode::ItemTypeA2);
    assert_eq!(product_code2 as u8, 4 + 3);
    assert_eq!(product_code2, ProductCode::ItemTypeB3);

    let sum = f0() + f1() + f2() + f3();

    assert_eq!(sum, 100 + 2 + 4 + 6);
}
