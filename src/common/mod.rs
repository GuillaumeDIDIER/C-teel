//pub use self::register::{Register, RegisterAllocator};
//pub use self::label::{Label, LabelAllocator};

/*
This contains common primitives that are used in most codegen passes.

Register deals with physicla and pseudo registers. RegisterAllocator is the structure that allocates pseudo register numbers.
Label deals with allocating labels in the code. The lables are simply integers.
Ops represent the names of operations in intel assembly splitted in three categories (Unary, Binary and Branch)

*/

pub mod register;
pub mod label;
pub mod ops;
