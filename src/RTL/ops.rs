/* Opération x86_64

*/
// Opérations unaires
#[derive(Debug)]
pub enum x64UnaryOp {
    addi(i64),
    sete,
    setne,
    setl,
    setle,
    setg,
    setge,
}

// Opérations binaires
#[derive(Debug)]
pub enum x64BinaryOp {
    mov,
    add,
    sub,
    mul,
    div,
    cmp, // dest - src
    test,// dest & src
}

// Operations de branchement
#[derive(Debug)]
pub enum x64Branch {
    je,
    jne,
    jle,
    jl,
    jge,
    jg,
}
