/* Opération x86_64

*/
// Opérations unaires
#[derive(Debug)]
pub enum x64UnaryOp {
    addi(i64),
    setei(i64),
    setnei(i64),
}

// Opérations binaires
#[derive(Debug)]
pub enum x64BinaryOp {
    mov,
    add,
    sub,
    mul,
    div,
    sete,
    setne,
    setl,
    setle,
    setg,
    setge,
}

// Operations de branchement unaire
#[derive(Debug)]
pub enum x64UnaryBranch {
    jz,
    jnz,
    jlei(i64),
    jgi(i64),
}

// opérations de branchement binaires
#[derive(Debug)]
pub enum x64BinaryBranch {
    jl,
    jle,
}
