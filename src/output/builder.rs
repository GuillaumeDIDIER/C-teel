use output::*;
use ltl;
use common::ops::*;
use std::collections::{HashSet, HashMap};
use common::label::Label;
use common::register::Register;
use std::borrow::Cow;

impl Output {
    pub fn from_ltl(ltl_file: ltl::File) -> Self {
        let mut globals = Vec::new();
        for global in ltl_file.globals {
            globals.push(GlobalDef::new(global));
        }
        let mut functions = Vec::new();
        for function in ltl_file.functions {
            functions.push(FunctionOutput::from_ltl(function));
        }
        Output{
            globals: globals,
            functions: functions,
        }
    }
}

impl FunctionOutput {
    pub fn from_ltl(mut ltl_function: ltl::FuncDefinition) -> Self {
        let instructions = Vec::new();
        let labels = HashSet::new();
        let mut visited : HashSet<Label> = HashSet::new();
        let mut f = FunctionOutput{
            name: ltl_function.name,
            instructions: instructions,
            labels: labels,
        };
        f.visit(& mut visited, ltl_function.entry, &mut ltl_function.body);
        f
    }

    fn visit(& mut self, visited :  & mut HashSet<Label>, label: Label, instructions: &mut HashMap<Label, ltl::Instruction>) {
        if visited.contains(&label) {
            self.labels.insert(label);
            self.instructions.push((None, format!("    jmp .{}.{}", self.name, label)));
        } else {
            visited.insert(label);
            self.output(visited, label, instructions);
        }
    }

    fn output(& mut self, visited : & mut HashSet<Label>, label: Label, instructions: &mut HashMap<Label, ltl::Instruction>) {
        if let Some(i) =  instructions.remove(&label) {
            match i {
                ltl::Instruction::Const(value, op, next) => {
                    self.instructions.push((Some(label), format!("    movq ${}, {}", value, op)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::AccessGlobal(name, reg, next) => {
                    self.instructions.push((Some(label), format!("    movq {}(%rip), {}", name, reg)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::AssignGlobal(reg, name, next) => {
                    self.instructions.push((Some(label), format!("    movq {}, {}(%rip)", reg, name)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Load(src, offset, dest, next) => {
                    self.instructions.push((Some(label), format!("    movq {}({}), {}", offset, src, dest)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Store(src, dest, offset, next) => {
                    self.instructions.push((Some(label), format!("    movq {}, {}({})", src, offset, dest)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::UnaryOp(x64UnaryOp::addi(i), reg, next) => {
                    self.instructions.push((Some(label), format!("    addq ${}, {}", i, reg)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::UnaryOp(op, reg, next) => { // setcc instruction
                    let byte_reg = match reg {
                        ltl::Operand::Reg(Register::Rax) => Cow::from("%al"),
                        ltl::Operand::Reg(Register::Rbx) => Cow::from("%bl"),
                        ltl::Operand::Reg(Register::Rcx) => Cow::from("%cl"),
                        ltl::Operand::Reg(Register::Rdx) => Cow::from("%dl"),
                        ltl::Operand::Reg(Register::Rsi) => Cow::from("%sil"),
                        ltl::Operand::Reg(Register::Rdi) => Cow::from("%dil"),
                        ltl::Operand::Reg(Register::Rsp) => Cow::from("%spl"),
                        ltl::Operand::Reg(Register::Rbp) => Cow::from("%bpl"),
                        ltl::Operand::Reg(Register::R8 ) => Cow::from("%r8b"),
                        ltl::Operand::Reg(Register::R9 ) => Cow::from("%r9b"),
                        ltl::Operand::Reg(Register::R10) => Cow::from("%r10b"),
                        ltl::Operand::Reg(Register::R11) => Cow::from("%r11b"),
                        ltl::Operand::Reg(Register::R12) => Cow::from("%r12b"),
                        ltl::Operand::Reg(Register::R13) => Cow::from("%r13b"),
                        ltl::Operand::Reg(Register::R14) => Cow::from("%r14b"),
                        ltl::Operand::Reg(Register::R15) => Cow::from("%r15b"),
                        _ => Cow::from(format!("{}", reg)),
                    };
                    self.instructions.push((Some(label), format!("    movq $0, {}\n    {} {}", reg, op, byte_reg)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::BinaryOp(x64BinaryOp::div, sreg, dreg, next) => {
                    assert!(dreg == ltl::Operand::Reg(Register::Rax));
                    self.instructions.push((Some(label), format!("    cqto\n    {} {}", x64BinaryOp::div, sreg)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::BinaryOp(op, sreg, dreg, next) => {
                    self.instructions.push((Some(label), format!("    {} {}, {}", op, sreg, dreg)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Call(name, next) => {
                    self.instructions.push((Some(label), format!("    call {}", name)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Goto(next) => {
                    self.instructions.push((Some(label), String::new()));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Branch(op, jump_label, next) => {
                    self.instructions.push((Some(label), format!("    {} .{}.{}", op, self.name, jump_label)));
                    self.visit(visited, next, instructions);
                    self.labels.insert(jump_label);
                    self.visit(visited, jump_label, instructions);
                },
                ltl::Instruction::Enter(size, next) => {
                    self.instructions.push((Some(label), format!("    enter ${}, $0", size)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Leave(next) => {
                    self.instructions.push((Some(label), String::from("    leave")));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::GetParam(index, dest, next) => {
                    self.instructions.push((Some(label), format!("    movq {}(%rbp) {}", index * 8, dest)));
                    self.visit(visited, next, instructions);

                },
                ltl::Instruction::PushParam(src, next) => {
                    self.instructions.push((Some(label), format!("    pushq {}", src)));
                    self.visit(visited, next, instructions);
                },
                ltl::Instruction::Return => {
                    self.instructions.push((Some(label), String::from("    retq")));
                },
                //_ => {}
            }
        }
    }
}

impl GlobalDef {
    pub fn new(name: String) -> Self {
        GlobalDef{
            name: name
        }
    }
}
