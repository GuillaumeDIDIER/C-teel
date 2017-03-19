use ertl::*;
use rtl;
use common::label::{Label, LabelAllocator};
use common::ops::*;
use std::collections::HashMap;
use ertl::liveness::LivenessInfo;
use common::register::{Register, RegisterAllocator};

/*
    The interesting methods and structires are here.

    The FunctionDefinitionBuilder data structure conatins teh temporary dat needed while doing the transformation.
*/

impl File {
    pub fn from_rtl(rtl_file: rtl::File) -> File { // Is there a possibilty of error ?
        let functions = rtl_file.functions.into_iter().map(
            |rtl_func: rtl::FuncDefinition| {
                FuncDefinition::from_rtl(rtl_func)
            }
        ).collect::<Vec<_>>();
        File{
            globals: rtl_file.globals,
            functions: functions,
        }
    }
}


impl FuncDefinition {
    pub fn from_rtl(rtl_func: rtl::FuncDefinition) -> FuncDefinition { // Initialise builder and transfer control
        let (builder, oldbody) = FuncDefinitionBuilder::new(rtl_func);
        builder.build(oldbody)
    }
}

struct FuncDefinitionBuilder {
    label_allocator: LabelAllocator,
    register_allocator: RegisterAllocator,
    name: String,
    formals: Vec<Register>,
    entry: Label,
    exit: Label,
    new_body: HashMap<Label, Instruction>,
    result: Register,
}

impl FuncDefinitionBuilder {
    fn new(rtl_func: rtl::FuncDefinition) -> (Self, HashMap<Label, rtl::Instruction>) {
        (FuncDefinitionBuilder{
            label_allocator: rtl_func.label_allocator,
            register_allocator: rtl_func.register_allocator,
            name: rtl_func.name,
            formals: rtl_func.formals,
            entry: rtl_func.entry,
            exit: rtl_func.exit,
            new_body: HashMap::new(),
            result: rtl_func.result,
        }, rtl_func.body)
    }

    fn build(mut self, old_body: HashMap<Label, rtl::Instruction>) -> FuncDefinition {

        // Build the body
        for (entry, instr) in old_body {
            self.instruction(entry, instr);
        }

        // Beginning of the function.

        let mut prev = self.entry; // Label used as the start for the next instruction throughout the loop.
        // fetch the parameters from register and stack. The instructions at teh beginning of the function body.
        for i in 0..self.formals.len() {
            let next = prev;
            prev = self.label_allocator.fresh();
            self.new_body.insert(prev, match i {
                0 => Instruction::BinaryOp(x64BinaryOp::mov, Register::Rdi, self.formals[i], next), // Rdi
                1 => Instruction::BinaryOp(x64BinaryOp::mov, Register::Rsi, self.formals[i], next), // Rsi
                2 => Instruction::BinaryOp(x64BinaryOp::mov, Register::Rdx, self.formals[i], next), // Rdx
                3 => Instruction::BinaryOp(x64BinaryOp::mov, Register::Rcx, self.formals[i], next), // Rcx
                4 => Instruction::BinaryOp(x64BinaryOp::mov, Register::R8,  self.formals[i], next), // R8
                5 => Instruction::BinaryOp(x64BinaryOp::mov, Register::R9,  self.formals[i], next), // R9
                _ => Instruction::GetParam(self.formals.len() - i, self.formals[i], next), // Stack
            });
        }

        // Save the callee_saved registers.
        // This is inserted before fetching the arguments
        let mut saved_registers = Vec::new();
        let calle_saved_reg = Register::callee_saved();
        for i in 0..calle_saved_reg.len() {
            let next = prev;
            prev = self.label_allocator.fresh();
            saved_registers.push(self.register_allocator.fresh());
            self.new_body.insert(prev, Instruction::BinaryOp(x64BinaryOp::mov, calle_saved_reg[i], saved_registers[i] , next));
        }
        // Inseted before backing up the registers.
        let new_entry = self.label_allocator.fresh();
        self.new_body.insert(new_entry, Instruction::AllocFrame(prev));

        // end of the function
        let mut new_exit = self.label_allocator.fresh();
        // move result
        self.new_body.insert(self.exit , Instruction::BinaryOp(x64BinaryOp::mov, self.result, Register::Rax, new_exit));
        // restore registers
        for i in 0..calle_saved_reg.len() {
            let exit = new_exit;
            new_exit = self.label_allocator.fresh();
            self.new_body.insert(exit, Instruction::BinaryOp(x64BinaryOp::mov, saved_registers[i], calle_saved_reg[i], new_exit));
        }
        // delete frame and return
        let ret = self.label_allocator.fresh();
        self.new_body.insert(new_exit, Instruction::DeleteFrame(ret));
        self.new_body.insert(ret, Instruction::Return);

        // Compute lifetimes and returns a freshly built function. Lifetimes are included because the are meaningless without the corresponding function body.
        let liveness = LivenessInfo::compute(&self.new_body);
        FuncDefinition{
            label_allocator: self.label_allocator,
            name: self.name,
            formals: self.formals.len(),
            entry: new_entry,
            body: self.new_body,
            liveness: liveness,
        }
    }

    // Instruction translation
    fn instruction(& mut self, entry: Label, rtl_instrution: rtl::Instruction) {
        match rtl_instrution {
            rtl::Instruction::Const(val, reg, next) => {
                self.new_body.insert(entry, Instruction::Const(val, reg, next));
            },
            rtl::Instruction::AccessGlobal(name, dest, next) => {
                self.new_body.insert(entry, Instruction::AccessGlobal(name, dest, next));
            },
            rtl::Instruction::AssignGlobal(src, name, next) => {
                self.new_body.insert(entry, Instruction::AssignGlobal(src, name, next));
            },
            rtl::Instruction::Load(addr, offset, dest, next) => {
                self.new_body.insert(entry, Instruction::Load(addr, offset, dest, next));
            },
            rtl::Instruction::Store(src, addr, offset, next) => {
                self.new_body.insert(entry, Instruction::Store(src, addr, offset, next));
            },
            rtl::Instruction::UnaryOp(op, reg, next) => {
                self.new_body.insert(entry, Instruction::UnaryOp(op, reg, next));
            },
            rtl::Instruction::BinaryOp(x64BinaryOp::div, reg1, reg2, next) => { // Special case for the division
                let tmp2 = self.label_allocator.fresh();
                let tmp3 = self.label_allocator.fresh();
                self.new_body.insert(entry, Instruction::BinaryOp(x64BinaryOp::mov, reg2, Register::Rax, tmp2));
                self.new_body.insert(tmp2, Instruction::BinaryOp(x64BinaryOp::div, reg1, Register::Rax, tmp3));
                self.new_body.insert(tmp3, Instruction::BinaryOp(x64BinaryOp::mov, Register::Rax, reg2, next));
            },
            rtl::Instruction::BinaryOp(op, reg1, reg2, next) => {
                self.new_body.insert(entry, Instruction::BinaryOp(op, reg1, reg2, next));
            },
            rtl::Instruction::Branch(op, label1, next) => {
                self.new_body.insert(entry, Instruction::Branch(op, label1, next));
            },
            rtl::Instruction::Call(result, name, params, next) => { // function calls
                let mut entry = entry;
                // Add parameters
                for (i, param) in params.iter().cloned().enumerate(){
                    entry = self.add_parameter(i, param, entry)
                }
                let tmp = self.label_allocator.fresh();
                // The call itself
                self.new_body.insert(entry, Instruction::Call(name, params.len(), tmp));
                // clean up stack if needed
                let tmp = if params.len() > 6{
                    let tmp2 = self.label_allocator.fresh();
                    self.new_body.insert(
                        tmp,
                        Instruction::UnaryOp(
                            x64UnaryOp::addi(((params.len() - 6) * 8) as i64),
                            Register::Rsp, tmp2
                        )
                    );
                    tmp2
                } else {
                    tmp
                };
                // Move result to its destination
                self.new_body.insert(
                    tmp,
                    Instruction::BinaryOp(x64BinaryOp::mov, Register::Rax, result, next)
                );
            },
            rtl::Instruction::Goto(label) => {
                self.new_body.insert(entry, Instruction::Goto(label));
            },
        }
    }
    // This must be synchronised with common::register.
    fn add_parameter(& mut self, index: usize, source: Register, entry: Label) -> Label /*exit*/ {
        let label = self.label_allocator.fresh();
        self.new_body.insert(entry, match index {
            0 => Instruction::BinaryOp(x64BinaryOp::mov, source, Register::Rdi, label),
            1 => Instruction::BinaryOp(x64BinaryOp::mov, source, Register::Rsi, label), // Rsi
            2 => Instruction::BinaryOp(x64BinaryOp::mov, source, Register::Rdx, label), // Rdx
            3 => Instruction::BinaryOp(x64BinaryOp::mov, source, Register::Rcx, label), // Rcx
            4 => Instruction::BinaryOp(x64BinaryOp::mov, source, Register::R8, label), // R8
            5 => Instruction::BinaryOp(x64BinaryOp::mov, source, Register::R9, label), // R9
            _ => Instruction::PushParam(source, label), // Stack
        });
        label
    }
}
