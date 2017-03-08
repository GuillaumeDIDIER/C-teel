use ertl::*;
use rtl;
use rtl::label::LabelAllocator;
use rtl::ops::*;
use std::collections::HashMap;
use ertl::liveness::LivenessInfo;
use rtl::register::RegisterAllocator;

impl File {
    pub fn from_rtl(rtl_file: rtl::File) -> Result<File, String> {
        match rtl_file.functions.into_iter().map(
            |rtl_func: rtl::FuncDefinition| {
                FuncDefinition::from_rtl(rtl_func)
            }
        ).collect::<Result<Vec<_>,_>>() {
            Ok(functions) => {
                Ok(File{
                    globals: rtl_file.globals,
                    functions: functions,
                })
            },
            Err(e) => Err(e)
        }
    }
}


impl FuncDefinition {
    pub fn from_rtl(rtl_func: rtl::FuncDefinition) -> Result<FuncDefinition, String> {
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

    fn build(mut self, old_body: HashMap<Label, rtl::Instruction>) -> Result<FuncDefinition, String> {
        for (entry, instr) in old_body {
            try!(self.instruction(entry, instr));
        }

        // Beginning of the function.

        let mut prev = self.entry;
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
                _ => Instruction::GetParam(i - 6, self.formals[i], next), // Stack
            });
        }
        let mut saved_registers = Vec::new();
        let calle_saved_reg = Register::callee_saved();
        for i in 0..calle_saved_reg.len() {
            let next = prev;
            prev = self.label_allocator.fresh();
            saved_registers.push(self.register_allocator.fresh());
            self.new_body.insert(prev, Instruction::BinaryOp(x64BinaryOp::mov, calle_saved_reg[i], saved_registers[i] , next));
        }
        let new_entry = self.label_allocator.fresh();
        self.new_body.insert(new_entry, Instruction::AllocFrame(prev));

        // end of the function
        let mut new_exit = self.label_allocator.fresh();
        self.new_body.insert(self.exit , Instruction::BinaryOp(x64BinaryOp::mov, self.result, Register::Rax, new_exit));
        for i in 0..calle_saved_reg.len() {
            let exit = new_exit;
            new_exit = self.label_allocator.fresh();
            self.new_body.insert(exit, Instruction::BinaryOp(x64BinaryOp::mov, saved_registers[i], calle_saved_reg[i], new_exit));
        }
        let ret = self.label_allocator.fresh();
        self.new_body.insert(new_exit, Instruction::DeleteFrame(ret));
        self.new_body.insert(ret, Instruction::Return);
        Ok(FuncDefinition{
            label_allocator: self.label_allocator,
            name: self.name,
            formals: self.formals.len(),
            entry: new_entry,
            body: self.new_body,
        })
    }

    fn instruction(& mut self, entry: Label, rtl_instrution: rtl::Instruction) -> Result<(),String> {
        match rtl_instrution {
            rtl::Instruction::Const(val, reg, next) => {
                self.new_body.insert(entry, Instruction::Const(val, reg, next));
                Ok(())
            },
            rtl::Instruction::AccessGlobal(name, dest, next) => {
                self.new_body.insert(entry, Instruction::AccessGlobal(name, dest, next));
                Ok(())
            },
            rtl::Instruction::AssignGlobal(src, name, next) => {
                self.new_body.insert(entry, Instruction::AssignGlobal(src, name, next));
                Ok(())
            },
            rtl::Instruction::Load(addr, offset, dest, next) => {
                self.new_body.insert(entry, Instruction::Load(addr, offset, dest, next));
                Ok(())
            },
            rtl::Instruction::Store(src, addr, offset, next) => {
                self.new_body.insert(entry, Instruction::Store(src, addr, offset, next));
                Ok(())
            },
            rtl::Instruction::UnaryOp(op, reg, next) => {
                self.new_body.insert(entry, Instruction::UnaryOp(op, reg, next));
                Ok(())
            },
            rtl::Instruction::BinaryOp(op, reg1, reg2, next) => {
                self.new_body.insert(entry, Instruction::BinaryOp(op, reg1, reg2, next));
                Ok(())
            },
            rtl::Instruction::Branch(op, label1, next) => {
                self.new_body.insert(entry, Instruction::Branch(op, label1, next));
                Ok(())
            },
            rtl::Instruction::Call(result, name, params, next) => {
                let mut entry = entry;
                for i in 0..params.len(){
                    entry = try!(self.add_parameter(i, params[i], entry))
                }
                let tmp = self.label_allocator.fresh();
                self.new_body.insert(entry, Instruction::Call(name, params.len(), tmp));
                let tmp = if params.len() > 6{
                    let tmp2 = self.label_allocator.fresh();
                    self.new_body.insert(
                        tmp,
                        Instruction::UnaryOp(
                            x64UnaryOp::addi((params.len() - 6) as i64),
                            Register::Rsp, tmp2
                        )
                    );
                    tmp2
                } else {
                    tmp
                };
                self.new_body.insert(
                    tmp,
                    Instruction::BinaryOp(x64BinaryOp::mov, Register::Rax, result, next)
                );
                Ok(())
            }
            _ => Err(String::from("Unimplemented"))
        }
    }

    fn add_parameter(& mut self, index: usize, source: Register, entry: Label) -> Result<Label /*exit*/, String> {
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
        Ok(label)
    }
}
