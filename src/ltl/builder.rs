use ertl;
use ltl::*;
use common::label::LabelAllocator;
use std::collections::HashMap;
use common::ops::*;

impl File {
    pub fn from_ertl(ertl_file: ertl::File) -> File {
        let functions = ertl_file.functions.into_iter().map(
            |ertl_func| {
                FuncDefinition::from_ertl(ertl_func)
            }
        ).collect();
        File{
            globals: ertl_file.globals,
            functions: functions,
        }
    }
}

struct FuncDefinitionBuilder {
    label_allocator: LabelAllocator,
    name: String,
    entry: Label,
    register_affectations: HashMap<Register, Operand>,
    spilled: usize,
    body: HashMap<Label, Instruction>,
}

impl FuncDefinition {
    pub fn from_ertl(ertl_func: ertl::FuncDefinition) -> FuncDefinition {
        let graph = interference::Graph::new(&ertl_func.body, &ertl_func.liveness);
        let (register_affectations, spilled) = graph.color_simple();
        let (builder, old_body)= FuncDefinitionBuilder::new(ertl_func, register_affectations, spilled);
        builder.build(old_body)
    }
}

impl FuncDefinitionBuilder {
    fn new(ertl_func: ertl::FuncDefinition, register_affectations: HashMap<Register, Operand>, spilled: usize) -> (Self, HashMap<Label, ertl::Instruction>) {
        (FuncDefinitionBuilder{
            label_allocator: ertl_func.label_allocator,
            name: ertl_func.name,
            entry: ertl_func.entry,
            body: HashMap::new(),
            spilled: spilled,
            register_affectations: register_affectations,
        }, ertl_func.body)
    }
    fn build(mut self, old_body: HashMap<Label, ertl::Instruction>) -> FuncDefinition{
        for (entry, instr) in old_body {
            self.instruction(entry, instr);
        }
        FuncDefinition{
            name: self.name,
            entry: self.entry,
            body: self.body,
        }
    }

    fn lookup(&self, reg: Register) -> Operand {
        self.register_affectations[&reg]
    }

    fn fix_dest(&mut self, dest: Operand, tmp_reg: Register, next: Label) -> (Register, Label) {
        match dest {
            Operand::Spilled(i) => {
                let tmp = self.label_allocator.fresh();
                self.body.insert(tmp, Instruction::BinaryOp(x64BinaryOp::mov, Operand::Reg(tmp_reg), Operand::Spilled(i), next));
                (tmp_reg, tmp)
            },
            Operand::Reg(r) => {
                (r, next)
            },
        }
    }

    fn fix_src(&mut self, entry: Label, src: Operand, tmp_reg: Register) -> (Label, Register) {
        match src {
            Operand::Spilled(i) => {
                let tmp = self.label_allocator.fresh();
                self.body.insert(entry, Instruction::BinaryOp(x64BinaryOp::mov, Operand::Spilled(i), Operand::Reg(tmp_reg), tmp));
                (tmp, tmp_reg)
            },
            Operand::Reg(r) => {
                (entry, r)
            },
        }
    }

    fn instruction(& mut self, entry: Label, ertl_instr: ertl::Instruction) {
        match ertl_instr {
            ertl::Instruction::Const(val, dest, next) => {
                let dest = self.lookup(dest);
                self.body.insert(entry, Instruction::Const(val, dest , next));
            },
            ertl::Instruction::AccessGlobal(name, dest, next) => {
                let dest_op = self.lookup(dest);
                let (dest, next) = self.fix_dest(dest_op, Register::R11, next);
                self.body.insert(entry, Instruction::AccessGlobal(name, dest, next));
            },
            ertl::Instruction::AssignGlobal(src, name, next) => {
                let src_op = self.lookup(src);
                let (entry, src) = self.fix_src(entry, src_op, Register::R11);
                self.body.insert(entry, Instruction::AssignGlobal(src, name, next));
            },
            ertl::Instruction::Load(adr, offset, dest, next) => {
                let adr_op = self.lookup(adr);
                let dest_op = self.lookup(dest);
                let (entry, adr) = self.fix_src(entry, adr_op, Register::R11);
                let (dest, next) = self.fix_dest(dest_op, Register::R10, next);
                self.body.insert(entry, Instruction::Load(adr, offset, dest, next));
            },
            ertl::Instruction::Store(src, adr, offset, next) => {
                let adr_op = self.lookup(adr);
                let src_op = self.lookup(src);
                let (entry, src) = self.fix_src(entry, src_op, Register::R11);
                let (entry, adr) = self.fix_src(entry, adr_op, Register::R10);
                self.body.insert(entry, Instruction::Store(src, adr, offset, next));
            },
            ertl::Instruction::UnaryOp(op, reg, next) => {
                let reg_op = self.lookup(reg);
                self.body.insert(entry, Instruction::UnaryOp(op, reg_op, next));
            },
            ertl::Instruction::BinaryOp(x64BinaryOp::mov, src, dest, next) if self.lookup(src) == self.lookup(dest) => {
                self.body.insert(entry, Instruction::Goto(next));
            },
            ertl::Instruction::BinaryOp(x64BinaryOp::mul, src, dest, next) => {
                let dest_op = self.lookup(dest);
                let src_op = self.lookup(src);
                let (real_dest, next) = self.fix_dest(dest_op, Register::R11, next);
                let (entry, _) = self.fix_src(entry, dest_op, real_dest);
                let dest_op = Operand::Reg(real_dest);
                self.body.insert(entry, Instruction::BinaryOp(x64BinaryOp::mul, src_op, dest_op, next));
            },
            ertl::Instruction::BinaryOp(op, src, dest, next) => {
                let src_op  = self.lookup(src);
                let dest_op = self.lookup(dest);
                let (entry, src_op) = if let Operand::Spilled(_) = self.lookup(dest) {
                    let (entry, src) = self.fix_src(entry, src_op, Register::R11);
                    (entry, Operand::Reg(src))
                } else {
                    (entry, src_op)
                };
                self.body.insert(entry, Instruction::BinaryOp(op, src_op, dest_op, next));
            },
            ertl::Instruction::Call(name, _, next) => {
                self.body.insert(entry, Instruction::Call(name, next));
            },
            ertl::Instruction::Branch(op, jmp_dest, next) => {
                self.body.insert(entry, Instruction::Branch(op, jmp_dest, next));
            },
            ertl::Instruction::Goto(next) => {
                self.body.insert(entry, Instruction::Goto(next));
            },
            ertl::Instruction::AllocFrame(next) => {
                self.body.insert(entry, Instruction::Enter(self.spilled*8, next));
            },
            ertl::Instruction::DeleteFrame(next) => {
                self.body.insert(entry, Instruction::Leave(next));
            },
            ertl::Instruction::GetParam(index, dest, next) => {
                let dest_op = self.lookup(dest);
                let (dest, next) = self.fix_dest(dest_op, Register::R10, next);
                self.body.insert(entry, Instruction::GetParam(index, dest, next));
            },
            ertl::Instruction::PushParam(src, next) => {
                let src_op  = self.lookup(src);;
                //let (entry, src) = self.fix_src(entry, src_op, Register::R10);
                self.body.insert(entry, Instruction::PushParam(src_op, next));
            },
            ertl::Instruction::Return => {
                self.body.insert(entry, Instruction::Return);
            },
        }
    }
}
