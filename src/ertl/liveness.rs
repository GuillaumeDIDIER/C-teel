use ertl::Label;
use ertl::Register;
use ertl::Instruction;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct LivenessInfo {
    predecessors: HashSet<Label>,
    successors: HashSet<Label>,
    defs: HashSet<Register>,
    uses: HashSet<Register>,
    ins: HashSet<Register>,
    outs: HashSet<Register>,
}

impl LivenessInfo {
    pub fn compute(instructions: &HashMap<Label, Instruction>) -> HashMap<Label, LivenessInfo> {
        HashMap::new()
    }
}
