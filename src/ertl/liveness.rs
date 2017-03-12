use ertl::Label;
use common::register::Register;
use ertl::Instruction;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::collections::VecDeque;
//use std::collections::hash_set::Union;


#[derive(Debug)]
pub struct LivenessInfo {
    pub predecessors: HashSet<Label>,
    pub successors: HashSet<Label>,
    pub defs: HashSet<Register>,
    pub uses: HashSet<Register>,
    pub ins: HashSet<Register>,
    pub outs: HashSet<Register>,
}

impl LivenessInfo {
    pub fn compute(instructions: &HashMap<Label, Instruction>) -> HashMap<Label, LivenessInfo> {
        /*let mut predecessors = HashMap::new();
        let mut successors = HashMap::new();
        let mut defs = HashMap::new();
        let mut
        let mut ins = HashMap::new();
        let mut outs = HashMap:new();*/

        let mut liveness = HashMap::new();
        for (label, instruction) in instructions {
            liveness.insert(*label, LivenessInfo::new(instruction));
        }
        let keys = instructions.keys().cloned();
        for label in keys.clone() {
            for succ in liveness[&label].successors.clone() {
                liveness.get_mut(&succ).unwrap().predecessors.insert(label);
            }
        }

        let mut working_set = VecDeque::from_iter(keys);
        while let Some(l) = working_set.pop_front() {
            let old_ins = liveness[&l].ins.clone();
            println!("Begin Union\n", );
            let outs = liveness[&l].successors.iter().fold(HashSet::new(), |acc, l| {println!("{:?}", &liveness[l].ins);acc.union(&liveness[l].ins).cloned().collect()});
            println!("\n Result: {:?}\n", outs);
            liveness.get_mut(&l).unwrap().outs = outs;
            let tmp = &liveness[&l].outs - &liveness[&l].defs;
            let ins : HashSet<Register> = liveness[&l].uses.union(&tmp).cloned().collect();//use(l) ∪ (out(l)\def (l))
            println!("{:?} {} {:?}", ins, if ins == old_ins {"=="} else {"!="}, old_ins);
            if ins != old_ins {
                working_set.extend(liveness[&l].predecessors.iter());
                liveness.get_mut(&l).unwrap().ins = ins;
            }
        }
        liveness
    }

    pub fn new(instr: &Instruction) -> Self {
        let (defs, uses) = instr.define_use();
        LivenessInfo{
            predecessors: HashSet::new(),
            successors: HashSet::from_iter(instr.successors().iter().cloned()),
            defs: HashSet::from_iter(defs.iter().cloned()),
            uses: HashSet::from_iter(uses.iter().cloned()),
            ins: HashSet::new(),
            outs: HashSet::new(),
        }
    }
}
