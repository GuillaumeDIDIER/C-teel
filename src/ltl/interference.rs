use std::collections::{HashSet, HashMap};
use ltl::Register;
use ltl::Label;
use ertl::liveness;
use ertl;
use rtl::ops;


#[derive(Clone)]
pub struct Arcs {
    pub prefs: HashSet<Register>,
    pub intfs: HashSet<Register>,
}


impl Arcs {
    pub fn new() -> Self {
        Arcs{
            prefs: HashSet::new(),
            intfs: HashSet::new(),
        }
    }
}

pub struct Graph {
    graph: HashMap<Register, Arcs>,
}

impl Graph {
    pub fn new(instructions: &HashMap<Label, ertl::Instruction>, liveness: &HashMap<Label, liveness::LivenessInfo>) -> Self {
        let mut graph = HashMap::new();
        let mut movs = HashMap::new();
        for (label, instruction) in instructions {
            match *instruction {
                ertl::Instruction::BinaryOp(ops::x64BinaryOp::mov, register1, register2, _) => {
                    {
                        let arc1 = graph.entry(register1).or_insert(Arcs::new());
                        arc1.prefs.insert(register2);
                    }
                    {
                        let arc2 = graph.entry(register2).or_insert(Arcs::new());
                        arc2.prefs.insert(register1);
                    }
                    movs.insert(label, register1);
                },
                _ => {},
            }
        }
        for (label, live_info) in liveness {
            let mut outs = live_info.outs.clone();
            let defs = live_info.defs.clone();
            if movs.contains_key(label) {
                outs.remove(&movs[&label]);
            }

            outs = &outs - &defs;
            for var in defs {
                for out in outs.clone() {
                    {
                        let arc1 = graph.entry(var).or_insert(Arcs::new());
                        arc1.intfs.insert(out);
                    }
                    {
                        let arc2 = graph.entry(out).or_insert(Arcs::new());
                        arc2.intfs.insert(var);
                    }
                }
            }
        }
        let keys : HashSet<Register> = graph.keys().cloned().collect();
        for ref reg in keys {
            let arcs = graph.get_mut(reg).unwrap();
            let conflicts : HashSet<Register> = arcs.prefs.intersection(&arcs.intfs).cloned().collect();
            if ! conflicts.is_empty() {
                for register in conflicts {
                    arcs.prefs.remove(&register);
                }
            }
        }
        
        Graph{
            graph: graph,
        }
    }

}
