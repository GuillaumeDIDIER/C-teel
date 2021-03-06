use std::collections::{HashSet, HashMap};
use ltl::Register;
use ltl::Label;
use ertl::liveness;
use ertl;
use common::ops;
use ltl::tree::Operand;


#[derive(Clone, Debug, Default)]
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

#[derive(Debug)]
pub struct Graph { //Contains the graph and useful metadata.
    graph: HashMap<Register, Arcs>,
    result: HashMap<Register, Operand>,
    possible_colors : HashMap<Register, HashSet<Register>>,
    spilled: usize,
    todo: HashSet<Register>,
}

impl Graph {

    // Initialise the graph and compute interferences, possible colors and preferences.
    pub fn new(instructions: &HashMap<Label, ertl::Instruction>, liveness: &HashMap<Label, liveness::LivenessInfo>) -> Self {
        let mut graph = HashMap::new();
        let mut movs = HashMap::new();
        for (label, instruction) in instructions {
            if let ertl::Instruction::BinaryOp(ops::x64BinaryOp::mov, register1, register2, _) = *instruction {
                {
                    let arc1 = graph.entry(register1).or_insert_with(Arcs::new);
                    arc1.prefs.insert(register2);
                }
                {
                    let arc2 = graph.entry(register2).or_insert_with(Arcs::new);
                    arc2.prefs.insert(register1);
                }
                movs.insert(label, register1);
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
                        let arc1 = graph.entry(var).or_insert_with(Arcs::new);
                        arc1.intfs.insert(out);
                    }
                    {
                        let arc2 = graph.entry(out).or_insert_with(Arcs::new);
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

        let mut possible_colors : HashMap<Register, HashSet<Register>> = HashMap::new();
        let allocatable : HashSet<Register> = Register::allocatable().iter().cloned().collect();
        let mut todo : HashSet<Register> = HashSet::new();

        for reg in graph.keys().cloned() {
            todo.insert(reg);
            if reg.is_pseudo() {
                possible_colors.insert(reg, &allocatable - &graph[&reg].intfs);
            } else {
                possible_colors.insert(reg, [reg].iter().cloned().collect());
            }
        }

        Graph{
            graph: graph,
            possible_colors: possible_colors,
            result: HashMap::new(),
            spilled: 0,
            todo: todo,
        }
    }

    // Simple coloring algorithm.

    pub fn color_simple(mut self) -> (HashMap<Register, Operand>, usize) {

        while ! self.todo.is_empty() {
            // Select a register to color or spill.
            let (register, color) : (Register, Option<Register>) =
                // First case : single possible color with preference
                if let Some(reg) = self.todo.iter().find(|&reg_ref| {
                    if self.possible_colors[reg_ref].len() == 1 {
                        let color = self.possible_colors[reg_ref].iter().nth(0).unwrap();
                        let prefs = &self.graph[reg_ref].prefs;
                        prefs.iter().any(|&pref_ref| {self.result.get(&pref_ref) == Some(&Operand::Reg(*color))})
                    } else {
                        false
                    }
                }) {
                    (*reg, Some(*self.possible_colors[reg].iter().nth(0).unwrap()))

                // second case : single possible color without preference
                } else if let Some(reg) = self.todo.iter().find(|&reg_ref| {self.possible_colors[reg_ref].len() == 1}) {
                    (*reg, Some(*self.possible_colors[reg].iter().nth(0).unwrap()))

                // Third case : preference and multiple possible colors
                // This case was the trickiest to implement
                } else if let Some(reg) = self.todo.iter().find(| & reg_ref| {
                    let prefs = &self.graph[reg_ref].prefs;
                    prefs.iter().any(|& pref_ref| { if let Some(&Operand::Reg(ref c)) = self.result.get(&pref_ref) {self.possible_colors[reg_ref].contains(c)} else {false}})
                }) {
                    let pref = self.graph[reg].prefs.iter().find(|& pref_ref| {
                        match self.result.get(pref_ref) {
                            Some(&Operand::Reg(ref c)) => self.possible_colors[reg].contains(c),
                            _ => false,
                        }
                    }).unwrap();
                    let color = match self.result[pref] {
                        Operand::Reg(c) => c,
                        _ => {panic!("Weird thing occured");}
                    };
                    (*reg, Some(color))
                // case 4 : possible colors without preference
                } else if let Some(reg) = self.todo.iter().find(|& reg_ref| {!self.possible_colors[reg_ref].is_empty()}) {
                    (*reg, Some(*self.possible_colors[reg].iter().nth(0).unwrap()))
                // case 5 :Need to spill at least one pseudo register
                } else {
                    let reg = *self.todo.iter().nth(0).unwrap();
                    (reg, None)
                }
            ;
            // remove it from the todo list and color it (or spill it)
            self.todo.remove(&register);
            if let Some(color) = color {
                self.color_register(register, color);
            } else {
                self.spill(register);
            }
        }
        (self.result, self.spilled)

    }

    fn color_register(&mut self, reg: Register, color: Register){ // Color a register and deals with interferences and preferences.
        assert!(self.possible_colors[&reg].contains(&color));
        self.result.insert(reg, Operand::Reg(color));
        let interferences = self.graph[&reg].intfs.clone().into_iter();
        for interference in interferences {
            if let Some(pcolors) = self.possible_colors.get_mut(&interference){
                pcolors.remove(&color);
            } else {
                panic!("Missing node in possible colors graph")
            }
            self.graph.get_mut(&interference).unwrap().prefs.remove(&color);
        }
    }

    fn spill(& mut self, reg: Register){
        self.result.insert(reg, Operand::Spilled(self.spilled));
        self.spilled += 1;
    }

}
