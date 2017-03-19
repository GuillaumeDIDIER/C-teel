use std::collections::{HashSet, HashMap};
use ltl::Register;
use ltl::Label;
use ertl::liveness;
use ertl;
use common::ops;
use ltl::tree::Operand;


#[derive(Clone, Debug)]
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
pub struct Graph {
    graph: HashMap<Register, Arcs>,
    result: HashMap<Register, Operand>,
    possible_colors : HashMap<Register, HashSet<Register>>,
    spilled: usize,
    todo: HashSet<Register>,
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
            //println!("{:?} {:?}", label, live_info);
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
        //println!("{:?}", keys);
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
                //println!("{:?} : {:?} - {:?}", reg, allocatable, graph.get(&reg).unwrap().intfs);
                //println!("= {:?}", &allocatable - &graph.get(&reg).unwrap().intfs);
                possible_colors.insert(reg, &allocatable - &graph.get(&reg).unwrap().intfs);
            } else {
                possible_colors.insert(reg, [reg].iter().cloned().collect());
            }
        }
        //println!("{:#?}", possible_colors);

        Graph{
            graph: graph,
            possible_colors: possible_colors,
            result: HashMap::new(),
            spilled: 0,
            todo: todo,
        }
    }

    pub fn color_simple(mut self) -> (HashMap<Register, Operand>, usize) {

        while ! self.todo.is_empty() {
            let (register, color) : (Register, Option<Register>) =
                if let Some(reg) = self.todo.iter().find(|&reg_ref| {
                    if self.possible_colors[reg_ref].len() == 1 {
                        let color = self.possible_colors[reg_ref].iter().nth(0).unwrap();
                        let prefs = &self.graph[reg_ref].prefs;
                        if let Some(_) = prefs.iter().find(|&pref_ref| {self.result.get(pref_ref) == Some(&Operand::Reg(*color))}) {
                            return true;
                        }
                    }
                    return false;
                }) {
                    //println!("Unique possible color with pref {:?} : {:?}", reg, *self.possible_colors[reg].iter().nth(0).unwrap());
                    (*reg, Some(*self.possible_colors[reg].iter().nth(0).unwrap()))
                } else if let Some(reg) = self.todo.iter().find(|&reg_ref| {self.possible_colors[reg_ref].len() == 1}) {
                    //println!("Unique possible color without pref {:?} : {:?}", reg, *self.possible_colors[reg].iter().nth(0).unwrap());
                    (*reg, Some(*self.possible_colors[reg].iter().nth(0).unwrap())) // Missing a case here !!!
                } /*else if let Some(reg) = self.todo.iter().find(| & reg_ref| { // This is currently broken
                    let prefs = &self.graph[reg_ref].prefs;
                    prefs.iter().find(|& pref_ref| { self.result.get(pref_ref).is_some()}).is_some()
                }) {
                    let pref = &self.graph[reg].prefs.iter().find(|& pref_ref| {
                        match self.result.get(pref_ref) {
                            Some(&Operand::Reg(_)) => true,
                            _ => false,
                        }
                    }).unwrap();
                    let color = match *self.result.get(pref).unwrap(){Operand::Reg(c) => c, _=>{panic!("Wierd thing occured");}};
                    println!("color with pref {:?} : {:?}", reg, color);
                    (*reg, Some(color))
                } */else if let Some(reg) = self.todo.iter().find(|& reg_ref| {self.possible_colors[reg_ref].len() > 0}) {
                    //println!("Color without pref {:?} : {:?}", reg, *self.possible_colors[reg].iter().nth(0).unwrap());
                    (*reg, Some(*self.possible_colors[reg].iter().nth(0).unwrap()))
                } else {
                    let reg = *self.todo.iter().nth(0).unwrap();
                    //println!("Spilled {:?}", reg);
                    (reg, None)
                }
            ;
            self.todo.remove(&register);
            if let Some(color) = color {
                self.color_register(register, color);
            } else {
                self.spill(register);
            }
        }
        (self.result, self.spilled)

    }

    fn color_register(&mut self, reg: Register, color: Register){
        assert!(self.possible_colors[&reg].contains(&color));
        //println!("color_register {:?} {:?}", reg, color);
        self.result.insert(reg, Operand::Reg(color));
        let interferences = self.graph.get(&reg).unwrap().intfs.clone().into_iter();
        for interference in interferences {
            if let Some(pcolors) = self.possible_colors.get_mut(&interference){
                pcolors.remove(&color);
                //println!("  interfers with {:?}, remove {:?} :", interference, color);
            } else {
                panic!("Missing node in possible colors graph")
            }
            self.graph.get_mut(&interference).unwrap().prefs.remove(&color);
            println!("    {:?}", self.possible_colors[&interference]);
        }
    }

    fn spill(& mut self, reg: Register){
        self.result.insert(reg, Operand::Spilled(self.spilled));
        self.spilled += 1;
    }

}
