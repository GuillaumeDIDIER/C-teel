#[derive(Debug, Default)]
pub struct LabelAllocator {
    count: i64,
}

pub type Label = i64;


impl LabelAllocator {
    pub fn new() -> Self {
        LabelAllocator{count: 0}
    }
    pub fn fresh(& mut self) -> Label {
        let res : Label = self.count;//format!("L{}", self.count);
        self.count += 1;
        res
    }
}
