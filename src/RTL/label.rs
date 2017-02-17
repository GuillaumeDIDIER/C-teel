
pub struct LabelAllocator {
    count: i64,
}

pub type Label = String;

impl<'a> LabelAllocator {
    pub fn new() -> Self {
        LabelAllocator{count: 0}
    }
    pub fn fresh(& mut self) -> Label {
        let res: String = format!("L{}", self.count);
        self.count += 1;
        res
    }
}
