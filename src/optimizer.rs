
pub struct Optimizer {
}

pub trait Instruction: {
    fn is_code_instruction(&self) -> bool;
    fn is_redundant(pair: (&Self, &Self)) -> bool;
}

pub fn optimize<T>(instructions: Vec<T>) -> Vec<T> where T: Instruction {
    instructions
}
