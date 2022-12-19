pub trait Instruction {
    fn is_code_instruction(&self) -> bool;
    fn is_redundant(pair: (&Self, &Self)) -> bool;
}

pub fn optimize<T>(mut instructions: Vec<T>) -> Vec<T>
where
    T: Instruction + Clone,
{
    let mut optimized_set = Vec::new();

    let mut index = 0;
    let mut offset = 1;
    while index + offset < instructions.len() {
        // If first isn't a real code instruction, skip it and continue
        let first = &instructions[index];
        if !T::is_code_instruction(first) {
            optimized_set.push(first.clone());
            index += 1;
            continue;
        }

        // If second isn't a real code instruction, skip it and continue
        let second = &instructions[index + offset];
        if !T::is_code_instruction(second) {
            offset += 1;
            continue;
        }

        if T::is_redundant((first, second)) {
            // Actually remove both instructions from the vector, so we don't see them again
            instructions.remove(index + offset);
            instructions.remove(index);

            // Index is back to being correct, but offset isn't
            offset = 1;
        } else {
            // Add the instruction
            optimized_set.push(first.clone());

            // First increments
            index += 1;

            // Second is now first + 1
            offset = 1;
        }
    }

    // Add everything else
    while index < instructions.len() {
        optimized_set.push(instructions[index].clone());
        index += 1;
    }

    optimized_set
}
