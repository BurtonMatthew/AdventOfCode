enum Op
{
    Add,
    Mul,
    In,
    Out,
    Tjmp,
    Fjmp,
    LessThan,
    Equals,
    Halt
}

fn to_op(code: i32) -> Op
{
    match code
    {
        1 => Op::Add,
        2 => Op::Mul,
        3 => Op::In,
        4 => Op::Out,
        5 => Op::Tjmp,
        6 => Op::Fjmp,
        7 => Op::LessThan,
        8 => Op::Equals,
        99 => Op::Halt,
        _ => panic!("Invalid Opcode")
    }
}

fn num_params(op: &Op) -> usize
{
    match op
    {
        Op::Add => 2,
        Op::Mul => 2,
        Op::In => 0,
        Op::Out => 1,
        Op::Tjmp => 2,
        Op::Fjmp => 2,
        Op::LessThan => 2,
        Op::Equals => 2,
        Op::Halt => 0,
    }
}

fn has_output(op: &Op) -> bool
{
    match op
    {
        Op::Add => true,
        Op::Mul => true,
        Op::In => true,
        Op::Out => false,
        Op::Tjmp => false,
        Op::Fjmp => false,
        Op::LessThan => true,
        Op::Equals => true,
        Op::Halt => false,
    }
}

#[derive(Clone)]
pub struct Program
{
    tape: Vec<i32>,
    istream: Vec<i32>,
    input_index: usize,
    pc: usize
}

impl Program
{
    pub fn from_tape(tape: Vec<i32>) -> Self
    {
        Program { tape: tape, istream: Vec::new(), input_index: 0, pc: 0 }
    }

    pub fn push_input(&mut self, input: i32)
    {
        self.istream.push(input);
    }
}

impl Iterator for Program
{
    type Item = i32;
    fn next(&mut self) -> Option<i32>
    {
        loop
        {
            // read opcode
            let mut opcode = self.tape[self.pc];
            let op = to_op(opcode % 100);
            opcode /= 100;
            self.pc += 1;

            // read params
            let mut params : [i32; 2] = [0;2];
            for i in 0..num_params(&op)
            {
                params[i] = if opcode % 10 == 0 { self.tape[self.tape[self.pc] as usize] } else { self.tape[self.pc] };
                opcode /= 10;
                self.pc += 1;
            }

            // read destination
            let out_index = if has_output(&op) 
                            { 
                                let index = self.tape[self.pc];
                                self.pc += 1;
                                index
                            } 
                            else { 0 } as usize;
        
            match op
            {
                Op::Add => self.tape[out_index] = params[0] + params[1],
                Op::Mul => self.tape[out_index] = params[0] * params[1],
                Op::In =>  { self.tape[out_index] = self.istream[self.input_index]; self.input_index +=1; },
                Op::Out => return Some(params[0]),
                Op::Tjmp => if params[0] != 0 { self.pc = params[1] as usize; },
                Op::Fjmp => if params[0] == 0 { self.pc = params[1] as usize; },
                Op::LessThan => self.tape[out_index] = if params[0] < params[1] { 1 } else { 0 },
                Op::Equals => self.tape[out_index] = if params[0] == params[1] { 1 } else { 0 },
                Op::Halt => return None,
            }
        }
    }
}