#![allow(dead_code)]
use std::collections::VecDeque;

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
    AdjBase,
    Halt,
}

impl Op
{
    fn from_i64(code: i64) -> Self
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
            9 => Op::AdjBase,
            99 => Op::Halt,
            _ => panic!("Invalid Opcode")
        }
    }

    fn num_params(&self) -> usize
    {
        match self
        {
            Op::Add => 2,
            Op::Mul => 2,
            Op::In => 0,
            Op::Out => 1,
            Op::Tjmp => 2,
            Op::Fjmp => 2,
            Op::LessThan => 2,
            Op::Equals => 2,
            Op::AdjBase => 1,
            Op::Halt => 0,
        }
    }

    fn has_output(&self) -> bool
    {
        match self
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
            Op::AdjBase => false,
        }
    }
}

enum AddrMode
{
    Position,
    Immediate,
    Relative,
}

impl AddrMode
{
    fn from_i64(mode: i64) -> Self
    {
        match mode
        {
            0 => AddrMode::Position,
            1 => AddrMode::Immediate,
            2 => AddrMode::Relative,
            _ => panic!("Invalid Addressing Mode"),
        }
    }
}

#[derive(Clone)]
pub struct Program
{
    tape: Vec<i64>,
    istream: VecDeque<i64>,
    pc: usize,
    base: i64,
    halted: bool,
    awaiting_input: bool,
}

impl Program
{
    pub fn from_tape(tape: Vec<i64>) -> Self
    {
        Program { tape: tape, istream: VecDeque::new(), pc: 0, base: 0, halted: false, awaiting_input: false }
    }

    pub fn push_input(&mut self, input: i64)
    {
        self.istream.push_back(input);
        self.awaiting_input = false;
    }

    pub fn needs_input(&self) -> bool 
    { 
        self.awaiting_input 
    }

    pub fn read_at(&self, index: usize) -> i64
    {
        self.tape[index]
    }
}

impl Iterator for Program
{
    type Item = i64;
    fn next(&mut self) -> Option<i64>
    {
        if self.halted || self.awaiting_input { return None }
        loop
        {
            // read opcode
            let mut opcode = self.tape[self.pc];
            let op = Op::from_i64(opcode % 100);
            opcode /= 100;
            self.pc += 1;

            // read params
            let mut params : [i64; 2] = [0;2];
            for i in 0..op.num_params()
            {
                params[i] = match AddrMode::from_i64(opcode % 10)
                            { 
                                AddrMode::Position => self.tape[self.tape[self.pc] as usize], 
                                AddrMode::Immediate => self.tape[self.pc] ,
                                AddrMode::Relative => self.tape[(self.tape[self.pc] + self.base) as usize],
                            };
                opcode /= 10;
                self.pc += 1;
            }

            // read destination
            let out_index = if op.has_output()
                            { 
                                let index = match AddrMode::from_i64(opcode % 10) 
                                            { 
                                                AddrMode::Position => self.tape[self.pc],
                                                AddrMode::Relative => self.tape[self.pc] + self.base,
                                                AddrMode::Immediate => panic!("Cannot Access Destination Param By Immediate Mode"),
                                            };
                                self.pc += 1;
                                index
                            } 
                            else { 0 } as usize;
        
            match op
            {
                Op::Add => self.tape[out_index] = params[0] + params[1],
                Op::Mul => self.tape[out_index] = params[0] * params[1],
                Op::In =>  if let Some(input) = self.istream.pop_front() { self.tape[out_index] = input; }
                           else { self.awaiting_input = true; self.pc -= 2; return None},
                Op::Out => return Some(params[0]),
                Op::Tjmp => if params[0] != 0 { self.pc = params[1] as usize; },
                Op::Fjmp => if params[0] == 0 { self.pc = params[1] as usize; },
                Op::LessThan => self.tape[out_index] = if params[0] < params[1] { 1 } else { 0 },
                Op::Equals => self.tape[out_index] = if params[0] == params[1] { 1 } else { 0 },
                Op::AdjBase => self.base += params[0],
                Op::Halt => { self.halted = true; return None },
            }
        }
    }
}