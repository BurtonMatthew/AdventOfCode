#![allow(dead_code)]
use std::ops::{Index, IndexMut};
use std::str::FromStr;

#[derive(Clone,std::cmp::PartialEq)]
pub struct Vec2<T>
{
    data: Vec<T>,
    width: usize
}

impl<T> Index<usize> for Vec2<T>
{
    type Output = [T];

    fn index(&self, row: usize) -> &[T]
    {
        assert!(row < self.height());
        let row_start = row * self.width;
        &self.data[row_start .. row_start + self.width]
    }
}

impl<T> Index<(usize, usize)> for Vec2<T>
{
    type Output = T;

    fn index(&self, pos: (usize, usize)) -> &T
    {
        &self[pos.1][pos.0]
    }
}

impl<T> IndexMut<usize> for Vec2<T>
{
    fn index_mut(&mut self, row: usize) -> &mut [T]
    {
        assert!(row < self.height());
        let row_start = row * self.width;
        &mut self.data[row_start .. row_start + self.width]
    }
}

impl<T> IndexMut<(usize, usize)> for Vec2<T>
{
    fn index_mut(&mut self, pos: (usize, usize)) -> &mut T
    {
        &mut self[pos.1][pos.0]
    }
}

impl FromStr for Vec2<u8>
{
    type Err = ();
    fn from_str(s: &str) -> Result<Vec2<u8>, ()> 
    {
        Ok(Vec2::from_vec(s.bytes().filter(|c| *c != '\n' as u8 && *c != '\r' as u8).collect(), s.lines().next().unwrap().len()))
    }
}

impl FromStr for Vec2<char>
{
    type Err = ();
    fn from_str(s: &str) -> Result<Vec2<char>, ()> 
    {
        Ok(Vec2::from_vec(s.chars().filter(|c| *c != '\n' && *c != '\r').collect(), s.lines().next().unwrap().len()))
    }
}

impl<T> Vec2<T>
{
    pub fn new() -> Self
    {
        Self { data: Vec::new(), width: 0 }
    }

    pub fn with_capacity(width: usize, height: usize) -> Self
    {
        Self { data: Vec::with_capacity(width * height), width: width }
    }

    pub fn from_vec(data : Vec<T>, width: usize) -> Self
    {
        assert!(data.len() % width == 0);
        Self { data: data, width: width }
    }

    pub fn width(&self) -> usize
    {
        self.width
    }

    pub fn height(&self) -> usize
    {
        self.data.len() / self.width
    }

    pub fn get(&self, pos: (usize, usize)) -> Option<&T>
    {
        if pos.0 < self.width && pos.1 < self.height() { Some(&self[pos]) }
        else { None }
    }

    pub fn push_row(&mut self, row: Vec<T>) 
    {
        if self.width > 0 
        {
            assert_eq!(self.width, row.len());
        } 
        else 
        {
            self.width = row.len();
        }
        self.data.extend(row);
    }

    pub fn rows(&self) -> RowIter<T>
    {
        RowIter { vec: self, index: 0 }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T>
    {
        self.data.iter()
    }

    pub fn neighbors8_with_padding<'a>(&'a self, padding: &'a T) -> Neighborhood8Iter<T>
    {
        Neighborhood8Iter { vec: self, x: 0, y: 0, padding }
    }
}

pub struct RowIter<'a, T:'a>
{
    vec: &'a Vec2<T>,
    index: usize
}

impl<'a, T> Iterator for RowIter<'a, T>
{
    type Item = &'a[T];

    fn next(&mut self) -> Option<&'a[T]>
    {
        if self.index < self.vec.height()
        {
            let result = Some(&self.vec[self.index]);
            self.index += 1;
            result
        }
        else
        {
            None
        }
    }
}

pub struct Neighborhood8Iter<'a, T:'a>
{
    vec: &'a Vec2<T>,
    padding: &'a T,
    x: usize,
    y: usize
}

impl<'a, T> Iterator for Neighborhood8Iter<'a, T>
{
    type Item = (&'a T, [&'a T; 8]);

    fn next(&mut self) -> Option<Self::Item>
    {
        if (self.y as usize) < self.vec.height()
        {
            let is_left_edge = self.x == 0;
            let is_right_edge = self.x == self.vec.width()-1;
            let is_top_edge = self.y == 0;
            let is_bot_edge = self.y == self.vec.height()-1;

            let top =       if !is_top_edge { &self.vec[self.y-1][self.x] } else { self.padding };
            let top_right = if !is_top_edge && !is_right_edge { &self.vec[self.y-1][self.x+1] } else { self.padding };
            let right =     if !is_right_edge { &self.vec[self.y][self.x+1] } else { self.padding };
            let bot_right = if !is_bot_edge &&!is_right_edge { &self.vec[self.y+1][self.x+1] } else { self.padding };
            let bot =       if !is_bot_edge { &self.vec[self.y+1][self.x] } else { self.padding };
            let bot_left =  if !is_bot_edge && !is_left_edge { &self.vec[self.y+1][self.x-1] } else { self.padding };
            let left =      if !is_left_edge { &self.vec[self.y][self.x-1] } else { self.padding };
            let top_left =  if !is_top_edge && !is_left_edge { &self.vec[self.y-1][self.x-1] } else { self.padding };

            let result = Some((&self.vec[self.y][self.x], [top, top_right, right, bot_right, bot, bot_left, left, top_left]));

            if is_right_edge
            {
                self.x = 0;
                self.y += 1;
            }
            else
            {
                self.x += 1;
            }
            result
        }
        else
        {
            None
        }
    }
}