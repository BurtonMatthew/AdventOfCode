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