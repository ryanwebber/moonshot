
pub struct Counter<T>(pub T) where T: std::ops::AddAssign<usize> + Clone;

impl<T> Counter<T> where T: std::ops::AddAssign<usize> + Clone {
    pub fn new(value: T) -> Self {
        Self(value)
    }

    pub fn next(&mut self) -> Self {
        let n = Self(self.0.clone());
        self.0 += 1;
        n
    }    
}

impl<T> Clone for Counter<T> where T: std::ops::AddAssign<usize> + Clone {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub trait IdentifierAllocator<T> {
    fn generate_id(&mut self) -> T;
}

impl<T> IdentifierAllocator<T> for Counter<T> where T: std::ops::AddAssign<usize> + Clone {
    fn generate_id(&mut self) -> T {
        self.next().0
    }
}
