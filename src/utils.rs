use std::{ops::AddAssign, str::Utf8Error};

pub struct Counter<T>(pub T)
where
    T: std::ops::AddAssign<usize> + Clone;

impl<T> Counter<T>
where
    T: std::ops::AddAssign<usize> + Clone,
{
    pub fn new(value: T) -> Self {
        Self(value)
    }

    pub fn next(&mut self) -> Self {
        let n = Self(self.0.clone());
        self.0 += 1;
        n
    }
}

impl<T> AddAssign<usize> for Counter<T>
where
    T: std::ops::AddAssign<usize> + Clone,
{
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl<T> Clone for Counter<T>
where
    T: std::ops::AddAssign<usize> + Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub trait IdentifierAllocator<T> {
    fn generate_id(&mut self) -> T;
}

impl<T> IdentifierAllocator<T> for Counter<T>
where
    T: std::ops::AddAssign<usize> + Clone,
{
    fn generate_id(&mut self) -> T {
        self.next().0
    }
}

pub struct StringWriter {
    buf: Vec<u8>,
}

impl StringWriter {
    pub fn new() -> Self {
        Self { buf: Vec::new() }
    }

    pub fn with<F, E>(f: F) -> Result<String, E>
    where
        F: FnOnce(&mut Self) -> Result<(), E>,
    {
        let mut writer = Self::new();
        f(&mut writer)?;
        Ok(writer
            .as_str()
            .expect("Unable to interpret buffer as utf-8 string")
            .to_string())
    }

    pub fn as_str(&mut self) -> Result<&str, Utf8Error> {
        std::str::from_utf8(self.buf.as_slice())
    }
}

impl std::io::Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buf.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.buf.flush()
    }
}
