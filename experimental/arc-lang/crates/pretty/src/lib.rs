use std::fmt;
use std::io;
use std::io::LineWriter;
use std::io::Write;

pub struct Pretty<W: Write> {
    writer: LineWriter<W>,
    indent: usize,
}

impl Pretty<io::Stderr> {
    pub fn print() -> Self {
        Self::new(io::stderr())
    }
}

impl Pretty<Vec<u8>> {
    pub fn text() -> Self {
        Self::new(Vec::new())
    }
}

impl<W: Write> Pretty<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer: LineWriter::new(writer),
            indent: 0,
        }
    }

    pub fn flush(&mut self) {
        self.writer.flush().unwrap();
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn dedent(&mut self) {
        self.indent -= 1;
    }

    pub fn lit(&mut self, s: &str) -> io::Result<()> {
        self.writer.write_all(s.as_bytes())
    }

    pub fn fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        self.writer.write_fmt(args)
    }

    pub fn newline(&mut self) -> io::Result<()> {
        self.lit("\n")?;
        for _ in 0..self.indent {
            self.lit("    ")?;
        }
        Ok(())
    }

    pub fn opt<T>(
        &mut self,
        x: &Option<T>,
        f: impl FnOnce(&mut Self, &T) -> io::Result<()>,
    ) -> io::Result<()> {
        if let Some(x) = x {
            f(self, x)?;
        }
        Ok(())
    }

    pub fn sep<'a, T: Clone + 'a>(
        &mut self,
        v: impl IntoIterator<Item = &'a T>,
        sep: &str,
        f: impl Fn(&mut Self, &'a T) -> io::Result<()>,
    ) -> io::Result<()> {
        let mut iter = v.into_iter().peekable();
        while let Some(x) = iter.next() {
            f(self, x)?;
            if iter.peek().is_some() {
                self.lit(sep)?;
            }
        }
        Ok(())
    }

    pub fn seq<'a, T: Clone + 'a>(
        &mut self,
        v: impl IntoIterator<Item = &'a T>,
        f: impl Fn(&mut Self, &'a T) -> io::Result<()>,
    ) -> io::Result<()> {
        self.sep(v, ", ", f)
    }

    pub fn delim(
        &mut self,
        l: &str,
        r: &str,
        f: impl FnOnce(&mut Self) -> io::Result<()>,
    ) -> io::Result<()> {
        self.lit(l)?;
        f(self)?;
        self.lit(r)?;
        Ok(())
    }

    pub fn brace(&mut self, f: impl FnOnce(&mut Self) -> io::Result<()>) -> io::Result<()> {
        self.delim("{", "}", f)
    }

    pub fn paren(&mut self, f: impl FnOnce(&mut Self) -> io::Result<()>) -> io::Result<()> {
        self.delim("(", ")", f)
    }

    pub fn brack(&mut self, f: impl FnOnce(&mut Self) -> io::Result<()>) -> io::Result<()> {
        self.delim("[", "]", f)
    }

    pub fn angle(&mut self, f: impl FnOnce(&mut Self) -> io::Result<()>) -> io::Result<()> {
        self.delim("<", ">", f)
    }
}
