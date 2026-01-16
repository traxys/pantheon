use std::{
    hash::Hash,
    ops::{Add, Sub},
};

use crate::Source;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub source: Source,
}

impl Location {
    pub fn render_context(&self, context: usize, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}:\n", self.source.path.to_string_lossy())?;

        let start_line = self
            .source
            .lines
            .iter()
            .enumerate()
            .find_map(|(i, &line)| if self.start <= line { Some(i) } else { None })
            .unwrap()
            .saturating_sub(context + 1);

        let end_line = self
            .source
            .lines
            .iter()
            .enumerate()
            .find_map(|(i, &line)| if self.end <= line { Some(i) } else { None })
            .unwrap()
            .sub(1)
            .add(context)
            .min(self.source.lines.len());

        let span_range = self.start..self.end;

        // FIXME: compute the tab width
        let tab_width = 8;
        let prefix = "  ";

        for &line_start in &self.source.lines[start_line..=end_line] {
            let line = self.source.data[line_start..].lines().nth(0).unwrap();
            let line_end = line_start + line.len();
            writeln!(f, "{prefix}{line}")?;
            let line_range = line_start..line_end;
            if line_range.contains(&self.start) {
                write!(f, "{prefix}")?;
                for (i, c) in line.char_indices() {
                    let fill = if span_range.contains(&(i + line_start)) {
                        '^'
                    } else {
                        ' '
                    };

                    for _ in 0..(if c == '\t' {
                        tab_width - ((i + prefix.len()) % tab_width)
                    } else {
                        1
                    }) {
                        write!(f, "{fill}")?;
                    }
                }
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct SpannedValue<T> {
    pub location: Location,
    pub v: T,
}

impl<T: PartialEq> PartialEq for SpannedValue<T> {
    fn eq(&self, other: &Self) -> bool {
        self.v == other.v
    }
}

impl<T: Eq> Eq for SpannedValue<T> {}

impl<T: Hash> Hash for SpannedValue<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.v.hash(state);
    }
}

impl<T> SpannedValue<T> {
    pub fn span(&self) -> Location {
        self.location.clone()
    }
}

impl<T: ToOwned + ?Sized> SpannedValue<&T> {
    pub fn map_to_owned(self) -> SpannedValue<T::Owned> {
        SpannedValue {
            v: self.v.to_owned(),
            location: self.location,
        }
    }
}

impl<T> From<SpannedValue<T>> for Location {
    fn from(value: SpannedValue<T>) -> Self {
        value.location
    }
}

pub trait Spanned: Sized {
    fn spanned<W: Into<Location>>(self, span: W) -> SpannedValue<Self>;
}

impl<T> Spanned for T {
    fn spanned<W: Into<Location>>(self, span: W) -> SpannedValue<Self> {
        SpannedValue {
            location: span.into(),
            v: self,
        }
    }
}
