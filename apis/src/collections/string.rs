use core::fmt;

use crate::Allocator;

use super::{ApisError, Vec};

/// An UTF-8 encoded, growable string
pub struct String<'a> {
    vec: Vec<'a, u8>,
}

impl<'a> String<'a> {
    pub fn new(alloc: &'a Allocator) -> Self {
        Self {
            vec: Vec::new(alloc),
        }
    }

    pub fn push_str(&mut self, s: &str) -> Result<(), ApisError> {
        self.vec.extend_from_slice_copy(s.as_bytes())
    }
}

impl<'a> fmt::Write for String<'a> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s).map_err(|_| fmt::Error)
    }
}

impl<'bump> fmt::Display for String<'bump> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<'a> fmt::Debug for String<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<'a> core::ops::Deref for String<'a> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        unsafe { core::str::from_utf8_unchecked(&self.vec) }
    }
}

impl<'a> PartialEq for String<'a> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        PartialEq::eq(&self[..], &other[..])
    }
}

#[macro_export]
macro_rules! format {
    (in $alloc:expr, $fmt:expr, $($args:expr),* $(,)?) => {{
        use core::fmt::Write;
        let mut s = $crate::collections::String::new($alloc);
        match write!(&mut s, $fmt, $($args),*) {
            Err(_) => Err($crate::collections::ApisError::Fmt),
            Ok(_) => Ok(s),
        }
    }};
}

macro_rules! impl_eq {
    ($($lhs:ty, $rhs: ty);*  $(;)?) => {
        $(
        impl<'o, 'a> PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                PartialEq::eq(&self[..], &other[..])
            }
        }

        impl<'o, 'b, 'a> PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                PartialEq::eq(&self[..], &other[..])
            }
        }
        )*
    };
}

impl_eq! {
    String<'a>, str;
    String<'a>, &'o str;
}

#[cfg(test)]
mod test {
    use crate::{mem_test, Allocator};

    #[macro_rules_attribute::apply(mem_test)]
    fn fmt(a: &mut Allocator) {
        assert_eq!(crate::format!(in a, "{}{}", "abc", 1).unwrap(), "abc1");
    }
}
