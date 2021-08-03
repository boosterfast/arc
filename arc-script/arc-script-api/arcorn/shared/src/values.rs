/// Get value from variable.
///
/// ```
/// let a = 5;
/// let b = arcorn::val!(a);
/// ```
#[macro_export]
macro_rules! val {
    ($arg:expr) => {
        $arg.clone()
    };
}
