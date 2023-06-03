use std::collections::HashMap;

/// A single scope within the scope stack.
#[derive(Debug, Clone)]
pub struct Scope<T> {
    symbols: HashMap<String, T>,
}

impl<T> Scope<T>
where
    T: Clone,
{
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    /// Insert a symbol with a value into the current scope.
    pub fn insert(&mut self, name: &str, value: T) {
        self.symbols.insert(name.to_string(), value);
    }

    /// Try to resolve a symbol in the current scope.
    pub fn resolve(&self, name: &str) -> Option<T> {
        self.symbols.get(name).cloned()
    }
}

impl<T> Default for Scope<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

/// A stack of scopes. The top of the stack is the current scope.
#[derive(Debug, Clone)]
pub struct ScopeStack<T> {
    stack: Vec<Scope<T>>,
}

impl<T> ScopeStack<T>
where
    T: Clone,
{
    pub fn new() -> Self {
        Self {
            stack: vec![Scope::new()],
        }
    }

    /// Try to resolve a symbol in the stack of scopes.
    pub fn resolve(&self, name: &str) -> Option<T> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.resolve(name))
    }

    pub fn insert(&mut self, name: &str, value: T) {
        self.stack
            .last_mut()
            .unwrap()
            .symbols
            .insert(name.to_string(), value);
    }

    pub fn push_scope(&mut self) {
        self.stack.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.stack.pop();
    }
}

impl<T> Default for ScopeStack<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
