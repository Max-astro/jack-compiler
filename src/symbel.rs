use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
};

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Kind {
    Static,
    Field,
    Arg,
    Var,
    None,
}

#[derive(Clone, Debug)]
struct Symbel {
    name: String,
    vtype: String,
    kind: Kind,
    index: usize,
}

impl Symbel {
    pub fn new(name: String, vtype: String, kind: Kind, index: usize) -> Self {
        Symbel {
            name,
            vtype,
            kind,
            index,
        }
    }

    pub fn vtype(&self) -> String {
        self.vtype.clone()
    }

    pub fn kind(&self) -> Kind {
        self.kind.clone()
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

type Scope = Rc<RefCell<HashMap<String, Symbel>>>;
type ScopeList = VecDeque<Scope>;

pub struct SymbelTable {
    class_scope: Scope,
    subroutine_scope: Scope,
    subroutine_list: ScopeList,
    field_count: usize,
    static_count: usize,
    arg_count: usize,
    var_count: usize,
}

impl SymbelTable {
    fn new_scope() -> Scope {
        Rc::new(RefCell::new(HashMap::new()))
    }

    pub fn new() -> Self {
        let sub = SymbelTable::new_scope();
        let mut deq = VecDeque::new();
        deq.push_back(sub.clone());

        SymbelTable {
            class_scope: SymbelTable::new_scope(),
            subroutine_scope: sub,
            subroutine_list: deq,
            field_count: 0,
            static_count: 0,
            arg_count: 0,
            var_count: 0,
        }
    }

    pub fn start_subroutine(&mut self) {
        let new = SymbelTable::new_scope();
        self.subroutine_list.push_back(new.clone());
        self.subroutine_scope = new;
    }

    pub fn pop_subroutine(&mut self) {
        self.subroutine_scope = self.subroutine_list.pop_back().unwrap();
    }

    pub fn var_count(&self, kind: Kind) -> usize {
        match kind {
            Kind::Arg => self.arg_count,
            Kind::Var => self.var_count,
            Kind::Field => self.field_count,
            Kind::Static => self.static_count,
            _ => 0,
        }
    }

    pub fn kind_of(&self, name: String) -> Kind {
        if let Some(typ) = self.subroutine_scope.borrow_mut().get(&name) {
            typ.kind()
        } else if let Some(typ) = self.class_scope.borrow_mut().get(&name) {
            typ.kind()
        } else {
            Kind::None
        }
    }

    pub fn index_of(&self, name: String) -> usize {
        if let Some(typ) = self.subroutine_scope.borrow_mut().get(&name) {
            typ.index()
        } else if let Some(typ) = self.class_scope.borrow_mut().get(&name) {
            typ.index()
        } else {
            panic!("{} not defined!", name);
        }
    }

    pub fn type_of(&self, name: String) -> String {
        if let Some(typ) = self.subroutine_scope.borrow_mut().get(&name) {
            typ.vtype()
        } else if let Some(typ) = self.class_scope.borrow_mut().get(&name) {
            typ.vtype()
        } else {
            panic!("{} not defined!", name);
        }
    }

    pub fn define(&mut self, name: String, vtype: String, kind: Kind) {
        match kind {
            Kind::Arg => {
                self.arg_count += 1;
                let index = self.arg_count;
                self.subroutine_scope.borrow_mut().insert(
                    name.clone(),
                    Symbel {
                        name,
                        vtype,
                        kind,
                        index,
                    },
                );
            }
            Kind::Var => {
                self.var_count += 1;
                let index = self.var_count;
                self.subroutine_scope.borrow_mut().insert(
                    name.clone(),
                    Symbel {
                        name,
                        vtype,
                        kind,
                        index,
                    },
                );
            }
            Kind::Field => {
                self.field_count += 1;
                let index = self.field_count;
                self.class_scope.borrow_mut().insert(
                    name.clone(),
                    Symbel {
                        name,
                        vtype,
                        kind,
                        index,
                    },
                );
            }
            Kind::Static => {
                self.static_count += 1;
                let index = self.static_count;
                self.class_scope.borrow_mut().insert(
                    name.clone(),
                    Symbel {
                        name,
                        vtype,
                        kind,
                        index,
                    },
                );
            }
            _ => panic!("None type can not define!"),
        }
    }
}
