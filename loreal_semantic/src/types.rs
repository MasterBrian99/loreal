use loreal_ast::*;
use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConcreteType {
    Int,
    Float,
    Bool,
    Char,
    String,
    Nil,
    List(Box<ConcreteType>),
    Tuple(Vec<ConcreteType>),
    Function {
        params: Vec<ConcreteType>,
        return_type: Box<ConcreteType>,
    },
    Named(SmolStr),
    Unknown,
}

impl ConcreteType {
    fn from_ast_type(ty: &Type) -> Self {
        match ty {
            Type::Named { name, .. } => match name.as_str() {
                "Int" => ConcreteType::Int,
                "Float" => ConcreteType::Float,
                "Bool" => ConcreteType::Bool,
                "Char" => ConcreteType::Char,
                "String" => ConcreteType::String,
                "Nil" => ConcreteType::Nil,
                _ => ConcreteType::Named(name.clone()),
            },
            Type::List { element_type, .. } => {
                ConcreteType::List(Box::new(Self::from_ast_type(element_type)))
            }
            Type::Tuple { types, .. } => {
                ConcreteType::Tuple(types.iter().map(Self::from_ast_type).collect())
            }
            Type::Function {
                params,
                return_type,
                ..
            } => ConcreteType::Function {
                params: params.iter().map(Self::from_ast_type).collect(),
                return_type: Box::new(Self::from_ast_type(return_type)),
            },
            Type::Map { .. } => ConcreteType::Unknown,
        }
    }

    fn is_compatible(&self, other: &ConcreteType) -> bool {
        match (self, other) {
            (ConcreteType::Unknown, _) | (_, ConcreteType::Unknown) => true,
            (ConcreteType::Int, ConcreteType::Int) => true,
            (ConcreteType::Float, ConcreteType::Float) => true,
            (ConcreteType::Bool, ConcreteType::Bool) => true,
            (ConcreteType::Char, ConcreteType::Char) => true,
            (ConcreteType::String, ConcreteType::String) => true,
            (ConcreteType::Nil, ConcreteType::Nil) => true,
            (ConcreteType::List(a), ConcreteType::List(b)) => a.is_compatible(b),
            (ConcreteType::Tuple(a), ConcreteType::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.is_compatible(y))
            }
            (
                ConcreteType::Function {
                    params: p1,
                    return_type: r1,
                },
                ConcreteType::Function {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(x, y)| x.is_compatible(y))
                    && r1.is_compatible(r2)
            }
            (ConcreteType::Named(a), ConcreteType::Named(b)) => a == b,
            _ => false,
        }
    }

    fn to_string(&self) -> String {
        match self {
            ConcreteType::Int => "Int".to_string(),
            ConcreteType::Float => "Float".to_string(),
            ConcreteType::Bool => "Bool".to_string(),
            ConcreteType::Char => "Char".to_string(),
            ConcreteType::String => "String".to_string(),
            ConcreteType::Nil => "Nil".to_string(),
            ConcreteType::List(elem) => format!("[{}]", elem.to_string()),
            ConcreteType::Tuple(types) => {
                let type_strs: Vec<String> = types.iter().map(|t| t.to_string()).collect();
                format!("({})", type_strs.join(", "))
            }
            ConcreteType::Function {
                params,
                return_type,
            } => {
                let param_strs: Vec<String> = params.iter().map(|t| t.to_string()).collect();
                format!("({}) -> {}", param_strs.join(", "), return_type.to_string())
            }
            ConcreteType::Named(name) => name.to_string(),
            ConcreteType::Unknown => "?".to_string(),
        }
    }

    pub fn to_ast_type(&self) -> Type {
        match self {
            ConcreteType::Int => Type::Named {
                name: "Int".into(),
                span: Span::new(0, 0),
            },
            ConcreteType::Float => Type::Named {
                name: "Float".into(),
                span: Span::new(0, 0),
            },
            ConcreteType::Bool => Type::Named {
                name: "Bool".into(),
                span: Span::new(0, 0),
            },
            ConcreteType::Char => Type::Named {
                name: "Char".into(),
                span: Span::new(0, 0),
            },
            ConcreteType::String => Type::Named {
                name: "String".into(),
                span: Span::new(0, 0),
            },
            ConcreteType::Nil => Type::Named {
                name: "Nil".into(),
                span: Span::new(0, 0),
            },
            ConcreteType::List(elem) => Type::List {
                element_type: Box::new(elem.to_ast_type()),
                span: Span::new(0, 0),
            },
            ConcreteType::Tuple(types) => Type::Tuple {
                types: types.iter().map(|t| t.to_ast_type()).collect(),
                span: Span::new(0, 0),
            },
            ConcreteType::Function {
                params,
                return_type,
            } => Type::Function {
                params: params.iter().map(|t| t.to_ast_type()).collect(),
                return_type: Box::new(return_type.to_ast_type()),
                span: Span::new(0, 0),
            },
            ConcreteType::Named(name) => Type::Named {
                name: name.clone(),
                span: Span::new(0, 0),
            },
            ConcreteType::Unknown => Type::Named {
                name: "Unknown".into(),
                span: Span::new(0, 0),
            },
        }
    }
}
