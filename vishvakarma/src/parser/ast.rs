use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    MalformedModule, Source,
    parser::{
        TokenError, UnexpectedToken,
        span::{Location, SpannedValue},
    },
};

use super::token::{TokenKind, TokenParser};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetKind {
    Executable,
    Library,
    ProcMacro,
    Test,
    BareMetalBin,
}

impl TargetKind {
    fn is_directive_supported(self, directive: Directive) -> bool {
        match directive {
            Directive::Default => {
                matches!(self, Self::Executable | Self::Library | Self::BareMetalBin)
            }
            Directive::Main => matches!(self, Self::Executable | Self::BareMetalBin),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Directive {
    Default,
    Main,
}

#[derive(Debug, Clone)]
pub struct TargetExpr {
    pub directives: Vec<Directive>,
    pub kind: TargetKind,
    pub args: Arguments,
}

#[derive(Debug, Clone)]
pub enum Expression {
    String(Rc<str>),
    Array(Vec<Rc<SpannedValue<Expression>>>),
    Identifier(ItemPath),
    Target(TargetExpr),
}

#[derive(Debug)]
pub enum ParseError {
    Token(TokenError),
    UnknownTarget { name: String, location: Location },
    DuplicateDirective { name: String, location: Location },
    UnsupportedDirective { name: String, location: Location },
    DuplicateArgument { name: String, location: Location },
    DuplicateModule { name: String, location: Location },
    MalformedModule(MalformedModule),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Token(_) => write!(f, "Invalid token"),
            ParseError::UnknownTarget { name, location } => {
                writeln!(f, "Unknown target `{name}`")?;

                location.render_context(1, f)
            }
            ParseError::DuplicateDirective { name, location } => {
                writeln!(f, "Duplicate directive `{name}`")?;

                location.render_context(1, f)
            }
            ParseError::UnsupportedDirective { name, location } => {
                writeln!(f, "Directive `{name}` is not supported for this statement")?;

                location.render_context(1, f)
            }
            ParseError::DuplicateArgument { name, location } => {
                writeln!(f, "Duplicate argument `{name}`")?;

                location.render_context(1, f)
            }
            ParseError::DuplicateModule { name, location } => {
                writeln!(f, "Duplicate module `{name}`")?;

                location.render_context(1, f)
            }
            ParseError::MalformedModule(_) => write!(f, "Could not read module"),
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseError::Token(token_error) => Some(token_error),
            ParseError::MalformedModule(e) => Some(e),
            _ => None,
        }
    }
}

impl From<TokenError> for ParseError {
    fn from(value: TokenError) -> Self {
        Self::Token(value)
    }
}

impl From<MalformedModule> for ParseError {
    fn from(value: MalformedModule) -> Self {
        Self::MalformedModule(value)
    }
}

impl Expression {
    fn parse(
        tokens: &mut TokenParser,
        root: &ItemPath,
        directives: &[SpannedValue<&str>],
    ) -> Result<SpannedValue<Self>, ParseError> {
        tokens.parse_seq(|tokens| {
            let token = tokens.next()?;
            let mut peeker = tokens.peek_cursor();

            match token.kind {
                TokenKind::PathSep => {
                    let mut path = Vec::new();

                    loop {
                        let part = tokens.next_identifier("path expression")?;
                        path.push(part.v.to_owned());

                        let mut peeker = tokens.peek_cursor();
                        match peeker.peek()? {
                            TokenKind::PathSep => tokens.next().unwrap(),
                            _ => break,
                        };
                    }

                    Ok(Expression::Identifier(ItemPath(Rc::from(path))))
                }
                TokenKind::String => Ok(Expression::String(token.escaped_string().into())),
                TokenKind::LBracket => {
                    let mut content = Vec::new();

                    loop {
                        let mut peeker = tokens.peek_cursor();
                        match peeker.peek()? {
                            TokenKind::RBracket => {
                                tokens.assert_next(TokenKind::RBracket, "array").unwrap();
                                break;
                            }
                            _ => content.push(Rc::new(Expression::parse(tokens, root, &[])?)),
                        }

                        let token = tokens.next()?;
                        match token.kind {
                            TokenKind::Comma => continue,
                            TokenKind::RBracket => break,
                            _ => {
                                return Err(ParseError::Token(TokenError::Unexpected(
                                    UnexpectedToken::at(
                                        token,
                                        vec![TokenKind::Comma, TokenKind::RBracket],
                                        "array expression".to_string(),
                                    ),
                                )));
                            }
                        };
                    }

                    Ok(Expression::Array(content))
                }
                TokenKind::Identifier => {
                    if peeker.peek()? == TokenKind::LBrace {
                        let mut supplied_directives = HashSet::new();

                        let kind = match token.value.v {
                            "executable" => TargetKind::Executable,
                            "library" => TargetKind::Library,
                            "proc-macro" => TargetKind::ProcMacro,
                            "test" => TargetKind::Test,
                            "bare-metal" => TargetKind::BareMetalBin,
                            _ => {
                                return Err(ParseError::UnknownTarget {
                                    name: token.value.v.to_owned(),
                                    location: token.value.span(),
                                });
                            }
                        };

                        for directive in directives {
                            let parsed = match directive.v {
                                "@main" => Directive::Main,
                                "@def" => Directive::Default,
                                _ => {
                                    return Err(ParseError::UnsupportedDirective {
                                        name: directive.v.to_string(),
                                        location: directive.span(),
                                    });
                                }
                            };

                            if !kind.is_directive_supported(parsed) {
                                return Err(ParseError::UnsupportedDirective {
                                    name: directive.v.to_string(),
                                    location: directive.span(),
                                });
                            };

                            if !supplied_directives.insert(parsed) {
                                return Err(ParseError::DuplicateDirective {
                                    name: directive.v.to_string(),
                                    location: directive.span(),
                                });
                            }
                        }

                        tokens.assert_next(TokenKind::LBrace, "target expresssion")?;
                        let args = Arguments::parse(tokens, root)?;
                        tokens.assert_next(TokenKind::RBrace, "target expresssion")?;
                        Ok(Expression::Target(TargetExpr {
                            directives: supplied_directives.into_iter().collect(),
                            kind,
                            args,
                        }))
                    } else {
                        Ok(Expression::Identifier(root.join(token.value.v.to_string())))
                    }
                }
                _ => Err(TokenError::Unexpected(UnexpectedToken::at(
                    token,
                    vec![
                        TokenKind::PathSep,
                        TokenKind::String,
                        TokenKind::LBracket,
                        TokenKind::Identifier,
                    ],
                    "expresssion".to_owned(),
                ))
                .into()),
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct Arguments(Rc<HashMap<SpannedValue<String>, Rc<SpannedValue<Expression>>>>);

impl Arguments {
    pub fn iter(&self) -> impl Iterator<Item = (&SpannedValue<String>, &SpannedValue<Expression>)> {
        self.0.iter().map(|(k, v)| (k, &**v))
    }

    fn parse(tokens: &mut TokenParser, root: &ItemPath) -> Result<Self, ParseError> {
        let mut args = HashMap::new();

        loop {
            let mut peeker = tokens.peek_cursor();
            if peeker.peek()? == TokenKind::RBrace {
                break;
            }

            let name = tokens.next_identifier("arguments")?.map_to_owned();
            if args.contains_key(&name) {
                return Err(ParseError::DuplicateArgument {
                    name: name.v.to_owned(),
                    location: name.location,
                });
            }

            tokens.assert_next(TokenKind::Colon, "arguments")?;

            let value = Expression::parse(tokens, root, &[])?;
            args.insert(name, value.into());

            let mut peeker = tokens.peek_cursor();
            if peeker.peek()? != TokenKind::Comma {
                break;
            }

            assert_eq!(tokens.next().unwrap().kind, TokenKind::Comma);
        }

        Ok(Self(Rc::new(args)))
    }
}

#[derive(Debug)]
pub enum Statement {
    Module(String),
    Assign {
        name: String,
        value: Rc<SpannedValue<Expression>>,
    },
    Expr(SpannedValue<Expression>),
}

impl Statement {
    fn parse<'a>(
        tokens: &mut TokenParser<'a>,
        root: &ItemPath,
        mut directives: Vec<SpannedValue<&'a str>>,
    ) -> Result<SpannedValue<Self>, ParseError> {
        tokens.parse_seq(|tokens| -> Result<_, ParseError> {
            let mut peeker = tokens.peek_cursor();

            match peeker.peek()? {
                TokenKind::Directive => {
                    let token = tokens.next().unwrap();

                    directives.push(token.value);

                    Ok(Statement::parse(tokens, root, directives)?.v)
                }
                TokenKind::Module => {
                    if let Some(directive) = directives.first() {
                        return Err(ParseError::UnsupportedDirective {
                            name: directive.v.to_string(),
                            location: directive.span(),
                        });
                    }

                    // mod keyword
                    tokens.next().unwrap();

                    let name = tokens.next_identifier("module statement")?;
                    tokens.assert_next(TokenKind::Semicolon, "module statement")?;
                    Ok(Statement::Module(name.v.to_owned()))
                }
                TokenKind::Identifier => {
                    if peeker.peek()? == TokenKind::Equal {
                        let token = tokens.next().unwrap().value;
                        let name = token.v.to_string();

                        tokens.next().unwrap(); // Skip the equal sign

                        Ok(Statement::Assign {
                            name,
                            value: Rc::new(Expression::parse(tokens, root, &directives)?),
                        })
                    } else {
                        Ok(Statement::Expr(Expression::parse(
                            tokens,
                            root,
                            &directives,
                        )?))
                    }
                }
                _ => Ok(Statement::Expr(Expression::parse(
                    tokens,
                    root,
                    &directives,
                )?)),
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemPath(Rc<[String]>);

impl std::fmt::Display for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for path in &*self.0 {
            write!(f, "::{path}")?;
        }
        Ok(())
    }
}

impl ItemPath {
    pub fn join(&self, child: String) -> ItemPath {
        let mut path = self.0.to_vec();
        path.push(child);
        path.into()
    }

    pub fn append(&self, mut sub_path: Vec<String>) -> ItemPath {
        let mut path = self.0.to_vec();
        path.append(&mut sub_path);
        path.into()
    }

    pub fn parts(&self) -> (&[impl Deref<Target = str>], &str) {
        (&self.0[0..self.0.len() - 1], self.0.last().unwrap())
    }
}

impl From<Vec<String>> for ItemPath {
    fn from(value: Vec<String>) -> Self {
        Self(value.into())
    }
}

#[derive(Debug)]
pub struct Module {
    pub path: ItemPath,
    pub location: PathBuf,
    pub statements: Vec<SpannedValue<Statement>>,
    pub children: HashMap<String, Module>,
}

impl Module {
    pub fn parse_root(path: PathBuf) -> Result<Self, ParseError> {
        let source = Source::new(&path, Path::new("root.vvk"))?;

        Self::parse(&source, &path, Vec::new())
    }

    pub fn get_descendent(&self, module: Option<PathBuf>) -> &Module {
        match module {
            Some(path) => {
                let mut current = self;
                for p in path.components() {
                    match current.children.get(
                        p.as_os_str()
                            .to_str()
                            .expect("only utf-8 paths are supported"),
                    ) {
                        Some(m) => current = m,
                        None => break,
                    };
                }
                current
            }
            None => self,
        }
    }

    fn parse(source: &Source, root: &Path, path: Vec<String>) -> Result<Self, ParseError> {
        let mut tokens = TokenParser::new(source);

        let module_location = source.path.parent().unwrap();

        let mut statements = Vec::new();
        let mut children = HashMap::new();

        let module_path = path.clone().into();

        while !tokens.is_empty() {
            let statement = Statement::parse(&mut tokens, &module_path, vec![])?;

            if let Statement::Module(module) = &statement.v {
                if children.contains_key(module) {
                    return Err(ParseError::DuplicateModule {
                        name: module.to_owned(),
                        location: statement.location,
                    });
                }

                let mut sub_location = module_location.join(module);
                sub_location.push("build.vvk");
                let mut sub_path = path.clone();
                sub_path.push(module.clone());

                let code = Source::new(root, &sub_location)?;

                children.insert(module.clone(), Module::parse(&code, root, sub_path)?);
            }

            statements.push(statement);
        }

        Ok(Self {
            statements,
            children,
            location: module_location.to_owned(),
            path: ItemPath(path.into()),
        })
    }
}
