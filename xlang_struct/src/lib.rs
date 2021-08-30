pub enum PathComponent {
    Root,
    Text(String),
    SpecialComponent(String),
    Type(Type),
    GenericArgs(Vec<GenericArguments>),
}

pub struct Path {
    components: Vec<PathComponent>,
}
