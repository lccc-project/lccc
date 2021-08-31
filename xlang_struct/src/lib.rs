pub enum PathComponent {
    Root,
    Text(String),
    SpecialComponent(String),
}

pub struct Path {
    components: Vec<PathComponent>,
}
