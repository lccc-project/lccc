
fn main() {
    let name = std::env::vars().filter(|(k,v)|k.eq(&"LCCC_BACKENDS".to_string()))
        .map(|(k,v)|v).next();

}