use toml;
fn main() {
    let mut map = std::collections::BTreeMap::new();
    map.insert("key".to_string(), toml::Value::String("value".to_string()));
    let val = toml::Value::Table(map);
    let s = toml::to_string(&val).unwrap();
    println!("{:?}", s);
}
