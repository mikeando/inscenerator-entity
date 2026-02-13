extern crate yaml_rust;
use yaml_rust::Yaml;

fn main() {
    let mut hash = yaml_rust::yaml::Hash::new();
    hash.insert(Yaml::String("key".to_string()), Yaml::String("value".to_string()));
    let y = Yaml::Hash(hash);
    println!("{:?}", y);
}
