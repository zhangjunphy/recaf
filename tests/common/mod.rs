use std::path::PathBuf;

pub fn read_resource_file(relative_path: &str) -> Result<String, std::io::Error> {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(relative_path);
    std::fs::read_to_string(path)
}
