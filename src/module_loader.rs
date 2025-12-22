use crate::ast::Program;
use crate::lexer::Lexer;
use crate::parser::Parser as JlangParser;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;

/// Module cache key (URL + checksum)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ModuleKey {
    url: String,
    checksum: String,
}

impl ModuleKey {
    fn new(url: String, checksum: String) -> Self {
        Self { url, checksum }
    }
}

/// Module loader that handles importing external JLang modules
/// with URL-based fetching and SHA256 verification
pub struct ModuleLoader {
    /// Cache of loaded modules (URL+checksum -> Program)
    cache: HashMap<ModuleKey, Program>,
    /// Current import chain for circular dependency detection
    import_chain: Vec<String>,
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            import_chain: Vec::new(),
        }
    }

    /// Load a module from a URL with SHA256 verification
    /// Returns a reference to the parsed Program (not executed)
    pub fn load_module(&mut self, url: &str, expected_checksum: &str) -> Result<&Program, String> {
        let key = ModuleKey::new(url.to_string(), expected_checksum.to_string());

        // Check if already loaded (cached)
        if self.cache.contains_key(&key) {
            return Ok(&self.cache[&key]);
        }

        // Check for circular dependency
        if self.import_chain.contains(&url.to_string()) {
            let chain_str = self
                .import_chain
                .iter()
                .chain(std::iter::once(&url.to_string()))
                .map(|s| format!("  -> {}", s))
                .collect::<Vec<_>>()
                .join("\n");
            return Err(format!(
                "Circular dependency detected!\nImport chain:\n{}",
                chain_str
            ));
        }

        // Add to import chain
        self.import_chain.push(url.to_string());

        // Fetch the source code
        let source = self.fetch_source(url)?;

        // Verify checksum
        self.verify_checksum(&source, expected_checksum)?;

        // Parse the module (DO NOT execute)
        let program = self.parse_module(&source)?;

        // Remove from import chain (successfully loaded)
        self.import_chain.pop();

        // Cache the module
        self.cache.insert(key.clone(), program);

        Ok(&self.cache[&key])
    }

    /// Fetch source code from a URL
    fn fetch_source(&self, url: &str) -> Result<String, String> {
        if url.starts_with("file://") {
            // Local file
            let path = url.strip_prefix("file://").unwrap();
            fs::read_to_string(path).map_err(|e| format!("Failed to read file '{}': {}", path, e))
        } else if url.starts_with("https://") || url.starts_with("http://") {
            // Remote file via HTTP(S)
            #[cfg(not(target_arch = "wasm32"))]
            {
                let response = ureq::get(url)
                    .call()
                    .map_err(|e| format!("Failed to fetch '{}': {}", url, e))?;

                response
                    .into_string()
                    .map_err(|e| format!("Failed to read response from '{}': {}", url, e))
            }
            #[cfg(target_arch = "wasm32")]
            {
                Err("HTTP(S) fetching not supported in WASM".to_string())
            }
        } else {
            Err(format!("Unsupported URL scheme: {}", url))
        }
    }

    /// Verify SHA256 checksum of source code
    fn verify_checksum(&self, source: &str, expected: &str) -> Result<(), String> {
        let mut hasher = Sha256::new();
        hasher.update(source.as_bytes());
        let result = hasher.finalize();
        let actual = format!("{:x}", result);

        if actual != expected {
            Err(format!(
                "Checksum mismatch!\nExpected: {}\nActual:   {}",
                expected, actual
            ))
        } else {
            Ok(())
        }
    }

    /// Parse a module into an AST
    fn parse_module(&self, source: &str) -> Result<Program, String> {
        let lexer = Lexer::new(source);
        let mut parser = JlangParser::new_with_source(lexer, source.to_string());
        parser.parse().map_err(|e| format!("{}", e))
    }

    /// Check if a module is already loaded
    pub fn is_loaded(&self, url: &str, checksum: &str) -> bool {
        let key = ModuleKey::new(url.to_string(), checksum.to_string());
        self.cache.contains_key(&key)
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verify_checksum() {
        let loader = ModuleLoader::new();
        let source = "N = {0, ..., 10}";

        // Calculate actual checksum
        let mut hasher = Sha256::new();
        hasher.update(source.as_bytes());
        let result = hasher.finalize();
        let checksum = format!("{:x}", result);

        // Should succeed with correct checksum
        assert!(loader.verify_checksum(source, &checksum).is_ok());

        // Should fail with incorrect checksum
        assert!(loader.verify_checksum(source, "wrong").is_err());
    }

    #[test]
    fn test_parse_module() {
        let loader = ModuleLoader::new();
        let source = "N = {0, ..., 10}\nstdout(5)";

        let result = loader.parse_module(source);
        assert!(result.is_ok());

        let program = result.unwrap();
        assert_eq!(program.statements.len(), 2);
    }
}
