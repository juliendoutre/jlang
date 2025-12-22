use crate::token::Position;
use std::fmt;

/// Enhanced error with position information and helpful context
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub position: Option<Position>,
    pub source: Option<String>,
    pub suggestion: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            position: None,
            source: None,
            suggestion: None,
        }
    }

    pub fn with_position(mut self, position: Position) -> Self {
        self.position = Some(position);
        self
    }

    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Format the error with code snippet and highlighting
    pub fn format_with_context(&self) -> String {
        let mut output = String::new();

        // Error header
        output.push_str(&format!(
            "\x1b[1;31merror:\x1b[0m \x1b[1m{}\x1b[0m\n",
            self.message
        ));

        // Position and source snippet
        if let (Some(pos), Some(source)) = (&self.position, &self.source) {
            output.push_str(&format!(
                "  \x1b[1;34m-->\x1b[0m {}:{}\n",
                "source", pos.line
            ));

            // Get the relevant lines
            let lines: Vec<&str> = source.lines().collect();
            let line_idx = pos.line.saturating_sub(1);

            // Show context: line before (if exists), error line, line after (if exists)
            let start_line = line_idx.saturating_sub(1);
            let end_line = (line_idx + 1).min(lines.len().saturating_sub(1));

            let line_num_width = (end_line + 1).to_string().len();

            // Empty line before snippet
            output.push_str(&format!(
                "{:width$} \x1b[1;34m|\x1b[0m\n",
                "",
                width = line_num_width
            ));

            for i in start_line..=end_line {
                if i >= lines.len() {
                    break;
                }

                let line_num = i + 1;
                let line_content = lines[i];

                if i == line_idx {
                    // This is the error line - highlight it
                    output.push_str(&format!(
                        "\x1b[1;34m{:width$} |\x1b[0m {}\n",
                        line_num,
                        line_content,
                        width = line_num_width
                    ));

                    // Add caret pointing to error column
                    let spaces = " ".repeat(pos.column.saturating_sub(1));
                    output.push_str(&format!(
                        "{:width$} \x1b[1;34m|\x1b[0m {}\x1b[1;31m^\x1b[0m\n",
                        "",
                        spaces,
                        width = line_num_width
                    ));
                } else {
                    // Context line
                    output.push_str(&format!(
                        "\x1b[1;34m{:width$} |\x1b[0m {}\n",
                        line_num,
                        line_content,
                        width = line_num_width
                    ));
                }
            }

            // Empty line after snippet
            output.push_str(&format!(
                "{:width$} \x1b[1;34m|\x1b[0m\n",
                "",
                width = line_num_width
            ));
        }

        // Suggestion
        if let Some(suggestion) = &self.suggestion {
            output.push_str(&format!("\x1b[1;32mhelp:\x1b[0m {}\n", suggestion));
        }

        output
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.source.is_some() {
            write!(f, "{}", self.format_with_context())
        } else {
            write!(f, "error: {}", self.message)
        }
    }
}

/// Enhanced runtime error with position tracking
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub position: Option<Position>,
    pub suggestion: Option<String>,
}

impl RuntimeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            position: None,
            suggestion: None,
        }
    }

    pub fn with_position(mut self, position: Position) -> Self {
        self.position = Some(position);
        self
    }

    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    pub fn format_error(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!(
            "\x1b[1;31mruntime error:\x1b[0m \x1b[1m{}\x1b[0m\n",
            self.message
        ));

        if let Some(pos) = &self.position {
            output.push_str(&format!(
                "  \x1b[1;34mat\x1b[0m line {}, column {}\n",
                pos.line, pos.column
            ));
        }

        if let Some(suggestion) = &self.suggestion {
            output.push_str(&format!("\x1b[1;32mhelp:\x1b[0m {}\n", suggestion));
        }

        output
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_error())
    }
}

/// Common error suggestions
pub fn suggest_for_unexpected_token(expected: &str, found: &str) -> String {
    match (expected, found) {
        (")", _) => "Did you forget to close a parenthesis?".to_string(),
        ("}", _) => "Did you forget to close a brace?".to_string(),
        ("]", _) => "Did you forget to close a bracket?".to_string(),
        ("=", ":") => "Use '=' for assignment, not ':'".to_string(),
        (_, "EOF") => format!("Expected '{}' but reached end of file", expected),
        _ => format!("Expected '{}', found '{}'", expected, found),
    }
}

pub fn suggest_for_division_by_zero() -> String {
    "Check that your divisor is not zero. Consider adding a conditional check.".to_string()
}

pub fn suggest_for_undefined_variable(var_name: &str) -> String {
    format!(
        "Variable '{}' is not defined. Did you forget to declare it? Check for typos.",
        var_name
    )
}

pub fn suggest_for_array_out_of_bounds(index: usize, length: usize) -> String {
    format!(
        "Array has length {}, but you're trying to access index {}. Valid indices are 0 to {}.",
        length,
        index,
        length.saturating_sub(1)
    )
}

pub fn suggest_for_type_mismatch(expected: &str, found: &str) -> String {
    format!("Expected type '{}', but got '{}'", expected, found)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_error_basic() {
        let err = ParseError::new("Unexpected token");
        assert_eq!(err.message, "Unexpected token");
    }

    #[test]
    fn test_parse_error_with_position() {
        let err = ParseError::new("Syntax error").with_position(Position::new(5, 10));
        assert_eq!(err.position, Some(Position::new(5, 10)));
    }

    #[test]
    fn test_parse_error_with_suggestion() {
        let err = ParseError::new("Missing semicolon")
            .with_suggestion("Add a semicolon at the end of the statement");
        assert!(err.suggestion.is_some());
    }

    #[test]
    fn test_parse_error_format_with_context() {
        let source = "x = 1\ny = \nz = 3";
        let err = ParseError::new("Expected expression")
            .with_position(Position::new(2, 5))
            .with_source(source)
            .with_suggestion("Add a value after the '=' sign");

        let formatted = err.format_with_context();
        assert!(formatted.contains("error:"));
        assert!(formatted.contains("help:"));
    }

    #[test]
    fn test_runtime_error_basic() {
        let err = RuntimeError::new("Division by zero");
        assert_eq!(err.message, "Division by zero");
    }

    #[test]
    fn test_runtime_error_with_position() {
        let err = RuntimeError::new("Array out of bounds").with_position(Position::new(10, 5));
        assert_eq!(err.position, Some(Position::new(10, 5)));
    }

    #[test]
    fn test_suggest_for_unexpected_token() {
        let suggestion = suggest_for_unexpected_token(")", "EOF");
        assert!(suggestion.contains("Did you forget to close a parenthesis?"));
    }

    #[test]
    fn test_suggest_for_undefined_variable() {
        let suggestion = suggest_for_undefined_variable("myVar");
        assert!(suggestion.contains("myVar"));
        assert!(suggestion.contains("not defined"));
    }

    #[test]
    fn test_suggest_for_array_out_of_bounds() {
        let suggestion = suggest_for_array_out_of_bounds(10, 5);
        assert!(suggestion.contains("10"));
        assert!(suggestion.contains("5"));
    }
}
