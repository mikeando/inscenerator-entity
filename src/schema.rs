use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use anyhow::anyhow;

/// Rules for discovering and validating child entities.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ChildEntityRules {
    /// Regex pattern to match the child entity name.
    pub name_regex: String,
    /// The expected entity type for matching children.
    pub node_type: String,
    /// Whether this child is required.
    pub required: bool,
    /// Whether multiple children of this type are allowed.
    pub multiple: bool,
}

/// Description of an entity type, including its allowed children.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct EntityTypeDescription {
    /// The name of the entity type.
    pub name: String,
    /// List of rules for child entities.
    pub children: Vec<ChildEntityRules>,
    /// Whether to allow additional children not covered by the rules.
    pub allow_additional: bool,
}

/// A collection of entity type descriptions.
#[derive(Default, Clone)]
pub struct Schema {
    /// Map of entity type names to their descriptions.
    pub entity_types: HashMap<String, EntityTypeDescription>,
}

impl Schema {
    /// Creates a new, empty schema.
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets an entity type description by name.
    ///
    /// # Errors
    ///
    /// Returns an error if the entity type is not found in the schema.
    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.entity_types
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    /// Adds a new entity type description to the schema.
    pub fn add_entity_type(&mut self, description: EntityTypeDescription) {
        self.entity_types.insert(description.name.clone(), description);
    }
}
