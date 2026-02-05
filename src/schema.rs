use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use anyhow::anyhow;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ChildEntityRules {
    pub name_regex: String,
    pub node_type: String,
    pub required: bool,
    pub multiple: bool,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct EntityTypeDescription {
    pub name: String,
    pub children: Vec<ChildEntityRules>,
    pub allow_additional: bool,
}

#[derive(Default, Clone)]
pub struct Schema {
    pub entity_types: HashMap<String, EntityTypeDescription>,
}

impl Schema {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_entity_type(&self, entity_type: &str) -> anyhow::Result<&EntityTypeDescription> {
        self.entity_types
            .get(entity_type)
            .ok_or_else(|| anyhow!("Invalid entity type {}", entity_type))
    }

    pub fn add_entity_type(&mut self, description: EntityTypeDescription) {
        self.entity_types.insert(description.name.clone(), description);
    }
}
