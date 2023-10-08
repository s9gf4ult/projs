use serde::Deserialize;
use serde_json::{json, Result};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Name<T>(T);

#[derive(Debug, Deserialize)]
enum LocationType {
    #[serde(rename = "PointOfInterest")]
    PointOfInterest,
    #[serde(rename = "Neighborhood")]
    Neighborhood,
    #[serde(rename = "Hotel")]
    Hotel,
}

#[derive(Debug, Deserialize)]
struct Location {
    id: Name<String>,
    name: Name<String>,
    full_name: Name<String>,
    #[serde(flatten)]
    typ: OneOfType,
    city: Option<Name<String>>,
    state: Option<Name<String>>,
    country: Option<Name<String>>,
    coordinates: Option<GeoPosition>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum OneOfType {
    #[serde(with = "one_of_text")]
    Text(String),
    #[serde(with = "one_of_location_type")]
    LocationType(LocationType),
}

#[derive(Debug, Deserialize)]
struct GeoPosition {
    // add fields to represent coordinates
    // e.g., latitude: f64, longitude: f64
}

#[derive(Debug, Deserialize)]
struct SuggestionsResponse {
    location_suggestions: Vec<Location>,
}

mod one_of_text {
    use serde::{de, erializer, Deserialize};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: serde_json::Value = Deserialize::deserialize(deserializer)?;
        if let Some(string) = value.as_str() {
            Ok(string.into())
        } else {
            Err(de::Error::invalid_value(
                serde::de::Unexpected::Other("expected a string"),
                &"a string",
            ))
        }
    }
}

mod one_of_location_type {
    use serde::{Deserialize, Deserializer};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<LocationType, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: serde_json::Value = Deserialize::deserialize(deserializer)?;
        if let Some(string) = value_str() {
            match string {
                "PointOfInterest" => Ok(LocationType::PointOfInterest),
                "Neighborhood" => Ok(LocationType::Neighborhood),
                "Hotel" => Ok(LocationType::Hotel),
                _ => Err(serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(string),
                    &"PointOfInterest, Neighborhood, or Hotel",
                )),
            }
        } else {
            Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Other("expected a string"),
                &"a string",
            ))
        }
    }
}

fn main() {
    // Deserialize the JSON into SuggestionsResponse
    let json_data = r#"{
        "locationSuggestions": [
            {
                "id": "locationId",
                "name": "location",
                "fullName": "location_long",
                "type": "PointOfInterest",
                "city": null,
                "state": null,
                "country": null,
                "coordinates": null
            }
        ]
    }"#;

    let response: SuggestionsResponse = serde_json::from_str(json_data).unwrap();

    // Access the deserialized data
    for location in response.location_suggestions {
        println!("Location: {:?}", location);
    }
}
