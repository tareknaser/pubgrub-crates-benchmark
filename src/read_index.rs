use std::collections::{BTreeMap, HashMap};

use crates_index::GitIndex;
use internment::{ArcIntern, Intern};
use rayon::iter::ParallelIterator;

use crate::index_data;

pub fn read_index(
    index: &GitIndex,
    create_filter: impl Fn(&str) -> bool + Sync + 'static,
) -> &'static HashMap<ArcIntern<str>, BTreeMap<Intern<semver::Version>, index_data::Version>> {
    dbg!("Start reading index");
    let crates = index
        .crates_parallel()
        .map(|c| c.unwrap())
        .filter(|crt| create_filter(crt.name()))
        .map(|crt| {
            let name: ArcIntern<str> = crt.name().into();
            let ver_lookup = crt
                .versions()
                .iter()
                .filter_map(|v| TryInto::<index_data::Version>::try_into(v).ok())
                .map(|v| (v.vers, v))
                .collect();
            (name, ver_lookup)
        })
        .collect();
    dbg!("Done reading index");
    &*Box::leak(Box::new(crates))
}

#[cfg(test)]
pub fn read_test_file(
    iter: impl IntoIterator<Item = index_data::Version>,
) -> &'static HashMap<ArcIntern<str>, BTreeMap<Intern<semver::Version>, index_data::Version>> {
    let mut deps: HashMap<ArcIntern<str>, BTreeMap<Intern<semver::Version>, index_data::Version>> =
        HashMap::new();

    for v in iter {
        deps.entry(v.name.clone())
            .or_default()
            .insert(v.vers.clone(), v);
    }
    &*Box::leak(Box::new(deps))
}
