use std::collections::{BTreeMap, BTreeSet};

use cargo::util::interning::InternedString;
use internment::Intern;
use itertools::Itertools;

fn is_default<D: Default + PartialEq>(t: &D) -> bool {
    t == &D::default()
}

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Dependency {
    pub name: InternedString,
    pub package_name: InternedString,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub req: Intern<semver::VersionReq>,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub features: Intern<Vec<InternedString>>,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub default_features: bool,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub kind: crates_index::DependencyKind,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub optional: bool,
}

impl TryFrom<&crates_index::Dependency> for Dependency {
    type Error = semver::Error;

    fn try_from(dep: &crates_index::Dependency) -> Result<Self, Self::Error> {
        let mut features = dep
            .features()
            .iter()
            .map(|s| s.as_str().into())
            .collect_vec();
        features.sort_unstable();
        Ok(Dependency {
            name: dep.name().into(),
            package_name: dep.crate_name().into(),
            req: dep.requirement().parse::<semver::VersionReq>()?.into(),
            features: features.into(),
            kind: dep.kind(),
            optional: dep.is_optional(),
            default_features: dep.has_default_features(),
        })
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(from = "Vec<Dependency>")]
#[serde(into = "Vec<Dependency>")]
pub struct DependencyList {
    deps: BTreeMap<InternedString, Intern<Vec<Dependency>>>,
}

impl DependencyList {
    pub fn iter(&self) -> impl Iterator<Item = &Dependency> {
        self.deps.iter().flat_map(|(_, v)| v.iter())
    }

    pub fn get<Q: ?Sized>(&self, name: &Q) -> &[Dependency]
    where
        InternedString: std::borrow::Borrow<Q>,
        Q: std::cmp::Ord,
    {
        self.deps.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }
}

impl From<Vec<Dependency>> for DependencyList {
    fn from(value: Vec<Dependency>) -> Self {
        let mut deps: BTreeMap<InternedString, Vec<Dependency>> = BTreeMap::new();
        for dep in value {
            deps.entry(dep.name).or_default().push(dep);
        }
        DependencyList {
            deps: deps.into_iter().map(|(k, v)| (k, Intern::new(v))).collect(),
        }
    }
}

impl Into<Vec<Dependency>> for DependencyList {
    fn into(self) -> Vec<Dependency> {
        self.deps
            .iter()
            .flat_map(|(_, v)| v.iter())
            .cloned()
            .collect()
    }
}

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Version {
    pub name: InternedString,
    pub vers: Intern<semver::Version>,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub deps: DependencyList,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub features: Intern<BTreeMap<InternedString, Intern<BTreeSet<InternedString>>>>,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub links: Option<InternedString>,
    #[serde(skip_serializing_if = "is_default")]
    #[serde(default)]
    pub yanked: bool,
}

impl TryFrom<&crates_index::Version> for Version {
    type Error = semver::Error;

    fn try_from(ver: &crates_index::Version) -> Result<Self, Self::Error> {
        let deps: Result<Vec<_>, _> = ver
            .dependencies()
            .iter()
            .map(|d| TryInto::<Dependency>::try_into(d))
            .collect();

        Ok(Version {
            name: ver.name().into(),
            vers: ver.version().parse::<semver::Version>()?.into(),
            deps: deps?.into(),
            features: Intern::new(
                ver.features()
                    .iter()
                    .map(|(f, ts)| {
                        (
                            f.as_str().into(),
                            Intern::new(ts.iter().map(|f| f.as_str().into()).collect()),
                        )
                    })
                    .collect(),
            ),
            links: ver.links().map(|s| s.into()),
            yanked: ver.is_yanked(),
        })
    }
}
