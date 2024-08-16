use std::{cell::RefCell, collections::HashMap, rc::Rc};

use pubgrub::VersionSet;
use semver_pubgrub::SemverPubgrub;

#[derive(Debug, PartialEq, Eq, Clone, Hash, serde::Deserialize, serde::Serialize)]
#[serde(transparent)]
pub struct RcSemverPubgrub {
    pub(crate) inner: Rc<SemverPubgrub>,
}

impl RcSemverPubgrub {
    pub fn new(inner: SemverPubgrub) -> Self {
        Self {
            inner: Rc::new(inner),
        }
    }
}

impl std::fmt::Display for RcSemverPubgrub {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

thread_local! {
    static ARC_SEMVER_PUBGRUB_EMPTY: RefCell<RcSemverPubgrub> = RefCell::new(RcSemverPubgrub {
        inner: Rc::new(SemverPubgrub::empty()),
    });

    static ARC_SEMVER_PUBGRUB_SINGLETON: RefCell<HashMap<semver::Version, RcSemverPubgrub>> = RefCell::new(HashMap::default());
}

impl VersionSet for RcSemverPubgrub {
    type V = <SemverPubgrub as VersionSet>::V;

    fn empty() -> Self {
        ARC_SEMVER_PUBGRUB_EMPTY.with_borrow(|v| v.clone())
    }

    fn singleton(v: Self::V) -> Self {
        ARC_SEMVER_PUBGRUB_SINGLETON.with_borrow_mut(|map| {
            map.entry(v)
                .or_insert_with_key(|v| RcSemverPubgrub {
                    inner: Rc::new(SemverPubgrub::singleton(v.clone())),
                })
                .clone()
        })
    }

    fn complement(&self) -> Self {
        RcSemverPubgrub {
            inner: Rc::new(self.inner.complement()),
        }
    }

    fn intersection(&self, other: &Self) -> Self {
        RcSemverPubgrub {
            inner: Rc::new(self.inner.intersection(&other.inner)),
        }
    }

    fn contains(&self, v: &Self::V) -> bool {
        self.inner.contains(v)
    }

    fn full() -> Self {
        RcSemverPubgrub {
            inner: Rc::new(SemverPubgrub::full()),
        }
    }

    fn union(&self, other: &Self) -> Self {
        RcSemverPubgrub {
            inner: Rc::new(self.inner.union(&other.inner)),
        }
    }

    fn is_disjoint(&self, other: &Self) -> bool {
        self.inner.is_disjoint(&other.inner)
    }

    fn subset_of(&self, other: &Self) -> bool {
        self.inner.subset_of(&other.inner)
    }
}
