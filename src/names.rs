use std::rc::Rc;

use semver_pubgrub::SemverCompatibility;

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Names<'c> {
    Bucket(&'c str, SemverCompatibility, bool),
    BucketFeatures(&'c str, SemverCompatibility, &'c str),
    Wide(
        &'c str,
        &'c semver::VersionReq,
        &'c str,
        SemverCompatibility,
    ),
    WideFeatures(
        &'c str,
        &'c semver::VersionReq,
        &'c str,
        SemverCompatibility,
        &'c str,
    ),
    Links(&'c str),
}

pub fn new_bucket<'c>(
    crate_: &'c str,
    compat: SemverCompatibility,
    all_features: bool,
) -> Rc<Names<'c>> {
    Rc::new(Names::Bucket(crate_, compat, all_features))
}
pub fn new_wide<'c>(
    crate_: &'c str,
    req: &'c semver::VersionReq,
    from: &'c str,
    compat: SemverCompatibility,
) -> Rc<Names<'c>> {
    Rc::new(Names::Wide(crate_, req, from, compat))
}
pub fn new_links<'c>(crate_: &'c str) -> Rc<Names<'c>> {
    Rc::new(Names::Links(crate_))
}

impl<'c> Ord for Names<'c> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        match (self, other) {
            (Names::Bucket(l1, l2, l3), Names::Bucket(r1, r2, r3)) => {
                (l1, l2, l3).cmp(&(r1, r2, r3))
            }
            (Names::Bucket(_, _, _), _) => Ordering::Greater,
            (Names::BucketFeatures(_, _, _), Names::Bucket(_, _, _)) => Ordering::Less,
            (Names::BucketFeatures(l1, l2, l3), Names::BucketFeatures(r1, r2, r3)) => {
                (l1, l2, l3).cmp(&(r1, r2, r3))
            }
            (Names::BucketFeatures(_, _, _), _) => Ordering::Greater,
            (Names::Wide(_, _, _, _), Names::Bucket(_, _, _)) => Ordering::Less,
            (Names::Wide(_, _, _, _), Names::BucketFeatures(_, _, _)) => Ordering::Less,
            (Names::Wide(l1, lreq, l2, l3), Names::Wide(r1, rreq, r2, r3)) => {
                (l1, l2, l3).cmp(&(r1, r2, r3)).then_with(|| {
                    if lreq == rreq {
                        Ordering::Equal
                    } else {
                        lreq.to_string().cmp(&rreq.to_string())
                    }
                })
            }
            (Names::Wide(_, _, _, _), _) => Ordering::Greater,
            (Names::WideFeatures(_, _, _, _, _), Names::Bucket(_, _, _)) => Ordering::Less,
            (Names::WideFeatures(_, _, _, _, _), Names::BucketFeatures(_, _, _)) => Ordering::Less,
            (Names::WideFeatures(_, _, _, _, _), Names::Wide(_, _, _, _)) => Ordering::Less,
            (
                Names::WideFeatures(l1, lreq, l2, l3, l4),
                Names::WideFeatures(r1, rreq, r2, r3, r4),
            ) => (l1, l2, l3, l4).cmp(&(r1, r2, r3, r4)).then_with(|| {
                if lreq == rreq {
                    Ordering::Equal
                } else {
                    lreq.to_string().cmp(&rreq.to_string())
                }
            }),
            (Names::WideFeatures(_, _, _, _, _), _) => Ordering::Greater,
            (Names::Links(l), Names::Links(r)) => l.cmp(r),
            (Names::Links(_), _) => Ordering::Less,
        }
    }
}

impl<'c> PartialOrd for Names<'c> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'c> Names<'c> {
    pub fn is_real(&self) -> bool {
        matches!(self, &Self::Bucket(..))
    }
    pub fn crate_(&self) -> &'c str {
        match self {
            Names::Bucket(c, _, _) => *c,
            Names::BucketFeatures(c, _, _) => *c,
            Names::Wide(c, _, _, _) => *c,
            Names::WideFeatures(c, _, _, _, _) => *c,
            Names::Links(_) => panic!(),
        }
    }
    pub fn with_features(&self, feat: &'c str) -> Rc<Self> {
        Rc::new(match self {
            Names::Bucket(a, b, _) => Names::BucketFeatures(*a, *b, feat),
            Names::BucketFeatures(a, b, _) => Names::BucketFeatures(*a, *b, feat),
            Names::Wide(a, b, c, d) => Names::WideFeatures(*a, b, c, *d, feat),
            Names::WideFeatures(a, b, c, d, _) => Names::WideFeatures(*a, b, c, *d, feat),
            Names::Links(_) => panic!(),
        })
    }
}

impl<'c> std::fmt::Display for Names<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Names::Bucket(n, m, a) => {
                f.write_str("Bucket:")?;
                f.write_str(n)?;
                f.write_str("@")?;
                f.write_str(&match m {
                    SemverCompatibility::Major(i) => format!("{}.x.y", i),
                    SemverCompatibility::Minor(i) => format!("0.{}.x", i),
                    SemverCompatibility::Patch(i) => format!("0.0.{}", i),
                })?;
                if *a {
                    f.write_str("/All-FEATURES")?;
                }
                Ok(())
            }
            Names::BucketFeatures(n, m, o) => {
                f.write_str("Bucket:")?;
                f.write_str(n)?;
                f.write_str("@")?;
                f.write_str(&match m {
                    SemverCompatibility::Major(i) => format!("{}.x.y", i),
                    SemverCompatibility::Minor(i) => format!("0.{}.x", i),
                    SemverCompatibility::Patch(i) => format!("0.0.{}", i),
                })?;
                f.write_str("/")?;
                f.write_str(o)
            }
            Names::Wide(c, range, parent, parent_com) => {
                f.write_str("Range:")?;
                f.write_str(c)?;
                f.write_str("(From:")?;
                f.write_str(parent)?;
                f.write_str("@")?;
                f.write_str(&match parent_com {
                    SemverCompatibility::Major(i) => format!("{}.x.y", i),
                    SemverCompatibility::Minor(i) => format!("0.{}.x", i),
                    SemverCompatibility::Patch(i) => format!("0.0.{}", i),
                })?;
                f.write_str("):")?;
                f.write_str(&range.to_string())
            }
            Names::WideFeatures(c, range, parent, parent_com, feat) => {
                f.write_str("Range:")?;
                f.write_str(c)?;
                f.write_str("(From:")?;
                f.write_str(parent)?;
                f.write_str("@")?;
                f.write_str(&match parent_com {
                    SemverCompatibility::Major(i) => format!("{}.x.y", i),
                    SemverCompatibility::Minor(i) => format!("0.{}.x", i),
                    SemverCompatibility::Patch(i) => format!("0.0.{}", i),
                })?;
                f.write_str("):")?;
                f.write_str(&range.to_string())?;
                f.write_str("/")?;
                f.write_str(feat)
            }
            Names::Links(name) => {
                f.write_str("Links:")?;
                f.write_str(name)
            }
        }
    }
}

impl std::fmt::Debug for Names<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

impl serde::Serialize for Names<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}
