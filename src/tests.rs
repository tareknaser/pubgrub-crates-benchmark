use std::path::Path;

use super::*;

fn case_from_file_name(file_name: &str) -> (&str, semver::Version) {
    let (name, rest) = file_name.split_once("@").unwrap();
    let ver = rest.strip_suffix(".ron").unwrap();
    (name, ver.parse().unwrap())
}

fn crates_data_from_file<P: AsRef<Path>>(
    path: P,
) -> (
    HashMap<InternedString, BTreeMap<semver::Version, index_data::Version>>,
    Result<HashMap<InternedString, BTreeMap<semver::Version, Summary>>, anyhow::Error>,
) {
    let data = std::fs::read_to_string(path).unwrap();
    let data: Vec<index_data::Version> = ron::de::from_str(&data).unwrap();
    let crates = read_test_file(data);
    let cargo_crates = crates
        .iter()
        .map(|(n, vs)| {
            vs.iter()
                .map(|(v, d)| d.try_into().map(|d| (v.clone(), d)))
                .collect::<Result<_, _>>()
                .map(|d| (n.clone(), d))
        })
        .collect::<Result<_, _>>();
    (crates, cargo_crates)
}

#[must_use]
fn check<'c>(
    dp: &mut Index<'c>,
    root: Rc<Names<'c>>,
    ver: semver::Version,
    run_cargo: bool,
) -> bool {
    dp.reset();
    let res = resolve(dp, root.clone(), ver.clone());
    match res.as_ref() {
        Ok(map) => {
            if !dp.check(root.clone(), &map) {
                return false;
            }
        }

        Err(PubGrubError::NoSolution(derivation)) => {
            eprintln!("{}", DefaultStringReporter::report(&derivation));
        }
        Err(_e) => {
            return false;
        }
    }

    if run_cargo {
        let cargo_out = cargo_resolver::resolve(root.crate_().into(), &ver, dp);

        if res.is_ok() != cargo_out.is_ok() {
            return false;
        }
    }
    true
}

#[test]
fn files_pass_tests() {
    // Switch to https://docs.rs/snapbox/latest/snapbox/harness/index.html
    let mut faild: Vec<_> = vec![];
    for case in std::fs::read_dir("out/index_ron").unwrap() {
        let case = case.unwrap().path();
        let file_name = case.file_name().unwrap().to_string_lossy();
        let (name, ver) = case_from_file_name(&file_name);
        eprintln!("Running: {name} @ {ver}");
        let start_time = std::time::Instant::now();
        let (crates, cargo_crates) = crates_data_from_file(&case);
        let run_cargo = cargo_crates.is_ok();
        let mut dp = Index::new(&crates, cargo_crates.unwrap_or_default());
        let root = new_bucket(&name, (&ver).into(), true);
        if !check(&mut dp, root, ver, run_cargo) {
            dp.make_index_ron_file();
            faild.push(file_name.to_string());
        };
        dp.make_pubgrub_ron_file();
        eprintln!(" in {}s", start_time.elapsed().as_secs());
    }
    assert_eq!(faild.as_slice(), &Vec::<String>::new());
}
