use rustmorlocinternals::serial;

fn m0(x0: &str) -> String {
    let a0: Vec<(Vec<String>, Vec<i64>)> = serial::deserialize(x0).into();
    let mut a1 = Vec::new();
    for i0 in a0 {
        a1.push(morloc_pack_map(i0));
    }
    let a2 = morloc_id(&a1);
    let mut a3 = Vec::new();
    for i1 in a2 {
        a3.push(morloc_unpack_map(i1));
    }
    let a4 = serial::serialize(&a3);
    a3
}
