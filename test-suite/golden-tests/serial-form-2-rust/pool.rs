use rustmorlocinternals::serial;

fn m0(x0: &str) -> String {
    let a0: (Vec<String>, Vec<i64>) = serial::deserialize(x0).into();
    let a1 = morloc_pack_map(&a0);
    let a2 = morloc_id(&a1);
    let a3 = morloc_unpack_map(&a2);
    let a4 = serial::serialize(&a3);
    a4
}
