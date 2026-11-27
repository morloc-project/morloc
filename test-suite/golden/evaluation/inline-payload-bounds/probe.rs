pub fn probe(size: i64, data: i64) -> String {
    let schema = rustmorloc::parse_schema("au1").unwrap();
    let value: Vec<u8> = vec![1, 2, 3];
    unsafe {
        let made = rustmorloc::put_value(&value, &schema);
        let off = u32::from_le_bytes(std::slice::from_raw_parts(made.add(20), 4).try_into().unwrap()) as usize;
        let len = u64::from_le_bytes(std::slice::from_raw_parts(made.add(24), 8).try_into().unwrap()) as usize;
        let mut p = std::slice::from_raw_parts(made, 32 + off + len).to_vec();
        if p[13] != 0 {
            return "not inline".to_string();
        }
        let base = 32 + off;
        if size >= 0 {
            p[base..base + 8].copy_from_slice(&(size as u64).to_le_bytes());
        }
        if data >= 0 {
            p[base + 8..base + 16].copy_from_slice(&data.to_le_bytes());
        }
        match std::panic::catch_unwind(|| rustmorloc::get_value::<Vec<u8>>(p.as_ptr(), &schema)) {
            Ok(v) => format!("ok {}", v.len()),
            Err(_) => "refused".to_string(),
        }
    }
}
