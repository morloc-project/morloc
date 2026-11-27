extern "C" {
    fn fork() -> i32;
    fn waitpid(pid: i32, status: *mut i32, options: i32) -> i32;
    fn _exit(code: i32) -> !;
}

pub fn seq_p<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![1, 2]);
    let pid = unsafe { fork() };
    if pid == 0 {
        sink(&vec![5]);
        unsafe { _exit(0) };
    }
    unsafe { waitpid(pid, std::ptr::null_mut(), 0) };
    sink(&vec![3]);
}

pub fn returns_p<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![1]);
    let pid = unsafe { fork() };
    if pid == 0 {
        sink(&vec![5]);
        return;
    }
    unsafe { waitpid(pid, std::ptr::null_mut(), 0) };
    sink(&vec![3]);
}

pub fn raises_p<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![1]);
    let pid = unsafe { fork() };
    if pid == 0 {
        sink(&vec![5]);
        panic!("the child raises");
    }
    unsafe { waitpid(pid, std::ptr::null_mut(), 0) };
    sink(&vec![3]);
}
