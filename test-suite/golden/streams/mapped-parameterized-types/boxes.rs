// A user-mapped `data` with a parameter is a generic enum; each payload arm
// holds its fields inline, boxing only the ones that hold the type itself.
#[derive(Clone)]
pub enum MyBox<T> {
    Empty,
    Full(T),
}

#[derive(Clone)]
pub struct MyWrap<T> {
    pub item: T,
}

pub fn grow(b: &MyBox<i64>) -> MyBox<i64> {
    match b {
        MyBox::Full(p) => MyBox::Full(p + 10),
        MyBox::Empty => MyBox::Empty,
    }
}

pub fn shout(b: &MyBox<String>) -> MyBox<String> {
    match b {
        MyBox::Full(p) => MyBox::Full(format!("{}!", p)),
        MyBox::Empty => MyBox::Empty,
    }
}

pub fn bump(w: &MyWrap<i64>) -> MyWrap<i64> {
    MyWrap { item: w.item + 10 }
}

pub fn inc(x: i64) -> i64 {
    x + 10
}

#[derive(Clone)]
pub enum MyTree<T> {
    Leaf(T),
    Node(Box<MyTree<T>>, Box<MyTree<T>>),
}

pub fn depth(t: &MyTree<i64>) -> i64 {
    match t {
        MyTree::Node(l, r) => 1 + std::cmp::max(depth(l), depth(r)),
        MyTree::Leaf(_) => 1,
    }
}

#[derive(Clone)]
pub enum MyRose<T> {
    Tip(T),
    Branch(T, Vec<MyRose<T>>),
}

pub fn rose_size(t: &MyRose<i64>) -> i64 {
    match t {
        MyRose::Branch(_, kids) => 1 + kids.iter().map(rose_size).sum::<i64>(),
        MyRose::Tip(_) => 1,
    }
}
