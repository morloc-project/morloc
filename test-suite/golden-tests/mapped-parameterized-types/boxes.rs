// A user-mapped `data` with a parameter is a generic enum; each payload arm
// holds its fields as a boxed tuple, as a monomorphic mapped enum does.
#[derive(Clone)]
pub enum MyBox<T> {
    Empty,
    Full(std::boxed::Box<(T,)>),
}

#[derive(Clone)]
pub struct MyWrap<T> {
    pub item: T,
}

pub fn grow(b: &MyBox<i64>) -> MyBox<i64> {
    match b {
        MyBox::Full(p) => MyBox::Full(std::boxed::Box::new((p.0 + 10,))),
        MyBox::Empty => MyBox::Empty,
    }
}

pub fn shout(b: &MyBox<String>) -> MyBox<String> {
    match b {
        MyBox::Full(p) => MyBox::Full(std::boxed::Box::new((format!("{}!", p.0),))),
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
    Leaf(std::boxed::Box<(T,)>),
    Node(std::boxed::Box<(MyTree<T>, MyTree<T>)>),
}

pub fn depth(t: &MyTree<i64>) -> i64 {
    match t {
        MyTree::Node(b) => 1 + std::cmp::max(depth(&b.0), depth(&b.1)),
        MyTree::Leaf(_) => 1,
    }
}

#[derive(Clone)]
pub enum MyRose<T> {
    Tip(std::boxed::Box<(T,)>),
    Branch(std::boxed::Box<(T, Vec<MyRose<T>>)>),
}

pub fn rose_size(t: &MyRose<i64>) -> i64 {
    match t {
        MyRose::Branch(b) => 1 + b.1.iter().map(rose_size).sum::<i64>(),
        MyRose::Tip(_) => 1,
    }
}
