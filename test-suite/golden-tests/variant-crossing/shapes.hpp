#pragma once
#include <memory>
#include <variant>

// Each arm lives behind a shared_ptr inside the variant. That is morloc's
// convention: it gives a recursive arm a finite size and lets an arm holding
// another `data` type need only a forward declaration.
struct Shape_Circle;
struct Shape_Rect;
struct Shape_Dot;

struct Shape {
    std::variant<std::shared_ptr<Shape_Circle>,
                 std::shared_ptr<Shape_Rect>,
                 std::shared_ptr<Shape_Dot>> v;
};

struct Shape_Circle { double f0; };
struct Shape_Rect   { double f0; double f1; };
struct Shape_Dot    { };

inline double area(Shape s) {
    if (auto p = std::get_if<std::shared_ptr<Shape_Circle>>(&s.v)) return 3.0 * (*p)->f0 * (*p)->f0;
    if (auto p = std::get_if<std::shared_ptr<Shape_Rect>>(&s.v))   return (*p)->f0 * (*p)->f1;
    return 0.0;
}

inline Shape grow(Shape s) {
    if (auto p = std::get_if<std::shared_ptr<Shape_Circle>>(&s.v))
        return Shape{std::make_shared<Shape_Circle>(Shape_Circle{(*p)->f0 + 1.0})};
    if (auto p = std::get_if<std::shared_ptr<Shape_Rect>>(&s.v))
        return Shape{std::make_shared<Shape_Rect>(Shape_Rect{(*p)->f0 + 1.0, (*p)->f1 + 1.0})};
    return s;
}
