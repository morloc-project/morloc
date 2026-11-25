#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>

mlc::ArrowTable makePeople() {
    struct ArrowSchema schema;
    struct ArrowArray array;

    ArrowSchemaInit(&schema);
    ArrowSchemaSetTypeStruct(&schema, 2);

    ArrowSchemaSetFormat(schema.children[0], "u");
    ArrowSchemaSetName(schema.children[0], "name");

    ArrowSchemaSetFormat(schema.children[1], "i");
    ArrowSchemaSetName(schema.children[1], "age");

    ArrowArrayInitFromSchema(&array, &schema, nullptr);
    ArrowArrayStartAppending(&array);

    ArrowArrayAppendString(array.children[0], ArrowCharView("Alice"));
    ArrowArrayAppendInt(array.children[1], 30);
    ArrowArrayFinishElement(&array);

    ArrowArrayAppendString(array.children[0], ArrowCharView("Bob"));
    ArrowArrayAppendInt(array.children[1], 25);
    ArrowArrayFinishElement(&array);

    ArrowArrayFinishBuildingDefault(&array, nullptr);

    return mlc::ArrowTable(std::move(schema), std::move(array));
}

#endif
