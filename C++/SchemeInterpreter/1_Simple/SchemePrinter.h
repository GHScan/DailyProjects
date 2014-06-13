#ifndef SCHEME_PRINTER_H
#define SCHEME_PRINTER_H

class SchemeRef;

class SchemePrinter {
public:

    static void printSExpression(ostream& so, const SchemeRef &exp);
};

#endif
