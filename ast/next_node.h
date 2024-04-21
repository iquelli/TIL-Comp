#ifndef __TIL_AST_NEXT_NODE_H__
#define __TIL_AST_NEXT_NODE_H__

#include <cdk/ast/basic_node.h>

namespace til {

/**
 * Class for describing next nodes.
 */
class next_node : public cdk::basic_node {
    int _nesting;

  public:
    next_node(int lineno, int nesting)
        : basic_node(lineno), _nesting(nesting) {}

    int nesting() { return _nesting; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_next_node(this, level);
    }
};

} // til

#endif
