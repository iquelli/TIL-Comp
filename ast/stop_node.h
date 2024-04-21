#ifndef __TIL_AST_STOP_NODE_H__
#define __TIL_AST_STOP_NODE_H__

#include <cdk/ast/basic_node.h>

namespace til {

/**
 * Class for describing stop nodes.
 */
class stop_node : public cdk::basic_node {
    int _nesting;

  public:
    stop_node(int lineno, int nesting)
        : basic_node(lineno), _nesting(nesting) {}

    int nesting() { return _nesting; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_stop_node(this, level);
    }
};

} // til

#endif
