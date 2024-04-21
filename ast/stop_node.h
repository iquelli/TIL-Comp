#ifndef __TIL_AST_STOP_NODE_H__
#define __TIL_AST_STOP_NODE_H__

#include <cdk/ast/basic_node.h>

namespace til {

/**
 * Class for describing stop nodes.
 */
class stop_node : public cdk::basic_node {
    size_t _level;

  public:
    stop_node(int lineno, size_t level) : basic_node(lineno), _level(level) {}

    size_t level() { return _level; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_stop_node(this, level);
    }
};

} // til

#endif