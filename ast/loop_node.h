#ifndef __TIL_AST_LOOP_NODE_H__
#define __TIL_AST_LOOP_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

/**
 * Class for describing loop-cycle nodes.
 */
class loop_node : public cdk::basic_node {
    cdk::expression_node *_condition;
    cdk::basic_node *_block;

  public:
    loop_node(int lineno, cdk::expression_node *condition,
              cdk::basic_node *block)
        : basic_node(lineno), _condition(condition), _block(block) {}

    cdk::expression_node *condition() { return _condition; }

    cdk::basic_node *block() { return _block; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_loop_node(this, level);
    }
};

} // til

#endif
