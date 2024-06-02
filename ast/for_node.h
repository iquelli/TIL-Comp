#ifndef __TIL_AST_FOR_NODE_H__
#define __TIL_AST_FOR_NODE_H__

#include <cdk/ast/basic_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

/**
 * Class for describing for nodes.
 */
class for_node : public cdk::basic_node {
    cdk::basic_node *_init;
    cdk::expression_node *_cond;
    cdk::basic_node *_inc;
    cdk::basic_node *_block;

  public:
    for_node(int lineno, cdk::basic_node *init, cdk::expression_node *cond, cdk::basic_node *inc, cdk::basic_node *block)
        : basic_node(lineno), _init(init), _cond(cond), _inc(inc), _block(block) {}

    cdk::basic_node *init() { return _init; }

    cdk::expression_node *cond() { return _cond; }

    cdk::basic_node *inc() { return _inc; }

    cdk::basic_node *block() { return _block; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_for_node(this, level);
    }
};

} // til

#endif
