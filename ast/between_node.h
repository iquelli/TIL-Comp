#ifndef __TIL_AST_BETWEEN_NODE_H__
#define __TIL_AST_BETWEEN_NODE_H__

#include <cdk/ast/basic_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

/**
 * Class for describing between nodes.
 */
class between_node : public cdk::basic_node {
    cdk::expression_node *_low, *_high, *_func, *_vec;

  public:
    between_node(int lineno, cdk::expression_node *low, cdk::expression_node *high, cdk::expression_node *func, cdk::expression_node *vec)
        : basic_node(lineno), _low(low), _high(high), _func(func), _vec(vec) {}

    cdk::expression_node *low() { return _low; }

    cdk::expression_node *high() { return _high; }

    cdk::expression_node *func() { return _func; }

    cdk::expression_node *vec() { return _vec; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_between_node(this, level);
    }
};

} // til

#endif
