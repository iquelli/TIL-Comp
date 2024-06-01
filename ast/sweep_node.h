#ifndef __TIL_AST_SWEEP_NODE_H__
#define __TIL_AST_SWEEP_NODE_H__

#include <cdk/ast/basic_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

/**
 * Class for describing sweep nodes.
 */
class sweep_node : public cdk::basic_node {
    cdk::expression_node *_vec, *_low, *_high, *_func, *_cond;

  public:
    sweep_node(int lineno, cdk::expression_node *vec, cdk::expression_node *low, cdk::expression_node *high, cdk::expression_node *func, cdk::expression_node *cond)
        : basic_node(lineno), _vec(vec), _low(low), _high(high), _func(func), _cond(cond) {}

    cdk::expression_node *vec() { return _vec; }

    cdk::expression_node *low() { return _low; }

    cdk::expression_node *high() { return _high; }

    cdk::expression_node *func() { return _func; }

    cdk::expression_node *cond() { return _cond; }

    void accept(basic_ast_visitor *sp, int level) {
        sp->do_sweep_node(this, level);
    }
};

} // til

#endif
