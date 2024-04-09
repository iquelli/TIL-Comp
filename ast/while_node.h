#ifndef __SIMPLE_AST_WHILE_NODE_H__
#define __SIMPLE_AST_WHILE_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing while-cycle nodes.
   */
  class while_node : public cdk::basic_node {
    cdk::expression_node *_condition;
    cdk::basic_node *_block;

  public:
    while_node(int lineno, cdk::expression_node *condition, cdk::basic_node *block) :
        basic_node(lineno), _condition(condition), _block(block) {
    }

    cdk::expression_node *condition() { return _condition; }

    cdk::basic_node *block() { return _block; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_while_node(this, level); }

  };

} // til

#endif
