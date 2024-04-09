#ifndef __SIMPLE_AST_READ_NODE_H__
#define __SIMPLE_AST_READ_NODE_H__

#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing read nodes.
   */
  class read_node : public cdk::basic_node {
    cdk::lvalue_node *_argument;

  public:
    read_node(int lineno, cdk::lvalue_node *argument) :
        cdk::basic_node(lineno), _argument(argument) {
    }

    cdk::lvalue_node *argument() { return _argument; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_read_node(this, level); }

  };

} // til

#endif
