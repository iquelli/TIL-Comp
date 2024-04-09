#ifndef __SIMPLE_AST_PROGRAM_NODE_H__
#define __SIMPLE_AST_PROGRAM_NODE_H__

#include <cdk/ast/basic_node.h>

namespace til {

  /**
   * Class for describing program nodes.
   */
  class program_node : public cdk::basic_node {
    cdk::basic_node *_statements;

  public:
    program_node(int lineno, cdk::basic_node *statements) :
        cdk::basic_node(lineno), _statements(statements) {
    }

    cdk::basic_node *statements() { return _statements; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_program_node(this, level); }

  };

} // til

#endif
