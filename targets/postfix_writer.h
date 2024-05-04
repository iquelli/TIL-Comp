#ifndef __TIL_TARGETS_POSTFIX_WRITER_H__
#define __TIL_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <cdk/emitters/basic_postfix_emitter.h>
#include <set>
#include <sstream>

namespace til {

//!
//! Traverse syntax tree and generate the corresponding assembly code.
//!
class postfix_writer : public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    std::set<std::string> _functionsToDeclare;

    /** Code generation. */
    cdk::basic_postfix_emitter &_pf;
    int _lbl;

    /** Semantic analysis. */
    bool _inFunctionBody = false;
    bool _lastBlockInstrSeen = false;

    /** To have access to a function's segments. */
    std::vector<std::string> _functionLabels;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler,
                   cdk::symbol_table<til::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf)
        : basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {}

  public:
    ~postfix_writer() { os().flush(); }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
        std::ostringstream oss;
        if (lbl < 0)
            oss << ".L" << -lbl;
        else
            oss << "_L" << lbl;
        return oss.str();
    }

    /** Method used to print error messages. */
    void error(int lineno, std::string e) {
        std::cerr << lineno << ": " << e << std::endl;
    }

  protected:
    void process_additive_expr(cdk::binary_operation_node *const node, int lvl);
    void process_multiplicative_expr(cdk::binary_operation_node *const node,
                                     int lvl);
    void process_comparison_expr(cdk::binary_operation_node *const node,
                                 int lvl);

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
};

} // til

#endif
