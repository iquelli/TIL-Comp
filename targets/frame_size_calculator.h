#ifndef __TIL_TARGETS_FRAME_SIZE_CALCULATOR_H__
#define __TIL_TARGETS_FRAME_SIZE_CALCULATOR_H__

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <stack>

namespace til {

class frame_size_calculator : public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;

    size_t _localsize;

  public:
    frame_size_calculator(std::shared_ptr<cdk::compiler> compiler,
                          cdk::symbol_table<til::symbol> &symtab)
        : basic_ast_visitor(compiler), _symtab(symtab), _localsize(0) {}

  public:
    ~frame_size_calculator();

  public:
    size_t localsize() const { return _localsize; }

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
};

} // til

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, node)                                    \
    {                                                                          \
        try {                                                                  \
            til::type_checker checker(compiler, symtab, this);                 \
            (node)->accept(&checker, 0);                                       \
        } catch (const std::string &problem) {                                 \
            std::cerr << (node)->lineno() << ": " << problem << std::endl;     \
            return;                                                            \
        }                                                                      \
    }

#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node)

#endif
