#ifndef __SIMPLE_FACTORY_H__
#define __SIMPLE_FACTORY_H__

#include <memory>
#include <cdk/yy_factory.h>
#include "til_scanner.h"

namespace til {

  /**
   * This class implements the compiler factory for the Simple compiler.
   */
  class factory: public cdk::yy_factory<til_scanner> {
    /**
     * This object is automatically registered by the constructor in the
     * superclass' language registry.
     */
    static factory _self;

  protected:
    /**
     * @param language name of the language handled by this factory (see .cpp file)
     */
    factory(const std::string &language = "til") :
        cdk::yy_factory<til_scanner>(language) {
    }

  };

} // til

#endif
