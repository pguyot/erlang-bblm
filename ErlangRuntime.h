//
//  ErlangRuntime.h
//  ErlangBBLM
//
//  Created by Paul Guyot on 29/10/2014.
//
//

#import <Foundation/Foundation.h>

namespace com_semiocast_bblm_erlang {
    NSString* ParseErlangString(NSString* expression);
    NSString* EvalErlang(NSString* expression);
    NSString* SymbolToManPage(NSString* symbol);
    NSString* ErlangManPage(NSString* page);
}
