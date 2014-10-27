//
//  ErlangBBLM.h
//  ErlangBBLM
//
//  Created by Paul Guyot on 26/10/2014.
//
//

#ifndef ErlangBBLM_ErlangBBLM_h
#define ErlangBBLM_ErlangBBLM_h

#define BUNDLE_IDENTIFIER			@"com.semiocast.bblm.erlang"
#define ATTRIBUTES_FOR_COMPLETION	@"ErlangAttributesForCompletion"
#define DOCTAGS_FOR_COMPLETION		@"ErlangDocTagsForCompletion"
#define FUNCTIONS_FOR_COMPLETION	@"ErlangFunctionsForCompletion"
#define TYPES_FOR_COMPLETION        @"ErlangTypesForCompletion"

#define kErlangCodeLangType 'Erlg'
#define kErlangApplicationResourceFileLangType 'ErlA'
#define kErlangApplicationUpgradeFileLangType 'ErlU'
#define kErlangReleaseResourceFileLangType 'ErlR'
#define kErlangScriptFileLangType 'ErlS'

// Prototypes of tested methods.
namespace com_semiocast_bblm_erlang {
    NSString* ParseErlangString(NSString* expression);
    NSString* EvalErlang(NSString* expression);
}

#endif
