// ==============================
// File:            ErlangBBLM.cpp
// Project:			ErlangBBLM
// Written by:		Paul Guyot (paulguyot@ieee.org) with the help of
//                  Jim Correia and the BBEdit SDK.
// 
// Licensed under the MIT License.
// 
// Copyright (c) 2008-2009 by Paul Guyot, Semiocast.
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
// ===========

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#include <Carbon/Carbon.h>
#include <CoreFoundation/CoreFoundation.h>

#include "BBEdit SDK/Interfaces/Language Modules/BBLMInterface.h"
#include "BBEdit SDK/Interfaces/Tools/BBXTInterface.h"
#include "BBEdit SDK/Interfaces/Language Modules/BBLMTextIterator.h"

#define BUNDLE_IDENTIFIER			CFSTR("com.semiocast.bblm.erlang")
#define ATTRIBUTES_FOR_COMPLETION	CFSTR("ErlangAttributesForCompletion")
#define DOCTAGS_FOR_COMPLETION		CFSTR("ErlangDocTagsForCompletion")
#define FUNCTIONS_FOR_COMPLETION	CFSTR("ErlangFunctionsForCompletion")

#define DEBUG 0

// DEBUGing macros
#if DEBUG
void debugf_(const char* func,const char* fileName,long line, const char*fmt,...)
{
	va_list arg;
	char msg[256];
	va_start(arg, fmt);
	vsnprintf(msg,256 ,fmt, arg);
    DebugAssert(COMPONENT_SIGNATURE, DEBUG_NO_OPTIONS, "BBErl.LM" ": " , msg, nil, fileName, line, 0 );

}
#define debugf(FMT,...)  debugf_( __FUNCTION__,__FILE__, __LINE__,FMT,__VA_ARGS__);
#else
#define debugf(FMT,...) 
#endif

CFArrayRef gAttributesDict = NULL;
CFArrayRef gFunctionsDict = NULL;
CFArrayRef gDocTagsDict = NULL;
CFSetRef gPredefinedNames = NULL;

static const CFStringRef kCompletionDictKeys[] = {kBBLMCompletionSymbolType, kBBLMSymbolCompletionDisplayString, kBBLMSymbolCompletionText};

static OSErr Init()
{
	OSErr result = noErr;
	CFBundleRef myBundle = CFBundleGetBundleWithIdentifier(BUNDLE_IDENTIFIER);
	if (myBundle != NULL)
	{
		gAttributesDict = (CFArrayRef) CFBundleGetValueForInfoDictionaryKey(myBundle, ATTRIBUTES_FOR_COMPLETION);
		if (gAttributesDict != NULL)
		{
			CFRetain(gAttributesDict);
		}
		gDocTagsDict = (CFArrayRef) CFBundleGetValueForInfoDictionaryKey(myBundle, DOCTAGS_FOR_COMPLETION);
		if (gDocTagsDict != NULL)
		{
			CFRetain(gDocTagsDict);
		}
		gFunctionsDict = (CFArrayRef) CFBundleGetValueForInfoDictionaryKey(myBundle, FUNCTIONS_FOR_COMPLETION);
		if (gFunctionsDict != NULL)
		{
			CFRetain(gFunctionsDict);

            // Create the set of predefined names.
            CFIndex nbValues = CFArrayGetCount(gFunctionsDict);
            CFMutableSetRef theSet = CFSetCreateMutable(NULL, nbValues / 3, &kCFTypeSetCallBacks);
            CFIndex indexValue;
            for (indexValue = 0; indexValue < nbValues; indexValue += 3)
            {
                CFStringRef theName = (CFStringRef) CFArrayGetValueAtIndex(gFunctionsDict, indexValue);
                CFSetAddValue(theSet, theName);
            }
            gPredefinedNames = theSet;
		}
	} else {
		result = fnfErr;
		debugf("CANNOT GET BUNDLE", NULL);
	}
	
	return result;
}

static void Dispose()
{
	if (gAttributesDict != NULL)
	{
		CFRelease(gAttributesDict);
		gAttributesDict = NULL;
	}
	if (gDocTagsDict != NULL)
	{
		CFRelease(gDocTagsDict);
		gDocTagsDict = NULL;
	}
	if (gFunctionsDict != NULL)
	{
		CFRelease(gFunctionsDict);
		gFunctionsDict = NULL;
	}
	if (gPredefinedNames != NULL)
	{
		CFRelease(gPredefinedNames);
		gPredefinedNames = NULL;
	}
}

// My own color stuff.
enum
{
	kErlRunIsCommentTag = kBBLMFirstUserRunKind,
	kErlRunIsMacroName
};

// My own function kinds.
enum
{
	kErlSpecAttr = kBBLMFirstUserFunctionKind
};

#define iswordchar(x) (isalnum(x)||x=='_')

struct runloc
{
	UInt32 	pos;
	UInt32	last_start;
};

enum
{
	kBufsize = 3
};

typedef UniChar charbuf[kBufsize]; 

static UniChar start(struct runloc& r, BBLMTextIterator &p, BBLMParamBlock &pb, charbuf chars)
{
	r.last_start = pb.fCalcRunParams.fStartOffset;
	r.pos = pb.fCalcRunParams.fStartOffset;
	p += r.pos;
	
	if (chars)
	{
		for (int i=1; i <kBufsize; i++)
			chars[i] = 0;
			
		chars[0] = *p;
	}
	
	return *p;
}

static UniChar nextchar(struct runloc& r, BBLMTextIterator &p, BBLMParamBlock &pb)
{
	if (r.pos < pb.fTextLength)
	{
		r.pos++;
		p++;

		return *p;
	}
		
	return 0;
}

static bool addRun(BBLMRunCode kind, int  start,int len , const BBLMCallbackBlock& bblm_callbacks)
{
	if (len > 0) { // Tie off the code run we were in, unless the length is zero.
		return bblmAddRun(	&bblm_callbacks, 'Erlg',
							kind, start, len, false);
							
	}
	else{
		return true;
	}
}					

static bool addRunAt (BBLMRunCode kind, struct runloc& r, const BBLMCallbackBlock& bblm_callbacks, int off=0)
{
	bool more_runs = addRun(kind, r.last_start, r.pos - r.last_start+1+off, bblm_callbacks);
	r.last_start =  r.pos+1+off;
	return more_runs;
}

// Determine if a given string is a macro.
// The iterator is set on the first character of the macro.
static bool isMacro(BBLMTextIterator& p, int inNameLen, const BBLMCallbackBlock& bblm_callbacks)
{
	// TODO.
	return true;
}

// Color a macro, i.e. anything until a word break (any non-valid macro character will do).
static bool colormacro(
				BBLMParamBlock &pb,
				struct runloc &r,
				BBLMTextIterator &p,
				const BBLMCallbackBlock &bblm_callbacks)
{
	bool result = true;
	UniChar c;
	
	UInt32 startChar = r.pos;
	BBLMTextIterator nameIter(p);
	nameIter++;
	
	while (c = nextchar(r, p, pb)) {
		if (!iswordchar(c)) {
			UInt32 nameLen = r.pos - startChar - 1;
			// Is it a macro?
			if (isMacro(nameIter, nameLen, bblm_callbacks)) {
				result = addRunAt(kErlRunIsMacroName, r, bblm_callbacks, -1);
			}
			break;
		}
	}
	return result;
}

// Color a string, i.e. define a run until the next " character.
// If a return is encountered, exit.
// If a backslash is encountered, skip the next character.
static bool colorstr(
				BBLMParamBlock &pb,
				struct runloc &r,
				BBLMTextIterator &p,
				const BBLMCallbackBlock &bblm_callbacks)
{
	bool result = true;
	UniChar c;
	
	while (c = nextchar(r, p, pb)) {
		if (c == '"') {
			result = addRunAt(kBBLMRunIsSingleString,r,bblm_callbacks);
			break;
		}
		if (c == '\r' || c == '\n') {
			// Not a string.
			break;
		}
		if (c == '\\'){
			nextchar(r, p, pb);
		}
	}
	return result;
}

// Color a comment, i.e. define a run until the next endline.
// We also color edoc tags (actually, anything starting with @ in a double % comment).
static bool colorcomment(BBLMParamBlock &pb,
				struct runloc &r,
				BBLMTextIterator &p,
				const BBLMCallbackBlock &bblm_callbacks)
{
	bool more_runs = true;
	UniChar c;
	c = nextchar(r, p, pb);
	
	if (c == '%')
	{
		// Double %, scan for edoc tags.
		while (c = nextchar(r, p, pb))
		{
			if (c=='@') {
				more_runs = addRunAt(kBBLMRunIsLineComment, r, bblm_callbacks, -1);
				if (!more_runs) {
					break;
				}

				while (c = nextchar(r, p, pb)) {
					if (c==' ' || c=='\r' || c=='\n') {
						break;
					}
				}
				more_runs = addRunAt(kErlRunIsCommentTag, r, bblm_callbacks, -1);
				if (!more_runs) {
					break;
				}
			}
			if (c=='\r'|| c=='\n'){
				break;
			}
		}
	} else {
		while (c) {
			if (c=='\r'|| c=='\n'){
				break;
			}
			
			c = nextchar(r, p, pb);
		}
	}
	return more_runs && addRunAt(kBBLMRunIsLineComment,r,bblm_callbacks);
}

// Compute the runs for color.
// If a " is encountered, this might be a string.
// If a ? is encountered, this might be a macro invocation.
// If a % is encountered, this is a comment.
// If a backslash is encountered, skip the next character (typically for $\?, $\" or $\%)
static void CalculateRuns(BBLMParamBlock &pb,
			const BBLMCallbackBlock &bblm_callbacks)

{
	bool more_runs = true;
	UniChar c;	
	struct runloc r;
	BBLMTextIterator p(pb);
	
	c = start(r, p, pb, NULL);
	
	while (c && more_runs)
	{
		// Process a char

		// If we're in the basic 'code' state, check for each interesting char (rundelims[i].start).
		switch (c)
		{
		case '"': 
			// This might be a (valid) string.
			// Until now, we had code.
			more_runs = addRunAt(kBBLMRunIsCode, r, bblm_callbacks, -1);
			if (more_runs)
			{
				more_runs = colorstr(pb,r,p,bblm_callbacks);
			}
			break;
		case '?': 
			// This might be a macro invocation.
			// Until now, we had code.
			more_runs = addRunAt(kBBLMRunIsCode, r, bblm_callbacks, -1);
			if (more_runs)
			{
				more_runs = colormacro(pb, r, p, bblm_callbacks);
			}
			break;
		case '%':
			// This is a comment, for sure.
			// Until now, we had code.
			more_runs = addRunAt(kBBLMRunIsCode, r, bblm_callbacks, -1);
			if (more_runs)
			{
				more_runs = colorcomment(pb, r, p, bblm_callbacks); 
			}
			break;
		case '\\':
			// Skip the next character.
			nextchar(r, p, pb);
			break;
			
		default:
			break;
		}
		c = nextchar(r, p, pb);
	}
	if (more_runs)
	{
		addRunAt(kBBLMRunIsCode, r, bblm_callbacks, -1);
	}
}

static bool isCommentKind(BBLMRunCode kind)
{
	return (
	    kind == kBBLMRunIsLineComment ||
	    kind == kErlRunIsCommentTag);
}

static void AdjustRange(BBLMParamBlock &params,
						const BBLMCallbackBlock &callbacks)
{	
	DescType language;
	BBLMRunCode kind;
	SInt32 charPos;
	SInt32 length;
	UInt32 index = params.fAdjustRangeParams.fStartIndex;
	
	while(	index > 0 &&
			bblmGetRun(&callbacks, index, language, kind, charPos, length) &&
			isCommentKind(kind)){
	 	index--;
	};
	params.fAdjustRangeParams.fStartIndex = index;
}

static void eat_str(BBLMTextIterator& inIter)
{
    UInt16 theChar;
    do {
	    theChar = inIter.GetNextChar();
		if (theChar == '"') {
			break;
		}
		if (theChar == '\\') {
			theChar = inIter.GetNextChar();
		}
	} while (theChar != '\r' && theChar != '\n' && theChar != 0);
}

static bool eat_line(BBLMTextIterator& inIter)
{
    UInt16 theChar = *inIter;
    while (theChar != '\r' && theChar != '\n' && theChar != 0) {
	    theChar = inIter.GetNextChar();
	}
	return theChar != 0;
}

// Match an atom followed by something in parentheses, counting commas and noting when the first parameter started and ended.
static bool match_atom_parentheses(BBLMTextIterator& inIter, UInt32* outAtomStart, UInt32* outAtomEnd, UInt32* outFirstParamStart, UInt32* outFirstParamEnd, UInt32* outParamEnd, UInt32* outNbArgs)
{
    enum EMatchAtomPState {
        kMatchAtomPUnmatch,
        kMatchAtomPInAtom,
        kMatchAtomPBeforeOpeningParenthesis,
        kMatchAtomPBeforeFirstParamStart,
        kMatchAtomPBeforeFirstParamEnd,
        kMatchAtomPInsideParentheses,
		kMatchAtomPClosingParenthesis
    };
    
    UInt32 theAtomStart = 0;
    UInt32 theAtomEnd = 0;
	UInt32 theFirstParamStart = 0;
	UInt32 theFirstParamEnd = 0;
    UInt32 theParamEnd = 0;
    UInt32 theNbArgs = 1;
	
	int parenthesisCount = 0;
	int bracketsCount = 0;
	int accoladeCount = 0;
	
    EMatchAtomPState state; 
	// We first need an atom, which starts with a lower case letter.
    UInt16 theChar = inIter.GetNextChar();
    if (theChar == 0 || !islower(theChar)) {
        state = kMatchAtomPUnmatch;
    } else {
        theAtomStart = inIter.Offset() - 1;
        state = kMatchAtomPInAtom;
    }
    
	// Then we need to skip until the end of the atom.
    while (state == kMatchAtomPInAtom) {
        theChar = inIter.GetNextChar();
        if (theChar == 0) {
            state = kMatchAtomPUnmatch;
        } else if (isspace(theChar) || theChar == '\r' || theChar == '\n') {
            theAtomEnd = inIter.Offset() - 1;
            state = kMatchAtomPBeforeOpeningParenthesis;
        } else if (theChar == '(') {
			theFirstParamStart = inIter.Offset();
            theAtomEnd = theFirstParamStart - 1;
            state = kMatchAtomPBeforeFirstParamStart;
        } else if (theChar == '%') {
            theAtomEnd = inIter.Offset() - 1;
            eat_line(inIter);
            state = kMatchAtomPBeforeOpeningParenthesis;
        } else if (!iswordchar(theChar)) {
            state = kMatchAtomPUnmatch;
        }
    }
    
    // Skip spaces and comments until the opening parenthesis.
    while (state == kMatchAtomPBeforeOpeningParenthesis) {
        theChar = inIter.GetNextChar();
        if (theChar == 0) {
            state = kMatchAtomPUnmatch;
        } else if (theChar == '(') {
			theFirstParamStart = inIter.Offset();
            state = kMatchAtomPBeforeFirstParamStart;
        } else if (theChar == '%') {
            eat_line(inIter);
        } else if (!isspace(theChar) && theChar != '\r' && theChar != '\n') {
            state = kMatchAtomPUnmatch;
        }
    }

	bool startLine = false;
	
    // We are looking for the first param start.
	while (state == kMatchAtomPBeforeFirstParamStart) {
		startLine = false;
		theChar = inIter.GetNextChar();
		if (theChar == 0) {
			state = kMatchAtomPUnmatch;
		} else if (isspace(theChar)) {
		    // startLine = false;
        } else if (theChar == '%') {
            eat_line(inIter);
			startLine = true;
		} else if (theChar == '\r' || theChar == '\n') {
			startLine = true;
        } else if (theChar == ')') {
			if (parenthesisCount == 0) {
                state = kMatchAtomPClosingParenthesis;
                theParamEnd = inIter.Offset();
            } else {
                parenthesisCount--;
            }
		} else if (startLine) {
			state = kMatchAtomPUnmatch;
		} else if (theChar == ',') {
			state = kMatchAtomPUnmatch;
		} else {
		    inIter--;
		    theFirstParamStart = inIter.Offset();
			state = kMatchAtomPBeforeFirstParamEnd;
        }
	}

    // We are looking for the first param end.
	while (state == kMatchAtomPBeforeFirstParamEnd) {
		startLine = false;
		theChar = inIter.GetNextChar();
		if (theChar == 0) {
			state = kMatchAtomPUnmatch;
        } else if (theChar == '%') {
            eat_line(inIter);
			startLine = true;
		} else if (theChar == '\r' || theChar == '\n') {
			startLine = true;
        } else if (theChar == ')') {
			if (parenthesisCount == 0) {
                state = kMatchAtomPClosingParenthesis;
    		    theFirstParamEnd = inIter.Offset() - 1;
                theParamEnd = inIter.Offset();
            } else {
                parenthesisCount--;
            }
        } else if (theChar == '[') {
            bracketsCount++;
        } else if (theChar == ']') {
            bracketsCount--;
            if (bracketsCount < 0) {
                state = kMatchAtomPUnmatch;
            }
        } else if (theChar == '{') {
            accoladeCount++;
        } else if (theChar == '}') {
            accoladeCount--;
            if (accoladeCount < 0) {
                state = kMatchAtomPUnmatch;
            }
        } else if (theChar == '(') {
            parenthesisCount++;
		} else if (startLine) {
			state = kMatchAtomPUnmatch;
		} else if ((parenthesisCount == 0) && (bracketsCount == 0) && (accoladeCount == 0) && (isspace(theChar) || theChar == ',')) {
		    inIter--;
		    theFirstParamEnd = inIter.Offset();
			state = kMatchAtomPInsideParentheses;
        }
	}

    // We are inside parentheses and we count them (as well as arguments).
	// If a line starts with a non space character, we give up. Usually, this
	// means this is no function/attribute definition.
	while (state == kMatchAtomPInsideParentheses) {
		startLine = false;
		theChar = inIter.GetNextChar();
		if (theChar == 0) {
			state = kMatchAtomPUnmatch;
		} else if (theChar == '(') {
			parenthesisCount++;
		} else if (theChar == ')') {
			if (parenthesisCount == 0) {
				state = kMatchAtomPClosingParenthesis;
				theParamEnd = inIter.Offset();
			} else {
				parenthesisCount--;
			}
        } else if (theChar == '[') {
            bracketsCount++;
        } else if (theChar == ']') {
            bracketsCount--;
            if (bracketsCount < 0) {
                state = kMatchAtomPUnmatch;
            }
        } else if (theChar == '{') {
            accoladeCount++;
        } else if (theChar == '}') {
            accoladeCount--;
            if (accoladeCount < 0) {
                state = kMatchAtomPUnmatch;
            }
		} else if (theChar == '"') {
			eat_str(inIter);
		} else if (theChar == ',') {
			if ((parenthesisCount == 0) && (bracketsCount == 0) && (accoladeCount == 0)) {
				theNbArgs++;
			}
		} else if (theChar == '%') {
			eat_line(inIter);
			startLine = true;
		} else if (theChar == '\r' || theChar == '\n') {
			startLine = true;
		} else if (startLine && !isspace(theChar)) {
			state = kMatchAtomPUnmatch;
		}
	}
	
    // We got an atom if we found the opening parenthesis.
    bool theResult = state == kMatchAtomPClosingParenthesis;
    if (theResult) {
        if (outAtomStart != NULL) {
            *outAtomStart = theAtomStart;
        }
        if (outAtomEnd != NULL) {
            *outAtomEnd = theAtomEnd;
        }
        if (outFirstParamStart != NULL) {
    		*outFirstParamStart = theFirstParamStart;
        }
        if (outFirstParamEnd != NULL) {
    		*outFirstParamEnd = theFirstParamEnd;
        }
        if (outParamEnd != NULL) {
    		*outParamEnd = theParamEnd;
		}
		
		if (outNbArgs != NULL) {
            if (theFirstParamEnd == 0) {
                theNbArgs = 0;
            }
            *outNbArgs = theNbArgs;
        }
    }

	return theResult;	
}


// Match an attribute.
static bool match_attribute(BBLMTextIterator& inIter, UInt32* outAtomStart, UInt32* outAtomEnd, UInt32* outFirstParamStart, UInt32* outFirstParamEnd)
{
    bool theResult = match_atom_parentheses(inIter, outAtomStart, outAtomEnd, outFirstParamStart, outFirstParamEnd, NULL, NULL);
    if (theResult) {
        // We look for the final dot.
        while (true) {
            UInt16 theChar = inIter.GetNextChar();
            if (theChar == 0) {
                theResult = false;
                break;
            }
            if (theChar == '.') {
                break;
            }
            if (!isspace(theChar) && theChar != '\r' && theChar != '\n') {
                theResult = false;
                break;
            }
        }
    }
	
    return theResult;
}

// Match a clause.
static bool match_clause(BBLMTextIterator& inIter, UInt32* outAtomStart, UInt32* outAtomEnd, UInt32* outParamEnd, UInt32* outNbArgs)
{
    return match_atom_parentheses(inIter, outAtomStart, outAtomEnd, NULL, NULL, outParamEnd, outNbArgs);
}

static OSErr addAttribute(UInt32 attrStart, UInt32 attrEnd, UInt32 firstParamStart, UInt32 firstParamEnd, UInt32 paramEnd, BBLMParamBlock &pb, const BBLMCallbackBlock &bblm_callbacks)
{
	debugf("addAttribute(%i, %i)", attrStart, attrEnd);
	OSErr err = noErr;
	UInt32 offset = 0;
	UInt32 namelen = attrEnd - attrStart;
	BBLMTextIterator	text(pb);
	BBLMTextIterator	attrIter(text, attrStart + 1);
	bool skip = false;
	bool parseDialyzerTypeDecl = false;
    UInt32 theKind = kBBLMPragmaMark;
	if (namelen == 12 && attrIter.strcmp("include_lib", namelen - 1) == 0) {
	    theKind = kBBLMSysInclude;
	} else if (namelen == 8 && attrIter.strcmp("include", namelen - 1) == 0) {
	    theKind = kBBLMInclude;
	} else if (namelen == 5 && attrIter.strcmp("spec", namelen - 1) == 0) {
	    theKind = kErlSpecAttr;
		parseDialyzerTypeDecl = true;
	} else if (namelen == 5 && attrIter.strcmp("type", namelen - 1) == 0) {
	    theKind = kBBLMTypedef;
		parseDialyzerTypeDecl = true;
	} else if (namelen == 7 && attrIter.strcmp("record", namelen - 1) == 0) {
	    theKind = kBBLMTypedef;
	} else if (namelen == 7 && attrIter.strcmp("define", namelen - 1) == 0) {
	    theKind = kBBLMTypedef;
	} else if (namelen == 6 && attrIter.strcmp("ifdef", namelen - 1) == 0) {
	    skip = true;
	}
	if (!skip)
	{
		attrIter--;
		
		// The name depends on the kind.
		void* nameAddr;
		UInt32 paramLen = firstParamEnd - firstParamStart;
		UInt16 buffer[namelen + paramLen + 2];
		bool nameIsUnicode;
		
		if (theKind == kBBLMSysInclude || theKind == kBBLMInclude) {
			namelen = firstParamEnd - firstParamStart - 2;	// Skip quotes.
			BBLMTextIterator nameIter(text, firstParamStart + 1);
			nameAddr = nameIter.Address();
			nameIsUnicode = pb.fTextIsUnicode;
		} else if (theKind == kBBLMTypedef || theKind == kErlSpecAttr) {
			// Build our own name.
			UInt32 attrLen = namelen;
			namelen += paramLen + 2;
			UInt32 indexCopy;
			for (indexCopy = 0; indexCopy < attrLen; indexCopy++)
			{
				buffer[indexCopy] = attrIter[indexCopy];
			}
			buffer[attrLen] = '(';		
			BBLMTextIterator paramIter(text, firstParamStart);
			for (indexCopy = 0; indexCopy < paramLen; indexCopy++)
			{
				UInt16 theChar = paramIter[indexCopy];
				if (parseDialyzerTypeDecl && theChar == ':')
				{
					break;
				}
				buffer[attrLen + 1 + indexCopy] = theChar;
			}
			buffer[attrLen + 1 + indexCopy] = ')';
			namelen = attrLen + indexCopy + 2;
			nameAddr = &buffer[0];
			nameIsUnicode = true;
		} else {
			nameAddr = attrIter.Address();
			nameIsUnicode = pb.fTextIsUnicode;
		}

		err = bblmAddTokenToBuffer(	&bblm_callbacks, 
									pb.fFcnParams.fTokenBuffer,
									nameAddr,
									namelen,
									nameIsUnicode,
									&offset);
		
		if (err == noErr)
		{
			BBLMProcInfo theProcInfo;
			theProcInfo.fFunctionStart = attrStart;     //	char offset in file of first character of function
			theProcInfo.fFunctionEnd = paramEnd;        //	char offset of last character of function
			
			theProcInfo.fSelStart = firstParamStart;    //	first character to select when choosing function
			theProcInfo.fSelEnd = paramEnd;             //	last character to select when choosing function
			
			theProcInfo.fFirstChar = 0;                 //	first character to make visible when choosing function
			
			theProcInfo.fKind = theKind;
			
			theProcInfo.fIndentLevel = 0;               //	indentation level of token
			theProcInfo.fFlags = 0;                     //	token flags (see BBLMFunctionFlags)
			theProcInfo.fNameStart = offset;            //	char offset in token buffer of token name
			theProcInfo.fNameLength = namelen;          //	length of token name

			UInt32 index;
			err = bblmAddFunctionToList(&bblm_callbacks,	
										pb.fFcnParams.fFcnList,
										theProcInfo,
										&index);																
		}
	}
	return err;
}

static void addFunction(UInt32 atomStart, UInt32 atomEnd, UInt32 paramEnd, UInt32 functionEnd, UInt32 nbArgs, BBLMParamBlock &pb, const BBLMCallbackBlock &bblm_callbacks)
{
	BBLMTextIterator	text(pb);
	BBLMTextIterator	nameIter(text, atomStart);
	UInt32 namelen = atomEnd - atomStart;
    UInt16 theName[namelen + 16];
    unsigned int indexName;
    for (indexName = 0; indexName < namelen; indexName++) {
        theName[indexName] = nameIter.GetNextChar();
    }
    theName[indexName++] = '/';
    char theArity[16];
    sprintf(theArity, "%u", (unsigned int) nbArgs);
    for (unsigned int j = 0; j < sizeof(theArity); j++) {
        char theArityChar = theArity[j];
        theName[indexName++] = theArityChar;
        if (theArityChar == 0) {
            break;
        }
    }
    indexName--;
    
	OSErr err;
	UInt32 offset = 0;
	err = bblmAddTokenToBuffer(	&bblm_callbacks, 
								pb.fFcnParams.fTokenBuffer,
								theName,
								indexName,
								true,
								&offset);
	
	BBLMProcInfo theProcInfo;
	theProcInfo.fFunctionStart = atomStart;     //	char offset in file of first character of function
	theProcInfo.fFunctionEnd = functionEnd;     //	char offset of last character of function
	
	theProcInfo.fSelStart = atomStart;          //	first character to select when choosing function
	theProcInfo.fSelEnd = functionEnd;          //	last character to select when choosing function
	
	theProcInfo.fFirstChar = atomStart;         //	first character to make visible when choosing function
	
	theProcInfo.fKind = kBBLMFunctionMark;
	
	theProcInfo.fIndentLevel = 0;               //	indentation level of token
	theProcInfo.fFlags = 0;                     //	token flags (see BBLMFunctionFlags)
	theProcInfo.fNameStart = offset;            //	char offset in token buffer of token name
	theProcInfo.fNameLength = indexName;        //	length of token name

    UInt32 index;
	(void) bblmAddFunctionToList(&bblm_callbacks,	
								pb.fFcnParams.fFcnList,
								theProcInfo,
								&index);

	(void) bblmAddFoldRange(&bblm_callbacks, paramEnd + 1, functionEnd - paramEnd - 1, kBBLMManualFold);
}

static void addCallout(UInt32 startCallout, UInt32 startText, UInt32 endText, UInt32 inKind, BBLMParamBlock &pb, const BBLMCallbackBlock &bblm_callbacks)
{
	BBLMTextIterator	text(pb);
	BBLMTextIterator	nameIter(text, startCallout);
	UInt32 namelen = endText - startCallout;

	OSErr err;
	UInt32 offset = 0;
	err = bblmAddTokenToBuffer(	&bblm_callbacks, 
								pb.fFcnParams.fTokenBuffer,
								nameIter.Address(),
								namelen,
								pb.fTextIsUnicode,
								&offset);
	
	BBLMProcInfo theProcInfo;
	theProcInfo.fFunctionStart = startCallout;  //	char offset in file of first character of function
	theProcInfo.fFunctionEnd = endText;         //	char offset of last character of function
	
	theProcInfo.fSelStart = startText;          //	first character to select when choosing function
	theProcInfo.fSelEnd = endText;              //	last character to select when choosing function
	
	theProcInfo.fFirstChar = 0;                 //	first character to make visible when choosing function
	
	theProcInfo.fKind = inKind;
	
	theProcInfo.fIndentLevel = 0;               //	indentation level of token
	theProcInfo.fFlags = 0;                     //	token flags (see BBLMFunctionFlags)
	theProcInfo.fNameStart = offset;            //	char offset in token buffer of token name
	theProcInfo.fNameLength = namelen;          //	length of token name

    UInt32 index;
	(void) bblmAddFunctionToList(&bblm_callbacks,	
								pb.fFcnParams.fFcnList,
								theProcInfo,
								&index);
}

static void ScanForCalloutInComments(BBLMTextIterator& iter, BBLMParamBlock& pb, const BBLMCallbackBlock& bblm_callbacks)
{
    UInt16 theChar;
	theChar = iter.GetNextChar();
	if (theChar == '%')
	{
		// Double % comment, that's where we can find @todo callout.
		do {
			theChar = iter.GetNextChar();
			if (theChar == '@')
			{
				if (iter.CharsLeft() > 5 && ((iter.strcmp("todo ") == 0) || (iter.strcmp("TODO ") == 0)))
				{
					UInt32 startTodo = iter.Offset() - 1;
					iter += 5;
					UInt32 startText = iter.Offset();
					bool end_line = eat_line(iter);
					UInt32 endText = iter.Offset() - 1;
					if (!end_line)
					{
						endText++;
					}
					addCallout(startTodo, startText, endText, kBBLMToDoCallout, pb, bblm_callbacks);
				}
			}
		} while (theChar != 0 && theChar != '\n' && theChar != '\r');
	} else {
		if (theChar != 0 && theChar != '\n' && theChar != '\r')
		{
			eat_line(iter);
		}
	}
}

static void ScanForFunctions(BBLMParamBlock& pb,
			const BBLMCallbackBlock& bblm_callbacks)
{
    BBLMTextIterator text(pb);
    BBLMTextIterator iter(pb);
    UInt16 theChar;
    UInt32 atomStart = 0;
    UInt32 atomEnd = 0;
	UInt32 firstParamStart = 0;
	UInt32 firstParamEnd = 0;
	UInt32 paramEnd;
	UInt32 nbArgs = 0;

    UInt32 firstClauseAtomStart = 0;
    UInt32 firstClauseAtomEnd = 0;
	UInt32 firstClauseParamEnd = 0;
	UInt32 firstClauseNbArgs = 0;
	UInt32 functionEnd = 0;

	bool startline = true;
    do {
        theChar = iter.GetNextChar();
        switch (theChar) {
            case '\0':
            case '\r':
            case '\n':
                startline = true;
                break;
                
            case ' ':
                startline = false;
                break;

            // Comment: look for @todos.
            case '%':
				ScanForCalloutInComments(iter, pb, bblm_callbacks);
                startline = true;
                break;

            // Attribute.
            case '-':
                if (match_attribute(iter, &atomStart, &atomEnd, &firstParamStart, &firstParamEnd)) {
                    paramEnd = iter.Offset() - 2;
                    atomStart--;
                    addAttribute(atomStart, atomEnd, firstParamStart, firstParamEnd, paramEnd, pb, bblm_callbacks);
                }
                eat_line(iter);
                startline = true;
                break;

            // Whatever, just skip until the end of the line.
            default:
                if (startline && islower(theChar)) {
                    iter--;
                    if (match_clause(iter, &atomStart, &atomEnd, &paramEnd, &nbArgs)) {
                        bool sameFunction = false;
                        if (nbArgs == firstClauseNbArgs) {
                            UInt32 nameLen = atomEnd - atomStart;
                            if (nameLen == (firstClauseAtomEnd - firstClauseAtomStart)) {
                                sameFunction = true;
                                for (UInt32 i = 0; i < nameLen; i++) {
                                    if (text[atomStart + i] != text[firstClauseAtomStart + i]) {
                                        sameFunction = false;
                                        break;
                                    }
                                }
                            }
                        }
                        if (!sameFunction) {
                            // Add the function.
                            if (firstClauseAtomEnd > 0) {
								debugf("addFunction(%i, %i, %i, %i) [1]", firstClauseAtomStart, firstClauseAtomEnd, firstClauseParamEnd, functionEnd);
                                addFunction(firstClauseAtomStart, firstClauseAtomEnd, firstClauseParamEnd, functionEnd, firstClauseNbArgs, pb, bblm_callbacks);
                            }

                            // Save info for current function.
                            firstClauseAtomStart = atomStart;
                            firstClauseAtomEnd = atomEnd;
                            firstClauseParamEnd = paramEnd;
                            firstClauseNbArgs = nbArgs;
                        }
                    }
                }
                eat_line(iter);
                functionEnd = iter.Offset() - 1;
                startline = true;
                break;
        }
	} while (theChar != 0);

    // Add the last function.
    if (firstClauseAtomEnd > 0) {
		debugf("addFunction(%i, %i, %i, %i) [2]", firstClauseAtomStart, firstClauseAtomEnd, firstClauseParamEnd, functionEnd);
        addFunction(firstClauseAtomStart, firstClauseAtomEnd, firstClauseParamEnd, functionEnd, firstClauseNbArgs, pb, bblm_callbacks);
    }
}

static void SetCategories(BBLMCategoryTable inCategoryTable)
{
    inCategoryTable['?'] = 'a';   
    inCategoryTable['_'] = 'a';   
    inCategoryTable['-'] = 'a';
    inCategoryTable['!'] = 'a';
    inCategoryTable[':'] = 'a';
    inCategoryTable['.'] = 'a';
}

static void AdjustRangeForTextCompletion(BBLMParamBlock &pb, bblmAdjustCompletionRangeParams& ioParams)
{
//!!!:correia rewrote function

	// Use the proposed range in most cases.
	// For comments, adjust to include the leading '@'...

	ioParams.fOutAdjustedCompletionRange = ioParams.fInProposedCompletionRange;

	SInt16 theKind = ioParams.fInCompletionRangeStartRun.kind;

	debugf("AdjustRangeForTextCompletion, run language = %.8X, run kind = %i", ioParams.fInCompletionRangeStartRun.language, theKind);
	debugf("AdjustRangeForTextCompletion, run startPos = %i, run length = %i", ioParams.fInCompletionRangeStartRun.startPos, ioParams.fInProposedCompletionRange.length);
	
	if (theKind == kErlRunIsCommentTag || theKind == kBBLMRunIsLineComment)
	{
		// For comment ranges, we want to allow '@xxx' completion, but '@' isn't
		// in the general completion category table. Instead of generating our
		// own completion range from scratch, we'll just adjust the incoming
		// range to suit our needs (extend to include the leading '@')
		//
		// If the proposed range is {kCFNotFound, 0} and we have an empty
		// selection range, use that.
		
		CFRange range = ioParams.fInProposedCompletionRange;
		BBLMTextIterator text(pb);
		
		if (range.location == kCFNotFound && 
			ioParams.fInSelectionRange.location != kCFNotFound && 
			ioParams.fInSelectionRange.length == 0)
		{
			range = ioParams.fInSelectionRange;
		}
		
		if (range.location != kCFNotFound && range.location > 0 &&
		    text[range.location - 1] == '@')
		{
			range.location--;
			range.length++;
			ioParams.fOutAdjustedCompletionRange = range;
		}
	}
}

// Tell BBEdit if it can take tokens from the given run as completion candidates.
// We filter out comments and strings.
static void FilterRunForTextCompletion(bblmFilterCompletionRunParams ioParams)
{
	SInt16 runKind = ioParams.fInRunInfo.kind;
	ioParams.fOutCanCompleteTokensInRun =
		(runKind != kBBLMRunIsLineComment)
		&& (runKind != kBBLMRunIsSingleString);
}

static void SetCategoryTableForTextCompletion(BBLMCategoryTable inCategoryTable)
{
    inCategoryTable['?'] = 'a';   
    inCategoryTable['_'] = 'a';   
    inCategoryTable['-'] = 'a';
    inCategoryTable[':'] = 'a';
    inCategoryTable['.'] = '-';
}

static void AddSymbols(CFStringRef inPartial, CFMutableArrayRef inCompletionArray, CFArrayRef inDict, CFStringRef inType)
{
	CFIndex nbValues = CFArrayGetCount(inDict);
	CFIndex indexValue;
	for (indexValue = 0; indexValue < nbValues; indexValue += 3)
	{
		CFStringRef theCompletionCandidate = (CFStringRef) CFArrayGetValueAtIndex(inDict, indexValue);
		if (CFStringHasPrefix(theCompletionCandidate, inPartial))
		{
			CFStringRef theCompletionReplacement = (CFStringRef) CFArrayGetValueAtIndex(inDict, indexValue + 1);
			CFStringRef theCompletionDisplayString = (CFStringRef) CFArrayGetValueAtIndex(inDict, indexValue + 2);
			CFStringRef theValues[] = {inType, theCompletionDisplayString, theCompletionReplacement};
			CFDictionaryRef theCompletionDict =
				CFDictionaryCreate(
					NULL,
					(const void**) kCompletionDictKeys,
					(const void**) theValues,
					3,
					&kCFTypeDictionaryKeyCallBacks,
					&kCFTypeDictionaryValueCallBacks);
			CFArrayAppendValue(inCompletionArray, theCompletionDict);
			CFRelease(theCompletionDict);
		}
	}
}

static void CreateTextCompletionArray(bblmCreateCompletionArrayParams& ioParams)
{
	CFMutableArrayRef theCompletionArray = CFArrayCreateMutable(NULL, 0, &kCFTypeArrayCallBacks);
	UInt32 additionalLookup = kBBLMSymbolLookupPredefinedNames | kBBLMSymbolLookupClippings | kBBLMSymbolLookupWordsInFrontWindow;
	
	SInt16 runKind = ioParams.fInCompletionRangeStartRun.kind;
	if (runKind == kBBLMRunIsLineComment || runKind == kBBLMRunIsSingleString) {
		additionalLookup = kBBLMSymbolLookupWordsInSystemDict | kBBLMSymbolLookupWordsInFrontWindow | kBBLMSymbolLookupClippings;
	} else if (runKind == kErlRunIsCommentTag) {
		additionalLookup = kBBLMSymbolLookupClippings;
		AddSymbols(ioParams.fInPartialSymbol, theCompletionArray, gDocTagsDict, kBBLMSymbolTypeSGMLAttributeName);
	} else {
		// Code (and everything else).
		additionalLookup = kBBLMSymbolLookupWordsInFrontWindow | kBBLMSymbolLookupClippings;
		AddSymbols(ioParams.fInPartialSymbol, theCompletionArray, gFunctionsDict, kBBLMSymbolTypeFunction);
		AddSymbols(ioParams.fInPartialSymbol, theCompletionArray, gAttributesDict, kBBLMSymbolTypeMacro);
	}
	
	ioParams.fOutAdditionalLookupFlags = additionalLookup;
	ioParams.fOutSymbolCompletionArray = theCompletionArray;
	ioParams.fOutPreferredCompletionIndex = 0;
}

static void PredefinedNameLookup(bblmKeywordCFStringParams& ioParams)
{
    // Match the name against the function names.
    // The set was built during initialization.
    if (CFSetContainsValue(gPredefinedNames, ioParams.fToken))
    {
        ioParams.fKeywordMatched = true;
    }
}

extern "C"
{

OSErr ErlangMachO(BBLMParamBlock &params,
			const BBLMCallbackBlock &bblm_callbacks,
			const BBXTCallbackBlock &bbxt_callbacks)
{
	OSErr result;

	if ((params.fSignature != kBBLMParamBlockSignature) ||
		(params.fVersion < kBBLMParamBlockVersion))
	{
		return paramErr;
	}
	
	switch (params.fMessage)
	{
		case kBBLMInitMessage:
			debugf("Init. Version = %i", params.fVersion);
			result = Init();
			break;
		
		case kBBLMDisposeMessage:
			debugf("kBBLMDisposeMessage", NULL);
			Dispose();
			result = noErr;
			break;
		
		case kBBLMCalculateRunsMessage:
			debugf("kBBLMCalculateRunsMessage", NULL);
			CalculateRuns(params, bblm_callbacks);
			result = noErr;
			break;

		case kBBLMScanForFunctionsMessage:
			debugf("kBBLMScanForFunctionsMessage", NULL);
			ScanForFunctions(params, bblm_callbacks);
			result = noErr;
			break;

		case kBBLMAdjustRangeMessage:
			debugf("kBBLMAdjustRangeMessage", NULL);
			AdjustRange(params, bblm_callbacks);
			result = noErr;
			break;
		
		case kBBLMMapRunKindToColorCodeMessage:
			debugf("kBBLMMapRunKindToColorCodeMessage", NULL);
			switch (params.fMapRunParams.fRunKind){
			case kErlRunIsCommentTag:
				params.fMapRunParams.fColorCode = kBBLMSGMLAttributeNameColor;
				params.fMapRunParams.fMapped =	true;
				break;
			case kErlRunIsMacroName:
				params.fMapRunParams.fColorCode = kBBLMSGMLImageTagColor;
				params.fMapRunParams.fMapped =	true;
				break;
			default:
				params.fMapRunParams.fMapped =	false;
			}
			result = noErr;
			break;
		case kBBLMMapColorCodeToColorMessage:
			debugf("kBBLMMapColorCodeToColorMessage", NULL);
		/*	if (params.fMapColorParams.fColorCode == kBBLMFirstUserColorCode){
				params.fMapColorParams.fRGBColor = {0x7E67, 0,0};
				params.fMapColorParams.fMapped = true;
			}	*/
			//else{
				params.fMapColorParams.fMapped = false;
			//}	
			result = noErr;
			break;

		case kBBLMEscapeStringMessage:
		{
			debugf("kBBLMEscapeStringMessage", NULL);
			result = userCanceledErr;
			break;
		}
		case kBBLMAdjustEndMessage:
		{
			debugf("kBBLMAdjustEndMessage (offset = %i)", params.fAdjustEndParams.fEndOffset);
			result = noErr;
			break;
		}
		case kBBLMGuessLanguageMessage:
		{
			debugf("kBBLMGuessLanguageMessage", NULL);
			result = userCanceledErr;
			break;
		}

		case kBBLMMatchKeywordMessage:
		{
			debugf("kBBLMMatchKeywordMessage", NULL);
		    result = noErr;
		    break;
		}

		case kBBLMSetCategoriesMessage:
		{
			debugf("kBBLMSetCategoriesMessage", NULL);
		    SetCategories(params.fCategoryParams.fCategoryTable);
		    result = noErr;
		    break;
		}

        case kBBLMAdjustRangeForTextCompletion:
		{
			debugf("kBBLMAdjustRangeForTextCompletion", NULL);
			AdjustRangeForTextCompletion(params, params.fAdjustCompletionRangeParams);
            result = noErr;
            break;
        }
		case kBBLMFilterRunForTextCompletion:
		{
			debugf("kBBLMFilterRunForTextCompletion", NULL);
			FilterRunForTextCompletion(params.fFilterCompletionRunParams);
			result = noErr;
			break;
		}
		case kBBLMSetCategoriesForTextCompletionMessage:
		{
			debugf("kBBLMSetCategoriesForTextCompletionMessage", NULL);
		    SetCategoryTableForTextCompletion(params.fCategoryParams.fCategoryTable);
		    result = noErr;
		    break;
		}
		case kBBLMCreateTextCompletionArray:
		{
			debugf("kBBLMCreateTextCompletionArray", NULL);
			CreateTextCompletionArray(params.fCreateCompletionArrayParams);
			result = noErr;
			break;
		}
		case kBBLMMatchPredefinedNameMessage:
		{
			debugf("kBBLMMatchPredefinedNameMessage", NULL);
		    PredefinedNameLookup(params.fMatchKeywordWithCFStringParams);
			result = noErr;
			break;
		}
		
		default:
		{
			debugf("Unknown message = %i", params.fMessage);
			result = paramErr;
			break;
		}
	}
	return result;	
}

}

// =================================================================== //
// Heuristics are bug ridden by definition.  If they didn't have bugs, //
// then they'd be algorithms.                                          //
// =================================================================== //
