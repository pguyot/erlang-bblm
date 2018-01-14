//
//  ErlangRuntime.mm
//  ErlangBBLM
//
//  Created by Paul Guyot on 29/10/2014.
//
//

#import "ErlangRuntime.h"

namespace com_semiocast_bblm_erlang {
    
    // http://erlang.org/doc/reference_manual/data_types.html#id76876
    static unichar EscapeChar(unichar c) {
        unichar r;
        switch (c) {
            case 'b':
                r = '\b';
                break;
            case 'd':
                r = 127; // DEL
                break;
            case 'e':
                r = '\e';
                break;
            case 'f':
                r = '\f';
                break;
            case 'n':
                r = '\n';
                break;
            case 'r':
                r = '\r';
                break;
            case 's':
                r = ' ';
                break;
            case 't':
                r = '\t';
                break;
            case 'v':
                r = '\v';
                break;
                /*
                 // UNIMPLEMENTED
                 \XYZ, \YZ, \Z	character with octal representation XYZ, YZ or Z
                 \xXY	character with hexadecimal representation XY
                 \x{X...}	character with hexadecimal representation; X... is one or more hexadecimal characters
                 \^a...\^z
                 \^A...\^Z	control A to control Z
                 */
            default:
                //          \'	single quote
                //          \"	double quote
                //          \\	backslash
                r = c;
        }
        return r;
    }
    
    NSString* ParseErlangString(NSString* expression) {
        NSString* result = nil;
        NSUInteger exprLen = [expression length];
        if (exprLen >= 2) {
            if ([expression characterAtIndex:0] == '"' && [expression characterAtIndex: exprLen - 1] == '"') {
                NSUInteger indexEnd = [expression length] - 1;
                NSUInteger len = indexEnd - 1;
                NSUInteger indexExpr;
                NSUInteger indexBuffer;
                unichar* strBuffer = (unichar*) malloc(sizeof(unichar) * len);
                for (indexBuffer = 0, indexExpr = 1; indexExpr < indexEnd; indexExpr++, indexBuffer++) {
                    unichar c = [expression characterAtIndex:indexExpr];
                    if (c == '\\') {
                        indexExpr++;
                        len--;
                        c = [expression characterAtIndex:indexExpr];
                        strBuffer[indexBuffer] = EscapeChar(c);
                    } else {
                        strBuffer[indexBuffer] = c;
                    }
                }
                result = [NSString stringWithCharacters:strBuffer length:len];
                free(strBuffer);
            } else if ([expression characterAtIndex:0] == '[') {
                NSScanner* scanner = [NSScanner scannerWithString:expression];
                // Skip leading bracket
                unichar* strBuffer = (unichar*) malloc(sizeof(unichar) * (exprLen - 2) / 2);
                NSUInteger len = 0;
                [scanner scanString:@"[" intoString:nil];
                while (true) {
                    int c;
                    BOOL r = [scanner scanInt:&c];
                    if (r == NO) {
                        // Expect the end bracket.
                        r = [scanner scanString: @"]" intoString: nil];
                        if (r == YES) {
                            result = [NSString stringWithCharacters:strBuffer length:len];
                        }
                        free(strBuffer);
                        break;
                    }
                    strBuffer[len] = c;
                    len++;
                    [scanner scanString: @"," intoString: nil];
                }
                
            }
        }
        return result;
    }
    
    ///
    /// Convert a symbol to a man page.
    /// Actually sanitize the string by only keeping [_a-zA-Z0-9-]
    /// Stop if any other character is encountered, including :
    /// (thus module:function will lookup for module).
    NSString* SymbolToManPage(NSString* symbol) {
        NSUInteger len = [symbol length];
        NSUInteger indexIn;
        NSUInteger indexOut = 0;
        unichar* strBuffer = (unichar*) malloc(sizeof(unichar) * len);
        for (indexIn = 0; indexIn < len; indexIn++) {
            unichar c = [symbol characterAtIndex:indexIn];
            if ((c >= 'A' && c <= 'Z')
                || (c >= 'a' && c <= 'z')
                || (c >= '0' && c <= '9')
                || (c == '_')
                || (c == '-')) {
                strBuffer[indexOut] = c;
                indexOut++;
            } else {
                break;
            }
        }
        NSString* result = [NSString stringWithCharacters:strBuffer length:indexOut];
        free(strBuffer);
        return result;
    }

    ///
    /// Try to fetch the man page for a given symbol.
    ///
    NSString* ErlangManPage(NSString* page) {
        NSPipe* stdoutPipe = [NSPipe pipe];
        NSFileHandle* stdoutFile = stdoutPipe.fileHandleForReading;
        
        NSString* erlManCommand = [NSString stringWithFormat:@"erl -man %@ | col -bx", page];
        
        NSTask *task = [NSTask new];
        NSDictionary *environmentDict = [[NSProcessInfo processInfo] environment];
        NSString *shellString = [environmentDict objectForKey:@"SHELL"];
        task.launchPath = shellString;
        task.arguments = @[@"-c", erlManCommand];
        
        task.standardOutput = stdoutPipe;
        [task launch];
        NSData *data = [stdoutFile readDataToEndOfFile];
        [task waitUntilExit];
        [stdoutFile closeFile];
        NSString* output = nil;
        if ([task terminationStatus] == 0) {
            output = [[[NSString alloc] initWithData: data encoding: NSUTF8StringEncoding] autorelease];
        }
        [task terminate];
        [task release];
        return output;
    }

    ///
    /// Try to eval an Erlang expression, get the result.
    /// The result is serialized with io:format("~p", [Expr]).
    /// Instead of passing expression on command line (using -eval argument), we get Erlang VM to parse stdin.
    /// This avoids parametrized text passed to /bin/sh
    ///
#define ERL_EVAL_COMMAND_LINE @"erl -eval 'error_logger:tty(false), {ok, Expr, _} = io:parse_erl_exprs([]), Result = (catch begin {value, R, _} = erl_eval:exprs(Expr, []), R end), io:format(\"~p\", [Result]), init:stop().' -noshell"
    
    NSString* EvalErlang(NSString* expression) {
        NSPipe* stdoutPipe = [NSPipe pipe];
        NSFileHandle* stdoutFile = stdoutPipe.fileHandleForReading;
        NSPipe* stdinPipe = [NSPipe pipe];
        NSFileHandle* stdinFile = stdinPipe.fileHandleForWriting;
        
        NSTask *task = [NSTask new];
        NSDictionary *environmentDict = [[NSProcessInfo processInfo] environment];
        NSString *shellString = [environmentDict objectForKey:@"SHELL"];
        task.launchPath = shellString;
        task.arguments = @[@"-c", ERL_EVAL_COMMAND_LINE];
        
        task.standardOutput = stdoutPipe;
        task.standardInput = stdinPipe;
        [task launch];
        [stdinFile writeData:[expression dataUsingEncoding:NSUTF8StringEncoding]];
        [stdinFile closeFile];
        NSData *data = [stdoutFile readDataToEndOfFile];
        [stdoutFile closeFile];
        NSString* output = [[[NSString alloc] initWithData: data encoding: NSUTF8StringEncoding] autorelease];
        [task terminate];
        [task release];
        return output;
    }
}
