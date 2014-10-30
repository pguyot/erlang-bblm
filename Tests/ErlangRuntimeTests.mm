//
//  ErlangRuntimeTests.mm
//  ErlangBBLM
//
//  Created by Paul Guyot on 26/10/2014.
//
//

#import <Cocoa/Cocoa.h>
#import <XCTest/XCTest.h>

#include "ErlangRuntime.h"

@interface ErlangRuntimeTests : XCTestCase

@end

using namespace com_semiocast_bblm_erlang;

@implementation ErlangRuntimeTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testParseErlangString {
    XCTAssertEqualObjects(@"", ParseErlangString(@"[]"));
    XCTAssertEqualObjects(@"", ParseErlangString(@"\"\""));
    XCTAssertEqualObjects(@"A", ParseErlangString(@"[65]"));
    XCTAssertEqualObjects(@"foo", ParseErlangString(@"\"foo\""));
    XCTAssertEqualObjects(@"foo\tbar", ParseErlangString(@"\"foo\\tbar\""));
    XCTAssertEqualObjects(@"foo\\bar", ParseErlangString(@"\"foo\\\\bar\""));
    XCTAssertEqualObjects(@"暑い", ParseErlangString(@"\"暑い\""));

    XCTAssertNil(ParseErlangString(@"["));
    XCTAssertNil(ParseErlangString(@"{error,foo}"));
    XCTAssertNil(ParseErlangString(@"\"foo"));
    XCTAssertNil(ParseErlangString(@""));
}

- (void)testEvalErlang {
    // Requires that erlang is installed and in the PATH.
    XCTAssertEqualObjects(@"42", EvalErlang(@"41+1."));
}

- (void)testErlangManPage {
    // Requires that erlang is installed and in the PATH.
    XCTAssertNil(ErlangManPage(@"foobar"));
    XCTAssertNotNil(ErlangManPage(@"mnesia"));
    XCTAssert([ErlangManPage(@"mnesia") hasPrefix: @"mnesia(3)"]);
}

@end
