//
//  main.m
//  DocHelper
//
//  Created by Paul Guyot on 28/10/2014.
//
//

#import <Cocoa/Cocoa.h>
#include "ErlangRuntime.h"

using namespace com_semiocast_bblm_erlang;

@interface EventHandler : NSObject <NSApplicationDelegate>
@end


@implementation EventHandler

- (void)handleGetURLAppleEvent:(NSAppleEventDescriptor *)event withReplyEvent:(NSAppleEventDescriptor *)replyEvent {
    NSURL* url = [NSURL URLWithString:[[event paramDescriptorForKeyword:keyDirectObject] stringValue]];
    NSString* lookupSymbol = [url resourceSpecifier];
    NSString* manPage = SymbolToManPage(lookupSymbol);
    NSString* manPageTxt = ErlangManPage(manPage);
    if (manPageTxt) {
        // Waiting for the proper API — Case 297940 — to open it in BBEdit
        // Open in Terminal.app for now.
        NSAppleScript* script = [[NSAppleScript alloc] initWithSource:[NSString stringWithFormat:@"tell application \"Terminal\"\n"
                                                                       "set manTab to do script \"erl -man %@; exit\"\n"
                                                                       "set current settings of manTab to settings set id 1000\n"
                                                                       "set custom title of manTab to \"erl -man %@\"\n"
                                                                       "set number of rows of manTab to 48\n"
                                                                       "set number of columns of manTab to 80\n"
                                                                       "activate\n"
                                                                       "end tell", manPage, manPage]];
        [script executeAndReturnError:nil];
        [script release];
    } else {
        // Try a search on erlang.org
        NSString* lookupURL = [NSString stringWithFormat:@"http://www.erlang.org/erldoc?q=%@", lookupSymbol];
        [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:lookupURL]];
    }
}

- (void)applicationDidFinishLaunching:(NSNotification *)notification {
    [[NSApplication sharedApplication] terminate: self];
}

@end


int main(int argc, const char * argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSApplication* app = [NSApplication sharedApplication];
    EventHandler* handler = [EventHandler new];
    [[NSAppleEventManager sharedAppleEventManager] setEventHandler:handler
                                                   andSelector:@selector(handleGetURLAppleEvent:withReplyEvent:)
                                                 forEventClass:kInternetEventClass
                                                    andEventID:kAEGetURL];
    [app setDelegate: handler];
    [app run];
    [handler release];
    [pool release];
    return 0;
}
