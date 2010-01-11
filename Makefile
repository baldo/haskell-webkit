bin = Test

from_chs = \
	Graphics/UI/Gtk/WebKit/NetworkRequest.hs \
	Graphics/UI/Gtk/WebKit/NetworkResponse.hs \
	Graphics/UI/Gtk/WebKit/WebFrame.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebSettings.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
   	Graphics/UI/Gtk/WebKit/WebBackForwardList.hs \
	Graphics/UI/Gtk/WebKit/WebView.hs \
	Graphics/UI/Gtk/WebKit/WebNavigationAction.hs \
	Graphics/UI/Gtk/WebKit/WebPolicyDecision.hs \
	Graphics/UI/Gtk/WebKit/WebInspector.hs \
	Graphics/UI/Gtk/WebKit/General/General.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Network/Soup/Message.hs \
	Network/Soup/General/Types.hs

hs_files = \
	Graphics/UI/Gtk/WebKit.hs \
	Graphics/UI/Gtk/WebKit/NetworkRequest.hs \
	Graphics/UI/Gtk/WebKit/NetworkResponse.hs \
	Graphics/UI/Gtk/WebKit/WebFrame.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebSettings.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
   	Graphics/UI/Gtk/WebKit/WebBackForwardList.hs \
	Graphics/UI/Gtk/WebKit/WebView.hs \
	Graphics/UI/Gtk/WebKit/WebNavigationAction.hs \
	Graphics/UI/Gtk/WebKit/WebPolicyDecision.hs \
	Graphics/UI/Gtk/WebKit/WebInspector.hs \
	Graphics/UI/Gtk/WebKit/General/General.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Network/Soup.hs \
	Network/Soup/Message.hs \
	Network/Soup/General/Types.hs

%.hs: %.chs
	c2hs `pkg-config --cflags webkit-1.0 | sed 's/-[^ ]*/-C &/g'` -o $@ $<

all: Test

Test: Test.hs $(hs_files) $(generated)
	@ghc --make Test.hs `pkg-config --libs webkit-1.0`

Graphics/UI/Gtk/WebKit/NetworkRequest.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Network/Soup/General/Types.hs Network/Soup/Message.hs

Graphics/UI/Gtk/WebKit/NetworkResponse.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Network/Soup/General/Types.hs

Graphics/UI/Gtk/WebKit/WebFrame.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Graphics/UI/Gtk/WebKit/General/Enums.hs

Graphics/UI/Gtk/WebKit/WebHistoryItem.hs: Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebSettings.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Graphics/UI/Gtk/WebKit/General/Enums.hs

Graphics/UI/Gtk/WebKit/WebHistoryItem.hs: Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebBackForwardList.hs: Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebView.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Graphics/UI/Gtk/WebKit/General/Enums.hs

Graphics/UI/Gtk/WebKit/WebNavigationAction.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Graphics/UI/Gtk/WebKit/General/Enums.hs

Graphics/UI/Gtk/WebKit/WebPolicyDecision.hs: Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebInspector.hs: Graphics/UI/Gtk/WebKit/General/Types.hs

Network/Soup/Message.hs: Network/Soup/General/Types.hs

clean:
	@rm -f Test spplayer *.o *.hi $(bin) $(from_chs)
	@make -C Graphics/UI/Gtk clean
	@make -C Network clean
