bin = Test

from_chs = \
	Graphics/UI/Gtk/WebKit/CacheModel.hs \
	Graphics/UI/Gtk/WebKit/Download.hs \
	Graphics/UI/Gtk/WebKit/NetworkRequest.hs \
	Graphics/UI/Gtk/WebKit/NetworkResponse.hs \
	Graphics/UI/Gtk/WebKit/SecurityOrigin.hs \
	Graphics/UI/Gtk/WebKit/WebBackForwardList.hs \
	Graphics/UI/Gtk/WebKit/WebDataSource.hs \
	Graphics/UI/Gtk/WebKit/WebDatabase.hs \
	Graphics/UI/Gtk/WebKit/WebFrame.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebInspector.hs \
	Graphics/UI/Gtk/WebKit/WebNavigationAction.hs \
	Graphics/UI/Gtk/WebKit/WebPolicyDecision.hs \
	Graphics/UI/Gtk/WebKit/WebResource.hs \
	Graphics/UI/Gtk/WebKit/WebSettings.hs \
	Graphics/UI/Gtk/WebKit/WebView.hs \
	Graphics/UI/Gtk/WebKit/WebWindowFeatures.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/General.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Language/JavaScript/JavaScriptCore/Base.hs \
	Language/JavaScript/JavaScriptCore/General/Types.hs \
	Network/Soup/General/Enums.hs \
	Network/Soup/General/Types.hs \
	Network/Soup/Message.hs

hs_files = \
	Graphics/UI/Gtk/WebKit.hs \
	Graphics/UI/Gtk/WebKit/CacheModel.hs \
	Graphics/UI/Gtk/WebKit/Download.hs \
	Graphics/UI/Gtk/WebKit/NetworkRequest.hs \
	Graphics/UI/Gtk/WebKit/NetworkResponse.hs \
	Graphics/UI/Gtk/WebKit/SecurityOrigin.hs \
   	Graphics/UI/Gtk/WebKit/WebBackForwardList.hs \
	Graphics/UI/Gtk/WebKit/WebDataSource.hs \
	Graphics/UI/Gtk/WebKit/WebDatabase.hs \
	Graphics/UI/Gtk/WebKit/WebFrame.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebInspector.hs \
	Graphics/UI/Gtk/WebKit/WebNavigationAction.hs \
	Graphics/UI/Gtk/WebKit/WebPolicyDecision.hs \
	Graphics/UI/Gtk/WebKit/WebResource.hs \
	Graphics/UI/Gtk/WebKit/WebSettings.hs \
	Graphics/UI/Gtk/WebKit/WebView.hs \
	Graphics/UI/Gtk/WebKit/WebWindowFeatures.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/General.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Language/JavaScript/JavaScriptCore.hs \
	Language/JavaScript/JavaScriptCore/Base.hs \
	Language/JavaScript/JavaScriptCore/General/Types.hs \
	Network/Soup.hs \
	Network/Soup/General/Enums.hs \
	Network/Soup/General/Types.hs \
	Network/Soup/Message.hs

%.hs: %.chs
	c2hs `pkg-config --cflags webkit-1.0 | sed 's/-[^ ]*/-C &/g'` -o $@ $<

all: Test

Test: Test.hs $(hs_files)
	@ghc --make Test.hs `pkg-config --libs webkit-1.0`

Graphics/UI/Gtk/WebKit/CacheModel.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/Download.hs: \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/NetworkRequest.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Network/Soup/Message.hs \
	Network/Soup/General/Types.hs

Graphics/UI/Gtk/WebKit/NetworkResponse.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Network/Soup/General/Types.hs

Graphics/UI/Gtk/WebKit/SecurityOrigin.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebBackForwardList.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebDataSource.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebDatabase.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebFrame.hs: \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Language/JavaScript/JavaScriptCore/General/Types.hs

Graphics/UI/Gtk/WebKit/WebHistoryItem.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebInspector.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebNavigationAction.hs: \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebPolicyDecision.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebResource.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebSettings.hs: \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Graphics/UI/Gtk/WebKit/WebView.hs: \
	Graphics/UI/Gtk/WebKit/WebWindowFeatures.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Network/Soup/General/Types.hs

Graphics/UI/Gtk/WebKit/WebWindowFeatures.hs: \
	Graphics/UI/Gtk/WebKit/General/Types.hs

Language/JavaScript/JavaScriptCore/Base.hs: \
	Language/JavaScript/JavaScriptCore/General/Types.hs

Network/Soup/Message.hs: \
	Network/Soup/General/Enums.hs \
	Network/Soup/General/Types.hs

doc: $(hs_files)
	@mkdir -p doc/
	@haddock -h -o doc/ $(hs_files)

clean:
	@rm -f Test spplayer *.o *.hi $(bin) $(from_chs)
	@make -C Graphics/UI/Gtk clean
	@make -C Network clean
	@make -C Language/JavaScript clean
	@rm -rf doc/
