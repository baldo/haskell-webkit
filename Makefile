bin = Test

from_chs = \
	Graphics/UI/Gtk/WebKit/WebFrame.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebSettings.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebView.hs \
	Graphics/UI/Gtk/WebKit/General/General.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs

hs_files = \
	Graphics/UI/Gtk/WebKit.hs \
	Graphics/UI/Gtk/WebKit/WebFrame.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebSettings.hs \
	Graphics/UI/Gtk/WebKit/WebHistoryItem.hs \
	Graphics/UI/Gtk/WebKit/WebView.hs \
	Graphics/UI/Gtk/WebKit/General/General.hs \
	Graphics/UI/Gtk/WebKit/General/Types.hs \
	Graphics/UI/Gtk/WebKit/General/Enums.hs

%.hs: %.chs
	@c2hs `pkg-config --cflags webkit-1.0 | sed 's/-[^ ]*/-C &/g'` -o $@ $<

all: Test

Test: Test.hs $(hs_files) $(generated)
	@ghc --make Test.hs `pkg-config --libs webkit-1.0`

Graphics/UI/Gtk/WebKit/WebView.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Graphics/UI/Gtk/WebKit/General/Enums.hs

Graphics/UI/Gtk/WebKit/WebFrame.hs: Graphics/UI/Gtk/WebKit/General/Types.hs Graphics/UI/Gtk/WebKit/General/Enums.hs

clean:
	@rm -f Test spplayer *.o *.hi $(bin) $(from_chs)
	@make -C Graphics/UI/Gtk clean
