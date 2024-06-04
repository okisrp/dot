config.load_autoconfig(False)

c.colors.webpage.preferred_color_scheme = "dark"

c.aliases = {
    "w": "session-save",
    "q": "close",
    "qa": "quit",
    "wq": "quit --save",
    "so": "config-source",
    "ad": "adblock-update",
}

c.auto_save.session = False

c.completion.height = "30%"

c.completion.scrollbar.width = 0
c.completion.scrollbar.padding = 0

c.scrolling.bar = "never"

c.confirm_quit = [ "multiple-tabs" ]

c.content.autoplay = False

c.content.blocking.enabled = True
c.content.blocking.method = "adblock"

c.content.blocking.adblock.lists = [
    "https://easylist.to/easylist/easylist.txt",
    "https://easylist.to/easylist/easyprivacy.txt",
    "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt",
    "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
    "https://easylist.to/easylist/fanboy-social.txt",
]

c.url.searchengines = { "DEFAULT": "https://duckduckgo.com/?q={}" }
c.url.start_pages = [ "https://start.duckduckgo.com" ]

c.content.notifications.enabled = False

c.hints.chars = "asdfghjkl;"

c.tabs.favicons.show = "never"

my = {
    "padding": {
        "top": 2,
        "bottom": 2,
        "left": 4,
        "right": 4,
    },
}

c.statusbar.padding = my["padding"]
c.tabs.indicator.padding = my["padding"]
c.tabs.padding = my["padding"]

c.tabs.indicator.width = 1

c.window.hide_decoration = True
c.window.title_format = "{perc}{current_title}{title_sep}Qute Browser"

c.downloads.remove_finished = 2000

config.bind("al", "download-clear")

config.bind(",y", "open -t -- {clipboard}")

config.bind(",mpv", "hint links spawn --detach mpv {hint-url}")

config.bind(",s", "config-source ;; adblock-update")

config.bind("<Alt-K>", "tab-prev")
config.bind("<Alt-J>", "tab-next")

config.bind("<Alt-Shift-K>", "tab-move -")
config.bind("<Alt-Shift-J>", "tab-move +")

config.bind("<Alt-K>", "completion-item-focus --history prev", mode="command")
config.bind("<Alt-J>", "completion-item-focus --history next", mode="command")

config.bind("<Alt-K>", "prompt-item-focus prev", mode="prompt")
config.bind("<Alt-J>", "prompt-item-focus next", mode="prompt")

config.bind("<Alt-C>", "mode-leave", mode="command")

config.bind("<Alt-C>", "mode-leave", mode="insert")
config.bind("<Alt-C>", "clear-keychain ;; search ;; fullscreen --leave")
config.bind("<Alt-C>", "mode-leave", mode="caret")
config.bind("<Alt-C>", "mode-leave", mode="prompt")
config.bind("<Alt-C>", "mode-leave", mode="register")
config.bind("<Alt-C>", "mode-leave", mode="yesno")
config.bind("<Alt-C>", "mode-leave", mode="hint")
