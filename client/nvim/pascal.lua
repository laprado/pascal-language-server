local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

configs.pascal = {
  default_config = {
    cmd = {
      "pasls",
      -- Uncomment for debugging:
      --"--save-log", "pasls-log", "--save-replay", "pasls-replay"
    };
    filetypes = {"pascal"};
    root_dir = util.root_pattern(".git", "Makefile.fpc");
    init_options = {}
  };
  docs = {
    description = [[
https://github.com/Isopod/pascal-language-server

`pascal-language-server`, a language server for Pascal, based on fpc.
]];
    default_config = {
      root_dir = [[root_pattern(".git", "Makefile.fpc")]];
    };
  };
};

-- vim:et ts=2 sw=2
