## Using Pascal Language Server in Neovim

Assuming you have already compiled the server. If you haven't, do that first.

- Install [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
- Copy `pascal.lua` into  `~/.local/share/nvim/site/pack/vendor/opt/nvim-lspconfig/lua/lspconfig/`
  (yes, quite a mouthful)
- Make sure `pasls` is in `PATH` (or edit `pascal.lua` to use an absolute path
  pointing to the executable)
- Enable `pasls` in your `init.vim`:
  ```vim
  packadd nvim-lspconfig
  lua require'lspconfig'.pascal.setup{}
  ```

Note that this will not by itself enable any features as those are managed by
separate plugins in Neovim.

## Example config
The following example uses
[completion-nvim](https://github.com/nvim-lua/completion-nvim) for code
completion.

```vim
packadd nvim-lspconfig
packadd completion-nvim

lua << EOF
local nvim_lsp = require('lspconfig')

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  require'completion'.on_attach(client, bufnr)

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
end

require'lspconfig'.fpc.setup{on_attach=on_attach}

" Make code completion less intrusive
let g:completion_enable_auto_popup = 0
let g:completion_enable_auto_signature = 1

" Invoke code completion with <Ctrl>+<Space>
imap <c-space> <Plug>(completion_trigger)

EOF
```

## Compatibility

LSP is included in Neovim 0.5.0+
