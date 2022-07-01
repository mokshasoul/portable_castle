local not_vs_code = function () return not vim.g.vscode end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
	-- Packer can manage itself
	use {'wbthomason/packer.nvim', opt = true}
	-- main one
	use {'ms-jpq/coq_nvim', branch = 'coq'}
	-- 9000+ Snippets
	use {'ms-jpq/coq.artifacts', branch = 'artifacts'}

	use {'ms-jpq/coq.thirdparty', branch = '3p'}

	-- Collection of configurations for the built-in LSP client
	use 'neovim/nvim-lspconfig'

	-- Post-install/update hook with neovim command
	use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

	use 'lewis6991/impatient.nvim'
	use 'nathom/filetype.nvim'

	use 'luukvbaal/nnn.nvim'

	use 'khaveesh/vim-fish-syntax'

	use {
		'asvetliakov/vim-easymotion',
		cond = not(not_vs_code),
		as = 'vsc-easymotion'
	}

	-- Lua development
	use { 'tjdevries/nlua.nvim', cond = function() return not vim.g.vscode end }
	use { 'nvim-lua/completion-nvim', cond = not_vs_code }

	use {
		'phaazon/hop.nvim',
		branch = 'v1', -- optional but strongly recommended
		config = function()
			-- you can configure Hop the way you like here; see :h hop-config
			require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
		end
	}

	use {
		'kyazdani42/nvim-tree.lua',
		requires = {
			'kyazdani42/nvim-web-devicons', -- optional, for file icons
		},
		tag = 'nightly' -- optional, updated every week. (see issue #1193)
	}
end)
