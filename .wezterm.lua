-- Pull in the wezterm API
local wezterm = require 'wezterm'
local mux = wezterm.mux

-- This table will hold the configuration.
local config = {
  colors = {
    cursor_bg = 'transparent',
  },
}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Disable font ligatures
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- This is where you actually apply your config choices
config.font = wezterm.font('JetBrains Mono')
config.font_size = 18
config.initial_cols = 100
config.initial_rows = 30
config.cursor_blink_rate = 750
config.default_cursor_style = 'BlinkingBlock'
config.cursor_blink_ease_in = 'EaseIn'
config.cursor_blink_ease_out = 'EaseOut'
config.check_for_updates = false
config.warn_about_missing_glyphs = false
config.window_background_gradient = {
  orientation = 'Vertical',

  colors = {
    'rgba(59, 66, 82, 0.99)',
    'rgba(47, 48, 51, 0.95)'
  },

  interpolation = 'Linear',

  blend = 'Rgb',
}

config.color_scheme = 'Nord'

-- and finally, return the configuration to wezterm
return config
