-- Pull in the wezterm API
local wezterm = require 'wezterm'
local mux = wezterm.mux

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices
config.font = wezterm.font('JetBrains Mono')
config.font_size = 18
config.initial_cols = 100
config.initial_rows = 30
config.cursor_blink_rate = 250
config.default_cursor_style = 'BlinkingBar'
config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'

config.window_background_gradient = {
  orientation = 'Vertical',

  colors = {
    'rgba(59, 66, 82, 0.99)',
    'rgba(47, 48, 51, 0.95)'
  },

  noise=500,

  interpolation = 'Linear',

  blend = 'Rgb',
}

config.color_scheme = 'Nord'

-- and finally, return the configuration to wezterm
return config

