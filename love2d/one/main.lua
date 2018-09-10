local font = nil

print(love.window.getWidth())
print(love.window.getHeight())

-- #####################
-- ## TWEAKING CONFIG ##
-- #####################
local cfgFile = 'config.lua'
local cfgChangeTime = nil
local cfg = {}
function love.update()
	local currentChangeTime = love.filesystem.getLastModified(cfgFile)
	if cfgChangeTime ~= currentChangeTime then
		cfgChangeTime = currentChangeTime
		print('Loading ' .. cfgFile .. ' modified at ' .. os.date("%c", cfgChangeTime))
		cfg = love.filesystem.load(cfgFile)()
		font = love.graphics.newFont('Merienda-Bold.ttf', cfg.fontHeight)
		love.graphics.setFont(font)
	end
end

function love.draw()
	love.graphics.printf(
		cfg.text,
		cfg.x,
		cfg.y-cfg.fontHeight/2,
		cfg.limit,
		cfg.align,
		cfg.r,
		cfg.sx,
		cfg.sy)
end

