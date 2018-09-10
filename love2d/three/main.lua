local font = nil
local x = 0
local dir = 1

WIDTH = love.window.getWidth()

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
		love.graphics.setBackgroundColor(unpack(cfg.background))
		love.graphics.setColor(unpack(cfg.box.color))
	end

	x = x + dir * cfg.box.speed
	if x+cfg.box.width > WIDTH or x < 0 then
		dir = -dir
		x = x + dir * cfg.box.speed
	end
end

function love.draw()
	love.graphics.rectangle('fill',
		0, 0, 10, 5)
	love.graphics.rectangle(
		'fill',
		x,
		cfg.box.y,
		cfg.box.width,
		cfg.box.height)
end

