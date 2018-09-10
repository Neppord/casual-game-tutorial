local font = nil
local xdir = 1
local yspd = 0
local ballImage = love.graphics.newImage('ball.png')
ballImage:setFilter('nearest', 'nearest')

WIDTH = love.window.getWidth()
HEIGHT = love.window.getHeight()
local x = WIDTH/2
local y = HEIGHT/2

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
	end

	x = x + xdir * cfg.ball.xspd
	if x + cfg.ball.radius > WIDTH or x < cfg.ball.radius then
		xdir = -xdir
		x = x + xdir * cfg.ball.xspd
	end
	y = y + yspd
	yspd = yspd + cfg.ball.yacc
	if y + cfg.ball.radius > HEIGHT then
		yspd = -yspd * 0.9
		y = y - 5
	end
end

function love.draw()
	love.graphics.draw(ballImage, x, y,
		x/cfg.ball.rotdiv,
		1, 1, 
		cfg.ball.radius,
		cfg.ball.radius)
end

