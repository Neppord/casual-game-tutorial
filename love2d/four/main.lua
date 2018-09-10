local font = nil
local x = 50
local y = 50
local xdir = 1
local ydir = 1
local score = 0

WIDTH = love.window.getWidth()
HEIGHT = love.window.getHeight()

hitSound = love.audio.newSource('hit.wav', 'static')
missSound = love.audio.newSource('miss.wav', 'static')

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
		love.graphics.setColor(unpack(cfg.ball.color))
	end

	x = x + xdir * cfg.ball.speed
	y = y + ydir * cfg.ball.speed
	if x+cfg.ball.radius > WIDTH or x < cfg.ball.radius then
		xdir = -xdir
		x = x + xdir * cfg.ball.speed
	end
	if y+cfg.ball.radius > HEIGHT or y < cfg.ball.radius then
		ydir = -ydir
		y = y + ydir * cfg.ball.speed
	end
end

function love.mousepressed(mx, my, button)
	dx = math.abs(mx - x)
	dy = math.abs(my - y)
	d = math.sqrt(dx*dx + dy*dy)
	if d < cfg.ball.radius then
		x = math.random(cfg.ball.radius, WIDTH-cfg.ball.radius)
		y = math.random(cfg.ball.radius, HEIGHT-cfg.ball.radius)
		xdir = -1 + 2 * math.random(0, 1)
		ydir = -1 + 2 * math.random(0, 1)
		cfg.ball.radius = cfg.ball.radius * 0.9
		cfg.ball.speed = cfg.ball.speed * 1.2
		hitSound:play()
		score = score + 1
	else
		missSound:play()
	end
end

function love.draw()
	love.graphics.circle(
		'fill',
		x,
		y,
		cfg.ball.radius)
	local score = ('Score: %d'):format(score)
	love.graphics.printf(
		score,
		0,
		HEIGHT-cfg.fontHeight*1.5,
		WIDTH,
		'center',
		0,
		1,
		1)

end

