local images = {}
local raindrops = {}
local width, height = love.window.getWidth(), love.window.getHeight()
local countdown = 10

-- ##################### --
-- ## TWEAKING CONFIG ## --
-- ##################### --
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

	countdown = countdown - 1
	if countdown == 0 then
		add_raindrop()
		countdown = math.random(
			cfg.countdownMin,
			cfg.countdownMax)
	end

	local keep = {}
	for ix, drop in ipairs(raindrops) do
		drop.y = drop.y + math.random(cfg.yspdMin, cfg.yspdMax)
		if drop.y < height then
			keep[#keep + 1] = drop
		end
	end
	raindrops = keep

	if love.keyboard.isDown("escape") then
		love.event.quit()
	end
end

function add_raindrop()
	local xpos, ypos = math.random(0, width), 0
	raindrops[#raindrops+1] = {
		x=xpos,
		y=ypos
	}
end

function love.load()
	images.bg = love.graphics.newImage('bg.png')
	add_raindrop()
end

function size(t)
	local s = 0
	for a, b in ipairs(t) do
		s = s + 1
	end
	return s
end

function love.draw()
	-- love.graphics.setColor( 1, 1, 1 )
	love.graphics.setColor( 150, 150, 255 )
	love.graphics.clear()
	love.graphics.draw(images.bg)
	love.graphics.setColor(unpack(cfg.dropColor))
	for ix, drop in ipairs(raindrops) do
		love.graphics.rectangle(
			'fill',
			drop.x, drop.y,
			cfg.dropWidth, cfg.dropHeight)
	end

	local txt = ("Number of raindrops: %d"):format(size(raindrops))
	love.graphics.print(txt, 0, 0)
end

