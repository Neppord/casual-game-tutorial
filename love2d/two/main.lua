local font = nil
local sounds = {}
local keyDown = {}
local keys = {'a', 's', 'd'}

print(love.window.getWidth())
print(love.window.getHeight())

function love.load()
    for i in pairs({1, 2, 3}) do
        keyDown[#keyDown + 1] = false
        fname = ('%d.wav'):format(i)
        sounds[#sounds + 1] = love.audio.newSource(fname, 'static')
    end
end

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

    -- play sounds!
    -- for i in pairs({1, 2, 3}) do
    --  local key = keys[i]
    --  if not keyDown[i] and love.keyboard.isDown(key) then
    --      keyDown[i] = true
    --      sounds[i]:stop()
    --      sounds[i]:play()
    --  end
    --  if keyDown[i] and not love.keyboard.isDown(key) then
    --      keyDown[i] = false
    --  end
    -- end
end

function love.keypressed(key)
    for i in pairs({1, 2, 3}) do
        if key == keys[i] then
            sounds[i]:stop()
            sounds[i]:play()
        end
    end
end

function love.draw()
    for i in pairs({1, 2, 3}) do
        if love.keyboard.isDown(keys[i]) then
            love.graphics.setColor(255, 255, 255)
        else
            love.graphics.setColor(128, 128, 128)
        end
        love.graphics.printf(keys[i],
            cfg.x + cfg.spacing * i,
            cfg.y,
            500,
            'left',
            0,
            1,
            1)
    end
end

