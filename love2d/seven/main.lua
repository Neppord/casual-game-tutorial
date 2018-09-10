require 'tweak'
require 'anim'

-- globals
function loadPNG(name)
    local image = love.graphics.newImage(name .. '.png')
    image:setFilter('nearest', 'nearest')
    return image
end
ballImage = loadPNG('ball')
walkImage = loadPNG('walk')
standImage = loadPNG('stand')

local WIDTH = love.window.getWidth()
local HEIGHT = love.window.getHeight()

local font = nil
local walkAnim = nil
local thudSnd = love.audio.newSource('thud.wav', 'static')
local stepSnd = love.audio.newSource('step.wav', 'static')
local gubbe = {}

-- called when config changes
function configChanged(cfg)
    font = love.graphics.newFont('Merienda-Bold.ttf', cfg.fontHeight)
    love.graphics.setFont(font)
    love.graphics.setBackgroundColor(unpack(cfg.background))
    walkAnim = newAnim(walkImage, 32, 32, cfg.walkAnimDuration)
    standAnim = newAnim(standImage, 32, 32, 1)
    gubbe = {
        x = 50,
        y = HEIGHT-cfg.groundHeight,
        dx = 1,
        anim = standAnim,
        state = 'standing'
    }
    ball = {
        x = WIDTH/2,
        y = HEIGHT/2,
        xspd = 0,
        yspd = 0
    }
end

function walkKeyDown()
    return love.keyboard.isDown('right') ~=
           love.keyboard.isDown('left')
end

function jumpKeyDown()
    return love.keyboard.isDown(' ')
end

local cfg = nil
function maybeUpdateConfig()
    cfg = tweak('config.lua', cfg, configChanged)
end

function play(snd)
    if cfg.playSounds then
        snd:stop()
        snd:play()
    end
end

function updateGubbe(dt)

    updateAnim(gubbe.anim, dt)

    if gubbe.state == 'jumping' then
        gubbe.x = gubbe.x + gubbe.dx
        gubbe.x = math.max(0, gubbe.x)
        gubbe.x = math.min(WIDTH-1, gubbe.x)
        gubbe.y = gubbe.y + gubbe.dy
        gubbe.dy = gubbe.dy + cfg.gubbe.ay
        if gubbe.y > HEIGHT-cfg.groundHeight then
            gubbe.state = 'standing'
            setAnim(gubbe, walkAnim)
        end
    end

    if gubbe.state == 'standing' then
        if jumpKeyDown() then
            gubbe.state = 'jumping'
            gubbe.dy = -cfg.gubbe.impulse
        else
            if walkKeyDown() then
                gubbe.state = 'walking'
                setAnim(gubbe, walkAnim)
                gubbe.dx = -cfg.gubbe.walkSpeed
                if love.keyboard.isDown('right') then
                    gubbe.dx = cfg.gubbe.walkSpeed
                end
            end
        end
    end

    if gubbe.state == 'walking' then
        stepSnd:play()
        if jumpKeyDown() then
            gubbe.state = 'jumping'
            gubbe.dy = -cfg.gubbe.impulse
        else
            if not walkKeyDown() then
                gubbe.state = 'standing'
                setAnim(gubbe, standAnim)
            else
                if love.keyboard.isDown('left') then
                    gubbe.dx = -cfg.gubbe.walkSpeed
                end
                if love.keyboard.isDown('right') then
                    gubbe.dx = cfg.gubbe.walkSpeed
                end
                gubbe.x = gubbe.x + gubbe.dx
                gubbe.x = math.max(0, gubbe.x)
                gubbe.x = math.min(WIDTH-1, gubbe.x)
            end
        end
    end
end

function updateBall(dt)

    local dx = gubbe.x - ball.x
    local dy = gubbe.y - ball.y
    local xDist = math.abs(dx)
    local yDist = math.abs(dy)
    local dist = math.sqrt(xDist * xDist + yDist * yDist)
    if dist < cfg.ball.radius * 2 then
        ball.xspd = ball.xspd - dx * cfg.ball.kickConst
        ball.yspd = ball.yspd + dy * cfg.ball.kickConst
        ball.x = ball.x + dx
        ball.y = ball.y - dy
        thudSnd:play()
    end

    ball.x = ball.x + ball.xspd
    if ball.x + cfg.ball.radius > WIDTH or ball.x < cfg.ball.radius then
        thudSnd:play()
        ball.xspd = -ball.xspd
        ball.x = math.max(cfg.ball.radius, ball.x)
        ball.x = math.min(WIDTH-cfg.ball.radius, ball.x)
        ball.xspd = ball.xspd * cfg.ball.deceleration
    end

    ball.y = ball.y + ball.yspd
    ball.yspd = ball.yspd + cfg.ball.yacc
    if ball.y + cfg.ball.radius > HEIGHT - cfg.groundHeight then
        ball.yspd = -ball.yspd * cfg.ball.deceleration
        ball.y = HEIGHT - cfg.groundHeight - cfg.ball.radius
        if math.abs(ball.yspd) < cfg.ball.yStickAtSpeed then
            ball.yspd = 0
        else
            thudSnd:play()
        end
    end

    if ball.yspd == 0 then
        ball.xspd = ball.xspd * cfg.ball.deceleration
    end

    if ball.y + cfg.ball.radius < cfg.groundHeight then
        ball.y = cfg.groundHeight - cfg.ball.radius
    end

end

function love.update(dt)
    maybeUpdateConfig()
    updateGubbe(dt)
    updateBall(dt)
end

math.sign = math.sign or function(x) if x == math.abs(x) then return 1 else return -1 end end

function drawEntity(e)
    local tex, quad = getFrame(e)
    love.graphics.draw(tex, quad, e.x, e.y, 0, 4 * math.sign(e.dx), 4, 16, 32)
end

function love.draw()

    local py = 0
    local print = function(txt)
        love.graphics.print(txt, 0, py)
        py = py + cfg.fontHeight
    end

    love.graphics.rectangle('fill', 0, HEIGHT-cfg.groundHeight, WIDTH, cfg.groundHeight)

    drawEntity(gubbe)

    love.graphics.draw(ballImage,
        ball.x, ball.y,
        ball.x/cfg.ball.rotdiv,
        3, 3, 
        32,
        32)

    love.graphics.setColor(255, 0, 255, 255)

    love.graphics.rectangle('fill', ball.x, ball.y, 2, 2)
    love.graphics.rectangle('fill', gubbe.x, gubbe.y, 2, 2)

    love.graphics.setColor(255, 255, 255, 255)

    print(gubbe.state)
    print(gubbe.x)
    print(gubbe.y)
end

function love.keypressed(key)
    if key == 'escape' then
        love.event.quit()
    end
end

--[[
Itches
 ball isn't an entity
 drawEntity anim function?
 make anim unaware of entities
 gubbe isn't a module
 why separate anim variables instead of table?
--]]
