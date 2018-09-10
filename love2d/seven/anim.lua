-- utility functions for animation
function newAnim(image,
	             width, height,
	             duration,
	             hotSpotX, hotSpotY)
    local animation = {}
    animation.spriteSheet = image;
    animation.quads = {};
 
    for y = 0, image:getHeight() - height, height do
        for x = 0, image:getWidth() - width, width do
            table.insert(animation.quads, love.graphics.newQuad(x, y, width, height, image:getDimensions()))
        end
    end
 
    animation.duration = duration
    animation.numFrames = #animation.quads
    animation.currentTime = 0
    animation.hotSpotX = hotSpotX
    animation.hotSpotY = hotSpotY
 
    return animation
end

function setAnim(entity, anim)
    entity.anim = anim
    anim.currentTime = 0
end

function updateAnim(anim, dt)
    anim.currentTime = anim.currentTime + dt
    if anim.currentTime >= anim.duration then
        anim.currentTime = anim.currentTime - anim.duration
    end
end

function getFrame(entity)
    local anim = entity.anim
    local spriteNum = math.floor(anim.currentTime / anim.duration * anim.numFrames) + 1
    local tex = anim.spriteSheet
    local quad = anim.quads[spriteNum]
    return tex, quad
end
