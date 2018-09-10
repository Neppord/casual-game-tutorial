function tweak(fname, cfg, callback)
    if not cfg then
        cfg = {}
    end

    local lastModified = love.filesystem.getLastModified(fname)
    if cfg.changedTime ~= lastModified then
        cfg = love.filesystem.load(fname)()
        cfg.changedTime = lastModified
        print('Loading ' .. fname .. ' modified at ' ..
              os.date("%c", cfg.changedTime))
        callback(cfg)
    end

    return cfg
end
