local glue = require("glue")
glue.luapath(glue.bin .. "/lualib")

require("os_arch")
require("lfs")

-- local utility functions

local function aioString()
	o, a = getOS()
	if o == "windows" then
		return glue.bin .. "\\win\\aio.exe"	
	elseif o == "darwin" then
		return glue.bin .. "/mac/aio"	
	elseif o == "linux" then
		return glue.bin .. "/linux/aio"	
	end
end

local function buildFrontend()
	lfs.chdir(glue.bin .. "/../frontend")
	os.execute("npm run build")
end

local function buildBackend()
	lfs.chdir(glue.bin .. "/../backend")
	print(aioString())
	os.execute("..\\" .. aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 build --local-bin-path .")
end

local function helpText()
	print([[

shoebox build script, usage:

build <command>

command might be:
  frontend
  backend
	]])
end

-- main script

if #arg > 0 then

	if arg[1] == "frontend" then
		buildFrontend()
		os.exit(0)

	elseif arg[1] == "backend" then
		buildBackend()
		os.exit(0)
	end

	print("wrong argument to build script:", arg[1])

end

helpText()
os.exit(-1)




