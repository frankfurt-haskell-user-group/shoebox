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

local function osSep()
	o, a = getOS()
	if o == "windows" then
		return "\\"	
	elseif o == "darwin" then
		return "/"	
	elseif o == "linux" then
		return "/"	
	end
end

local function buildFrontend()
	lfs.chdir(glue.bin .. "/../frontend")
--	os.execute("bower update")
	os.execute("npm run build")
end

local function initFrontend()
	lfs.chdir(glue.bin .. "/../frontend")
	os.execute("bower update")
end

local function buildBackend()
	lfs.chdir(glue.bin .. "/../backend")
	os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 install --local-bin-path .")
end

local function testBackend()
	lfs.chdir(glue.bin .. "/../backend")
	os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 test")
end

local function initBackend()
	lfs.chdir(glue.bin .. "/../backend")
	os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Stack.0617 setup --resolver lts-8.20")
end

local function runApp()
	lfs.chdir(glue.bin .. "/..")
	os.execute("electron frontend" .. osSep() .. "dist" .. osSep() .. "index.html")
end

local function helpText()
	print([[

shoebox build script, usage:

build <command>

command might be:
  frontend
  frontend-init
  backend
  backend-test
  backend-init
  run
	]])
end

-- main script

if #arg > 0 then


	if arg[1] == "frontend" then
		buildFrontend()
		os.exit(0)

	elseif arg[1] == "frontend-init" then
		initFrontend()
		os.exit(0)

	elseif arg[1] == "backend-init" then
		initBackend()
		os.exit(0)

	elseif arg[1] == "backend" then
		buildBackend()
		os.exit(0)

	elseif arg[1] == "backend-test" then
		testBackend()
		os.exit(0)
		
	elseif arg[1] == "run" then
		runApp()
		os.exit(0)

	end


	print("wrong argument to build script:", arg[1])

end

helpText()
os.exit(-1)




