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
	o, a = getOS()
	lfs.chdir(glue.bin .. "/../frontend")
--	os.execute("bower update")
	if o == "windows" then
		os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Node.0817 cmd /C npm run build")
	else
		os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Node.0817 bash -c \"npm run build\"")
	end
end

local function initFrontend()
	o, a = getOS()
	lfs.chdir(glue.bin .. "/../frontend")
	if o == "windows" then
		os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Node.0817 cmd /C npm install")
	else
		os.execute(".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Node.0817 bash -c \"npm install\"")
	end
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
	lfs.chdir(glue.bin .. "/../frontend")

	o, a = getOS()
	if o == "windows" then
		os.execute("xcopy /Y /Q ..\\data dist\\data\\")
		os.execute("xcopy /Y /Q ..\\backend\\shoeB.exe dist\\backend\\")
        else
		os.execute("cp -R ../data dist/data")
		os.execute("mkdir -p dist/backend")
		os.execute("cp ../backend/shoeB dist/backend")
	end

	-- execute electron
	lfs.chdir("dist")
	os.execute(".." .. osSep() .. ".." .. osSep() .. aioString() .. " http://www.hgamer3d.org/tools/Electron.0817 index.html")
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




