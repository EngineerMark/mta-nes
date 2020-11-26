local nes = nil;
local map = nil;

function Start()
	local processor = Processor:Create();
	nes = Bus:Create(processor);

	local testCode = "A2 0A 8E 00 00 A2 03 8E 01 00 AC 00 00 A9 00 18 6D 01 00 88 D0 FA 8D 02 00 EA EA EA";
	local nOffset = 0x8000;
	local testCodeArray = split(testCode, " ");
	print(testCodeArray[2]);
	for i=1,#testCodeArray do
		nes.ram[nOffset] = tonumber(testCodeArray[i], 16);
		nOffset = nOffset+1;
	end

	nes.ram[0xFFFC] = 0x00;
	nes.ram[0xFFFD] = 0x80;

	map = nes.Processor:Disassemble(0x0000, 0xFFFF);
	--print(#map);

	nes.Processor:Reset();

	addEventHandler("onClientRender",getRootElement(),Render);
end
addEventHandler("onClientResourceStart", resourceRoot, Start);

function Render()
	if(getKeyState("space"))then
		repeat
			nes.Processor:Clock();
		until(not nes.Processor:Complete())
	end

	if(getKeyState("r"))then
		nes.Processor:Reset();
	end
	if(getKeyState("i"))then
		nes.Processor:Irq();
	end
	if(getKeyState("n"))then
		nes.Processor:Nmi();
	end

	DrawRam(10, 200, 0x0000, 16, 16);
	DrawRam(10, 400, 0x8000, 16, 16);
	DrawCode(350, 200, 26);
end

function DrawRam(x, y, nAddr, nRows, nColumns)
	local nRamX = x;
	local nRamY = y;

	for row=1,nRows do
		local sOffset = "$"..hex(nAddr,4)..": ";
		for col=1,nColumns do
			sOffset = sOffset.." "..hex(nes:Read(nAddr, true), 2);
			nAddr = nAddr+1;
		end
		dxDrawText(sOffset, nRamX, nRamY, 10000, 10000, tocolor(255,255,255,255));
		nRamY = nRamY+12;
	end
end

function DrawCode(x, y, nLines)
	local checkAddr = nes.Processor.pc;
	local it_a = map[checkAddr];
	local nLineY = bitRShift(nLines, 1)*10+y;
	if(it_a~=map[#map])then
		if(it_a~=nil)then
			dxDrawText(it_a, x, nLineY, 10000, 10000, tocolor(0, 255, 221, 255));
		end
		while(nLineY<(nLines*10)+y)do
			nLineY = nLineY+12;
			checkAddr = checkAddr+1;
			it_a = map[checkAddr];
			if(checkAddr~=#map)then
				if(it_a==nil)then
					--print(checkAddr.." is nil");
				else
					dxDrawText(it_a, x, nLineY, 10000, 10000, tocolor(255,255,255,255));
				end
			end
		end
	end

	checkAddr = nes.Processor.pc;
	it_a = map[checkAddr];
	nLineY = bitRShift(nLines, 1)*10+y;
	if(it_a~=map[#map])then
		while(nLineY>y)do
			nLineY = nLineY-12;
			checkAddr = checkAddr-1;
			it_a = map[checkAddr];
			if(checkAddr~=#map)then
				if(it_a==nil)then
					--print(checkAddr.." is nil");
				else
					dxDrawText(it_a, x, nLineY, 10000, 10000, tocolor(255,255,255,255));
				end
			end
		end
	end

end

function hex(n, d)
    return ("%0"..d.."x"):format(n);
end